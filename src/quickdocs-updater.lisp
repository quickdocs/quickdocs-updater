(in-package :cl-user)
(defpackage quickdocs-updater
  (:use :cl
        :quickdocs-database
        :split-sequence
        :sxql)
  (:import-from :quickdocs-updater.extracter
                :release-info)
  (:import-from :quickdocs-updater.release
                :release-repos-url
                :ql-dist-releases
                :ql-release-archive-url)
  (:import-from :quickdocs-updater.readme 
                :convert-readme)
  (:import-from :quickdocs-updater.cliki
                :updated-cliki-project-info)
  (:import-from :quickdocs-updater.http
                :send-get)
  (:import-from :datafly
                :retrieve-one
                :retrieve-one-value
                :execute)
  (:import-from :alexandria
                :ensure-list)
  (:export :update-dist
           :update-release))
(in-package :quickdocs-updater)

(defun update-dist (ql-dist-version)
  (check-type ql-dist-version string)
  (execute
   (delete-from :project (where (:= :ql_dist_version ql-dist-version))))
  ;; Update database
  (let ((releases (ql-dist-releases ql-dist-version)))
    ;; Update 'project' and 'system' tables
    (format *error-output* "~&Updating 'project' and 'system'...~%")
    (dolist (release releases)
      (update-release release ql-dist-version))

    (flet ((retrieve-system (system-name project-id)
             (retrieve-one
              (select :*
                (from :system)
                (where (:and (:= :name system-name)
                             (:= :project_id project-id)))
                (limit 1))
              :as 'quickdocs-database:system))
           (retrieve-project (project-name)
             (retrieve-one
              (select :*
                (from :project)
                (where (:and (:= :ql_dist_version ql-dist-version)
                             (:= :name project-name)))
                (limit 1))
              :as 'quickdocs-database:project)))

      ;; Update dependencies
      (format *error-output* "~&Updating dependencies...~%")
      (dolist (release releases)
        (let ((project (retrieve-project release)))
          (dolist (system-info (getf (release-info release ql-dist-version) :systems))
            (flet ((create (system-name depends-system-name &optional is-for-defsystem)
                     (let ((system (retrieve-system system-name (project-id project)))
                           (depends-system (retrieve-system depends-system-name (project-id project))))
                       (cond
                         ((and system depends-system)
                          (create-dependency (system-id system) (system-id depends-system)
                                             :is-for-defsystem is-for-defsystem))
                         (depends-system
                          (warn "~S doesn't exist in DB. Probably it was failed to update. Ignoring."
                                system-name))
                         (system
                          (warn "~S which ~S depends on doesn't exist. Ignoring."
                                depends-system-name
                                system-name))))))
              (dolist (depends-system-name (getf system-info :depends-on))
                (create (getf system-info :name) depends-system-name))
              (dolist (depends-system-name (getf system-info :defsystem-depends-on))
                (create (getf system-info :name) depends-system-name t))))))

      ;; Retrieve description and categories from cliki and update DB.
      (format *error-output* "~&Retrieving description and categories from CLiki...~%")
      (dolist (release releases)
        (let ((project (retrieve-project release)))
          (when project
            (format *error-output* "~&Retrieving ~S..." release)
            (let ((updated-at (retrieve-one-value
                               (select :updated_at
                                 (from :cliki)
                                 (where (:= :project_name release))
                                 (limit 1))
                               :updated-at)))
              (multiple-value-bind (cliki-info successp)
                  (updated-cliki-project-info release updated-at)
                (if successp
                    (when cliki-info ;; means if the page is new or updated
                      (if updated-at
                          (execute
                           (update :cliki
                             (set= :body (getf cliki-info :body)
                                   :updated_at (getf cliki-info :updated-at))
                             (where (:= :project_name release))))
                          (execute
                           (insert-into :cliki
                             (set= :project_name release
                                   :body (getf cliki-info :body)
                                   :updated_at (getf cliki-info :updated-at)))))
                      (execute
                       (delete-from :cliki_project_category
                         (where (:= :project_name release))))
                      (dolist (category (getf cliki-info :categories))
                        (execute
                         (insert-into :project_category
                           (set= :project_name release
                                 :category category)))))
                    (princ "none"))))
            (fresh-line)
            (sleep 3))))

      (format *error-output* "~&Done.~%")))
  t)

(defun update-release (release ql-dist-version &aux (release-info (release-info release ql-dist-version)))
  (unless release-info
    ;; TODO: Failed release should also be stored in database somehow.
    (warn "The extracted info of ~S cannot be read. Skipping." release)
    (return-from update-release))
  (let ((project
          (create-project :ql-dist-version ql-dist-version
                          :name (getf release-info :name)
                          :release-version (getf release-info :release-version)
                          :repos-url (release-repos-url release)
                          :archive-url (ql-release-archive-url release ql-dist-version)
                          :project-readme (when (getf release-info :readme-file)
                                            (make-project-readme
                                             :filename (getf release-info :readme-file)
                                             :raw (getf release-info :readme)
                                             :converted (convert-readme (make-string-input-stream (getf release-info :readme))
                                                                        (first
                                                                         (split-sequence #\.
                                                                                         (getf release-info :readme-file)
                                                                                         :from-end t
                                                                                         :count 1))))))))
    (dolist (system-info (getf release-info :systems))
      (let ((system
              (create-system :project-id (project-id project)
                             :name (getf system-info :name)
                             :version (getf system-info :version)
                             :description (getf system-info :description)
                             :long-description (getf system-info :long-description)
                             :license (getf system-info :license)
                             :homepage-url (getf system-info :homepage)
                             :authors (ensure-list (getf system-info :author))
                             :maintainers (ensure-list (getf system-info :maintainer)))))
        (create-system-packages (system-id system)
                                (getf system-info :packages)
                                :failed (getf system-info :failed)
                                :error-log (getf system-info :error-log))))
    project))
