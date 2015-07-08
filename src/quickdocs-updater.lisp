(in-package :cl-user)
(defpackage quickdocs-updater
  (:use :cl
        :quickdocs-database
        :sxql)
  (:import-from :quickdocs-updater.extracter
                :run-extract-dist
                :release-info)
  (:import-from :quickdocs-updater.release
                :release-repos-url)
  (:import-from :quickdocs-updater.readme 
                :pandoc)
  (:import-from :datafly
                :retrieve-one))
(in-package :quickdocs-updater)

(defun update-dist (dist)
  (check-type dist ql-dist:dist)
  ;; Extract dist
  (run-extract-dist dist)
  ;; Update database
  (dolist (release (ql-dist:provided-releases dist))
    (let (project)
      ;; Update project
      (setf project (update-release release))
      ;; Update dependencies
      (flet ((retrieve-system (system-name)
               (retrieve-one
                (select :*
                  (from :system)
                  (where (:and (:= :name system-name)
                               (:= :project_id (project-id project))))))))
        (dolist (system (getf (release-info release) :systems))
          (dolist (depends-system-name (append (getf system :depends-on)
                                               (getf system :defsystem-depends-on)))
            (let ((system (retrieve-system (getf system :name)))
                  (depends-system (retrieve-system depends-system-name)))
              (create-dependency (system-id system) (system-id depends-system))))))

      ;; TODO: cliki
      )))

(defun update-release (release &aux (release-info (release-info release)))
  (check-type release ql-dist:release)
  (let ((project
          (create-project :ql-dist-version (ql-dist:version (ql-dist:dist release))
                          :name (getf release-info :name)
                          :release-version (getf release-info :release-version)
                          :repos-url (release-repos-url release)
                          :archive-url (ql-dist:archive-url release)
                          :project-readme (make-project-readme
                                           :filename (getf release-info :readme-file)
                                           :raw (getf release-info :readme)
                                           :converted (pandoc (make-string-input-stream (getf release-info :readme)))))))
    (dolist (system-info (getf release-info :systems))
      (create-system :project-id (project-id project)
                     :name (getf system-info :name)
                     :version (getf system-info :version)
                     :description (getf system-info :description)
                     :long-description (getf system-info :long-description)
                     :license (getf system-info :license)
                     :homepage-url (getf system-info :homepage)
                     :authors (list (getf system-info :author))
                     :maintainers (list (getf system-info :maintainer))))
    project))
