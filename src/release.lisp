(in-package :cl-user)
(defpackage quickdocs-updater.release
  (:use :cl
        :split-sequence)
  (:import-from :quickdocs-updater.extracter
                :extractedp
                :extract-result-directory
                :release-info)
  (:import-from :alexandria
                :when-let
                :starts-with-subseq
                :compose
                :ignore-some-conditions)
  (:export :release-version
           :primary-system
           :release-repos-url)
  (:documentation "Function/method collections for Quicklisp releases."))
(in-package :quickdocs-updater.release)

(defun release-version (release)
  "Return the latest Quicklisp dist version that `release` was updated."
  (check-type release ql-dist:release)
  (when-let (match (nth-value 1
                              (ppcre:scan-to-strings "beta\\.quicklisp\\.org/archive/[^/]+/([^/]+)"
                                                     (ql-dist:archive-url release))))
    (aref match 0)))

(defun primary-system (release)
  (check-type release ql-dist:release)
  (flet ((remove-cl-prefix (name)
           (if (starts-with-subseq "cl-" name)
               (subseq name 3)
               name)))
    (let ((project-name (remove-cl-prefix (ql-dist:project-name release)))
          (provided-systems (ql-dist:provided-systems release)))
      (or (find project-name
                provided-systems
                :key (compose #'remove-cl-prefix #'ql-dist:name)
                :test #'string=)
          (first provided-systems)))))

;; NOTE: quicklisp-projects is updated every month and it could be different from the `release' has been included.
;;   It would be a problem when updating old dists.
(defun project-source-txt (release)
  (check-type release ql-dist:release)
  (merge-pathnames (format nil "~A/source.txt" (ql-dist:name release))
                   (asdf:system-relative-pathname :quickdocs-database
                                                  #P"modules/quicklisp-projects/")))

(defun project-source-info (release)
  (check-type release ql-dist:release)
  (let ((data (uiop:read-file-string (project-source-txt release))))
    (destructuring-bind (type source-url)
        (split-sequence #\Space data :count 2)
      (values type source-url))))

(defun release-repos-url (release)
  (check-type release ql-dist:release)
  (multiple-value-bind (type source-url)
      (project-source-info release)
    (ignore-some-conditions (quri:uri-error)
      (let* ((uri (quri:uri source-url))
             (domain (quri:uri-domain uri)))
        (cond
          ((string= domain "github.com")
           (let ((repos-id (ppcre:regex-replace "\\.[^\\.]*$" (quri:uri-path uri) "")))
             ;; TODO: request website URL to GitHub
             (concatenate 'string
                          "https://github.com"
                          repos-id)))
          ((string= domain "bitbucket.org")
           ;; TODO: request website URL to BitBucket
           source-url)
          ((string= domain "gitlab.common-lisp.net")
           (let ((repos-id (ppcre:regex-replace "\\.[^\\.]*$" (quri:uri-path uri) "")))
             (concatenate 'string
                          "http://gitlab.common-lisp.net"
                          repos-id)))
          ((string= domain "common-lisp.net")
           (let ((match (ppcre:scan-to-strings "://common-lisp\\.net/project/([^\\/]+)" source-url)))
             (format nil "http://common-lisp.net/project/~A"
                     (quri:url-encode
                      (if match
                          (aref match 0)
                          (ql-dist:project-name release))
                      :encoding :utf-8))))
          ((or (string= domain "weitz.de")
               (string= type "ediware-http"))
           (format nil "http://weitz.de/~A/"
                   (quri:url-encode (ql-dist:project-name release) :encoding :utf-8))))))))
