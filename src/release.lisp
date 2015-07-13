(in-package :cl-user)
(defpackage quickdocs-updater.release
  (:use :cl
        :split-sequence)
  (:import-from :quickdocs-updater.extracter
                :extractedp
                :extract-result-directory
                :release-info)
  (:import-from :quickdocs-updater.http
                :send-get)
  (:import-from :alexandria
                :when-let
                :starts-with-subseq
                :ignore-some-conditions)
  (:export :release-version
           :primary-system
           :release-repos-url
           :ql-dist-releases
           :ql-release-archive-url
           :ql-release-provided-systems)
  (:documentation "Function/method collections for Quicklisp releases."))
(in-package :quickdocs-updater.release)

(defun retrieve-dist-txt (ql-dist-version filename)
  (send-get (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/~A"
                    ql-dist-version
                    filename)))

(defun lines (content)
  (loop for line in (split-sequence #\Newline content)
        when (and (not (= (length line) 0))
                  (not (char= (aref line 0) #\#)))
          collect line))

(defun line-columns (line)
  (split-sequence #\Space line))

(defun ql-dist-releases (ql-dist-version)
  (let ((releases.txt (retrieve-dist-txt ql-dist-version "releases.txt")))
    (loop for line in (lines releases.txt)
          collect (first (line-columns line)))))

(defun ql-release-archive-url (release ql-dist-version)
  (let ((releases.txt (retrieve-dist-txt ql-dist-version "releases.txt")))
    (loop for line in (lines releases.txt)
          do (destructuring-bind (project archive-url &rest args)
                 (line-columns line)
               (declare (ignore args))
               (when (string= project release)
                 (return archive-url))))))

(defun ql-release-provided-systems (release ql-dist-version)
  (let ((systems.txt (retrieve-dist-txt ql-dist-version "systems.txt")))
    (loop for line in (lines systems.txt)
          append
          (destructuring-bind (project system-file system-name &rest args)
              (line-columns line)
            (declare (ignore system-file args))
            (if (string= project release)
                (list system-name)
                '())))))

(defun release-version (release ql-dist-version)
  "Return the latest Quicklisp dist version that `release` was updated."
  (check-type release string)
  (when-let (match (nth-value 1
                              (ppcre:scan-to-strings "beta\\.quicklisp\\.org/archive/[^/]+/([^/]+)"
                                                     (ql-release-archive-url release ql-dist-version))))
    (aref match 0)))

(defun primary-system (release ql-dist-version)
  (check-type release string)
  (flet ((remove-cl-prefix (name)
           (if (starts-with-subseq "cl-" name)
               (subseq name 3)
               name)))
    (let ((project-name (remove-cl-prefix release))
          (provided-systems (ql-release-provided-systems release ql-dist-version)))
      (or (find project-name
                provided-systems
                :key #'remove-cl-prefix
                :test #'string=)
          (first provided-systems)))))

;; NOTE: quicklisp-projects is updated every month and it could be different from the `release' has been included.
;;   It would be a problem when updating old dists.
(defun project-source-txt (release)
  (check-type release string)
  (merge-pathnames (format nil "~A/source.txt" release)
                   (asdf:system-relative-pathname :quickdocs-updater
                                                  #P"modules/quicklisp-projects/")))

(defun project-source-info (release)
  (check-type release string)
  (let ((data (uiop:read-file-string (project-source-txt release))))
    (destructuring-bind (type source-url)
        (split-sequence #\Space data :count 2)
      (values type source-url))))

(defun release-repos-url (release)
  (check-type release string)
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
           (let ((match (nth-value 1 (ppcre:scan-to-strings "://common-lisp\\.net/project/([^\\/]+)" source-url))))
             (format nil "http://common-lisp.net/project/~A"
                     (quri:url-encode
                      (if match
                          (aref match 0)
                          release)
                      :encoding :utf-8))))
          ((or (string= domain "weitz.de")
               (string= type "ediware-http"))
           (format nil "http://weitz.de/~A/"
                   (quri:url-encode release :encoding :utf-8))))))))
