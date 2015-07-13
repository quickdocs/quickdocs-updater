(in-package :cl-user)
(defpackage quickdocs-updater.extracter
  (:use :cl)
  (:export :*extract-dists-directory*
           :extract-result-directory
           :extractedp
           :release-info)
  (:documentation "Functions for controlling quickdocs-extracter"))
(in-package :quickdocs-updater.extracter)

(defparameter *extract-dists-directory*
  (asdf:system-relative-pathname :quickdocs-updater #P"dists/"))

(defun extract-result-directory (ql-dist-version)
  (check-type ql-dist-version string)
  (uiop:ensure-directory-pathname (merge-pathnames ql-dist-version *extract-dists-directory*)))

(defun extract-result-of-release (release ql-dist-version)
  (merge-pathnames (format nil "releases/~A" release)
                   (extract-result-directory ql-dist-version)))

(defun extractedp (dist)
  (uiop:directory-exists-p (extract-result-directory dist)))

(defun release-info (release ql-dist-version)
  (check-type release string)
  (let ((result-file (extract-result-of-release release ql-dist-version)))
    (unless (uiop:file-exists-p result-file)
      (error "Extracted release info of ~S does not exist at '~A'." release
             result-file))
    (ignore-errors (uiop:read-file-form result-file))))
