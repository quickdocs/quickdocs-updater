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
  (merge-pathnames ql-dist-version *extract-dists-directory*))

(defun extract-result-of-release (release)
  (merge-pathnames (format nil "releases/~A" (ql-dist:name release))
                   (extract-result-directory (ql-dist:dist release))))

(defun extractedp (dist)
  (uiop:directory-exists-p (extract-result-directory dist)))

(defun release-info (release)
  (check-type release ql-dist:release)
  (let ((result-file (extract-result-of-release release)))
    (unless (uiop:file-exists-p result-file)
      (restart-case
          (error "Extracted release info of ~S does not exist." (ql-dist:name release))
        ;; For testing mainly.
        (extract-now ()
          :report "Extract it now in the current thread"
          (return-from release-info
            (getf (quickdocs-extracter:serialize-release
                   (ql-dist:name release)
                   (ql-dist:dist release))
                  :systems)))))
    (with-open-file (in result-file)
      (uiop:with-safe-io-syntax ()
        (loop with eof = '#:eof
              for form = (read in nil eof)
              until (eq form eof)
              collect form)))))
