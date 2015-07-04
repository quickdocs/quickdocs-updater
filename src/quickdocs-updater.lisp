(in-package :cl-user)
(defpackage quickdocs-updater
  (:use :cl)
  (:import-from :quickdocs-updater.extracter
                :run-extract-dist))
(in-package :quickdocs-updater)

(defun update-dist (dist)
  (check-type dist ql-dist:dist)
  (run-extract-dist dist)
  (dolist (release (ql-dist:provided-releases dist))
    (update-release release)))

(defun update-release (release)
  (check-type release ql-dist:release))
