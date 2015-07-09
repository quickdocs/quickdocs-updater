(in-package :cl-user)
(defpackage quickdocs-updater.http
  (:use :cl)
  (:export :*user-agent*
           :send-get))
(in-package :quickdocs-updater.http)

(defparameter *user-agent*
  (format nil "Quickdocs-Updater/~A (http://quickdocs.org)"
          (asdf:component-version (asdf:find-system :quickdocs-updater))))

(defun send-get (url)
  (dex:get url
           :headers `(("User-Agent" . ,*user-agent*))
           :timeout 60))
