(in-package :cl-user)
(defpackage quickdocs-updater.http
  (:use :cl)
  (:import-from :alexandria
                :once-only
                :with-gensyms)
  (:export :*user-agent*
           :send-get
           :with-retry))
(in-package :quickdocs-updater.http)

(defparameter *user-agent*
  (format nil "Quickdocs-Updater/~A (http://quickdocs.org)"
          (asdf:component-version (asdf:find-system :quickdocs-updater))))

(defmacro with-retry (count &body body)
  (with-gensyms (retry return-block)
    (once-only (count)
      `(block ,return-block
         (tagbody
            ,retry
            (handler-bind ((dex:http-request-not-found #'dex:ignore-and-continue)
                           (dex:http-request-failed (dex:retry-request ,count)))
              (return-from ,return-block
                (progn ,@body))))))))

(defun send-get (url &key (timeout 60) basic-auth)
  (dex:get url
           :headers `(("User-Agent" . ,*user-agent*))
           :basic-auth basic-auth
           :timeout timeout))
