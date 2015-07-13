(in-package :cl-user)
(defpackage quickdocs-updater.http
  (:use :cl)
  (:import-from :alexandria
                :once-only
                :with-gensyms)
  (:export :*user-agent*
           :send-get))
(in-package :quickdocs-updater.http)

(defparameter *user-agent*
  (format nil "Quickdocs-Updater/~A (http://quickdocs.org)"
          (asdf:component-version (asdf:find-system :quickdocs-updater))))

(defun send-get (url &key (timeout 60) retries)
  (with-retry (or retries 0)
    (dex:get url
             :headers `(("User-Agent" . ,*user-agent*))
             :timeout timeout)))

(defmacro with-retry (retries &body body)
  (with-gensyms (try retry return-block)
    (once-only (retries)
      `(let ((,try 0))
         (block ,return-block
           (tagbody
              ,retry
              (handler-bind ((dex:http-request-failed
                               (lambda (e)
                                 (case (dex:response-status e)
                                   (404 (return-from ,return-block nil))
                                   (otherwise
                                    (when (< ,try ,retries)
                                      (sleep (* 3 (incf ,try)))
                                      (go ,retry)))))))
                (return-from ,return-block
                  (progn ,@body)))))))))
