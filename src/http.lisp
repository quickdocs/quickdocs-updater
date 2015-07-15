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
                                      (go ,retry))))))
                             (usocket:ns-error
                               (lambda (e)
                                 (when (< ,try ,retries)
                                   (let ((wait (* 10 (incf ,try))))
                                     (format *error-output* "~&~A~2%Waiting ~D seconds till retrying...~%"
                                             e
                                             wait)
                                     (sleep wait)
                                     (go ,retry))))))
                (return-from ,return-block
                  (progn ,@body)))))))))

(defun send-get (url &key (timeout 60) retries)
  (with-retry (or retries 0)
    (dex:get url
             :headers `(("User-Agent" . ,*user-agent*))
             :timeout timeout)))
