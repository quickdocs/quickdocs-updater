(in-package :cl-user)
(defpackage quickdocs-updater.cliki
  (:use :cl)
  (:import-from :quri
                :url-encode)
  (:export :cliki-project-info))
(in-package :quickdocs-updater.cliki)

(defun project-page-url (project-name)
  (format nil "http://cliki.net/~A"
          (quri:url-encode project-name :encoding :utf-8)))

(defun retrieve-cliki-project-page (project-name)
  (let ((try 0))
    (block nil
      (tagbody
       retry
         (handler-bind ((dex:http-request-failed
                          (lambda (e)
                            (case (dex:response-status e)
                              (404 (return nil))
                              (otherwise
                               (when (<= try 5)
                                 (sleep (* 3 (incf try)))
                                 (go retry)))))))
           (return
             (dex:get (project-page-url project-name)
                      :timeout 60)))))))

(defun cliki-project-info (project-name)
  (let ((html (retrieve-project-page project-name)))
    (when html
      (values
       (parse-description html)
       (parse-categories html)))))


;;
;; Description

(defun parse-description (html)
  (let ((article (plump:get-element-by-id (plump:parse html) "article")))
    (when article
      (plump:text article))))


;;
;; Category

(defun parse-categories (html)
  (map 'list
       #'plump:text
       (clss:select "#article .category" (plump:parse html))))
