(in-package :cl-user)
(defpackage quickdocs-updater.cliki
  (:use :cl)
  (:import-from :quickdocs-updater.http
                :send-get)
  (:import-from :quri
                :url-encode)
  (:export :cliki-project-info))
(in-package :quickdocs-updater.cliki)

(defun project-page-url (project-name)
  (format nil "http://cliki.net/~A"
          (quri:url-encode project-name :encoding :utf-8)))

(defun project-history-url (project-name)
  (format nil "http://cliki.net/site/history?article=~A"
          (quri:url-encode project-name :encoding :utf-8)))

(defun retrieve-cliki-project-page (project-name)
  (send-get (project-page-url project-name) :retries 5))

(defun retrieve-cliki-project-history (project-name)
  (handler-bind ((dex:http-request-failed
                   (lambda (e)
                     (when (= (dex:response-status e) 500)
                       (return-from retrieve-cliki-project-history nil)))))
    (send-get (project-history-url project-name) :retries 5)))

(defun cliki-project-last-updated-at (project-name)
  (let ((body (retrieve-cliki-project-history project-name)))
    (when body
      (let ((elem (clss:select "#pagehistory tr:first-child td:nth-child(4) [name=\"undo-revision\"]"
                    (plump:parse body))))
        (unless (= (length elem) 0)
          (let ((last-updated-at (plump:attribute (aref elem 0) "value")))
            (handler-case (parse-integer last-updated-at)
              (error (e)
                (warn "Failed to detect the last updated time of ~S due to the following error:~%  ~A"
                      project-name e)
                nil))))))))

(defun cliki-project-info (project-name)
  (let ((html (retrieve-cliki-project-page project-name)))
    (when html
      (list :name project-name
            :body (parse-article html)
            :categories (parse-categories html)))))

(defun updated-cliki-project-info (project-name &optional (if-updated-since 0))
  (when (null if-updated-since)
    (setf if-updated-since 0))
  (block nil
    (let ((last-updated-at (cliki-project-last-updated-at project-name)))
      (unless last-updated-at
        ;; In case that the page has been deleted
        (return (values nil nil)))
      (when (< last-updated-at if-updated-since)
        ;; Not modified
        (return (values nil t)))
      (values
       (append (cliki-project-info project-name)
               (list :updated-at last-updated-at))
       t))))


;;
;; Description

(defun parse-article (html)
  (let ((article (plump:get-element-by-id (plump:parse html) "article")))
    (when article
      (plump:text article))))


;;
;; Category

(defun parse-categories (html)
  (delete-duplicates
   (map 'list
        #'plump:text
        (clss:select "#article .category" (plump:parse html)))
   :test #'string=
   :from-end t))
