(in-package :cl-user)
(defpackage quickdocs-updater.quicklisp-blog
  (:use :cl
        :sxql)
  (:import-from :datafly
                :execute)
  (:export :latest-download-stats
           :update-download-stats))
(in-package :quickdocs-updater.quicklisp-blog)

(defvar *rss-url*
  "http://blog.quicklisp.org/feeds/posts/default")

(defun retrieve-entries (&optional (url *rss-url*))
  (let* ((body (babel:octets-to-string (dex:get url :force-binary t)))
         (root (plump:parse body)))
    (values (clss:select "entry" root)
            (let ((next (clss:select "link[rel=\"next\"]" root)))
              (when (and next
                         (/= 0 (length next)))
                (plump:get-attribute (aref next 0) "href"))))))

(defun find-latest-download-stats-entry ()
  (loop with url = *rss-url*
        do (multiple-value-bind (entries next)
               (retrieve-entries url)
             (let ((entry
                     (find-if (lambda (entry)
                                (ppcre:scan "download stats" (plump:text (aref (clss:select "title" entry) 0))))
                              entries)))
               (cond
                 (entry (return entry))
                 (next (setf url next))
                 (t (return nil)))))))

(defun latest-download-stats ()
  (let ((entry (find-latest-download-stats-entry)))
    (unless entry
      (return-from latest-download-stats))

    (let ((content (plump:parse (plump:text (aref (clss:select "content" entry) 0)))))
      (values
       (map 'list
            (lambda (node)
              (let ((match
                        (nth-value 1
                                   (ppcre:scan-to-strings "^\\s*(\\d+?)\\s+(.+?)\\s*$"
                                                          (plump:text node)))))
                (cons (aref match 1)
                      (parse-integer (aref match 0)))))
            (remove-if-not #'plump:text-node-p
                           (plump:children (aref (clss:select "pre" content) 0))))
       (local-time:timestamp-to-universal
        (local-time:parse-timestring
         (plump:text (aref (clss:select "published" entry) 0))))))))

(defun update-download-stats ()
  (format *error-output*
          "~&Updating Quicklisp download stats...~%")
  (execute
   (delete-from :quicklisp_download_stats))
  (multiple-value-bind (result updated)
      (latest-download-stats)
    (format *error-output*
            "~&Found new download stats (~A).~%Updating database...~%"
            (local-time:universal-to-timestamp updated))
    (loop for (name . count) in result
          do (execute
              (insert-into
                  :quicklisp_download_stats
                (set= :project_name name
                      :download_count count))))
    (format *error-output* "~&Done.~%")))
