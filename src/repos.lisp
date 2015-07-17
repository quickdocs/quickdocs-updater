(in-package :cl-user)
(defpackage quickdocs-updater.repos
  (:use :cl)
  (:import-from :quickdocs-updater.http
                :send-get
                :with-retry)
  (:export :*github-access-token*
           :repos-info))
(in-package :quickdocs-updater.repos)

(defvar *github-access-token* nil)

(defun github-api-headers ()
  (if *github-access-token*
      `(("Authorization" . ,(format nil "Basic ~A"
                                    (string-to-base64-string (format nil)))))))

(defun github-repos-info (repos-id)
  (let* ((url (format nil "https://api.github.com/repos/~A" repos-id))
         (body (send-get url :basic-auth (and *github-access-token*
                                              `(,*github-access-token* . "x-oauth-basic")))))
    (jonathan:parse (babel:octets-to-string body) :as :alist)))

;; version 2.0 is recommended, however we use 1.0 because it doesn't include "website"
(defun bitbucket-repos-info (repos-id &key (version 1.0))
  (let* ((url (format nil "https://api.bitbucket.org/~A/repositories/~A" version repos-id))
         (body (send-get url)))
    (jonathan:parse (babel:octets-to-string body) :as :alist)))

(defun utc-date-to-timestamp (date)
  (let ((match
            (nth-value
             1
             (ppcre:scan-to-strings "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})\\+00:00"
                                    date))))
    (when match
      (destructuring-bind (year mon day hour min sec)
          (coerce match 'list)
        (local-time:encode-timestamp 0
                                     (parse-integer sec)
                                     (parse-integer min)
                                     (parse-integer hour)
                                     (parse-integer day)
                                     (parse-integer mon)
                                     (parse-integer year)
                                     :timezone local-time:+utc-zone+)))))

(defun repos-info (type repos-id)
  (flet ((assocdr (key info)
           (cdr (assoc key info :test #'string=))))
    (handler-bind ((dex:http-request-not-found
                     (lambda (e)
                       (warn "~S is not found at ~S."
                             repos-id
                             (quri:render-uri (dex:request-uri e)))
                       (return-from repos-info nil)))
                   (dex:http-request-failed (dex:retry-request 5)))
      (ecase type
        (:github
         (let ((info (github-repos-info repos-id)))
           (when info
             (list :type :github
                   :repos-id repos-id
                   :description  (assocdr "description" info)
                   :homepage-url (assocdr "homepage" info)
                   :watch-count  (assocdr "watchers_count" info)
                   :forks-count  (assocdr "forks" info)
                   :stars-count  (assocdr "stargazers_count" info)
                   :created-at   (local-time:timestamp-to-universal
                                  (local-time:parse-timestring (assocdr "created_at" info)))
                   :updated-at   (local-time:timestamp-to-universal
                                  (local-time:parse-timestring (assocdr "updated_at" info)))))))
        (:bitbucket
         (let ((info (bitbucket-repos-info repos-id)))
           (when info
             (list :type :bitbucket
                   :repos-id repos-id
                   :description  (assocdr "description" info)
                   :homepage-url (let ((website (assocdr "website" info)))
                                   (if (= 0 (length website))
                                       nil
                                       website))
                   :watch-count  (assocdr "followers_count" info)
                   :forks-count  (assocdr "forks_count" info)
                   :stars-count  nil
                   :created-at   (local-time:timestamp-to-universal
                                  (utc-date-to-timestamp (assocdr "utc_created_on" info)))
                   :updated-at   (local-time:timestamp-to-universal
                                  (utc-date-to-timestamp (assocdr "utc_last_updated" info)))))))))))
