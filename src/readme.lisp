(in-package :cl-user)
(defpackage quickdocs-updater.readme
  (:use :cl)
  (:import-from :quickdocs-updater.release
                :readme-file)
  (:import-from :alexandria
                :ignore-some-conditions)
  (:export :convert-readme))
(in-package :quickdocs-updater.readme)

(defparameter *timeout-command*
  #+darwin "gtimeout" ;; of coreutils
  #-darwin "timeout")

(defparameter *pandoc-path* (or (uiop:getenv "PANDOC_PATH")
                                "pandoc"))

(defun which (command)
  (handler-case
      (let* ((result (with-output-to-string (s)
                       (uiop:run-program `("which" ,command)
                                         :output s
                                         :error-output *error-output*)))
             (newline-pos
               (position-if (lambda (char)
                              (or (char= char #\Newline)
                                  (char= char #\Return)))
                            result)))
        (if newline-pos
            (subseq result 0 newline-pos)
            result))
    (uiop:subprocess-error ()
      nil)))

(defun check-if-command-installed (&rest commands)
  (dolist (command commands)
    (or (which command)
        (error "Requiement ~S does not exist. Ensure if it's installed and check your PATH." command))))

(defun pandoc (file &key (from "markdown") (to "html"))
  (check-type file pathname)
  (assert (uiop:file-exists-p file))
  (check-if-command-installed *timeout-command* *pandoc-path*)
  (with-output-to-string (s)
    (uiop:run-program `(,*timeout-command* "10" ,*pandoc-path* "-f" ,from "-t" ,to ,(namestring (probe-file file)))
                      :force-shell t
                      :output s
                      :error-output *error-output*)))

(defun convert-readme (file)
  (check-type file pathname)
  (let ((type (pathname-type file)))
    (ignore-some-conditions (uiop:subprocess-error)
      (cond
        ((or (string= type "markdown")
             (string= type "md"))
         (pandoc file :from "markdown"))
        ((string= type "org")
         (pandoc file :from "org"))))))
