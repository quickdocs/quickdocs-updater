(in-package :cl-user)
(defpackage quickdocs-updater.readme
  (:use :cl)
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
        (cerror "Ignore and continue"
                "Requirement ~S does not exist. Ensure if it's installed and check your PATH." command))))

(defun pandoc (input &key (from "markdown") (to "html"))
  (check-type input (or pathname stream))
  (check-if-command-installed *timeout-command*)
  (check-if-command-installed *pandoc-path*)
  (with-output-to-string (s)
    (let ((in (if (streamp input)
                  input
                  (open input))))
      (unwind-protect
           (uiop:run-program `(,@(if (which *timeout-command*)
                                     `(,*timeout-command* "10")
                                     '())
                               ,*pandoc-path* "-f" ,from "-t" ,to)
                             :force-shell t
                             :input in
                             :output s
                             :error-output *error-output*)
        (unless (streamp input)
          (close in))))))

(defun convert-readme (input &optional (type "markdown"))
  (ignore-some-conditions (uiop:subprocess-error)
    (cond
      ((or (string= type "markdown")
           (string= type "md"))
       (pandoc input :from "markdown"))
      ((string= type "org")
       (pandoc input :from "org"))
      ((string= type "rst")
       (pandoc input :from "rst")))))
