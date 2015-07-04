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

(defparameter *emacs-path* (or (uiop:getenv "EMACS_PATH")
                               "emacs"))

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

(defun convert-readme (file)
  (check-type file pathname)
  (assert (uiop:file-exists-p file))
  (let ((type (pathname-type file)))
    (cond
      ((or (null type)
           (string= type "txt"))
       (convert-plaintext-into-html file))
      ((or (string= type "markdown")
           (string= type "md"))
       (convert-markdown-into-html file))
      ((string= type "org")
       (convert-org-into-html file)))))

(defun convert-plaintext-into-html (file)
  (concatenate 'string "<pre>" (uiop:read-file-string file) "</pre>"))

(defun convert-markdown-into-html (file)
  (check-if-command-installed *timeout-command* *pandoc-path*)
  (ignore-some-conditions (uiop:subprocess-error)
    (with-output-to-string (s)
      (uiop:run-program `(,*timeout-command* 10 ,*pandoc-path* ,(namestring file))
                        :force-shell t
                        :output s
                        :error-output *error-output*))))

(defun convert-org-into-html (file)
  (check-if-command-installed *timeout-command* *emacs-path*)
  (ignore-some-conditions (uiop:subprocess-error)
    (with-output-to-string (s)
      (uiop:run-program `(,*timeout-command*
                          10
                          ,*emacs-path* "-Q" "-batch" "--file" ,(namestring file)
                          "--eval" "(setq org-export-with-toc nil)"
                          "--eval" "(princ (org-export-as-html 3 nil 'string t))")
                        :force-shell t
                        :output s
                        :error-output *error-output*))))
