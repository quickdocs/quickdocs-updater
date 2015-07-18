(in-package :cl-user)
(defpackage quickdocs-updater-quicklisp-blog-asdf
  (:use :cl :asdf))
(in-package :quickdocs-updater-quicklisp-blog-asdf)

(defsystem quickdocs-updater-quicklisp-blog
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:dexador
               :plump
               :clss
               :babel
               :local-time

               :datafly
               :sxql)
  :components
  ((:file "src/quicklisp-blog"))
  :description "A part of Quickdocs Updater for retrieving Quicklisp blog infomation.")
