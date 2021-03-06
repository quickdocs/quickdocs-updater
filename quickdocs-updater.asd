#|
  This file is a part of quickdocs-updater project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage quickdocs-updater-asd
  (:use :cl :asdf))
(in-package :quickdocs-updater-asd)

(defsystem quickdocs-updater
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:quickdocs-serializer
               :quickdocs-database
               :datafly
               :sxql
               :cl-ppcre
               :dexador
               :quri
               :jonathan
               :babel
               :plump
               :clss
               :uiop
               :local-time
               :alexandria
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "quickdocs-updater" :depends-on ("extracter" "release" "readme" "cliki" "repos"))
                 (:file "release" :depends-on ("extracter" "http"))
                 (:file "readme")
                 (:file "cliki" :depends-on ("http"))
                 (:file "repos" :depends-on ("http"))
                 (:file "extracter")
                 (:file "http"))))
  :description "Updates Quickdocs database"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
