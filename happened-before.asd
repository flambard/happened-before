(defpackage :happened-before-system
  (:use :cl))

(in-package :happened-before-system)

(asdf:defsystem :happened-before
  :description ""
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.0.1"
  :license "MIT License"
  :depends-on ()
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "interval-tree-clock"
                    :depends-on ("packages"))
             ))))
