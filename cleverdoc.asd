;;;; A smarter way to write documentation and unit tests.

(in-package #:common-lisp-user)
(defpackage #:cleverdoc/asdf
  (:use #:cl
        #:asdf))
(in-package #:cleverdoc/asdf)

(defsystem :cleverdoc
  :name "Cleverdoc"
  :version "0.0.1"
  :description "Common Lisp documentation and unit testing library."
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :depends-on (:alexandria :flexi-streams :defrecord)
  :serial t
  :components ((:file "packages")
               (:file "inline-macros")
               (:file "testing")))
