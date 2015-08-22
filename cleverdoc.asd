(defpackage #:cleverdoc/asdf
  (:use #:cl
        #:asdf))
(in-package #:cleverdoc/asdf)

(defsystem :cleverdoc
  :name "Cleverdoc"
  :version "0.0.1"
  :description "A Common Lisp documentation and unit testing library"
  :author "William Yao <williamyaoh@gmail.com>"
  :depends-on (:alexandria
               :flexi-streams)
  :serial t
  :components ((:file "packages")
               (:file "ops")
               (:file "doc")
               (:file "test-ops")))
