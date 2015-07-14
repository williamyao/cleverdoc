(defpackage #:cleverdoc/asdf
  (:use #:cl
        #:asdf))
(in-package #:cleverdoc/asdf)

(defsystem :cleverdoc
  :name "Cleverdoc"
  :version "0.0.1"
  :description "A Common Lisp documentation and unit testing library"
  :author "William Yao <williamyao@derpymail.org>"
  :maintainer "William Yao <williamyao@derpymail.org>"
  :depends-on (:alexandria
               :flexi-streams)
  :serial t
  :components ((:file "packages")
               (:file "ops")
               (:file "doc")
	       (:file "test-ops")))
