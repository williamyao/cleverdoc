(in-package #:cl-user)
(defpackage #:cleverdoc
  (:use #:cl
        #:cl-util
        #:flexi-streams)
  (:export #:document #:test
           
           #:==> #:/=>
           #:=== #:=/=))
(pushnew :cleverdoc *features*)

