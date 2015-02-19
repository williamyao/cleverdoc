(in-package #:cl-user)
(defpackage #:cleverdoc
  (:use #:cl
        #:cl-util
        #:flexi-streams)
  (:export #:document #:test
           
           #:==> #:==/
           #:==x

           #:=== #:=/=
           #:=x=

           #:==>>  #:>>>
           #:{in}  #:{out}))
(pushnew :cleverdoc *features*)

