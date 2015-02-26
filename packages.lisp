(in-package #:cl-user)
(defpackage #:cleverdoc
  (:use #:cl
        #:cl-util
        #:flexi-streams)
  (:export #:document #:test
           #:list-tests
           
           #:==> #:==/
           #:==x

           #:=== #:=/=
           #:=x=

           #:==>>  #:>>>
           #:{in}  #:{out}

           #:with-float-tolerance))
(pushnew :cleverdoc *features*)

