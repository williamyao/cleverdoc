;;;; A smarter way to write documentation and unit tests.

(in-package #:common-lisp-user)
(defpackage #:cleverdoc
  (:use #:cl)
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

