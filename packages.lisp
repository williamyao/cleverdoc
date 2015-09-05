;;;; A smarter way to write documentation and unit tests.

(in-package #:common-lisp-user)

(pushnew :cleverdoc *features*)

(defpackage #:cleverdoc
  (:use #:cl)
  (:export))
