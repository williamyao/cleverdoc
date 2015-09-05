;;;; Functionality for creating and running tests.

;;; TODO 2015-09-05 williamyaoh@gmail.com
;;;  - somewhere down the line, add the ability to
;;;    suppress the definition of tests without
;;;    having to change source code, for smaller
;;;    core dumps and executables and such

(in-package #:cleverdoc)

(variable-specification *tests*
  "Mapping of symbols onto corresponding test closures.")
(defvar *tests* (make-hash-table :test 'eql))

(defmacro variable-specification (variable-name &body specification)
  "Set the documentation of the variable named by VARIABLE-NAME. For
convenience, SPECIFICATION acts like an argument list to (FORMAT NIL ...)

Provided for parallelism with FUNCTION-SPECIFICATION."
  `(progn
     (setf (documentation ',variable-name 'variable)
           (format nil ,@specification))
     ',variable-name))


(record:define test-run ()
  pass?
  (function-symbol *function*)
  message
  (millisecond-duration (- *milliseconds-run-start-time*
                           (milliseconds-current-time))))

(variable-specification *test-runs*
  "Set of `test-run' objects. ~
   Thread-local.")
(defvar *test-runs*)

(variable-specification *function*
  "Symbol of the function upon which a test is being defined, ~
   or which is currently being tested.")
(defvar *function*)

(variable-specification *milliseconds-run-start-time*
  "When an individual `test-run' was started.")
(defvar *milliseconds-run-start-time*)

(defmacro with-run (&body body)
  "Bind *MILLISECONDS-RUN-START-TIME* to the current time
and run the test body. Good for one `test-run' only."
  `(call-with-run
    (lambda ()
      (let ((*milliseconds-run-start-time* (milliseconds-current-time)))
        ,@body))))

(defun call-with-run (function) (funcall function))

(defun pass (message)
  (push (make-instance 'test-run
                       :pass? t
                       :message message)
        *test-runs*))

(defun fail (message)
  (push (make-instance 'test-run
                       :pass? nil
                       :message message)
        *test-runs*))


(defun milliseconds-current-time ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(defun percent-of (x y)
  "X is N percent of Y.
Return N, as an integer."
  (round (* 100 (/ x y))))
