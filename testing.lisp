;;;; Functionality for creating and running tests.

;;; TODO 2015-09-05 williamyaoh@gmail.com
;;;  - somewhere down the line, add the ability to
;;;    suppress the definition of tests without
;;;    having to change source code, for smaller
;;;    core dumps and executables and such

(in-package #:cleverdoc)

(variable-specification *tests*
  "Mapping of symbols onto corresponding test closures.
~
   Global.")
(defvar *tests* (make-hash-table :test 'eql))

(defmacro variable-specification (variable-name &body specification)
  "Set the documentation of the variable named by VARIABLE-NAME. For
convenience, SPECIFICATION acts like an argument list to (FORMAT NIL ...)

Provided for parallelism with FUNCTION-SPECIFICATION."
  `(progn
     (setf (documentation ',variable-name 'variable)
           (format nil ,@specification))
     ',variable-name))

(defun %get-package-level-predicate (test-level)
  (lambda (symbol)
    (eql (symbol-package symbol)
         (cond
           ((eql test-level :package) *package*)
           (t test-level)))))

(defun %get-symbol-level-predicate (test-level)
  (lambda (symbol) (eql symbol test-level)))

(defun %get-test-level-predicate (test-level)
  (cond
    ((eql test-level :all)
     #'identity)
    ((or (eql test-level :package)
         (packagep test-level))
     (%get-package-level-predicate test-level))
    ((and (symbolp test-level)
          (not (keywordp test-level)))
     (%get-symbol-level-predicate test-level))))

(defun get-tests (test-level)
  "Return a list of all tests defined at LEVEL.

LEVEL can be one of:

* :ALL -- returns all tests in *TESTS*
* :PACKAGE, or a package object
       -- returns all tests defined on symbols from
          the specified package, or the current one
          when passing :PACKAGE
* <a symbol>
       -- returns all tests specified on the symbol"
  (map 'list
       (lambda (symbol) (gethash symbol *tests*))
       (remove-if-not (%get-test-level-predicate test-level)
                      (alexandria:hash-table-keys *tests*))))

(defun run-tests-get-runs (test-level)
  "Return a list containing all the resulting `test-runs'
from running the tests defined at TEST-LEVEL.

LEVEL can be one of:

* :ALL -- returns all tests in *TESTS*
* :PACKAGE, or a package object
       -- returns all tests defined on symbols from
          the specified package, or the current one
          when passing :PACKAGE
* <a symbol>
       -- returns all tests specified on the symbol"
  (let ((*test-runs* '()))
    (dolist (test (get-tests test-level))
      (funcall test))
    *test-runs*))

(defun test-run-pretty-message (test-run)
  (if (pass? test-run)
      (pass-pretty-message test-run)
      (fail-pretty-message test-run)))

(defgeneric pass-pretty-message (test-run)
  (:documentation "Message to be printed for a `test-run' that
meets expectations."))

(defgeneric fail-pretty-message (test-run)
  (:documentation "Message to be printed for a `test-run' that
does not meet expectations."))

(defmacro define-test (symbol &body test-body)
  `(%define-test ',symbol
                 (lambda ()
                   ,@test-body)))

(defun %define-test (symbol test-function)
  (setf (gethash symbol *tests*)
        test-function))


(record:define test-run ()
  pass?
  (function-symbol *function*)
  arguments
  (millisecond-duration (- *milliseconds-run-start-time*
                           (milliseconds-current-time))))

(variable-specification *test-runs*
  "Set of `test-run' objects.
~
   Local to each invocation of RUN-TESTS-GET-RUNS.")
(defvar *test-runs*)

(variable-specification *function*
  "Symbol of the function upon which a test is being defined, ~
   or which is currently being tested.
~
   Local to each test closure.")
(defvar *function*)

(variable-specification *milliseconds-run-start-time*
  "When an individual `test-run' was started.
~
   Local (or at least confined to) each invocation of ~
   PASS/FAIL.")
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
