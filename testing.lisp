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

(variable-specification *show-passing-tests*
  "Whether to print out anything for passing tests or not.")
(defvar *show-passing-tests* nil)

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

(defun get-test-symbols (test-level)
  "Return a list of all symbols that have tests defined at LEVEL.

TEST-LEVEL can be one of:

* :ALL -- returns all symbols with tests in *TESTS*
* :PACKAGE, or a package object
       -- from either the specified package, or the current
          when passing :PACKAGE, return all symbols that
          have tests defined on them.
* <a symbol>
       -- returns the symbol, if it has a test defined."
  (remove-if-not (%get-test-level-predicate test-level)
                 (alexandria:hash-table-keys *tests*)))

(defun get-tests (test-level)
  "Return a list of all tests defined at LEVEL.

TEST-LEVEL can be one of:

* :ALL -- returns all tests in *TESTS*
* :PACKAGE, or a package object
       -- returns all tests defined on symbols from
          the specified package, or the current one
          when passing :PACKAGE
* <a symbol>
       -- returns all tests specified on the symbol"
  (map 'list
       (lambda (symbol) (gethash symbol *tests*))
       (get-test-symbols test-level)))

(defun run-tests-get-runs (test-level)
  "Return a list containing all the resulting `test-runs'
from running the tests defined at TEST-LEVEL.

TEST-LEVEL can be one of:

* :ALL -- runs all tests in *TESTS*
* :PACKAGE, or a package object
       -- runs all tests defined on symbols from
          the specified package, or the current one
          when passing :PACKAGE
* <a symbol>
       -- runs all tests specified on the symbol"
  (let ((*test-runs* '()))
    (dolist (test (get-tests test-level))
      (funcall test))
    *test-runs*))

(defun clear-tests (test-level)
  "Remove tests from *TESTS*.

TEST-LEVEL can be one of:

* :ALL -- clears all tests in *TESTS*
* :PACKAGE, or a package object
       -- from either the specified package, or the current
          when passing :PACKAGE, clear all tests.
* <a symbol>
       -- clear the test on the symbol, if it has
          one defined."
  (dolist (symbol (get-test-symbols test-level))
    (remhash symbol *tests*)))

(defmacro define-test (symbol &body test-body)
  `(%define-test ',symbol
                 (lambda ()
                   ,@test-body)))

(defun %define-test (symbol test-function)
  (setf (gethash symbol *tests*)
        test-function))


;; This is more general and is used for equality.
(record:define test-run ()
  pass?
  (millisecond-duration (- *milliseconds-run-start-time*
                           (milliseconds-current-time))))

(defgeneric second-duration (test-run)
  (:method ((test-run test-run))
    (milliseconds->seconds (millisecond-duration test-run))))

(record:define function-run (test-run)
  (arguments '())
  (test-function *function*))

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

;;; TODO 2015-09-06 williamyaoh@gmail.com
;;;  - These will have to be general to fit all
;;;    expectations. Will have to revisit these.
(defun pass ())
(defun fail ())


;;; Quick note. When testing against multiple value return,
;;; if the expectant is shorter than the resultant, then it should
;;; only compare up to the end of the expectant, but if the expectant
;;; is longer than the resultant, that should be an error.

(defun test-run-duration-message (test-run)
  (format nil "(~,3F sec)" (second-duration test-run)))

;;; TODO 2015-09-06 williamyaoh@gmail.com
;;;  - This may be responsible for including the header, which would make
;;;    sense.
(defun test-run-pretty-message (test-run)
  (if (pass? test-run)
      (pass-header test-run)
      (format nil "~A~&  ~A~%" (fail-header test-run) (fail-pretty-message test-run))))

(defgeneric pass-header (test-run)
  (:documentation "Contextual header for a `test-run' that
meets expectations. Since there's no point printing a detailed message
for passes, this includes the test duration. These may also be
disabled entirely by the test-running user.")
  (:method :around ((test-run test-run))
    (if *show-passing-tests*
        (call-next-method)
        ""))
  (:method ((test-run test-run))
    (format nil
            "Test met expectations. ~A"
            (test-run-duration-message test-run))))

(defgeneric fail-pretty-message (test-run)
  (:documentation "Message to be printed for a `test-run' that
does not meet expectations.")
  (:method ((test-run test-run))
    (format nil
            "Test did not meet expectations. ~A"
            (test-run-duration-message test-run))))

(defgeneric fail-header (test-run)
  (:documentation "Contextual header for a `test-run' that
does not meet expectations. Does not need to be as specialized as
the actual failure message.")
  (:method ((test-run test-run)) "!!!TEST FAILURE!!!"))

(record:define equality-run ()
  (resultant-values '())
  (expectant-values '()))

(record:define expect-true-run (test-run equality-run))
(record:define expect-false-run (test-run equality-run))

(record:define expect-true-function-run (function-run equality-run))
(record:define expect-false-function-run (function-run equality-run))

(defmethod pass-header ((test-run function-run))
  (format nil
          "Test for ~A::~A met expectations. ~A"
          (package-name (symbol-package (test-function test-run)))
          (symbol-name (test-function test-run))
          (test-run-duration-message test-run)))

(defmethod fail-header ((test-run function-run))
  (format nil
          "~A Test for ~A::~A did not meet expectations:"
          (call-next-method)
          (package-name (symbol-package (test-function test-run)))
          (symbol-name (test-function test-run))))

(defun agree-length (resultant-values expected-values)
  "Make RESULTANT 'agree' with EXPECTED in length.
Returns a copy of RESULTANT, but the same length as EXPECTED or
shorter."
  (map 'list
       (lambda (r e) (declare (ignore e)) r)
       resultant-values
       expected-values))

(defun maybe-multiple-value-message (list-of-values)
  "Return a simpler message for single values, and a more
accurate but complicated one for multiple values."
  (format nil
          "~:[multiple values (~;~]~{~S~^, ~}~:[)~;~]"
          (single-p list-of-values)
          list-of-values
          (single-p list-of-values)))

(defmethod fail-pretty-message ((test-run expect-true-run))
  (let ((trimmed-resultant-values (agree-length (resultant-values test-run)
                                                (expectant-values test-run))))
    (format nil
            "Value(s) did not equal; left side was ~A but right side was ~A ~A"
            (maybe-multiple-value-message trimmed-resultant-values)
            (maybe-multiple-value-message (expectant-values test-run))
            (test-run-duration-message test-run))))

;;; TODO 2015-09-06 williamyaoh@gmail.com
;;;  - These messages look pretty similar for failure and passing.
;;;    Might want to make them more distinctive somehow. Maybe using
;;;    xterm colors?
(defmethod fail-pretty-message ((test-run expect-false-run))
  (let ((trimmed-resultant-values (agree-length (resultant-values test-run)
                                                (expectant-values test-run))))
    (format nil
            "Value(s) equalled, but were not supposed to; ~
             left side was ~A and right side was ~A ~A"
            (maybe-multiple-value-message trimmed-resultant-values)
            (maybe-multiple-value-message (expectant-values test-run))
            (test-run-duration-message test-run))))

(defmethod fail-pretty-message ((test-run expect-true-function-run))
  (let ((trimmed-resultant-values (agree-length (resultant-values test-run)
                                                (expectant-values test-run))))
    (format nil
            "(~A~{ ~S~}) returned ~A, but expected ~A ~A"
            (test-function test-run)
            (arguments test-run)
            (maybe-multiple-value-message trimmed-resultant-values)
            (maybe-multiple-value-message (expectant-values test-run))
            (test-run-duration-message test-run))))

(defmethod fail-pretty-message ((test-run expect-false-function-run))
  (let ((trimmed-resultant-values (agree-length (resultant-values test-run)
                                                (expectant-values test-run))))
    (format nil
            "(~A~{ ~S~}) returned ~A, but was not supposed to ~A"
            (test-function test-run)
            (arguments test-run)
            (maybe-multiple-value-message trimmed-resultant-values)
            (test-run-duration-message test-run))))


(defun milliseconds->seconds (milliseconds)
  (/ milliseconds 1000))

(defun milliseconds-current-time ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(defun percent-of (x y)
  "X is N percent of Y.
Return N, as an integer."
  (round (* 100 (/ x y))))

(defun single-p (list)
  "Check if LIST is a list with only one element."
  (null (rest list)))
