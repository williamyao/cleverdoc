;;;; Functionality for creating and running tests.

;;; TODO 2015-09-05 williamyaoh@gmail.com
;;;  - somewhere down the line, add the ability to
;;;    suppress the definition of tests without
;;;    having to change source code, for smaller
;;;    core dumps and executables and such

(in-package #:cleverdoc)

;;; TODO 2015-09-07 williamyaoh@gmail.com
;;;  - A lot of code is using GET-LEVEL-PREDICATE to do
;;;    level-specific behaviour. Have them all point to
;;;    GET-PREDICATE-LEVEL's documentation to reduce clutter.

(defmacro variable-specification (variable-name &body specification)
  "Set the documentation of the variable named by VARIABLE-NAME. For
convenience, SPECIFICATION acts like an argument list to (FORMAT NIL ...)

Provided for parallelism with FUNCTION-SPECIFICATION."
  `(progn
     (setf (documentation ',variable-name 'variable)
           (format nil ,@specification))
     ',variable-name))

(defvar *tests* (make-hash-table :test 'eql))
(variable-specification *tests*
  "Mapping of symbols onto corresponding test closures.
~
   Global.")

(defvar *show-passing-tests* nil)
(variable-specification *show-passing-tests*
  "Whether to print out anything for passing tests or not.
~
   Global.")

(defun %get-package-level-predicate (test-level)
  (lambda (symbol)
    (eql (symbol-package symbol)
         (cond
           ((eql test-level :package) *package*)
           (t test-level)))))

(defun %get-symbol-level-predicate (test-level)
  (lambda (symbol) (eql symbol test-level)))

(defun get-level-predicate (level)
  (cond
    ((eql level :all)
     (constantly t))
    ((or (eql level :package)
         (packagep level))
     (%get-package-level-predicate level))
    ((and (symbolp level)
          (not (keywordp level)))
     (%get-symbol-level-predicate level))))
(function-specification get-level-predicate
  "Return a predicate that matches certain symbols, based on ~
   LEVEL.
~@
   LEVEL can be one of: ~@
~@
   * :ALL -- matches all symbols ~@
   * :PACKAGE, or a package object -- ~
       match all symbols from the specified package, ~
       or from the current package, when passing :PACKAGE ~@
   * <a symbol> -- matches the symbol")

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
  (remove-if-not (get-level-predicate test-level)
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

(defun percent-string (dividend divisor)
  "Return a two-character string of the percent, or ?? if the
divisor is zero. Needed to handle this degenerate case."
  (if (not (zerop divisor))
      (format nil "~2,'0d" (percent-of dividend divisor))
      "??"))

(defun test (&optional (test-level :all))
  "Run specified tests and print out human-readable results to
standard output.

TEST-LEVEL can be one of:

* :ALL -- runs all tests in *TESTS*
* :PACKAGE, or a package object
       -- runs all tests defined on symbols from
          the specified package, or the current one
          when passing :PACKAGE
* <a symbol>
       -- runs all tests specified on the symbol"
  (let ((runs (run-tests-get-runs test-level)))
    (dolist (run runs)
      (format *standard-output*
              "~A~%"
              (test-run-pretty-message run)))
    (let ((length (length runs))
          (passes (loop for run in runs if (pass? run) count run))
          (failures (loop for run in runs if (not (pass? run)) count run)))
      (format *standard-output*
              "=======~@
               RESULTS~@
               =======~@
                      ~@
               Ran ~D test~:P.~@
                      ~@
               !!!FAIL!!!: ~D (~A%)
   PASS   : ~D (~A%)"
              length
              failures
              (percent-string failures length)
              passes
              (percent-string passes length)))))

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


(defvar *test-runs*)
(variable-specification *test-runs*
  "Set of `test-run' objects.
~
   Local to each invocation of RUN-TESTS-GET-RUNS.")

(defvar *test-function*)
(variable-specification *test-function*
  "Symbol of the function upon which a test is being defined, ~
   or which is currently being tested.
~
   Local to each test closure.")

(defvar *milliseconds-run-start-time*)
(variable-specification *milliseconds-run-start-time*
  "When an individual `test-run' was started.
~
   Local (or at least confined to) each invocation of ~
   PASS/FAIL.")

(defvar *arguments*)
(variable-specification *arguments*
  "The arguments being passed to the function ~
   currently under test.
~
   Local (or at least confined to) each invocation of ~
   PASS/FAIL.")

(defvar *resultant-values*)
(variable-specification *resultant-values*
  "The list of multiple values returned from evaluating the ~
   form currently under test.
~
   Local (or at least confined to) each invocation of ~
   PASS/FAIL.")

(defvar *expectant-values*)
(variable-specification *expectant-values*
  "The list of multiple values that are expected to be ~
   returned from the form currently under test.
~
   Local (or at least confined to) each invocation of ~
   PASS/FAIL.")

;; This is more general and is used for equality.
(record:define test-run ()
  pass?
  (millisecond-duration (- (milliseconds-current-time)
                           *milliseconds-run-start-time*)))

(defgeneric second-duration (test-run)
  (:method ((test-run test-run))
    (milliseconds->seconds (millisecond-duration test-run))))

(record:define function-run (test-run)
  (arguments *arguments*)
  (test-function *test-function*))

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
(defun pass ()
  (push (make-instance 'expect-true-function-run
                       :pass? t
                       :arguments *arguments*
                       :resultant-values *resultant-values*
                       :expectant-values *expectant-values*)
        *test-runs*))

(defun fail ()
  (push (make-instance 'expect-true-function-run
                       :pass? nil
                       :arguments *arguments*
                       :resultant-values *resultant-values*
                       :expectant-values *expectant-values*)
        *test-runs*))


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
      (format nil "~A~%" (pass-header test-run))
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
  (resultant-values *resultant-values*)
  (expectant-values *expectant-values*))

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


;;; TODO 2015-09-06 williamyaoh@gmail.com
;;;  - Right now the documentation part works for macros,
;;;    but for obvious reasons the testing part is a little
;;;    wonky. There may be a way to see macro recompiles, but
;;;    for now I don't have one. Maybe in the future, revisit
;;;    this?
(defmacro function-specification (function-name &body body)
  "Set the function documentation of the specified function
and define some tests on it.

The first line of BODY may be a format string, or an s-expression
with a format string as the first element. In the latter case, the
rest of the s-expression is used as arguments to FORMAT. This format
specifier is used to create the function documentation.

If a format specifier is the only form in BODY, a test is not
registered."
  `(progn
     ,(when (format-specifier-p (first body))
        (prog1 `(setf (documentation ',function-name 'function)
                      (format nil ,@(mklist (first body))))
          (setf body (rest body))))
     ,(when (not (null body))
        `(define-test ,function-name
           ,@(let ((*test-function* function-name))
               (map 'list #'inline-macroexpand body))))
     ',function-name))

(defun format-specifier-p (form)
  (or (stringp form)
      (and (consp form) (stringp (first form)))))


(defvar *inline-macros* (make-hash-table :test 'eql))
(variable-specification *inline-macros*
  "Hash table of symbols to expander functions. ~@
   Each expander function should have an arity of 2; left ~@
   and right lists of parameters.")

;;; TODO 2015-09-06 williamyaoh@gmail.com
;;;  - This is a little too complicated, even though
;;;    it looks short. Split it into some other
;;;    functions, and make the destructuring use
;;;    a more specialized error message on failed
;;;    destructuring.
(defmacro define-inline-macro (name
                               (&rest left-args)
                               (&rest right-args)
                               &body body)
  `(progn
     ,(alexandria:with-gensyms (left right)
        `(setf (gethash ',name *inline-macros*)
               (lambda (,left ,right)
                 (destructuring-bind (,@left-args) ,left
                   (destructuring-bind (,@right-args) ,right
                     ,@body)))))
     ',name))
(function-specification define-inline-macro
  "Almost exactly the same as DEFMACRO, but for operators ~
   that appear in the middle of an s-expression, not at ~
   the beginning.")

(defun inline-macroexpand-1 (form)
  (if (not (listp form))
      form
      (multiple-value-bind (position element)
          (position-any (alexandria:hash-table-keys *inline-macros*) form)
        (if (null position)
            form
            (inline-macroexpand-1 (funcall (gethash element *inline-macros*)
                                           (subseq form 0 position)
                                           (subseq form (+ position 1))))))))
(function-specification inline-macroexpand-1
  "Expand all inline macros at depth 1 in the form, ~
   from left to right.")

(defun inline-macroexpand (form)
  (if (not (listp form))
      form
      (map 'list
           #'inline-macroexpand
           (inline-macroexpand-1 form))))
(function-specification inline-macroexpand
  "Recursively expand all inline macros in the form.")

(defun get-inline-macros (level)
  (remove-if-not (get-level-predicate level)
                 (alexandria:hash-table-keys *inline-macros*)))
(function-specification get-inline-macros
  "Return a list containing the symbols of all the inline macros ~
   defined at LEVEL. ~@
~@
   LEVEL can be one of: ~@
~@
   * :ALL -- returns all inline macros ~@
   * :PACKAGE, or a package object -- ~
       return all inline macros defined in the specified package, ~
       or in the current package, when passing :PACKAGE ~@
   * <a symbol> -- returns a list containing the symbol, ~
       if there is an inline macro defined on it")

(defun clear-inline-macros (level)
  (dolist (inline-macro-symbol (get-inline-macros level))
    (remhash inline-macro-symbol *inline-macros*)))
(function-specification clear-inline-macros
  "Remove all inline macros defined at LEVEL. ~@
~@
   LEVEL can be one of: ~@
~@
   * :ALL -- removes all inline macros ~@
   * :PACKAGE, or a package object -- ~
       removes all inline macros defined in the specified package, ~
       or in the current package, when passing :PACKAGE ~@
   * <a symbol> -- removes the inline macro defined on the ~
       symbol, if there is one")





;;; TODO 2015-09-07 williamyaoh@gmail.com
;;;  - add the key parameters that POSITION has
(defun position-any (elements sequence)
  (loop for position upfrom 0
        for element in sequence
        if (member element elements)
          do (return-from position-any
               (values position element))
        finally (return (values nil nil))))
(function-specification position-any
  "Return the position of the first element of SEQUENCE ~
   that is a member of ELEMENTS.
~@
   As a secondary value, return the element found.")

(defun milliseconds->seconds (milliseconds)
  (/ milliseconds 1000))

(defun milliseconds-current-time ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(defun percent-of (x y)
  (round (* 100 (/ x y))))
(function-specification percent-of
  "X is N percent of Y. ~@
   Return N, as an integer.")

(defun single-p (list) (null (rest list)))
(function-specification single-p
  "Check if LIST is a list of a single element.")

(defun mklist (object) (if (listp object) object (list object)))
