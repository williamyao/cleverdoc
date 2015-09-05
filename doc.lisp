;;;; Implementation of tests and running suites.

(in-package :cleverdoc)

(declaim (inline %))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fn* nil)
  (defparameter *tests* (make-hash-table :test 'equal)))

(defparameter *passes* nil "Per-execution list of test passes.")
(defparameter *failures* nil "Per-execution list of test failures.")

(defvar {in} nil "Input stream used in testing input functions.")
(defvar {out} nil "Output stream used in testing output functions.")

(defmacro document (fn &body body)
  "Document and create test cases for the function designated by the
symbol FN.

BODY can contain arbitrary Lisp forms, which will be evaluated as
expected. Operations found within BODY will be run as tests.

DOCUMENT can contain, as its first form, a documentation string for the 
function."
  (let ((*fn* (mklist fn)))
    `(progn
       ,(when (stringp (car body))
              (aprog1 `(setf (documentation ',(car *fn*) 'function)
                             ,(car body))
                (setf body (cdr body))))
       (register-test ',*fn*
                      (lambda ()
                          ,@(mapcar (lambda (form)
                                      (expand-ops form))
                                    body))))))

(defun register-test (test test-closure)
  (setf (gethash test *tests*) test-closure)
  (first test)) ; the function symbol of the test

(defun test-in-level-p (test level)
  (cond ((eql level :package)
         (string= (package-name *package*)
                  (package-name (symbol-package (car test)))))
        ((eql level :full)
         t)
        (t (or (eql (car test) level)
               (equal test level)))))

;;; TODO 2015-07-14 -- make TEST return a boolean so it can be
;;; run as a test-op from ASDF etc.
(defun test (&key (level :package))
  "Run all tests defined at LEVEL and print the results.

Allowed values for LEVEL are:
* :PACKAGE -- all tests in the current package
* :FULL    -- all tests
* <symbol> -- all tests for the function designated by this symbol
* (<symbol> <args>*) -- tests for this specific function and args"
  (let ((*passes* '())
	(*failures* '()))
    (loop for test being the hash-keys of *tests*
       when (test-in-level-p test level)
       do (funcall (gethash test *tests*)))
    (display-results)))

(defun list-tests (&key (level :package))
  "Print out a list of tests defined at this LEVEL to
standard output.

Allowed values for LEVEL are:
* :PACKAGE -- all tests in the current package
* :FULL    -- all tests
* <symbol> -- all tests for the function designated by this symbol
* (<symbol> <args>*) -- tests for this specific function and args"
  (loop for test being the hash-keys of *tests*
     when (test-in-level-p test level)
     do (let ((sym (first test))
              (simple? (alexandria:sequence-of-length-p test 1)))
	  ;; Prints a no-default-argument test as its package qualified
	  ;; symbol, and tests with arguments as a list of the symbol
	  ;; and arguments.
          (format t "~:[(~;~]~a::~a~{ ~a~}~:[)~;~]~%"
                  simple?
                  (package-name (symbol-package sym))
                  (symbol-name sym)
                  (cdr test)
                  simple?))))

(defun display-results ()
  (cond
    ((or *passes* *failures*)
     (display-failures) ; Displaying successes seems pretty useless.
     (display-summary))
    (t (format t "No matching tests found."))))

(defun display-failures ()
  (loop for failure in *failures*
     do (display-failure failure)))

(defun display-failure (failure)
  (destructuring-bind (package function-name message) failure
    (format t "~&Test case for ~a::~a FAILED:" package function-name)
    (format t "~&~4t~a" message)))

(defun display-summary ()
  (let* ((numpass (length *passes*))
         (numfail (length *failures*))
         (total (+ numpass numfail)))
    (format t "~&~%=============~%   RESULTS~%=============~%")
    (format t "~%Tested ~d case~:p." total)
    (format t "~%~4tFAIL: ~d (~d%)" numfail (% numfail total))
    (format t "~%~4tPass: ~d (~d%)" numpass (% numpass total))))

(defun % (n1 n2)
  (round (* 100 (/ n1 n2 1.0))))

(defun result-base ()
  `(,(package-name (symbol-package (car *fn*)))
     ,(symbol-name (car *fn*))))

(defun append1 (l obj)
  (append l (list obj)))

(defun pass ()
  (setf *passes* (append1 *passes* (result-base))))
(defun fail (msg)
  (setf *failures*
        (append1 *failures*
                 (append1 (result-base) msg))))
