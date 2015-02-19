(in-package #:cleverdoc)

(declaim (inline %))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fn* nil)
  (defparameter *tests* (make-hash-table)))

(defparameter *passes* nil)
(defparameter *failures* nil)

(defun register-test (name clo)
  (setf (gethash name *tests*) clo)
  name)

(defun in-test-level (sym level)
  (cond ((eql level :package)
         (string= (package-name *package*)
                  (package-name (symbol-package sym))))
        ((eql level :full)
         t)
        (t (eql sym level))))

(defun test (&key (level :package))
  (let (*passes* *failures*)
    (loop for sym being the hash-keys of *tests*
       when (in-test-level sym level)
       do (funcall (gethash sym *tests*)))
    (display-results)))

(defun display-results ()
  (cond
    ((or *passes* *failures*)
     (display-failures)
     (display-summary))
    (t
     (format t "No matching tests found."))))

(defun display-failures ()
  (loop for failure in *failures*
     do (display-failure failure)))

(defun display-summary ()
  (let* ((numpass (length *passes*))
         (numfail (length *failures*))
         (total (+ numpass numfail)))
    (format t "~&~%=============~%   RESULTS~%=============~%")
    (format t "~%Performed ~d check~:p." total)
    (format t "~%~4tFAIL: ~d (~d%)" numfail (% numfail total))
    (format t "~%~4tPass: ~d (~d%)" numpass (% numpass total))))

(defun % (n1 n2)
  (round (* 100 (/ n1 n2 1.0))))

(defun display-failure (fcase)
  (destructuring-bind (pkg fn msg) fcase
    (format t "~&Specification for ~a::~a FAILED:" pkg fn)
    (format t "~&~4t~a" msg)))

(defun result-base ()
  `(,(package-name (symbol-package *fn*)) ,(symbol-name *fn*)))

(defun append1 (l obj)
  (append l (list obj)))

(defun pass ()
  (setf *passes* (append1 *passes* (result-base))))
(defun fail (msg)
  (setf *failures*
        (append1 *failures*
                 (append1 (result-base) msg))))

(defmacro document (fn &body body)
  (let ((*fn* fn))
    `(progn
       ,(when (stringp (car body))
         (aprog1 `(setf (documentation ',fn 'function) ,(car body))
           (setf body (cdr body))))
       (register-test ',fn
                      (lambda ()
                        (let ((*fn* ',fn))
                          ,@(mapcar (lambda (form)
                                      (expand-ops form))
                                    body)))))))
