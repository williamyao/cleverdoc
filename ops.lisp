;;;; Macros and functions for expanding test operations
;;;; into functional code.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ops* nil))

(defgeneric expand-op (op left right)
  (:documentation "Return the expanded code to implement the OP,
 given the LEFT and RIGHT forms.")
  (:method (op left right)
    (error
     "Op ~a is not implemented. You may not have defined it with DEFINE-OP."
     op))
  (:method :around (op left right)
           (expand-ops (call-next-method))))

(defmacro define-op (name (left right) &body body)
  "Define a method on EXPAND-OP for an op named NAME, and register it in *OPS*."
  (let ((opsym (gensym)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (pushnew ',name *ops*))
       (defmethod expand-op ((,opsym (eql ',name)) ,left ,right)
         ,@body))))

(defun find-op (form)
  (typecase form
    (cons (loop for sexp in form if (find sexp *ops*)
               do (return sexp)))
    (t nil)))

(defun expand-ops (form)
  "Recursively generate macroexpansions for all operations found in FORM."
  (let ((op (find-op form)))
    (cond
      (op (destructuring-bind (left right) (split-on-op op form)
            (expand-op op left right)))
      ((consp form)
       (loop for sexp in form collect (expand-ops sexp)))
      (t form))))

(defun split-on-op (op form)
  (let ((op-position (position op form)))
    (list (subseq form 0 op-position)
	  (when op-position
	    (subseq form (+ op-position 1))))))

;;; Not used.

(defun mklist (value) (if (listp value) value (list value)))

(defun position-any (values sequence &key (from-end nil)
				       (start 0)
				       (end nil)
				       (key nil)
				       (test nil)
				       (test-not nil))
  "Return the index of the first value in VALUES that is found
within SEQUENCE."
  (dolist (value (mklist values))
    (let ((position (position value sequence
			      :from-end from-end
			      :start start
			      :end end
			      :key key
			      :test test
			      :test-not test-not)))
      (when position
	(return-from position-any position)))))
