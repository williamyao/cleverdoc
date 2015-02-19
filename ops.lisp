(in-package #:cleverdoc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ops* nil))

(defun split (seps seq &key (test #'equal) (key #'identity))
  (let ((pos (typecase seps
               (sequence
                (dotimes (i (length seq))
                  (if (position (elt seq i) seps :test test :key key)
                      (return i))))
               (t (position seps seq)))))
    (if pos
        (values (subseq seq 0 pos)
                (subseq seq (1+ pos)))
        (values seq nil))))

(defgeneric expand-op (op left right)
  (:documentation "Return the expanded code to implement the OP, given the LEFT and RIGHT forms.")
  (:method (op left right)
    (error
     "Op ~a is not implemented. You may have not used DEFINE-OP."
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
  (let ((op (find-op form)))
    (cond
      (op (multiple-value-bind (l r) (split op form)
            (expand-op op l r)))
      ((consp form)
       (loop for sexp in form collect (expand-ops sexp)))
      (t form))))
