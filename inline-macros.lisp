;;;; Expansion and definition facilities for
;;;; inline macros.

(in-package #:cleverdoc)

(defvar *inline-macros* (make-hash-table :test 'eql))
#-(and)(variable-specification *inline-macros*
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
