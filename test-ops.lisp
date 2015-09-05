;;;; Implementation of the various test operations.

(in-package #:cleverdoc)

(defparameter *float-tolerance* 0.01)

(defmacro with-float-tolerance (tolerance &body body)
  `(let ((*float-tolerance* ,tolerance))
     ,@body))

(defun compare-result (result expected)
  (typecase expected
    (float
     (typecase result
       (number (>= *float-tolerance*
                   (abs (/ (- expected result)
                           expected))))
       (t (equal result expected))))
    (number
     (typecase result
       (number (= result expected))
       (t (equal result expected))))
    (t (equal result expected))))

(defun compare-results (mvlist1 mvlist2)
  (every #'compare-result mvlist1 mvlist2))

(define-op ==> (left right)
  `((,@*fn* ,@left) === ,@right))

(define-op ==/ (left right)
  `((,@*fn* ,@left) =/= ,@right))

(define-op === (left right)
  (unless (= (length left) 1)
    (error "Left-side argument to === should be a single form"))
  `(handler-case
       (if (compare-results (subseq (multiple-value-list ,@left)
                                    0 ,(length right))
                            (list ,@right))
           (pass)
           (fail ,(format nil "~{ ~a~} =/=~{ ~a~}"
                          left right)))
     (error (e)
       (fail (format nil "Encountered error ~s in ~a ===~{ ~a~}."
                     e ',@left ',right)))))

(define-op =/= (left right)
  (unless (= (length left) 1)
    (error "Left-side argument to =/= should be a single form"))
  `(handler-case
       (if (not (compare-results (subseq (multiple-value-list ,@left)
                                         0 ,(length right))
                                 (list ,@right)))
           (pass)
           (fail ,(format nil "~{ ~a~} ===~{ ~a~}"
                          left right)))
     (error (e)
       (fail (format nil "Encountered error ~s in ~a =/=~{ ~a~}."
                     e ',@left ',right)))))

(define-op ==x (left right)
  `((,@*fn* ,@left) =x= ,@right))

(define-op =x= (left right)
  (unless (= (length left) 1)
    (error "Left-side argument to ==x should be a single form"))
  `(handler-case ,@left
     ,@(mapcar (lambda (e)
                 `(,e () (pass)))
               right)
     (:no-error (&rest rest)
       (declare (ignore rest))
       (fail ,(format nil "No error encountered in ~a.
~4tWanted one of~{ ~a~}."
                                  (car left) right)))))

(defun output-type (sexp)
  (typecase sexp
    (string 'character)
    (t 'octet)))

(define-op ==>> (left right)
  `(let (({out} (flexi-streams:make-in-memory-output-stream
                 :element-type ,(output-type (car right)))))
     (handler-case (,@*fn* ,@left)
       (error (e) (fail (format nil "Encountered error ~s
~4twhile evaluating ~a."
                                e '(,@*fn* ,@left)))))
     (if (equal (flexi-streams:get-output-stream-sequence {out})
                ,(car right))
         (pass)
         (fail ,(format nil "(~a~{ ~a~}) did not write out ~s."
                        *fn* left (car right))))))
                        
(define-op >>> (left right)
  `(let (({in} (flexi-streams:make-in-memory-input-stream
                ,(typecase (car left)
                           (vector (car left))
                           (t (coerce left 'vector))))))
     ,right))
