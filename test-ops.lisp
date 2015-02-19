;---------------------------------------------------------------------
;  TESTING OPERATIONS

(in-package #:cleverdoc)

(define-op ==> (left right)
  `((,*fn* ,@left) === ,@right))

(define-op ==/ (left right)
  `((,*fn* ,@left) =/= ,@right))

;; (define-op =>> (left right)
;;   (with-gensyms (out)
;;     `(let ((,out (make-in-memory-output-stream
;;                   :element-type ,(typecase (car right)
;;                                    (string 'character)
;;                                    (t 'octet)))))
;;        (,*fn* ,@left)
;;        ((get-output-stream-sequence ,out) === ,@right))))

(define-op === (left right)
  (unless (= (length left) 1)
    (error "Left-side argument to === should be a single form"))
  `(handler-case
       (if (equal (subseq (multiple-value-list ,@left)
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
       (if (not (equal (subseq (multiple-value-list ,@left)
                               0 ,(length right))
                       (list ,@right)))
           (pass)
           (fail ,(format nil "~{ ~a~} ===~{ ~a~}"
                          left right)))
     (error (e)
       (fail (format nil "Encountered error ~s in ~a =/=~{ ~a~}."
                     e ',@left ',right)))))

(define-op ==x (left right)
  `((,*fn* ,@left) =x= ,@right))

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
