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

(defun test (&rest names)
  (let (*passes* *failures*)
    (loop for name in names
       do (funcall (gethash name *tests*)))
    (display-results)))

(defun display-results ()
  (display-failures)
  (display-summary))

(defun display-failures ()
  (loop for failure in *failures*
     do (display-failure failure)))

(defun display-summary ()
  (let* ((numpass (length *passes*))
         (numfail (length *failures*))
         (total (+ numpass numfail)))
    (format t "~&Performed ~d checks." total)
    (format t "~&~4tFAIL: ~d (~d%)" numfail (% numfail total))
    (format t "~&~4tPass: ~d (~d%)" numpass (% numpass total))))

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

(define-op ==> (left right)
  `((,*fn* ,@left) === ,@right))

(define-op /=> (left right)
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
  `(if (equal (subseq (multiple-value-list ,@left)
                      0 ,(length right))
              (list ,@right))
       (pass)
       (fail ,(format nil "~{ ~a~} =/=~{ ~a~}"
                      left right))))

(define-op =/= (left right)
  (unless (= (length left) 1)
    (error "Left-side argument to =/= should be a single form"))
  `(if (not (equal (subseq (multiple-value-list ,@left)
                           0 ,(length right))
                   (list ,@right)))
       (pass)
       (fail ,(format nil "~{ ~a~} ===~{ ~a~}"
                      left right))))

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
