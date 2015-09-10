;;;; Misecellaneous utilities.

(in-package #:cleverdoc)

(defun mklist (object) (if (listp object) object (list object)))

;;; TODO 2015-09-07 williamyaoh@gmail.com
;;;  - add the key parameters that POSITION has
(defun position-any (elements sequence)
  (loop for position upfrom 0
        for element in sequence
        if (member element elements)
          do (return-from position-any
               (values position element))
        finally (return (values nil nil))))
#-(and)(function-specification position-any
  "Return the position of the first element of SEQUENCE ~
   that is a member of ELEMENTS.
~@
   As a secondary value, return the element found.")

(defun %shorter (list1 list2)
  (cond
    ;; LISTP to avoid things like (%SHORTER NIL 2) ==> T
    ((and (listp list1) (null list2)) nil)
    ((and (listp list2) (null list1) t))
    (t (%shorter (rest list1) (rest list2)))))

(defun shorter (list1 &rest more-lists)
  (loop for shortest = list1 then (first rest)
        for rest = more-lists then (rest rest)
        while rest
        always (%shorter shortest (first rest))))
#-(and)(function-specification shorter
  "Return T if every list passed in is shorter than ~
   all subsequent lists. In other words, return the same ~
   thing as (< (MAP 'LIST #'LENGTH LISTS)), but without ~
   iterating through the entirety of every list.")

(defun milliseconds->seconds (milliseconds)
  (/ milliseconds 1000))

(defun milliseconds-current-time ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(defun percent-of (x y)
  (round (* 100 (/ x y))))
#-(and)(function-specification percent-of
  "X is N percent of Y. ~@
   Return N, as an integer.")

(defun percent-string (dividend divisor)
  "Return a two-character string of the percent, or ?? if the
divisor is zero. Needed to handle this degenerate case."
  (if (not (zerop divisor))
      (format nil "~2,'0d" (percent-of dividend divisor))
      "??"))

(defun single-p (list) (null (rest list)))
#-(and)(function-specification single-p
  "Check if LIST is a list of a single element.")

