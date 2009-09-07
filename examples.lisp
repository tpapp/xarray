(in-package :xarray)

(defparameter *a* (make-array '(4 5)))

;; fill with numbers
(dotimes (i (array-total-size *a*))
  (setf (row-major-aref *a* i) i))

*a*

#2A((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19))

(defparameter *b* (slice *a* 1 :all))

(defparameter *b* (permutation *a* 1 0)) ; basically, a transpose

*b*

#<PERMUTATION-VIEW 
#2A((0 5 10 15) (1 6 11 16) (2 7 12 17) (3 8 13 18) (4 9 14 19))  {C0A9509}>

(defparameter *s* (slice *b* '(2 3) '(-2 -1)))

*s* ; =>  #<SLICE-VIEW #2A((12 17) (13 18))  {B704451}>

(xsetf *s*
       (make-array '(2 2) :initial-element 0))

*a* ; => #2A((0 1 2 3 4) (5 6 7 8 9) (10 11 0 0 14) (15 16 0 0 19))

(carray* '(2 3) 1 2 3 4d0 5 6)
