;;;;
;;;;  Unit tests for xarray
;;;;
;;;;  Notes: currently, these tests are very basic, and mostly serve
;;;;  as examples.  Tests will become more important when methods are
;;;;  optimized.

(asdf:oos 'asdf:load-op :xarray)
(asdf:oos 'asdf:load-op :lift)

(in-package :cl-user)

(defpackage #:xarray-unit-tests
    (:use :cl :xarray :lift))

(in-package :xarray-unit-tests)

(defun fill-array-with-integers! (array)
  "Set the elements of array to integers from 0, in row-major order."
  (let ((type (array-element-type array)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref array i) (coerce i type))))
  array)

(defparameter *a* (fill-array-with-integers! (make-array '(4 5))))

(deftestsuite xarray () ()
  :equality-test #'equalp)

(addtest permutation
  (ensure-same (take (permutation *a* 1 0))
	       #2A((0 5 10 15)
		   (1 6 11 16)
		   (2 7 12 17)
		   (3 8 13 18)
		   (4 9 14 19))))

(addtest slice-drop
  (ensure-same (take (slice *a* 1 :all))
	       #(5 6 7 8 9)))

(addtest slice-rev
  (ensure-same (take (slice *a* :all :rev))
	       #2A((4 3 2 1 0)
		   (9 8 7 6 5)
		   (14 13 12 11 10)
		   (19 18 17 16 15))))

(addtest slice-rectangle-negative-index
  (ensure-same (take (slice *a* '(2 3) '(-2 -1)))
	       #2A((13 14) (18 19))))

(addtest row-major-projection
  (ensure-same (take (row-major-projection *a* 2 10))
	       #2A((0 1 2 3 4 5 6 7 8 9) (10 11 12 13 14 15 16 17 18 19))))

(addtest carray-1
  (let ((array (carray* '(2 3) 0 1d0 2 6/2 5s0 6)))
    (ensure-same array #2A((0.0d0 1.0d0 2.0d0) (3.0d0 5.0d0 6.0d0)))
    (ensure-same (array-element-type array) 'double-float)))

(addtest carray-2
  (let ((array (carray* '(2 3) 0 1d0 2 6/2 5s0 #C(6 19))))
    (ensure-same array 
		 #2A((#C(0.0d0 0.0d0) #C(1.0d0 0.0d0) #C(2.0d0 0.0d0))
		     (#C(3.0d0 0.0d0) #C(5.0d0 0.0d0) #C(6.0d0 19.0d0))))
    (ensure-same (array-element-type array) '(complex double-float))))

(run-tests :suite 'xarray)