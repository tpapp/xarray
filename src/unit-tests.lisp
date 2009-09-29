;;;;
;;;;  Unit tests for xarray
;;;;
;;;;  Notes: currently, these tests are very basic, and mostly serve
;;;;  as examples.  Tests will become more important when methods are
;;;;  optimized.

(asdf:oos 'asdf:load-op :xarray)
(asdf:oos 'asdf:load-op :lift)
(asdf:oos 'asdf:load-op :iterate)

(in-package :cl-user)

(defpackage #:xarray-unit-tests
    (:use :cl :xarray :lift :iterate))

(in-package :xarray-unit-tests)

;;; HELPER FUNCTIONS

(defun fill-array-with-integers! (array)
  "Set the elements of array to integers from 0, in row-major order."
  (let ((type (array-element-type array)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref array i) (coerce i type))))
  array)

(defun shuffle-vector! (vector)
  "Randomly shuffle elements in vector (modifying the latter).
Knuth (aka Fisher-Yates) shuffle."
  (check-type vector vector)
  (let ((n (length vector)))
    (unless (< 1 n)
      (return-from shuffle-vector! vector))
    (iter
      (for i :from n :downto 2)
      (rotatef (aref vector (random i))
               (aref vector (1- i))))
    vector))

(defun integer-vector (n)
  "A vector of n integers from 0."
  (fill-array-with-integers! (make-array n :element-type 'fixnum)))

(defun random-permutation (n)
  "Return a random permutation of the first n integers from 0."
  (shuffle-vector! (integer-vector n)))

(defun sort-using-order (vector)
  (take (slice vector (order vector #'<)) 'array))

(defun test-rm-subscripts (dimensions)
  (let ((array (make-array (coerce dimensions 'list))))
    (dotimes (i (reduce #'* dimensions))
      (let ((subscripts (rm-subscripts dimensions i)))
	(unless (= (apply #'array-row-major-index array subscripts) i)
	  (error "(rm-subscripts ~a ~a) = ~a, incorrect" dimensions i subscripts))))
    t))

(defun test-rm-index (dimensions)
  (dotimes (i (reduce #'* dimensions))
    (let ((subscripts (rm-subscripts dimensions i)))
      (unless (= (funcall #'rm-index dimensions (coerce subscripts 'vector)) i)
	(error "(rm-subscripts ~a ~a) = ~a, incorrect" dimensions i subscripts))))
  t)

(defun test-cm-index (dimensions)
  (dotimes (i (reduce #'* dimensions))
    (let ((subscripts (cm-subscripts dimensions i)))
      (unless (= (cm-index dimensions subscripts) i)
	(error "(cm-subscripts ~a ~a) /= ~a" dimensions i subscripts))))
  t)


(deftestsuite xarray () ()
  :equality-test #'equalp
  :dynamic-variables (*a* (fill-array-with-integers! (make-array '(4 5)))))

;;; UTILITIES

(addtest (xarray)
  indexing
  (ensure (test-rm-index #(9 2 1 2 3 4 5)))
  (ensure (test-rm-subscripts #(9 2 1 2 3 4 5)))
  (ensure (test-cm-index '(2 94 7 11))))

;;; VIEW

(addtest (xarray)
  permutation
  (ensure-same (take (permutation *a* 1 0) 'array)
	       #2A((0 5 10 15)
		   (1 6 11 16)
		   (2 7 12 17)
		   (3 8 13 18)
		   (4 9 14 19))))

(addtest (xarray)
  slice-drop
  (ensure-same (take (slice *a* 1 :all) 'array)
	       #(5 6 7 8 9)))

(addtest (xarray)
  slice-rev
  (ensure-same (take (slice *a* :all :rev) 'array)
	       #2A((4 3 2 1 0)
		   (9 8 7 6 5)
		   (14 13 12 11 10)
		   (19 18 17 16 15))))

(addtest (xarray)
  slice-rectangle-negative-index
  (ensure-same (take (slice *a* '(2 3) '(-2 -1)) 'array)
	       #2A((13 14) (18 19))))

(addtest (xarray)
  row-major-projection
  (ensure-same (take (row-major-projection *a* 2 10) 'array)
	       #2A((0 1 2 3 4 5 6 7 8 9) (10 11 12 13 14 15 16 17 18 19))))

;;; ARRAY

(addtest (xarray)
  carray-1
  (let ((array (carray* '(2 3) 0 1d0 2 6/2 5s0 6)))
    (ensure-same array #2A((0.0d0 1.0d0 2.0d0) (3.0d0 5.0d0 6.0d0)))
    (ensure-same (array-element-type array) (upgraded-array-element-type 'double-float))))

(addtest (xarray)
  carray-2
  (let ((array (carray* '(2 3) 0 1d0 2 6/2 5s0 #C(6 19))))
    (ensure-same array 
		 #2A((#C(0.0d0 0.0d0) #C(1.0d0 0.0d0) #C(2.0d0 0.0d0))
		     (#C(3.0d0 0.0d0) #C(5.0d0 0.0d0) #C(6.0d0 19.0d0))))
    (ensure-same (array-element-type array) (upgraded-array-element-type '(complex double-float)))))

;;; OPERATIONS

;;; !!! more unit tests need to be written

(addtest (xarray)
  order
  (ensure-same (integer-vector 5) (sort-using-order (random-permutation 5)))
  (ensure-same (integer-vector 129) (sort-using-order (random-permutation 129)) :test #'equalp))

(run-tests :suite 'xarray)
