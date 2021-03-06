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

(defun random-array (dimensions &key 
                     (element-type 'fixnum)
                     (function (lambda () 
                       (random (coerce 10 element-type)))))
  "Fill array with results from function, called for each element."
  (let ((array (make-array dimensions :element-type element-type)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref array i) (funcall function)))
    array))

(defun integer-vector (n)
  "A vector of n integers from 0."
  (fill-array-with-integers! (make-array n :element-type 'fixnum)))

(defun random-permutation (n)
  "Return a random permutation of the first n integers from 0."
  (shuffle-vector! (integer-vector n)))

(defun sort-using-order (vector)
  (take 'array (slice vector (xorder vector #'<))))

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


;;; TEST SUITE

(deftestsuite xarray () ()
  :equality-test #'equalp
  :dynamic-variables (*a* (fill-array-with-integers! (make-array '(4 5)))))

;;; *a* is available for interactive testing

(defparameter *a* (fill-array-with-integers! (make-array '(4 5))))

(addtest (xarray)
  indexing
  (ensure (test-rm-index #(9 2 1 2 3 4 5)))
  (ensure (test-rm-subscripts #(9 2 1 2 3 4 5)))
  (ensure (test-cm-index '(2 94 7 11))))

;;; VIEW

(addtest (xarray)
  permutation
  (ensure-same (take 'array (permutation *a* 1 0))
	       #2A((0 5 10 15)
		   (1 6 11 16)
		   (2 7 12 17)
		   (3 8 13 18)
		   (4 9 14 19))))

(addtest (xarray)
  slice-drop
  (ensure-same (take 'array (slice *a* 1 :all))
	       #(5 6 7 8 9)))

(addtest (xarray)
  slice-rev
  (ensure-same (take 'array (slice *a* :all :rev))
	       #2A((4 3 2 1 0)
		   (9 8 7 6 5)
		   (14 13 12 11 10)
		   (19 18 17 16 15))))

(addtest (xarray)
  drop
  (ensure-same (take 'array
                     (drop (fill-array-with-integers! (make-array '(1 4 1 5 1 1)))))
               #2A((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19))))

(addtest (xarray)
  slice-rectangle-negative-index
  (ensure-same (take 'array (slice *a* '(2 3) '(-2 -1)))
	       #2A((13 14) (18 19))))

(addtest (xarray)
  (ensure-same (take t (slice *a* 1 :all))
               #(5 6 7 8 9)))

;; (addtest (xarray)
;;   row-major-projection
;;   (ensure-same (take 'array (column-major-projection *a* 2 10))
;; 	       #2A((0 1 2 3 4 5 6 7 8 9) (10 11 12 13 14 15 16 17 18 19))))

(addtest (xarray)
  column-major-projection
  (ensure-same (take 'array (column-major-projection *a* 2 10))
               #2A((0 10 1 11 2 12 3 13 4 14) (5 15 6 16 7 17 8 18 9 19)))
  (ensure-same (take 'array (column-major-projection *a*))
               #(0 5 10 15 1 6 11 16 2 7 12 17 3 8 13 18 4 9 14 19)))

(addtest (xarray)
  flat-reduction
  ;; because of the implementation, this is also testing the flat view
  (ensure-same (xsum *a*) 190)
  (ensure-same (xprod *a*) 0)
  (ensure-same (xprod #(1 2 3)) 6)
  (ensure-same (xmax *a*) 19)
  (ensure-same (xmin *a*) 0))

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

(defun xmap-test (dims function nargs)
  (let* ((args (iter
                 (repeat nargs)
                 (collecting (random-array dims))))
         (result (apply #'xmap 'array
                        function args)))
    (iter
      (for i :from 0 :below (array-total-size result))
      (always (= (row-major-aref result i)
                 (apply function (mapcar (lambda (arg)
                                           (row-major-aref arg i))
                                         args)))))))
                                   
(addtest (xarray)
  xmap
  (ensure (xmap-test '(4 5 9) #'+ 5))
  (ensure (xmap-test '11 #'- 3))
  (ensure (xmap-test '(7 3) #'cos 1))
  (ensure (xmap-test '(119 7 13) #'exp 1)))

(addtest (xarray)
  xop
  (ensure-same (xop 'array #'* #(1 2 3) #(4 5 6))
               #2A((4 5 6) (8 10 12) (12 15 18))))

(addtest (xarray)
  xcollect
  (let ((counter 0))
    (ensure-same
     (xcollect 5 (lambda ()
                   (vector (incf counter) (incf counter))))
     #2A((1 2) (3 4) (5 6) (7 8) (9 10))
     :test #'x=)))

;;; ATOMS -- !!! need some unit testa

; (run-tests :suite 'xarray)
