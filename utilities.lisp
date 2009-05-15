(in-package :xarray)

(declaim (inline within-dimension-p vector-within-dimension-p 
		 map-and-convert-function))

;; functions to check for validity of subscripts and indexes

(defun within-dimension-p (subscript dimension)
  "Check if 0 <= subscript < dimension."
  (and (integerp subscript) (<= 0 subscript) (< subscript dimension)))

(defun vector-within-dimension-p (vector dimension)
  "Return non-nil iff all elements in vector are in [0,dimension)."
  (dotimes (i (length vector))
    (let ((elt (aref vector i)))
      (unless (within-dimension-p elt dimension)
	(return-from vector-within-dimension-p nil))))
  t)

;; conversion functions between row-major indexes and subscripts

(defun rm-index (dimensions subscripts)
  "Return a row-major flat index for given subscripts (coerced to a vector),
using dimensions (also a vector).  Checks for boundaries and rank."
  (let ((rank (length dimensions))
	(subscripts (coerce subscripts '(simple-array integer (*)))))
    (unless (= (length subscripts) rank)
      (error "number of subscripts does not match rank"))
    (do ((i (1- rank) (1- i))
	 (prod 1)
	 (sum 0))
	((minusp i) sum)
      (let ((d (aref dimensions i))
	    (s (aref subscripts i)))
	(unless (within-dimension-p s d)
	  (error 'xref-out-of-bounds-error :subscripts (coerce subscripts 'list)
		 :dimensions dimensions))
	(incf sum (* prod s))
	(setf prod (* prod d))))))


(defun rm-subscripts (dimensions i)
  "Return i decomposed to a list of subscripts, taking
  dimensions (which is a vector) as a row-major indexing scheme.  No
  error checking is performed, meant for internal use."
;  (check-type dimensions (array * (*)))
  ;; !!! speed it up if necessary, type declarations
  (do ((dimension-index (1- (length dimensions)) (1- dimension-index))
       (subscripts nil))
      ((minusp dimension-index) subscripts)
    (multiple-value-bind (quotient remainder)
	(floor i (aref dimensions dimension-index))
      (setf subscripts (cons remainder subscripts)
	    i quotient))))

;; (defun test-rm-subscripts (dimensions)
;;   (let ((array (make-array (coerce dimensions 'list))))
;;     (dotimes (i (reduce #'* dimensions))
;;       (let ((subscripts (rm-subscripts dimensions i)))
;; 	(unless (= (apply #'array-row-major-index array subscripts) i)
;; 	  (error "(rm-subscripts ~a ~a) = ~a, incorrect" dimensions i subscripts))))
;;     t))

;; (test-rm-subscripts #(9 2 1 2 3 4 5))

;; (defun test-rm-index (dimensions)
;;   (dotimes (i (reduce #'* dimensions))
;;     (let ((subscripts (rm-subscripts dimensions i)))
;;       (unless (= (funcall #'rm-index dimensions (coerce subscripts 'vector)) i)
;; 	(error "(rm-subscripts ~a ~a) = ~a, incorrect" dimensions i subscripts))))
;;   t)

;; (test-rm-index #(9 2 1 2 3 4 5))


(defun valid-permutation-p (vector &optional 
			    (dimension (length vector) dimension-given-p))
  "Return non-nil (t) iff vector is a valid permutation of integers
  0,1,...,(1- dimension)."
  (let ((flags (make-array dimension :element-type 'bit :initial-element #b0)))
    (if (and dimension-given-p (/= (length vector) dimension))
	(return-from valid-permutation-p nil))
    (dotimes (i dimension)
      (let ((p (aref vector i)))
	;; check if we have seen this index
	(if (and (<= 0 p) (< p dimension) (zerop (aref flags p)))
	    (setf (aref flags p) 1)
	    (return-from valid-permutation-p nil))))
    ;; check that wehave seen all indexes
    (dotimes (i dimension)
      (when (zerop (aref flags i))
	(return-from valid-permutation-p nil)))
    t))

(defun permute-sequence (permutation sequence)
  "Return permuted sequence as a list.  Works even if indexes repeat."
  (let* ((dimension (length permutation))
	 (vector (coerce sequence 'vector)))
    (unless (= dimension (length vector))
      (error "sequence length does not match permutation"))
    (do ((i (1- dimension) (1- i))
	  (permuted nil (cons (aref vector (aref permutation i)) permuted)))
	 ((minusp i) permuted))))
	
  (defun valid-integer-subset-p (vector dimension)
  "Return non-nil (t) iff vector is a valid subset (ie with no
  repetition) of integers 0,1,...,(1- dimension)."
  (let ((flags (make-array dimension :element-type 'bit :initial-element #b0)))
    (dotimes (i (length vector))
      (let ((p (aref vector i)))
	;; check if we have seen this index
	(if (and (<= 0 p) (< p dimension) (zerop (aref flags p)))
	    (setf (aref flags p) 1)
	    (return-from valid-integer-subset-p nil))))
    t))

;;;;  map & conversion interface
;;;;
;;;;  Some functions provide the option of both a map and conversion
;;;;  to another type.  The convenience function below this defines
;;;;  this interface.

(defun map-and-convert-function (map-function source-type
				 destination-type &optional identity-nil-p)
  (cond 
    ((and (not (equal source-type destination-type))
	  (not (eq destination-type t))
	  (not map-function))
     (lambda (x) (coerce x destination-type)))
    (map-function map-function)
    (t (if identity-nil-p
	   nil
	   #'identity))))

;;;;  convenience functions for setting up arrays

(defun numeric-type-classifier (list)
  "Numeric type classifier, finds the smallest subtype that can
  accomodate the elements of list, in the ordering fixnum < integer <
  float < complex < t.  Rational, float (any kind) are classified as
  float, and complex numbers as (complex double-float).  Meant to be
  used by simple array-constructing functions.
  Upgraded-array-element-type is called on end result."
  (upgraded-array-element-type 
   (case (reduce #'max list
		 :key (lambda (x)
			(typecase x
			  (fixnum 0)
			  (integer 1)
			  ((or rational float) 2)
			  (complex 3)
			  (t 4))))
     (0 'fixnum)
     (1 'integer)
     (2 'double-float)
     (3 '(complex double-float))
     (4 t))))

(defun fill-array-with-list (array list)
  "Fills array with elements from list, coerced to the appropriate
  type.  No error checking, meant to be used internally.  Return array."
  (let ((i 0)
	(type (array-element-type array)))
    (dolist (l list)
      (setf (row-major-aref array i) (coerce l type))
      (incf i)))
  array)
