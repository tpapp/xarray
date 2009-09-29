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

;;;; conversion functions between row-major indexes and subscripts
;;;;
;;;; Needed for mapping flat indexes to array subscripts.  When
;;;; traversing an array, it is recommended that column major
;;;; subscripts are used (faster implementation), but row major
;;;; subscripts can be used when they conform to the underlying
;;;; representation (eg CL arrays).

(defun rm-index (dimensions subscripts)
  "Return a row-major flat index for given subscripts (coerced to a
vector, list also accepted), using dimensions (also coerced to a
vector, list also accepted).  Checks for boundaries and rank."
  (let ((rank (length dimensions))
	(subscripts (coerce subscripts 'fixnum-vector))
	(dimensions (coerce dimensions 'fixnum-vector)))
    (unless (= (length subscripts) rank)
      (error "number of subscripts does not match rank"))
    (do ((i (1- rank) (1- i))
	 (prod 1)
	 (sum 0))
	((minusp i) sum)
      (let ((d (aref dimensions i))
	    (s (aref subscripts i)))
	(unless (within-dimension-p s d)
	  (error 'xref-out-of-bounds-error
		 :subscripts (coerce subscripts 'list)
		 :dimensions dimensions))
	(incf sum (* prod s))
	(setf prod (* prod d))))))

(defun rm-subscripts (dimensions i)
  "Return i decomposed to a list of subscripts, taking
  dimensions (which is coerced to a vector) as a row-major indexing
  scheme.  No error checking is performed, meant for internal use."
  ;; !!! speed it up if necessary, type declarations
  (let ((dimensions (coerce dimensions 'fixnum-vector)))
    (do ((dimension-index (1- (length dimensions)) (1- dimension-index))
	 (subscripts nil))
	((minusp dimension-index) subscripts)
      (multiple-value-bind (quotient remainder)
	  (floor i (aref dimensions dimension-index))
	(setf subscripts (cons remainder subscripts)
	      i quotient)))))

(defun cm-index (dimensions subscripts)
  "Calculate the column-major flat index from subscripts (list of fixnums) and dimensions (list of fixnums)."
  (declare (optimize speed))
  (iter
    (with cumprod := 1)
    (for d :in dimensions)
    (for s :in subscripts)
    (declare (fixnum s d cumprod))
    (assert (and (<= 0 s) (< s d)))
    (summing (the fixnum (* s cumprod)))
    (setf cumprod (* cumprod d))))

(defun cm-subscripts (dimensions i)
  "Return the column-major subscripts (list) for flat index
i (fixnum), using dimensions (list of fixnums).  No error checking,
for internal use only."
  (declare (optimize speed))
  (check-type i fixnum)
  (iter
    (for d :in dimensions)
    (for (values quotient remainder) := (floor i d))
    (declare (fixnum d quotient remainder))
    (collecting remainder)
    (setf i quotient)))

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
  (let ((flags (make-array dimension :element-type 'bit
			   :initial-element #b0)))
    (dotimes (i (length vector))
      (let ((p (aref vector i)))
	;; check if we have seen this index
	(if (and (<= 0 p) (< p dimension) (zerop (aref flags p)))
	    (setf (aref flags p) 1)
	    (return-from valid-integer-subset-p nil))))
    t))

(defun invert-permutation (permutation)
  "Return the inverse of a valid permutation vector (validity is
not checked, results are not defined for invalid permutations)."
  (check-type permutation fixnum-vector)
  (let* ((n (length permutation))
         (result (make-array n :element-type 'fixnum)))
    (dotimes (i n)
      (setf (aref result (aref permutation i)) i))
    result))


;;;;  convenience functions for setting up arrays

(defun numeric-type-classifier (list)
  "Numeric type classifier, finds the smallest subtype that can
  accomodate the elements of list, in the ordering fixnum < integer <
  float < complex < t.  Rational, float (any kind) are classified as
  double-float, and complex numbers as (complex double-float).  Meant
  to be used by simple array-constructing functions.
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

(defun make-symbol* (&rest args)
  "Build a symbol by concatenating each element of ARGS, and intern it
  in the current package.  Elements can be strings or symbols."
  (intern (apply #'concatenate 'string
                 (mapcar (lambda (arg)
                           (etypecase arg
                             (symbol (symbol-name arg))
                             (string arg)))
                         args))))
