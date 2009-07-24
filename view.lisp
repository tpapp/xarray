(in-package :xarray)

;;;; !!!! FIXME: issues I need to think about
;;;;
;;;; Currently I am allowing permutation and slice indexes to appear
;;;; more than once, this could be useful.  Should I forbid this?  Not
;;;; in the CL spirit.

(defclass view ()
  ((ancestor :initarg :ancestor :reader ancestor 
	     :documentation "an underlying object that is accessible with xref")))

(defmethod xtype ((object view))
  (xtype (ancestor object)))

(defmethod print-object ((object view) stream)
  (print-unreadable-object (object stream :type t :identity t)
    ;; TAKEing the easy way out, need to write this decently one day
    (print (take object) stream)))

;;;; permutations
;;;;
;;;; Permutations interchange the dimension indexes.

(defgeneric permutation (object &rest permutation)
  (:documentation "Permutation of indexes."))

;;;; permutation-view
;;;;
;;;; A general permutation, the only assumption is that object is
;;;; xrefable.

(defclass permutation-view (view)
  ((permutation :initarg :permutation :type int-vector
		:documentation "permutation")
   (dimensions :initarg :dimensions :reader dimensions :type int-vector
	       :documentation "dimensions")))

(defmethod permutation (object &rest permutation)
  (let ((permutation (coerce permutation 'int-vector)))
    (unless (vector-within-dimension-p #|valid-permutation-p|#
	     permutation (xrank object))
      (error "permutation ~a is not valid" permutation))
    (make-instance 'permutation-view :ancestor object :permutation permutation)))

(defmethod initialize-instance :after ((object permutation-view) &key)
  ;; save dimensions
  (with-slots (ancestor permutation dimensions) object
    (setf dimensions 
	  (coerce (permute-sequence permutation (xdims ancestor)) 'int-vector)))
  ;; !!! note: do we want to cache coefficients for calculating rm-index? not now
  object)

(defmethod xrank ((object permutation-view))
  (xrank (ancestor object)))

(defmethod xdims ((object permutation-view))
  (coerce (dimensions object) 'list))

(defmethod xdims* ((object permutation-view))
  (dimensions object))

(defmethod xdim ((object permutation-view) axis-number)
  (aref (dimensions object) axis-number))

(defmethod xref ((object permutation-view) &rest subscripts)
  (with-slots (ancestor permutation) object
    (apply #'xref ancestor (permute-sequence permutation subscripts))))

(defmethod (setf xref) (value (object permutation-view) &rest subscripts)
  (with-slots (ancestor permutation) object
    (setf (apply #'xref ancestor (permute-sequence permutation subscripts))
	  value)))

;;;; transpose
;;;;
;;;; !!! maybe I should write transpose as a special case of
;;;; !!! permutation.  Could make it much faster.  Do it when needed.


;;; AJR:  We have a possible evil conflict -- Lisp-Matrix Slices are
;;; something else, not corresponding to this matlab/R/python-ish
;;; meaning of slicing.

;;;; slices
;;;;
;;;; A slice is a view on a subset of indexes in each dimension.  When
;;;; a slice contains only a single index in a dimension, that
;;;; dimension can be dropped from the slice.  For valid index
;;;; specifications, see parse-index-specifications.

;;; AJR: point -- marginalization of a table into a subtable
;;; (guarantee rectangle-ness -- no ragged results).

(defgeneric slice (object &rest index-specifications)
  (:documentation "Slice of an object."))

;;;; slice-view
;;;;
;;;; A general slice, the only assumption is that object is xrefable.

(defclass slice-view (view)
  ((index-specifications :initarg :index-specifications
			 :reader index-specifications
			 :type vector
			 :documentation "vector of index specifications")
   (dimensions :initarg :dimensions
	       :reader dimensions
	       :type int-vector
	       :documentation "dimensions, cached")))

(defun parse-index-specification (index-specification dimension)
  "Parse a index specification, returning either

- an integer i, with the dimension dropped,

- a pair (start . length), where start is the starting index, and
  length is the number of valid indexes.  If dimension is negative,
  indexing is decreasing from start.  This is used for contiguous
  indexes

- a vector of indexes, for non-contiguous indexing.

All resulting indexes are valid, ie they are integers in
[0,dimension).

Range specifications:

Negative integers are interpreted as counted backwards from the right
edge of the domain, ie i < 0 denotes element dimension+i.

Valid index-specification specifications (a and b are integers):
 
 a                    index a, dimension dropped
 (list a)             index a, dimension not dropped
 (list a b)           range between a and b, inclusive.  If b < a, reversed.
 :all                 all valid indexes, increasing order
 :rev                 all valid indexes, decreasing order
 (vector i1 ... in)   vector of indexes, must be a set (no repetition)."
  (flet ((convert-and-check (i)
	   "Convert negative indexes if necessary, also check that they are valid."
	   (cond
	     ((and (<= 0 i) (< i dimension)) i)
	     ((and (minusp i) (<= 0 (+ dimension i))) (+ dimension i))
	     (t (error "subscript ~a is not in [0,~a)" i dimension)))))
    (cond
      ;; all
      ((and (symbolp index-specification) (eq index-specification :all))
       (cons 0 dimension))
      ;; all, reversed
      ((and (symbolp index-specification) (eq index-specification :rev))
       (cons (1- dimension) (- dimension)))
      ;; single index, dimension dropped
      ((integerp index-specification)
       (convert-and-check index-specification) 0)
      ;; range or single index (dimension not dropped)
      ((and (listp index-specification) (every #'integerp index-specification))
       (ecase (length index-specification)
	 (1 (cons (convert-and-check (car index-specification)) 1))
	 (2 (destructuring-bind (a b) index-specification
	      (let ((left (convert-and-check a))
		    (right (convert-and-check b)))
		(cons left (- right left (if (<= left right) -1 1))))))))
      ;; vector, arbitrary specification
      ((vectorp index-specification)
       (if (vector-within-dimension-p #|valid-integer-subset-p|#
	    index-specification dimension)
	   index-specification
	   (error "~a is not a valid integer subset of [0,~a)"
		  index-specification dimension)))
      (t (error "can't interpret index-specification ~a" index-specification)))))

(defun index-specification-dimension (index-specification)
  "Return dimension of parsed index-specification.  Internal function,
no error checking.  Return nil for dropped dimensions."
  (etypecase index-specification
    (integer nil)			      ; dropped
    (cons (abs (cdr index-specification)))    ; range
    (vector (length index-specification))))   ; enumerated indexes

(defmethod slice (object &rest index-specifications)
  ;; Implementation note: we cache dimensions.
  (let* ((parsed-index-specifications (map 'vector
					   #'parse-index-specification
					   index-specifications
					   (xdims object)))
	 (dimensions (iter
		       (for is :in-vector parsed-index-specifications)
		       (for d := (index-specification-dimension is))
		       (when d
			 (collecting d)))))
    (assert (= (length parsed-index-specifications) (xrank object)))
    (make-instance 'slice-view :ancestor object
		   :index-specifications parsed-index-specifications
		   :dimensions (coerce dimensions 'int-vector))))

(defmethod xrank ((object slice-view))
  (length (dimensions object)))

(defmethod xdims ((object slice-view))
  (coerce (dimensions object) 'list))

(defmethod xdims* ((object slice-view))
  (dimensions object))

(defmethod xdim ((object slice-view) axis-number)
  (aref (dimensions object) axis-number))

(defun convert-slice-subscripts (index-specifications subscripts)
  "Convert subscripts using index-specifications."
  (iter
    (for is :in-vector index-specifications)
    (generate subscript :in subscripts)
    (collecting
      (etypecase is
	(integer is)
	(cons (let* ((start (car is))	    ; first index of range
		     (length* (cdr is))	    ; has sign in it
		     (length (abs length*)) ; with sign removed
		     (ss (next subscript))) ; subscript
		(unless (and (<= 0 ss) (< ss length))
		  (error "subscript ~a is not in [0,~a)" ss length))
		(+ start (* (signum length*) ss))))
	(vector (aref is (next subscript)))))))

(defmethod xref ((object slice-view) &rest subscripts)
  (with-slots (ancestor index-specifications dimensions) object
  ;; Check that the length of subscripts matches rank.
    (unless (= (length dimensions) (length index-specifications))
      (error "incorrect number of subscripts"))
    ;; convert and apply
    (apply #'xref ancestor 
	   (convert-slice-subscripts index-specifications subscripts))))

(defmethod (setf xref) (value (object slice-view) &rest subscripts)
  (with-slots (ancestor index-specifications dimensions) object
  ;; Check that the length of subscripts matches rank.
    (unless (= (length dimensions) (length index-specifications))
      (error "incorrect number of subscripts"))
    ;; convert and apply
    (setf (apply #'xref ancestor 
		 (convert-slice-subscripts index-specifications subscripts))
	  value)))

;;;; row-major-projection
;;;;
;;;; A row-major-projection is a view that maps elements to an
;;;; xrefable object using a flattened index calculated as if the
;;;; storage model was row-major.

(defgeneric row-major-projection (object &rest dimensions)
  (:documentation "Row major projection to an xrefable object.  Total
  size needs to match the product of dimensions."))

;;;; row-major-projection-view
;;;;
;;;; A general, unoptimized case that makes no assumption on the
;;;; storage model.  If you are projecting onto an array, you might
;;;; want to use a more specialized class if you are concerned about
;;;; speed.

(defclass row-major-projection-view (view)
  ((dimensions :initarg :dimensions :reader dimensions
	       :type int-vector
	       :documentation "dimensions")
   (ancestor-dimensions :initarg :ancestor-dimensions :reader ancestor-dimensions
			:type (simple-array integer (*))
			:documentation "dimensions of ancestor")))

(defmethod initialize-instance :after ((object row-major-projection-view) &key)
  ;; save ancestor-dimensions
  (with-slots (ancestor ancestor-dimensions) object
    (setf ancestor-dimensions 
	  (coerce (xdims ancestor) 'int-vector)))
  ;; !!! note: do we want to cache coefficients for calculating rm-index? not now
  object)

(defmethod row-major-projection (object &rest dimensions)
  (unless (= (reduce #'* dimensions) (xsize object))
    (error "Size of the object does not match the product of dimensions."))
  (make-instance 'row-major-projection-view 
		 :ancestor object
		 :dimensions (coerce dimensions 'int-vector)))

(defmethod xrank ((object row-major-projection-view))
  (xrank (ancestor object)))

(defmethod xdims ((object row-major-projection-view))
  (coerce (dimensions object) 'list))

(defmethod xdim ((object row-major-projection-view) axis-number)
  (aref (dimensions object) axis-number))

(defmethod xsize ((object row-major-projection-view))
  (reduce #'* (dimensions object)))

(defmethod xref ((object row-major-projection-view) &rest subscripts)
  (with-slots (ancestor dimensions ancestor-dimensions) object
    (apply #'xref ancestor (rm-subscripts ancestor-dimensions
					  (rm-index dimensions subscripts)))))

(defmethod (setf xref) (value (object row-major-projection-view) &rest subscripts)
  (with-slots (ancestor dimensions ancestor-dimensions) object
    (setf (apply #'xref ancestor (rm-subscripts ancestor-dimensions
					  (rm-index dimensions subscripts)))
	  value)))
