(in-package :xarray)

;;;;  Basic operations for objects with an xarray interface.
;;;;
;;;;  Bivariate numerical operations are defined on two _conformable_
;;;;  objects (ie objects having the same xdim), or an object and the
;;;;  scalar.  Sophisticated recycling rules (eg R's vector recycling)
;;;;  are NOT supported, you have to be explicit and use a view for
;;;;  that.  The reason for this is that these things look clever
;;;;  initially, but in the long run they are just another obscure
;;;;  source of bugs.
;;;;
;;;;  Operations cannot guess the element type of the result, so an
;;;;  optional argument (with a reasonable default, if possible) is
;;;;  available for that purpose.  The result type does not have to be
;;;   a Lisp type, if you are producing something else.  The default
;;;   methods return Lisp arrays.

(defun xdims= (a b)
  "Return non-nil iff the dimensions of A and B are the same."
  (equal (xdims a) (xdims b)))

(defmacro define-elementwise-operation (operation &optional (name (make-symbol* "X")))
  `(defgeneric ,name (a b &optional result-type)
     (:documentation "The elementwise sum of two conformable arrays, or an array and a scalar.")
     (:method (a b &optional (result-type 'number))
       ;; default: both A and B are arrays
       (assert (xdims= a b))
       (let* ((dims (xdims a))
	      (result (make-array dims :element-type result-type))
	      (dims-vec (coerce dims 'fixnum-vector)))
	 (dotimes (i (xsize a))
	   (let ((subscripts (rm-subscripts dims-vec i)))
	     (setf (row-major-aref result i)
		   (,operation (apply #'xref a subscripts) (apply #'xref b subscripts)))))
	 result))
     (:method (a (b number) &optional (result-type 'number))
       ;; B is a scalar
       (let* ((dims (xdims a))
	      (result (make-array dims :element-type result-type))
	      (dims-vec (coerce dims 'fixnum-vector)))
	 (dotimes (i (xsize a))
	   (let ((subscripts (rm-subscripts dims-vec i)))
	     (setf (row-major-aref result i)
		   (,operation (apply #'xref a subscripts) b))))
	 result))
     (:method ((a number) b &optional (result-type 'number))
       ;; A is a scalar (I wish I could just call the method with B
       ;; and A, but would not work for - and /).
       (let* ((dims (xdims b))
	      (result (make-array dims :element-type result-type))
	      (dims-vec (coerce dims 'vector)))
	 (dotimes (i (xsize b))
	   (let ((subscripts (rm-subscripts dims-vec i)))
	     (setf (row-major-aref result i)
		   (,operation a (apply #'xref b subscripts)))))
	 result))))

(define-elementwise-operation +)
(define-elementwise-operation -)
(define-elementwise-operation *)
(define-elementwise-operation /)

(defgeneric x= (a b &optional eps)
  (:documentation "Return non-nil if A and B have the same
dimensions,, and the sup|A-B| <= eps.")
  (:method (a b &optional (eps #.(sqrt single-float-epsilon)))
    (assert (xdims= a b))
    (let ((dims (xdims a)))
      (dotimes (i (reduce #'* dims))
	(let ((subscripts (cm-subscripts dims i)))
	  (unless (<= (abs (- (apply #'xref a subscripts)
			      (apply #'xref b subscripts)))
		      eps)
	    (return-from x= nil)))))
    t))

(defun order (vector predicate &key key stable-p)
  "Return a vector of integers starting from 0, representing the
permutation of elements in vector that would result if sorted
according to predicate (which you can use in slice, etc).  Key is
passed to sort.  If stable-p, stable-sort is used."
  (bind (((length &rest other-dimensions) (xdims vector))
         (work (make-array length)))
    (when other-dimensions
      (error "first argument is not a vector"))
    (dotimes (i length)
      (setf (aref work i) (cons (xref vector i) i)))
    (funcall (if stable-p #'stable-sort #'sort) work predicate
             :key (if key
                      (compose key #'car)
                      #'car))
    (map 'fixnum-vector #'cdr work)))
