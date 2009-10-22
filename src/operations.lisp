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

(defun xorder (vector predicate &key key stable-p)
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

(defmacro define-flat-reduction (name modify-macro docstring &body body)
  "Define a generic function named NAME which reduces its argument
elementwise.  body is spliced into the iterate loop, and can be used
for early returns, etc.  See code for variable names."
  (check-type name symbol)
  (check-type modify-macro symbol)
  (check-type docstring (or null string))
  `(defgeneric ,name (a)
     (:documentation ,docstring)
     (:method (a)
       (let* ((a (flat a))
              (n (xsize a))
              (result (xref a 0)))
         (when (< 1 n)
           (iter
             (for i :from 1 :below n)
             (,modify-macro result (xref a i))
             ,@body))
         result))
     (:method ((a array))
       (let ((n (array-total-size a))
             (result (row-major-aref a 0)))
         (when (< 1 n)
           (iter
             (for i :from 1 :below n)
             (,modify-macro result (row-major-aref a i))
             ,@body))
         result))))
    
(define-flat-reduction xsum incf "Sum of the elements.")
(define-flat-reduction xprod multf "Product of the elements."
  (when (zerop result) (return)))
(define-flat-reduction xmax maxf "Maximum of the elements.")
(define-flat-reduction xmin minf "Minimum of the elements.")

;;;; Instead of defining x... operator names for every function on
;;;; earth, we provide some generic mapping constructs.
;;;;
;;;; There are two ways to specify a target: using a symbol or a list,
;;;; in which case the object will be created using xcreate or
;;;; xcreate*, using the dimensions of the first argument, or
;;;; supplying a target object of the appropriate type.

;;;; ?? xmap could call xmap*, which could be a generic function? for
;;;; those wanting to speed things up.

(defun xmap (target-or-spec function &rest arguments)
  "Apply function to arguments elementwise, and save the result in target."
  (flet ((check-dims (dims arguments)
           (dolist (arg arguments)
             (unless (equalp (xdims arg) dims)
               (error "Dimensions ~a are incompatible with target dimension ~a."
                      (xdims arg) dims)))))
    (let* ((target
            (typecase target-or-spec
              ((or symbol list)
                 (unless arguments
                   (error "Can't determine target dimensions without arguments."))
                 (let ((dims (xdims (car arguments))))
                   (check-dims dims (cdr arguments))
                   (xcreate* target-or-spec dims)))
              (t (check-dims (xdims target-or-spec) arguments)
                 target-or-spec)))
           (flat-arguments (mapcar #'column-major-projection arguments))
           (flat-target (column-major-projection target)))
      (dotimes (i (xsize flat-target))
        (setf (xref flat-target i) (apply function (mapcar (lambda (flat-arg)
                                                             (xref flat-arg i))
                                                           flat-arguments))))
      target)))

(defgeneric take (class object &key force-copy-p &allow-other-keys)
  (:method (class object &rest options)
    ;; fallback case: object created by xcreate, copied elementwise
    (let* ((dims (xdims object))
           (object-cm (column-major-projection object))
           (result (apply #'xcreate class dims options))
           (result-cm (column-major-projection result)))
      (dotimes (i (xsize object))
        (setf (xref result-cm i) (xref object-cm i)))
      result))
  (:method ((class (eql 'array)) object &key force-copy-p (element-type t))
    ;; result is an array
    (declare (ignore force-copy-p))
    (let ((array (make-array (xdims object) :element-type element-type))
	  (dimensions (coerce (xdims object) 'fixnum-vector)))
      (if (subtypep (xelttype object) element-type)
	  ;; coerce
	  (dotimes (i (xsize object))
	    (setf (row-major-aref array i)
		  (coerce (apply #'xref object (rm-subscripts dimensions i))
                          element-type)))
	  ;; no mapping 
	  (dotimes (i (xsize object))
	    (setf (row-major-aref array i)
		  (apply #'xref object (rm-subscripts dimensions i)))))
      array))
  (:documentation "Return an object converted to a given class, with
other properties (eg element types for arrays) as specified by the
optional keyword arguments.  The result may share structure with object, unless
force-copy-p."))



;;;; Generalized outer product.

(defun xop (result-spec function &rest vectors)
  "Generalized outer product of vectors, using function."
  (let* ((dims (mapcar (lambda (v)
                         (bind (((length) (xdims v)))
                           length))
                       vectors))
         (result (xcreate* result-spec dims)))
    (dotimes (i (reduce #'* dims))
      (let ((subscripts (cm-subscripts dims i)))
        (setf (apply #'xref result subscripts)
              (apply function
                     (mapcar (lambda (vector subscript)
                               (xref vector subscript))
                             vectors subscripts)))))
    result))
