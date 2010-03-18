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

(defmacro define-elementwise-operation (operation &optional 
                                        (documentation (format nil "Elementwise
                                        ~A of two arrays, or an array and a scalar." operation))
                                        (name (make-symbol* "X" operation)))
  "Defines an elementwise operation, with some default methods that return arrays."
  (check-type documentation string)
  `(defgeneric ,name (a b &key &allow-other-keys)
     (:documentation ,documentation)
     (:method (a b &key (element-type t))
       ;; default: both A and B are arrays
       (assert (xdims= a b))
       (let* ((dims (xdims a))
	      (result (make-array dims :element-type element-type))
              (dims-vec (coerce dims 'fixnum-vector)))
         (dotimes (i (xsize a))
	   (let ((subscripts (rm-subscripts dims-vec i)))
	     (setf (row-major-aref result i)
		   (,operation (apply #'xref a subscripts) (apply #'xref b subscripts)))))
	 result))
     (:method (a (b number) &key (element-type t))
       ;; B is a scalar
       (let* ((dims (xdims a))
	      (result (make-array dims :element-type element-type))
	      (dims-vec (coerce dims 'fixnum-vector)))
	 (dotimes (i (xsize a))
	   (let ((subscripts (rm-subscripts dims-vec i)))
	     (setf (row-major-aref result i)
		   (,operation (apply #'xref a subscripts) b))))
	 result))
     (:method ((a number) b &key (element-type t))
       ;; A is a scalar (I wish I could just call the method with B
       ;; and A, but would not work for - and /).
       (let* ((dims (xdims b))
	      (result (make-array dims :element-type element-type))
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

(defun xsort (vector predicate &key key stable-p)
  "Sort vector using predicate.  Calls xorder and returns order as the
second value."
  (let ((order (xorder vector predicate :key key :stable-p stable-p)))
    (values (as t (slice vector order))
            order)))


;;; !!! all of these should accept keyword operations -- Tamas

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
(defun xmean (a) "Mean of the elements."
  (/ (xsum a) (xsize a)))

;;;; Instead of defining x... operator names for every function on
;;;; earth, we provide some generic mapping constructs.
;;;;
;;;; There are two ways to specify a target: using a symbol or a list,
;;;; in which case the object will be created using xcreate or
;;;; xcreate*, using the dimensions of the first argument, or
;;;; supplying a target object of the appropriate type.

;;;; ?? xmap could call xmap%, which could be a generic function
;;;; specializing on class. for those wanting to speed things up.

(defun xmap (target function &rest arguments)
  "Apply function to arguments elementwise, and save the result in target."
  (flet ((check-dims (dims arguments)
           (dolist (arg arguments)
             (unless (equalp (xdims arg) dims)
               (error "Dimensions ~a are incompatible with target dimension ~a."
                      (xdims arg) dims)))))
    (let* ((target
            (typecase target
              ((or symbol list)
                 (unless arguments
                   (error "Can't determine target dimensions without arguments."))
                 (bind ((first (first arguments))
                        (dims (xdims first)))
                   (check-dims dims (cdr arguments))
                   (xcreate-similar target first dims)))
              (t (check-dims (xdims target) arguments)
                 target)))
           (flat-arguments (mapcar #'column-major-projection arguments))
           (flat-target (column-major-projection target)))
      (dotimes (i (xsize flat-target))
        (setf (xref flat-target i) (apply function (mapcar (lambda (flat-arg)
                                                             (xref flat-arg i))
                                                           flat-arguments))))
      target)))

(defun xop (result-spec function &rest vectors)
  "Generalized outer product of vectors, using function."
  (let* ((dims (mapcar (lambda (v)
                         (bind (((length) (xdims v)))
                           length))
                       vectors))
         (result-spec (mklist result-spec))
         (result (xcreate (car result-spec) dims (cdr result-spec))))
    (dotimes (i (reduce #'* dims))
      (let ((subscripts (cm-subscripts dims i)))
        (setf (apply #'xref result subscripts)
              (apply function
                     (mapcar (lambda (vector subscript)
                               (xref vector subscript))
                             vectors subscripts)))))
    result))

(defun xcollect (n function &optional (target-spec t))
  "Collect the result of calling function n times into an array (type
  of target-spec, or determined using xsimilar).  Indexing is (i ...),
  where i is from 0 to n-1.  The rest of the indexes are determined
  from the first value."
  (let* ((first (funcall function))
         (dims (cons n (xdims first)))
         (target (xcreate-similar target-spec first dims))
         (mask (make-sequence 'list (xrank first) :initial-element :all)))
    ;; first element
    (xsetf (apply #'slice target 0 mask)
           first)
    ;; rest
    (iter
      (for i :from 1 :below n)
      (xsetf (apply #'slice target i mask)
             (funcall function)))
    ;; value
    target))

(defgeneric xdot (a b)
  (:documentation "Dot product of two vectors.")
  (:method (a b)
    (bind (((n1) (xdims a))
           ((n2) (xdims b)))
      (assert (= n1 n2) 
              () "Incompatible vector lengths.")
      (iter
        (for i :from 0 :below n1)
        (summing (* (xref a i) (xref b i)))))))

(defun xconcat (target-type &rest arguments)
  "Concatenate atoms and/or vectors into a vector."
  (let* ((length (reduce #'+ arguments
                        :key (lambda (arg)
                               (acase (xrank arg)
                                 (0 1)
                                 (1 (xdim arg 0))
                                 (otherwise (error "Argument ~A has rank ~A" arg it))))))
         (result (xcreate-similar target-type (first arguments) (list length)))
         (position 0))
    (dolist (arg arguments)
      (ecase (xrank arg)
        (0 (setf (xref result position) arg)
           (incf position))
        (1 (iter
             (with arg-length :=  (xdim arg 0))
             (for arg-index :from 0 :below arg-length)
             (for result-index :from position)
             (setf (xref result result-index) (xref arg arg-index))
             (finally (incf position arg-length))))))
    result))

;; (xconcat t #(1 2 3) 4 5 6)
;; (xconcat t 1 #(2 3) 4 #(4 5))
;; (xconcat t 1 #(2 3) #2A((4 5)))
