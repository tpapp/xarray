(in-package :xarray)

;;;;  Array is the of course the most important of xrefable objects.
;;;;  The interface maps to CL functions in a straightforward manner.

(defmethod xelttype ((object array))
  (array-element-type object))

(defmethod xrank ((object array))
  (array-rank object))

(defmethod xdims ((object array))
  (array-dimensions object))

(defmethod xdim ((object array) axis-number)
  (array-dimension object axis-number))

(defmethod xsize ((object array))
  (array-total-size object))

(defmethod xref ((object array) &rest subscripts)
  (apply #'aref object subscripts))

(defmethod (setf xref) (value (object array) &rest subscripts)
  (setf (apply #'aref object subscripts) value))

;;; Extended interface

(defmethod xcreate ((class (eql 'array)) dimensions &optional options)
    (bind (((&key (element-type t)) options))
      (make-array dimensions :element-type element-type)))

(defmethod xsimilar ((object array) rank)
  (declare (ignore rank))
  `(array :element-type ,(array-element-type object)))

(defmethod take ((class (eql 'array)) object &key force-copy-p options)
  (declare (ignore force-copy-p))
  (bind (((&key (element-type t)) options)
         (array (make-array (xdims object) :element-type element-type))
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

;;;;  Convenience functions for vector and array construction.  All
;;;;  return simple-arrays of the specified type, the versions with *
;;;;  use numeric-type-classifier.

(defun cvector (element-type &rest elements)
  "Return a (simple-array element-type (*)) containing elements,
coerced to element-type."
  (let ((vector (make-array (length elements) :element-type element-type)))
    (fill-array-with-list vector elements)))

(defun carray (element-type dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type."
  (unless (= (length elements) (reduce #'* dimensions))
    (error "incorrect number of elements provided"))
  (let ((vector (make-array dimensions :element-type element-type)))
    (fill-array-with-list vector elements)))

(defun cvector* (&rest elements)
  "Return a (simple-array element-type (*)) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'cvector (numeric-type-classifier elements) elements))

(defun carray* (dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'carray (numeric-type-classifier elements) dimensions elements))
