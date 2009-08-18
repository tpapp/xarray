(in-package :xarray)

;;; ListOfList  is another useful xrefable object.
;;; The interface does NOT map to CL functions in a straightforward
;;; manner, unlike arrays.

;;; IN PROGRESS!

(defun listoflistp (x &key (ragged T))
  "Test for conformance of structure: list whose sublists are of the
same size (if ragged is T, then just test that list has elements of
type list)."
  (check-type x list)
  (dotimes (i (length x))
    (let ((n (length (elt x 0)))
	  (curr-elt (elt x i)))
      (check-type curr-elt list)
      (when (not (= n (length curr-elt)))
	(error "Element ~A does not match initial element length." i)))))

(defmethod xtype ((object list))
  ;; collect and rationalize all types into the most specific covering all.
#|
  (loop for sublist in object
     collect (loop do i in (length sublist)
		collect (type-of (elt sublist 0))))
|#
  )

(defmethod xrank ((object list))
  "Basically, assuming coherently sized object, return number of
nested lists in first object."
  (length (xdims object)))

(defmethod xdims ((object list))
#|
  (if (coherent-list-of-list-p object)
      (n-nested-lists object)
      nil)
|#
  )

(defmethod xdim ((object list) axis-number)
  (array-dimension object axis-number))

(defmethod xsize ((object list))
  (array-total-size object))

(defmethod xref-writable-p ((object list) &rest subscripts)
  "Lists always can be written to -- until we read-only it?!"
  (declare (ignore subscripts))
  t)

(defmethod xref ((object list) &rest subscripts)
  (apply #'aref object subscripts))

(defmethod (setf xref) (value (object list) &rest subscripts)
  (setf (apply #'aref object subscripts) value))

;;;; HOW TO TREAT THE FOLLOWING FOR LOL data structures?  (I think we
;;;; move the work in lisp-matrix and cls to this package, or factor
;;;; out into a list-of-list package?  but it is only the xref'able
;;;; stuff we want?).


;;;; Convenience functions for vector and list construction.  All
;;;;  return simple-lists of the specified type, the versions with *
;;;;  use numeric-type-classifier.

(defun cvector (element-type &rest elements)
  "Return a (simple-list element-type (*)) containing elements,
coerced to element-type."
  (let ((vector (make-list (length elements) :element-type element-type)))
    (fill-list-with-list vector elements)))

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
