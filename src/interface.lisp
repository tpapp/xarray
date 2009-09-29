(in-package :xarray)

;;;; General interface for objects accessible with xref (objects like
;;;; this are called xrefable).
;;;;
;;;; Objects accessible with xref are array-like objects, where
;;;; elements are indexed with (xrank object) subscripts, each ranging
;;;; from 0 to (1- (xdim object dimension)), inclusive.  Not all
;;;; elements need to be setable, if they are not, trying to call
;;;; (setf xref) on that element will signal a condition.
;;;;
;;;; Objects can have a particular type imposed on elements, which can
;;;; be queried with xtype.  Elements returned by xref are guaranteed
;;;; to be a subtype of this type, and (setf xref) needs to be given
;;;; elements of this subtype.
;;;; 
;;;; Conditions for the wrong number of subscripts, subscripts being
;;;; out of bounds, or writing non-writable elements or elements with
;;;; incorrect type are available. (!!! see notes there)

(defgeneric xtype (object)
  (:documentation "Return the type of elements.  If no restriction is
  imposed, return t."))

(defgeneric xrank (object)
  (:documentation "Returns the number of dimensions of object."))

(defgeneric xdims (object)
  (:documentation "Return a list of dimensions of object.  The list
  does not share structure with anything, so it can be freely
  modified."))

(defgeneric xdim (object axis-number)
  (:method (object axis-number)
    ;; not the most efficient, but a reasonable fallback if not defined
    (nth axis-number (xdims object)))
  (:documentation "Return the axis-number dimension of object."))

(defgeneric xsize (object)
  (:method (object) (reduce #'* (xdims object))) ; default fallback
  (:documentation "Return the total number of elements in object."))

(defgeneric xref (object &rest subscripts)
  (:documentation "Accesses the element of the object specified by subscripts."))

(defgeneric (setf xref) (value object &rest subscripts)
  (:documentation "Accesses the element of the object specified by subscripts."))


(define-condition xref-subscript-out-of-bounds (error)
  ((subscripts :initarg :subscripts :reader subscripts)
   (dimensions :initarg :dimensions :reader dimensions)))

(define-condition xref-wrong-number-of-subscripts (error)
  ((subscripts :initarg :subscripts :reader subscripts)
   (rank :initarg :rank :reader rank)))

(define-condition xref-setting-readonly (error)
  ;; !! maybe give some info on the writability?
  ((subscripts :initarg :subscripts :reader subscripts)))

(define-condition xref-incompatible-type (error)
  ;; !! maybe give some info on the type?
  ((subscripts :initarg :subscripts :reader subscripts)))

(defgeneric take (object class &key function &allow-other-keys)
  (:method (object (class (eql 'array)) &key function (element-type t))
    ;; fallback case
     (let ((array (make-array (xdims object) :element-type element-type))
	  (dimensions (coerce (xdims object) 'fixnum-vector)))
      (if function
	  ;; map
	  (dotimes (i (xsize object))
	    (setf (row-major-aref array i)
		  (funcall function 
			   (apply #'xref object (rm-subscripts dimensions i)))))
	  ;; no mapping 
	  (dotimes (i (xsize object))
	    (setf (row-major-aref array i)
		  (apply #'xref object (rm-subscripts dimensions i)))))
      array))
  (:documentation "Return an object converted to a given class, with
  other properties (eg element types for arrays) as specified by the
  optional keyword arguments.  If function is non-nil, it is called on
  each element."))

;;;; xsetf allow to set elements of an xrefable object to those of
;;;; another.

(defgeneric xsetf (destination source &key map-function)
  (:method (destination source &key map-function)
    (unless (equalp (xdims source) (xdims destination))
      (error "source and destination do not have conforming dimensions"))
    (let ((dimensions (xdims source))
	  (map-function (map-and-convert-function map-function
						  (xtype source)
						  (xtype destination) t)))
      (if map-function
	  (dotimes (i (xsize source))
	    (let ((subscripts (cm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (funcall map-function (apply #'xref source subscripts)))))
	  (dotimes (i (xsize source))
	    (let ((subscripts (cm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (apply #'xref source subscripts))))))
    destination)
  (:documentation "Copy the elements of source to destination, with
  the usual semantics for map-function and type conversion"))
