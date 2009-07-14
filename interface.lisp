(in-package :xarray)

;;;; General interface for objects accessible with xref (objects like
;;;; this are called xrefable).
;;;;
;;;; Objects accessible with xref are array-like objects, where
;;;; elements are indexed with (xrank object) subscripts, each ranging
;;;; from 0 to (1- (xdim object dimension)), inclusive.  Not all
;;;; elements are writable, this can be tested with xref-writable-p.
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


;;; Q: for Tamas: should we worry about confusing array-rank with
;;; numerical rank of matrices?  Or am I(AJR) just confused?
(defgeneric xrank (object)
  (:documentation "Returns the number of dimensions of object."))

(defgeneric xdims (object)
  (:documentation "Return a list of dimensions of object.  The list
  does not share structure with anything, so it can be freely
  modified."))

(defgeneric xdims* (object)
  (:method (object)
    "Default method: Leverages existing methods but might be
     inefficient."
    (coerce (xdims object) 'int-vector))
  (:documentation "Return an int-vector of dimensions of object.  May
  not be freshly created if a specialized method is used, so you
  should not modify it (default uses XDIMS)."))

(defgeneric xdim (object axis-number)
  (:method (object axis-number)
    ;; not the most efficient, but a reasonable fallback if not defined
    (nth axis-number (xdims object)))
  (:documentation "Return the axis-number dimension of object."))

(defgeneric xsize (object)
  (:method (object) (reduce #'* (xdims object))) ; default fallback
  (:documentation "Return the total number of elements in object."))

(defgeneric xref-writable-p (object &rest subscripts)
  (:method (object &rest subscripts) (declare (ignore subscripts)) t)
  (:documentation "Return non-nil if and only if the element in object
  addressed by subscripts is writable.   Default assumption is that
  objects are writable, and methods needed otherwise (for example,
  views might be locked to be read-only, set by flags.")) 

(defgeneric xref (object &rest subscripts)
  (:documentation "Accesses the element of the object specified by
  subscripts."))

(defgeneric (setf xref) (value object &rest subscripts)
  (:documentation "Accesses the element of the object specified by
  subscripts.  Methods should check x"))

;;;; !! at the experimental stage I am not using these conditions.
;;;; !! should the be mandatory later on? work out specs

(define-condition xref-out-of-bounds-error (error)
  ((subscripts :initarg :subscripts :reader subscripts)
   (dimensions :initarg :dimensions :reader dimensions)))

(define-condition xref-wrong-number-of-subscripts-error (error)
  ((subscripts :initarg :subscripts :reader subscripts)
   (rank :initarg :rank :reader rank)))

(define-condition xref-writing-readonly-error (error)
  ;; !! maybe give some info on the writability?
  ((subscripts :initarg :subscripts :reader subscripts)))

(define-condition xref-writing-type-error (error)
  ;; !! maybe give some info on the type?
  ((subscripts :initarg :subscripts :reader subscripts)))

;;;;  Even though this is a general interface, arrays are still a bit
;;;;  special, because they are the built-in CL type.  The function
;;;;  take copies the elements of an xrefable object to an array.

;; AJR: critical to ensure object is xref-able?  No, since an error
;; will get thrown at the xdims point.
(defgeneric take (object &key map-function type)
  (:method (object &key map-function (type (xtype object)))
    ;; fallback case
    (let ((array (make-array (xdims object) :element-type type))
	  (dimensions (xdims* object))
	  (map-function (map-and-convert-function map-function 
						  (xtype object) type t)))
      (if map-function
	  ;; map
	  (dotimes (i (xsize object))
	    (setf (row-major-aref array i)
		  (funcall map-function 
			   (apply #'xref object (rm-subscripts dimensions i)))))
	  ;; no map
	  (dotimes (i (xsize object))
	    (setf (row-major-aref array i)
		  (apply #'xref object (rm-subscripts dimensions i)))))
      array))
  (:documentation "Return an array with element-type type (default:
  xtype of object) containing the elements of an xrefable object.
  Elements are coerced if necessary, and map is applied when given.
  In case of a type conversion and no map, the converting function is
  automatically constructed."))

;;;; xsetf allow to set elements of an xrefable object to those of
;;;; another.

(defgeneric xsetf (destination source &key map-function)
  (:method (destination source &key map-function)
    (unless (equalp (xdims source) (xdims destination))
      (error "source and destination do not have conforming dimensions"))
    (let ((dimensions (coerce (xdims source) 'int-vector)) ; isn't this just xdims* ?
	  (map-function (map-and-convert-function map-function
						  (xtype source)
						  (xtype destination) t)))
      (if map-function
	  (dotimes (i (xsize source))
	    (let ((subscripts (rm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (funcall map-function (apply #'xref source subscripts)))))
	  (dotimes (i (xsize source))
	    (let ((subscripts (rm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (apply #'xref source subscripts))))))
    destination)
  (:documentation "Copy the elements of source to destination, with
  the usual semantics for map-function and type conversion"))
