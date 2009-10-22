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
;;;; be queried with xelttype.  Elements returned by xref are
;;;; guaranteed to be a subtype of this type, and (setf xref) needs to
;;;; be given elements that are a subtype of this.  The type of the
;;;; whole object (without dimensions!) can be queried with xtype, and
;;;; the result is accepted by xcreate*.
;;;;
;;;; Xcreate is a generic way to create xref'able object of a given
;;;; type specification.  See the comments there on type
;;;; specifications.
;;;; 
;;;; Conditions for the wrong number of subscripts, subscripts being
;;;; out of bounds, or writing non-writable elements or elements with
;;;; incorrect type are available. (!!! see notes there)

(defgeneric xtype (object)
  (:documentation "Return the type of object, in the format accepted
  by xcreate* (either a single symbol or a list).  Does not contain
  dimension information. (other than what is implicit in the class).")
  (:method (object)                     ; default
    (class-of object)))

(defgeneric xelttype (object)
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

(define-condition xdim-invalid-axis-number (error)
  ())

;;;; xsetf allow to set elements of an xrefable object to those of
;;;; another.

(defgeneric xsetf (destination source &key map-function)
  (:method (destination source &key 
                        (map-function
                         (element-conversion-function (xelttype source)
                                                      (xelttype destination))))
    (unless (equalp (xdims source) (xdims destination))
      (error "source and destination do not have conforming dimensions"))
    (let ((dimensions (xdims source)))
      (if (and map-function (not (eq map-function #'identity)))
          ;; map-function is not given or identity, don't apply
	  (dotimes (i (xsize source))
	    (let ((subscripts (cm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (apply #'xref source subscripts))))
          ;; use map-function
	  (dotimes (i (xsize source))
	    (let ((subscripts (cm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (funcall map-function (apply #'xref source subscripts)))))))
    destination)
  (:documentation "Copy the elements of source to destination.
Map-function, if given, will be used to map the elements, the default
is conversion (if necessary) with coerce."))

;;;; XCREATE is a generic way of creating objects, takes a type (a
;;;; symbol, not a list!), dimensions (a list), and additional other
;;;; keyword arguments.
;;;;
;;;; These specifications are useful for functions that return
;;;; xref'able objects (eg take, xmap).  They should be given as a
;;;; list (class &key ...), eg '(array :element-type double-float).
;;;; All conforming types should define a method for XCREATE.
;;;; XCREATE* can be used as a shorthand for destructuring the type
;;;; specifiers (sans dimension).  All methods should accept scalars
;;;; as DIMENSIONS, in which case they denote a vector.

(defgeneric xcreate (class dimensions &key &allow-other-keys)
  (:method ((class (eql 'array)) dimensions &key (element-type t))
    (make-array dimensions :element-type element-type))
  (:documentation "Return a new object of given type and dimensions,
  with additional options.  Dimensions can be a list, or a single
  number."))

(declaim (inline xcreate))
(defun xcreate* (class-and-options dimensions)
  (if (atom class-and-options)
      (funcall #'xcreate class-and-options dimensions)
      (apply #'xcreate (car class-and-options) dimensions (cdr class-and-options))))

(defgeneric take (class object &key force-copy-p &allow-other-keys)
  (:method ((class (eql 'array)) object &key force-copy-p (element-type t))
    ;; fallback case
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
