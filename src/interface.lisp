(in-package :xarray)

;;;; General interface for objects accessible with xref (objects like
;;;; this are called xrefable).
;;;;
;;;; We distinguish two levels of the interface: the basic (element
;;;; access, dimension information) and the extended (type
;;;; information, creation of similar objects).


;;;; Basic interface
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
;;;; be given elements that are a subtype of this.
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
;;;;
;;;; ??? should we lose the function? -- Tamas

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


;;;;  An object is characterized by its CLASS (a symbol), various
;;;;  class-specific OPTIONS (a list of keyword-value pairs, can be
;;;;  empty, all should have reasonable defaults so that they are
;;;;  optional) and the dimensions.
;;;;
;;;;  XTYPE will return (CONS CLASS OPTIONS).  TAKE and XCREATE are
;;;;  object conversion/creation methods that specialize on class, and
;;;;  are also given OPTIONS.  The XSIMILAR method should return
;;;;  (CONS CLASS OPTIONS) for creating a "similar" object with
;;;;  given dimensions, where "similarity" is of course something
;;;;  object specific, and designed to be a convenient default.  TAKE*
;;;;  and XMAP* use information returned by XSIMILAR.
;;;;
;;;;  Dimensions should always be a list.


(defgeneric xtype (object)
  (:documentation "Return (cons class options)."))

(defgeneric xcreate (class dimensions &optional options)
  (:documentation "Return a new object of given type and dimensions,
  with additional options.  Dimensions can be a list, or a single
  number.  xcreate can also be called as
  (xcreate (cons class options) dimensions), in which case it will
  split the cons and call xcreate again.")
  (:method ((class list) dimensions &optional options)
    ;; 
    (assert (null options))
    (xcreate (car class) dimensions (cdr class))))

(defgeneric xsimilar (object rank)
  (:documentation "Return (cons class options) for creating a similar
  object with new rank."))

;;;; xsimilar should be redesigned, it doesn't need to take objects
;;;; directly, should be called as (xsimilar class options rank)
;;;; instead.

(defgeneric take (class object &key force-copy-p options)
  (:method (class object &key force-copy-p options)
    ;; fallback case: object created by xcreate, copied elementwise
    (declare (ignore force-copy-p))
    (let* ((dims (xdims object))
           (object-cm (column-major-projection object))
           (result (funcall #'xcreate class dims options))
           (result-cm (column-major-projection result)))
      (dotimes (i (xsize object))
        (setf (xref result-cm i) (xref object-cm i)))
      result))
  (:method ((class list) object &key force-copy-p options)
    ;; 
    (assert (null options))
    (take (car class) object :force-copy-p force-copy-p
          :options (cdr class)))
  (:documentation "Return an object converted to a given class, with
other properties (eg element types for arrays) as specified by the
optional keyword arguments.  The result may share structure with
object, unless force-copy-p.  Similarly to xcreate, class can be (cons
class options)."))

(defun take* (object &optional force-copy-p)
  "Take an object using type information from xsimilar."
  (let ((type (xsimilar object (xrank object))))
    (take (car type) object :force-copy-p force-copy-p :options (cdr type))))
