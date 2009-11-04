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

(defgeneric xelttype (object)
  (:documentation "Return the type of elements.  If no restriction is
  imposed, return t."))

;;;; In most cases, you should only implement xdims, the rest will be
;;;; defined in a sane way by generic functions.  The rest only needs
;;;; to be implemented for efficiency reasons, but this should be a
;;;; minor concern for most applications.

(defgeneric xdims (object)
  (:documentation "Return a list of dimensions of object.  The list
  does not share structure with anything, so it can be freely
  modified."))

(defgeneric xrank (object)
  (:documentation "Returns the number of dimensions of object.")
  (:method (object)
    (length (xdims object))))

(defgeneric xdim (object axis-number)
  (:method (object axis-number)
    (let ((dim (nth axis-number (xdims object))))
      (if dim dim (error 'xdim-invalid-axis-number))))
  (:documentation "Return the axis-number dimension of object."))

(defgeneric xsize (object)
  (:documentation "Return the total number of elements in object.")
  (:method (object) (reduce #'* (xdims object) :initial-value 1)))

;;;; Accessors for elements.  (setf xref) can signal an error for
;;;; read-only elements, or does not need to be defined at all.

(defgeneric xref (object &rest subscripts)
  (:documentation "Accesses the element of the object specified by subscripts."))

(defgeneric (setf xref) (value object &rest subscripts)
  (:documentation "Accesses the element of the object specified by subscripts."))


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
;;;;  The XSIMILAR method should return (CONS CLASS OPTIONS) for
;;;;  creating a "similar" object with given dimensions, where
;;;;  "similarity" is of course something object specific, and
;;;;  designed to be a convenient default.  The purpose is to provide
;;;;  reasonable defaults for xmap, take, etc, when called with target
;;;;  specification t or (cons t options).
;;;;
;;;;  TAKE and XCREATE are object conversion/creation methods that
;;;;  specialize on class, and are also given OPTIONS.  When called
;;;;  with t or (cons t options), TAKE uses information returned by
;;;;  XSIMILAR (merging the options).  XCREATE* takes class and
;;;;  options from xsimilar called on the first argument, and merges
;;;;  that with the options.
;;;;
;;;;  Dimensions should always be a list.
;;;;
;;;;  XSIMILAR should always give an object which has all its elements
;;;;  writable, otherwise 

(defgeneric xsimilar (object rank)
  (:documentation "Return (cons class options) for creating a similar
  object with new rank.  If rank is t, use rank of object.  NOTE: for
  methods, make sure you specialize rank to fixnum if you are not
  handling t.")
  (:method (object (rank (eql t)))
    (xsimilar object (xrank object))))

(defgeneric xcreate (class dimensions &optional options)
  (:documentation "Return a new object of given type and dimensions,
  with additional options.  Dimensions can be a list, or a single
  number.  xcreate can also be called as
  (xcreate (cons class options) dimensions), in which case it will
  split the cons, merge options and call xcreate again.")
  (:method ((class list) dimensions &optional options)
    (xcreate (car class) dimensions (merge-options (cdr class) options))))

(defun xcreate-similar (target-spec object dimensions &optional more-options)
  "If target-spec is t or (cons t options), use xsimilar to determine
target spec using object (and also merge options), otherwise use
target-spec directly to create an object.  This function is meant for
internal use, when mapping functions need to determine a target spec
from one of the arguments."
  (bind ((dimensions (if (eq dimensions t)
                         (xdims object)
                         dimensions))
         ((:values class options) (if (atom target-spec)
                                      (values target-spec nil)
                                      (values (car target-spec) (cdr target-spec)))))
    (xcreate (if (eq class t)
                 (xsimilar object (length dimensions))
                 class)
             dimensions (merge-options options more-options))))

(defgeneric take (class object &key force-copy-p options)
  (:documentation "Return an object converted to a given class, with
other properties (eg element types for arrays) as specified by the
optional keyword arguments.  The result may share structure with
object, unless force-copy-p.  Similarly to xcreate, class can be (cons
class options).  When class is t, xsimilar* is called for the resulting
type.")
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
  (:method ((class (eql t)) object &key force-copy-p options)
    ;; take type from xsimilar
    (take (xsimilar object t) object :force-copy-p force-copy-p :options options))
  (:method ((class list) object &key force-copy-p options)
    ;; split class and merge options
    (take (car class) object :force-copy-p force-copy-p
          :options (merge-options (cdr class) options))))
