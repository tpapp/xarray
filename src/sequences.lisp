(in-package :xarray)

;;;;  Sequences accessible with xarray
;;;;  
;;;;  The purpose of these classes is to be able to use xarray
;;;;  operations (eg xmap, etc) without having to create the sequence.

(defclass seq ()
  ((xsize :initarg :xsize :accessor xsize
          :documentation "length of sequence")))

(defmethod xdims ((seq seq))
  (list (xsize seq)))

;;;;
;;;;  Integer sequences
;;;;

(defclass int-seq (seq)
  ((start :initform 0 :type integer :initarg :start :reader start
          :documentation "first integer"))
  (:documentation "A sequence of integers."))

(defmethod xelttype ((seq int-seq))
  'integer)

(defmethod xref ((seq int-seq) &rest subscripts)
  (bind (((i) subscripts))
    (assert (within-dimension-p i (xsize seq)))
    (+ (start seq) i)))

(defmethod print-object ((object int-seq) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (start xsize) object
      (format stream "from ~A, length ~A" start xsize))))

(defun int-seq (a &optional b)
  "Create integer sequence 0,...,a-1 or, if b is given, a,...,b (with
a<=b)."
  (check-type a integer)
  (if b
      (progn
        (check-type b integer)
        (<= a b)
        (make-instance 'int-seq :start a :xsize (1+ (- b a))))
      (make-instance 'int-seq :xsize a)))
