(in-package :xarray)

;;;;  Unless otherwise specified by specialized methods, all objects
;;;;  are xref'able as an array of rank 0, similarly to CL's way of
;;;;  handling arrays like this.

(defmethod xelttype (object)
  (type-of object))

(defmethod xdims (object)
  nil)

;;;;  Note: we MUST NOT implement xdim, 
;;;;

(defmethod xref (object &rest subscripts)
  (when subscripts
    (error 'xref-wrong-number-of-subscripts))
  object)

;; (setf xref) is of course not defined
    
(defmethod xsimilar (rank object)
  (declare (ignore rank))
  `(array :element-type ,(upgraded-array-element-type (type-of object))))
