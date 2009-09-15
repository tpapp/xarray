(in-package :xarray)

;;;; commonly used types

(deftype fixnum-vector ()
  "Simple array of one dimension, containing fixnums."
  '(simple-array fixnum (*)))
