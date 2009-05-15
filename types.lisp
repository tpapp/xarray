(in-package :xarray)

;;;; commonly used types

(deftype int-vector ()
  "Simple array of one dimension, containing integers."
  '(simple-array integer (*)))
