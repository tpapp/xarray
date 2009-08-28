(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :ffa :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export xref xtype xdims xdims* xdim xrank xsize xref-writeable-p take xsetf 
	   slice slice-view ;; we could generalize to subset, subset-view ?
	   take
	   carray carray* cvector cvector*

	   ;; New generics
	   xref* 
	   ;; xref-view 

	   ;; export conditions as well...?
	   ))


(defpackage #:xarray-user
  (:use :common-lisp :xarray))
