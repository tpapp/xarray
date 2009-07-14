(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :ffa :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export xref xtype xdims xdims* xdim xrank xsize xref-writeable-p take xsetf 
	   ;; export conditions as well?

	   ))
