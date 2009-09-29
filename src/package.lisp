(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; types -- for internal use

   ;; utilities
   
   rm-index rm-subscripts cm-index cm-subscripts
   
   ;; interface

   xtype xrank xdims xdims* xdim xsize xref-subscript-out-of-bounds
   xref-wrong-number-of-subscripts xref-setting-readonly
   xref-incompatible-type take xref

   ;; array

   cvector carray cvector* carray*

   ;; view

   view permutation permutation-view slice slice-view row-major-projection
   row-major-projection-view

   ;; operations
   
   xdim= x+ x- x* x/ x= order
   
   ))
