(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; types -- for internal use

   ;; utilities
   
   rm-index rm-subscripts cm-index cm-subscripts multf maxf minf
   
   ;; interface

   xtype xrank xdims xdims* xdim xsize xref-subscript-out-of-bounds
   xref-wrong-number-of-subscripts xref-setting-readonly
   xref-incompatible-type xdim-invalid-axis-number xref xcreate
   xcreate* take

   ;; array

   cvector carray cvector* carray*

   ;; view

   view permutation permutation-view slice slice-view drop
   column-major-projection column-major-projection-view
   column-major-projection-flat-view

   ;; operations
   
   xdim= x+ x- x* x/ x= order xsum xprod xmin xmax xmap xop
   
   ))
