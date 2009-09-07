(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect))
