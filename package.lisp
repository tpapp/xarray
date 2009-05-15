(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :ffa :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect))
