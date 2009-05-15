(defpackage #:xarray-asd
  (:use :cl :asdf))

(in-package #:xarray-asd)

(defsystem #:xarray
  :description "" 
  :author "Tamas K Papp"
  :license "LLGPL"
  :serial t
  :components ((:file "package")
	       (:file "types")
	       (:file "utilities")
	       (:file "interface")
	       (:file "array")
	       (:file "view"))
  :depends-on (:cl-utilities :iterate :metabang-bind :ffa))
