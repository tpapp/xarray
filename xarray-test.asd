(defpackage #:xarray-test-asd
  (:use :cl :asdf))

(in-package #:xarray-test-asd)

(defsystem #:xarray-test
  :description "" 
  :author "Tamas K Papp / AJ Rossini"
  :license "LLGPL"
  :serial t
  :depends-on (:xarray
	       :lift)
  :components ((:module
		"ut-core"
		:pathname "src/"
		:components ((:file "unittests")))))
