(defpackage #:xarray-asd
  (:use :cl :asdf))

(in-package #:xarray-asd)

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative
			      #+sbcl "fasl-sbcl"
			      #+openmcl "fasl-ccl"
			      #+cmu "fasl-cmucl"
			      #+clisp "fasl-clisp"
			      #-(or sbcl openmcl clisp cmucl) "fasl"
			      )))

(defsystem #:xarray
  :description "" 
  :author "Tamas K Papp"
  :license "MIT"
  :serial t
  :components 
  ((:module 
    "package-init"
    :pathname #P"src/"
    :components
    ((:file "package")))
   (:module
    "basics"
    :pathname #P"src/"
    :depends-on ("package-init")
    :serial t
    :components
    ((:file "types")
     (:file "utilities")
     (:file "interface")))
   (:module
    "functionality"
    :pathname #P"src/"
    :depends-on ("basics")
    :serial t
    :components
    ((:file "array")
     (:file "view")
     (:file "operations")
     (:file "sequences"))))
  :depends-on (:cl-utilities :iterate :metabang-bind))
