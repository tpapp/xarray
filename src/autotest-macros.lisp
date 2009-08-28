
;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-08-20 08:09:23 tony>
;;; Creation:   <2009-08-18 08:11:24 tony>
;;; File:       autotest-macros.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Macros to create xarray test suites for newly
;;;             xref'able objects in Lisp.  This assumes that the
;;;             intent of this package is to provide a clean,
;;;             data-analytic-natural interface, and not necessarily
;;;             yet-another-matrix package.   So unit-testing should
;;;             confirm that the underlying data structure supports
;;; the interface, with the underlying data structure (except for lisp
;;; arrays and list-of-list structures) supported externally and
;;; registering with the xarray package (for testing and xmake
;;; capabilities. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.




#|
 (defmacro create-test-suite (object-name list-to-create-testdata)
  ;; basically, rebuild the unittests, but for a particular backend
  ;; datastructure.   If we add an xmake-datastructure or similar, we
  ;; could avoid the LIST-TO-CREATE-TESTDATA input.
  )

|#