;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-08-18 08:14:42 tony>
;;; Creation:   <2009-08-18 08:11:24 tony>
;;; File:       autotest-macros.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Macros to create xarray test suites for newly
;;;             xref'able objects in Lisp. 

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