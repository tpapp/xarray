;;; -*- mode: lisp -*-
;;; Copyright (c) 2009--, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (LLGPL).

(in-package :cl-user)

(defpackage :xarray-ut
  (:use :common-lisp :lift :xarray)
  (:export run-xarray-tests))

(in-package :xarray-ut)

(defun run-xarray-tests ()
  (run-tests :suite 'xarray-ut))

;; (run-xarray-tests)

(deftestsuite xarray-ut () ())

(deftestsuite xarray-ut-xref (xarray-ut)
  ((array-ex1 #2A((1 2) (3 4)))
   (array-ex2 #(1 2)))
  (:tests
   (xref1 (ensure (equal 1 (xref array-ex1 0 0))))
   (slice1 (ensure (equal array-ex2 (slice array-ex1 '(1 *)))))))

(defparameter *array-ex1* #2A((1 2) (3 4)))
(aref *array-ex1* 1 1)
(equal 1  (xref *array-ex1* 0 0 ))

;; (describe (run-tests :suite 'lisp-stat-ut-testsupport2))

#|
(describe 
 (run-test
  :test-case 'numerical=a2
  :suite 'lisp-stat-ut-testsupport2 ))
|#


#|
(describe 
 (run-test
  :test-case 'numerical=a1
  :suite 'lisp-stat-ut-testsupport2 ))
|#

