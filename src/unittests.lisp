;;; -*- mode: lisp -*-
;;; Copyright (c) 2009--, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (LLGPL).

(in-package :cl-user)

(defpackage :xarray-ut
  (:use :common-lisp :lift :xarray)
  (:export run-xarray-tests xarray-ut))

(in-package :xarray-ut)

(defun run-xarray-tests ()
  (run-tests :suite 'xarray-ut))

;; (run-xarray-tests)

(deftestsuite xarray-ut ()
  ((array-ex1 #2A((1 2) (3 4)))
   (array-ex2 #(1 2))))

(deftestsuite xarray-ut-xref (xarray-ut) ())
(deftestsuite xarray-ut-xrank (xarray-ut) ())
(deftestsuite xarray-ut-xdims (xarray-ut) ())
(deftestsuite xarray-ut-slice (xarray-ut) ())


;; Initial set of tests done on the interface to native lisp ARRAY
;; data structure.  We probably should add a list-of-list structure as
;; a second example of an xref-able class.

;; xref on arrays

(addtest (xarray-ut-xref) xref-1
	 (ensure (equal 1 (xref array-ex1 0 0))))

(addtest (xarray-ut-xref) xref-2
	 (ensure (equal 1 (xref array-ex2 0))))

;; xrank on arrays

(addtest (xarray-ut-xrank) xrank-1
	 (ensure (= 1 (xrank array-ex2))))

(addtest (xarray-ut-xrank) xrank-2
	 (ensure (= 2 (xrank array-ex1))))

;; slice on arrays

(addtest (xarray-ut-slice) slice-0
	 (ensure (slice array-ex1 1)))

(addtest (xarray-ut-slice) slice-1
	 (ensure (equal array-ex2 (slice array-ex1 1))))

(addtest (xarray-ut-slice) slice-2
	 (ensure (equal array-ex2 (slice array-ex1 #(1 0)))))

#|
 (describe (run-tests :suite 'xarray-ut))

 (describe 
    (run-test
      :test-case 'slice-2
      :suite     'xarray-ut))
|#
