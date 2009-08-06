;;; -*- mode: lisp -*-
;;; Copyright (c) 2009--, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (LLGPL).

;;; (asdf:oos 'asdf:load-op 'xarray-test)
(in-package :cl-user)

(defpackage :xarray-ut
  (:use :common-lisp :lift :xarray)
  (:export run-xarray-tests xarray-ut))

(in-package :xarray-ut)

(defun run-xarray-tests ()
  (run-tests :suite 'xarray-ut))

;; (run-xarray-tests)

(deftestsuite xarray-ut ()
  ((array-ex0 #2A((11 12 13 14) (21 22 23 24) (31 32 33 34)))
   (array-ex1 #2A((21 22 23 24)))
   (array-ex2 #2A((12)(22)(32)))
   (array-ex3 #(12 22 32))))

(deftestsuite xarray-ut-xref (xarray-ut) ())
(deftestsuite xarray-ut-xrank (xarray-ut) ())
(deftestsuite xarray-ut-xdims (xarray-ut) ())
(deftestsuite xarray-ut-slice (xarray-ut) ())

;; Initial set of tests done on the interface to native lisp ARRAY
;; data structure.  We probably should add a list-of-list structure as
;; a second example of an xref-able class.

;; xref on arrays

(addtest (xarray-ut-xref) xref-1
	 (ensure (equal 21 (xref array-ex1 0 0))))

(addtest (xarray-ut-xref) xref-2
	 (ensure-error (equal 1 (xref array-ex2 0))))

;; xrank on arrays

(addtest (xarray-ut-xrank) xrank-1
	 (ensure (= 2 (xrank array-ex2))))

(addtest (xarray-ut-xrank) xrank-2
	 (ensure (= 2 (xrank array-ex1))))

(addtest (xarray-ut-xrank) xrank-3
	 (ensure (= 1 (xrank array-ex3))))

;; slice on arrays

#|

 (let

  ;(princ (slice array-ex0 '(0 1 2) '(0 1 2 3)))
  (princ (slice array-ex0 '(1) '(1 0)))
  (princ (slice array-ex0 '(1) :all))
  (princ (slice array-ex0 '(1) :rev))
  nil)

 (let
  ((array-ex0 #2A((11 12 13 14) (21 22 23 24) (31 32 33 34)))
   (array-ex1 #2A((21 22 23 24)))
   (array-ex2 #2A((12)(22)(32))))

  (macrolet
      ((ps0 (&rest indices) `(format t "~A~%" (slice array-ex0 ,@indices))))
    ;(ps0 1 1) ;=> 22 ERROR, to debug!
    (ps0 :all '(1))
    (ps0 '(1) :all)
    (ps0 (list 1) (list 1))
    (ps0 '(1) '(1 1))
    (ps0 (list 0 2) (list 1 3))
    (ps0 '(1) '(1 1))
    (ps0 '(1) '(1 0))
    (ps0 '(1) '(0))
    (ps0 :all '(2))
    (ps0 :rev '(2))
    (ps0 '(1) :all)
    (ps0 '(1) :rev)
    (ps0 '(1 0) '(1 0))
    (ps0 '(0 1) '(0 1)))
   nil)

 (defparameter arry-ex0 #2A((11 12 13 14) (21 22 23 24) (31 32 33 34)))
 (defparameter arry-ex1 #2A((21 22 23 24)))
 (defparameter arry-ex2 #2A((12)(22)(32)))
 (defparameter arry-ex3 #(12 22 32))

 (princ-and-equalp #2A((11))
		  (take  (slice arry-ex0 '(0) '(0))))

|#

(defmacro equalp-arr-xref (my-array my-xrefable)
  `(equalp ,my-array (take ,my-xrefable)))

(defmacro princ-and-equalp (my-array my-xrefable)
  `(progn 
     (princ ,my-array)
     (princ ,my-xrefable)
     (equalp-arr-xref ,my-array ,my-xrefable)))


(addtest (xarray-ut-slice) slice-0
	 (ensure
	  (equalp #2A((11))
		  (take  (slice array-ex0 '(0) '(0))))))

(addtest (xarray-ut-slice) slice-1
	 (ensure
	  (equalp array-ex1
		  (take (slice array-ex0 '(1) :all)))))

(addtest (xarray-ut-slice) slice-1a
	 (ensure
	  (equalp array-ex2
		  (take (slice array-ex0 :all '(1) )))))

(addtest (xarray-ut-slice) slice-2
	 (ensure
	  (equalp #2A ((12 13) (22 23))
		  (take (slice array-ex0 '(0 1) '(1 2))))))

#|
 (run-tests :suite 'xarray-ut)
 ; => #<Results for XARRAY-UT 9 Tests, 0 Failures, 0 Error>

 (describe (run-tests :suite 'xarray-ut))

 (describe (run-tests :suite 'xarray-ut-slice))

 (describe 
    (run-test
      :test-case 'slice-2
      :suite     'xarray-ut))
|#
