;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-09-02 08:10:15 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    Manipulating structures which are lists of lists
;;;             rather than arrays or matrix-likes.  Providing an
;;;             xarray interface to these structures

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


(in-package :xarray)

;;; ListOfList  is another useful xrefable object.
;;; The interface does NOT map to CL functions in a straightforward
;;; manner, unlike arrays.  So it makes a good example of a non-standard 

;;; Thoughts for organization: there are 2 general flavors of
;;; activities.  The first is that we do list-of-list to list-of-list
;;; transforms, and these do not rely on external packages being in
;;; existence.  The second is that we do transformations from them
;;; into other similar rectangular or ragged data structures.
;;; Within-structure should include item-selection and subsetting,
;;; while between-structure should include copying and ideally,
;;; "pass-through" referential structures.  The latter is probably
;;; going to take a performance hit, but should allow for maximal
;;; memory use.

;; Currently, we assume that the list-of-list representation is in
;; row-major form, i.e. that lists represent rows and not columns.
;; The original lisp-stat had the other way around.  We could augment
;; the top-level list with a property to check orientation
;; (row-major/column-major), but this hasn't been done yet.


#|
;; Test cases:
 (and T T nil T)
 (and T T T)
 (defparameter *x1* (list 1 2 3))
 (defparameter *x2* (list 1 2 3))
 (defparameter *x3* (list 1 2 3 4))
 (defparameter *x4* (list 1 2 3))
 (reduce #'(lambda (x y)
	      (if (= x y) y -1))
	  (mapcar #'length (list *x1* *x2* *x3*)))
 (reduce #'(lambda (x y)
	      (if (= x y) y -1))  (list 2 3 2))
 (lists-of-same-size *x1* *x2* *x4*) ; => T
 (lists-of-same-size *x1* *x3* *x4*) ; => F
 (lists-of-same-size *x1* *x2* *x3*) ; => F
 (lists-of-same-size *x3* *x1* *x3*) ; => F
|#


;;; IN PROGRESS!

(defun listoflistp (x &key (ragged T))
  "Test for conformance of structure: list whose sublists are of the
same size (if ragged is T, then just test that list has elements of
type list)."
  (declare (ignore ragged))
  (check-type x list)
  (dotimes (i (length x))
    (let ((n (length (elt x 0)))
	  (curr-elt (elt x i)))
      (check-type curr-elt list)
      (when (not (= n (length curr-elt)))
	(error "Element ~A does not match initial element length." i)))))



(defun transpose-listoflist (listoflist)
  "This function does the moral-equivalent of a matrix transpose on a
list-of-lists data structure"
  (apply #'mapcar #'list listoflist))

;; (defparameter LOL-2by3 (list (list 1 2) (list 3 4) (list 5 6)))
;; (defparameter LOL-3by2 (list (list 1 3 5) (list 2 4 6)))
;; (transpose-listoflists (transpose-listoflists LOL-2by3))
;; (transpose-listoflists (transpose-listoflists LOL-3by2))

(defun equal-listoflist (x y)
  "FIXME: This function, when written, should walk through 2 listoflists and
return T/nil based on equality."
  (and (= (list-length x) ;; top-level length same
	  (list-length y))
       ;; FIXME: within-level lengths same
       ;; FIXME: flattened values same, walking through
       ;; (loop over x and verify same tree as y)
       ))



#|
  (defparameter *mdfl-test*
      (list (list 'a 1 2.1)
            (list 'b 2 1.1)
            (list 'c 1 2.0)
            (list 'd 2 3.0)))
  (length *mdfl-test*)
  (length (elt *mdfl-test* 0))

  (defparameter *mdfl-test-dt* (make-datatable-from-listoflists *mdfl-test*))
  (array-dimensions *mdfl-test-dt*)

|#



;;;; XARRAY INTERFACE

(defmethod xtype ((object list))
  ;; collect and rationalize all types into the most specific covering all.
#|
  (loop for sublist in object
     collect (loop do i in (length sublist)
		collect (type-of (elt sublist 0))))
|#
  )

(defmethod xrank ((object list))
  "Basically, assuming coherently sized object, return number of
nested lists in first object."
  (length (xdims object)))

(defmethod xdims ((object list))
#|
  (if (coherent-list-of-list-p object)
      (n-nested-lists object)
      nil)
|#
  )

#|
(defmethod xdim ((object list) axis-number)
  (array-dimension object axis-number))

(defmethod xsize ((object list))
  (array-total-size object))
|#

(defmethod xref-writable-p ((object list) &rest subscripts)
  "Lists always can be written to -- until we read-only it?!"
  (declare (ignore subscripts))
  t)

(defmethod xref ((object list) &rest subscripts)
  (apply #'aref object subscripts))

(defmethod (setf xref) (value (object list) &rest subscripts)
  (setf (apply #'aref object subscripts) value))

;;;; HOW TO TREAT THE FOLLOWING FOR LOL data structures?  (I think we
;;;; move the work in lisp-matrix and cls to this package, or factor
;;;; out into a list-of-list package?  but it is only the xref'able
;;;; stuff we want?).


;;;; Convenience functions for vector and list construction.  All
;;;;  return simple-lists of the specified type, the versions with *
;;;;  use numeric-type-classifier.
#|
 (defun cvector-lol (element-type &rest elements)
  "Return a (simple-list element-type (*)) containing elements,
coerced to element-type."
  (let ((vector (make-list (length elements) :element-type element-type)))
    (fill-list-with-list vector elements)))
|#

(defun carray-lol (element-type dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type."
  (unless (= (length elements) (reduce #'* dimensions))
    (error "incorrect number of elements provided"))
  (let ((vector (make-array dimensions :element-type element-type)))
    (fill-array-with-list vector elements)))

(defun cvector*-lol (&rest elements)
  "Return a (simple-array element-type (*)) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'cvector (numeric-type-classifier elements) elements))

(defun carray*-lol (dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'carray (numeric-type-classifier elements) dimensions elements))


(defun listoflist->array (lol &key (type 'row-major))
  "From a listoflists structure, make an array.

FIXME: need to verify that the listoflists is a valid structure (same
size rows, typing if required, etc.

<example>
  (defparameter *mdfl-test*
      (list (list 'a 1 2.1)
            (list 'b 2 1.1)
            (list 'c 1 2.0)
            (list 'd 2 3.0)))
  (length *mdfl-test*)
  (length (elt *mdfl-test* 0))

  (defparameter *mdfl-test-dt* (make-datatable-from-listoflists *mdfl-test*))
  (array-dimensions *mdfl-test-dt*)
</example>"
  (let ((n (length lol))
	(p (length (elt lol 0))))
    (let ((result (make-array (list n p))))
      (dotimes (i n)
	(dotimes (j p)
	  (if (equal  type 'row-major)
	      (setf (aref result i j) (elt (elt lol i) j))
	      (setf (aref result i j) (elt (elt lol j) i)))))
      result)))

 