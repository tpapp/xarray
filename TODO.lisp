
;;(asdf:oos 'asdf:compile-op 'xarray :force t)
;;(asdf:oos 'asdf:load-op 'xarray)
;;(asdf:oos 'asdf:load-op 'xarray-test)


;; Tamas was thinking about this being a general interface, but then
;; included some specialized issues that probably should be considered
;; here but handled elsewhere.  
;;
;; What I (Tony) am currently thinking about is to pay a penalty
;; initially on speed of access and write a general interface using a
;; range of possible back-ends.  So that we can get the interface
;; clean:   xref pulls out a value and puts it int an array of the
;; same structure, xref* pulls out a value and sticks it into a lisp
;; array or scalar and returns it.   Speed can be handled later by
;; doing a compile-time/run-time tradeoff, we will pay the
;; compile-time penalty, in exchange for run-time advantages.  This
;; fits into the theme of rapid prototyping (slow exec) with rapid
;; execution (post-proto...).

;;; Checking current test state

(in-package :xarray-ut)
(run-tests :suite 'xarray-ut)
;; => #<Results for XARRAY-UT 13 Tests, 0 Errors, 2 Failures>
(describe (run-tests :suite 'xarray-ut))

;;; Development

(in-package :xarray-user)

#+nil 
(progn

  ;; test dev could go here
  )

;;; Alternative backend structures.
#+nil 
(progn
  ;; Need to develop an interface to vectors and lists (and lists of
  ;; lists -- but not vectors of vectors?) 
  )


;; or test dev could go here


