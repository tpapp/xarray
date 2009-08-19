
;;(asdf:oos 'asdf:compile-op 'xarray :force t)
;;(asdf:oos 'asdf:load-op 'xarray)
;;(asdf:oos 'asdf:load-op 'xarray-test)



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


