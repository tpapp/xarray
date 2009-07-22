
;;(asdf:oos 'asdf:compile-op 'xarray :force t)
;;(asdf:oos 'asdf:load-op 'xarray)
;;(asdf:oos 'asdf:load-op 'xarray-test)



;;; Checking current test state

(in-package :xarray-ut)
(run-tests :suite 'xarray-ut)
(describe (run-tests :suite 'xarray-ut))


;;; Development

(in-package :xarray-user)

#+nil 
(progn

  ;; test dev could go here
  )

;; or test dev could go here
