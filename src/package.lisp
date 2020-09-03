;;;; package.lisp

(defpackage #:py4cl
  (:use #:cl)
  (:export ; python-process
   #:*python-command*   ; The executable to run (string)
   #:python-start
   #:python-stop
   #:python-alive-p
   #:python-start-if-not-alive
   #:python-version-info
   #:python-interrupt)
  (:export ; callpython
   #:python-error
   #:python-eval
   #:python-exec
   #:python-call
   #:python-call-async
   #:python-method
   #:python-generator
   #:chain
   #:python-setf
   #:remote-objects
   #:remote-objects*)
  (:export ; import-export
   #:import-function
   #:import-module
   #:export-function)
  (:export ; lisp-classes
   #:python-getattr
   #:python-setattr)
  (:export ; config 
   #:*config*
   #:initialize
   #:save-config
   #:load-config
   #:config-var
   #:pycmd
   #:numpy-pickle-location
   #:numpy-pickle-lower-bound
   #:py-cd))
