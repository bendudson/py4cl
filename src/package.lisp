
;;;; package.lisp

(defpackage #:py4cl
  (:use #:cl #:iterate)
  (:shadowing-import-from #:iterate #:as #:for)
  (:export ; python-process
   #:pystart
   #:pystop
   #:python-alive-p
   #:python-start-if-not-alive
   #:pyversion-info
   #:pyinterrupt)
  (:export ; callpython
   #:pyerror
   #:pyeval
   #:pyexec
   #:pycall
   #:pycall-async
   #:pymethod
   #:chain
   #:pysetf
   #:remote-objects
   #:remote-objects*
   #:pycall-monitor
   #:pymethod-monitor
   #:pygenerator
   #:pyslot-value)
  (:export ; import-export
   #:pymethod-list
   #:pyslot-list
   #:defpyfun
   #:defpymodule
   #:defpyfuns
   #:export-function)
  (:export ; lisp-classes
   #:python-getattr)
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
