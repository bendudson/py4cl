;;;; package.lisp

(defpackage #:py4cl
  (:use #:cl #:iterate)
  (:shadowing-import-from #:iterate #:as #:for)
  (:export ; python-process
   #:*pycmd*   ; The executable to run (string)
   #:pystart
   #:pystop
   #:python-alive-p
   #:python-start-if-not-alive
   #:pyversion-info)
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
   #:remote-objects*)
  (:export ; import-export
   #:defpyfun
   #:defpymodule
   #:export-function)
  (:export ; lisp-classes
   #:python-getattr))
