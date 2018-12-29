;;;; py4cl.asd

(asdf:defsystem #:py4cl
  :serial t
  :description "Call Python libraries from Common Lisp"
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ()
  :pathname #P"src/"
  :components ((:file "package")
               (:file "reader")
               (:file "writer")
               (:file "callpython")))

(asdf:defsystem #:py4cl-tests
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on (#:py4cl
               #:clunit)
  :pathname #P"tests/"
  :components ((:file "tests")))
