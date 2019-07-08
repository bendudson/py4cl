(asdf:defsystem "py4cl-tests"
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ("py4cl"
               "clunit"
               "trivial-garbage")
  :pathname #P"tests/"
  :components ((:file "load-before-test")
               (:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl-tests :run))) 
