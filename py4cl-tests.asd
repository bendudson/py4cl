;;;; py4cl-tests.asd

(asdf:defsystem #:py4cl-tests
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on (#:py4cl
               #:clunit)
  :pathname #P"tests/"
  :components ((:file "tests")))
