;;;; py4cl.asd

(asdf:defsystem "py4cl"
  :serial t
  :description "Call Python libraries from Common Lisp"
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ("trivial-garbage")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "reader")
               (:file "writer")
               (:file "callbacks")
               (:file "callpython"))
  :in-order-to ((test-op (test-op "py4cl/tests"))))

;; This is to store the path to the source code
;; suggested here https://xach.livejournal.com/294639.html
(defpackage #:py4cl/config (:export #:*base-directory*))
(defparameter py4cl/config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem "py4cl/tests"
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ("py4cl"
               "clunit"
               "trivial-garbage")
  :pathname #P"tests/"
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl/tests :run)))
