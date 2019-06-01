
(defpackage #:py4cl/tests
  (:use #:cl #:clunit)
  (:export #:run))

(in-package :py4cl/tests)

(defsuite tests ())

;; Tests which start and stop python
(defsuite pytests (tests))

(deffixture pytests (@body)
  (py4cl:python-start)
  @body
  (py4cl:python-stop))

(defun run (&optional interactive?)
  "Run all the tests for py4cl."
  (run-suite 'tests :use-debugger interactive?))

;; Start and stop Python, check that python-alive-p responds
(deftest start-stop (tests)
  (assert-false (py4cl:python-alive-p))
  (py4cl:python-start)
  (assert-true (py4cl:python-alive-p))
  (py4cl:python-stop)
  (assert-false (py4cl:python-alive-p)))

(deftest eval-integer (pytests)
  (let ((result (py4cl:python-eval "1 + 2 * 3")))
    (assert-true (typep result 'integer))
    (assert-equalp 7 result)))

(deftest eval-malformed (pytests)
  (assert-condition py4cl:python-error
      (py4cl:python-eval "1 + ")))

(deftest eval-real (pytests)
  (let ((result (py4cl:python-eval "1.3 + 2.2")))
    (assert-true (typep result 'real))
    (assert-equalp 3.5 result)))

(deftest eval-vector (pytests)
  (let ((result (py4cl:python-eval "[i**2 for i in range(4)]")))
    (assert-true (typep result 'array))
    (assert-equalp #(0 1 4 9) result)))

(deftest eval-list (pytests)
  (let ((result (py4cl:python-eval "(1,2,3)")))
    (assert-true (typep result 'cons))
    (assert-equalp '(1 2 3) result)))

;; Check passing strings, including quote characters which need to be escaped
(deftest eval-string (pytests)
  (assert-equalp "say \"hello\" world"
      (py4cl:python-eval "'say \"hello\"' + ' world'")))

(deftest pythonize-format-string (tests)
  (assert-equalp "\"foo\""
                 (py4cl::pythonize (format nil "foo"))))

(deftest eval-format-string (pytests)
  (assert-equalp "foo"
                 (py4cl:python-eval
                  (py4cl::pythonize (format nil "foo")))))

;; This tests whether outputs to stdout mess up the return stream
(deftest eval-print (pytests)
  (unless (= 2 (first (py4cl:python-version-info)))
    ;; Should return the result of print, not the string printed
    (assert-equalp nil
        (py4cl:python-eval "print(\"hello\")")
        "This fails with python 2")

    ;; Should print the output to stdout
    (assert-equalp "hello world
"
                   (with-output-to-string (*standard-output*)
                     (py4cl:chain (print "hello world"))))

    ;; Check that the output is cleared, by printing a shorter string
    (assert-equalp "testing
"
                   (with-output-to-string (*standard-output*)
                     (py4cl:chain (print "testing"))))))

(deftest eval-params (pytests)
  ;; Values are converted into python values
  (let ((a 4)
        (b 7))
    (assert-equalp 11
        (py4cl:python-eval a "+" b)))

  ;; Arrays can also be passed
  (assert-equalp #2A((1 2) (3 4))
    (py4cl:python-eval #2A((1 2) (3 4))))

  (assert-equalp #2A((2 4) (6 8))
    (py4cl:python-eval #2A((1 2) (3 4)) "*" 2))

  (assert-equalp #3A(((2 4) (7 8)) ((8 5) (1 6)))
    (py4cl:python-eval #3A(((1 3) (6 7)) ((7 4) (0 5)))  "+" 1))

  ;; Test handling of real numbers in arrays
  (assert-equalp #(1.0 2.0)
      (py4cl:python-eval (vector 1.0 2.0)))

  ;; Test empty arrays
  (assert-equalp #()
                 (py4cl:python-eval #()))

  ;; Unless the values are strings
  (let ((str "hello"))
    (assert-condition py4cl:python-error
        (py4cl:python-eval "len(" str ")"))  ; "len(hello)"

    ;; To pass a string to python, run through pythonize:
    (assert-equalp 5
        (py4cl:python-eval "len(" (py4cl::pythonize str) ")"))))

(deftest complex-values (pytests)
  ;; Single values
  (assert-equality #'= #C(1 2)
    (py4cl:python-eval #C(1 2)))
  (assert-equality #'= #C(1 -2)
    (py4cl:python-eval #C(1 -2)))
  (assert-equality #'= #C(-1 -2)
    (py4cl:python-eval #C(-1 -2)))

  ;; Expressions. Tested using multiply to catch things like
  ;; "1+2j * 2+3j -> 1+7j rather than (-4+7j)
  ;; Note: Python doesn't have complex integers, so all returned
  ;;       values could be floats
  (assert-equality #'= #C(-4 7)
    (py4cl:python-eval #C(1 2) "*" #C(2 3)))
  (assert-equality #'= #C(4 7)
    (py4cl:python-eval #C(1 -2) "*" #C(-2 3)))
  
  ;; Lists of complex numbers
  (assert-equality #'= #C(6 9)
    (py4cl:python-call "sum" (list #C(1 2) #C(2 3) #C(3 4)))))

(deftest exec-print (pytests)
  (unless (= 2 (first (py4cl:python-version-info)))
      ;; Python 3
      (assert-equalp nil
          (py4cl:python-exec "print(\"hello\")")
        "This fails with python 2")))

(deftest call-lambda-no-args (pytests)
  (assert-equalp 3
      (py4cl:python-call "lambda : 3")))

(deftest call-one-arg-int (pytests)
  (assert-equalp 42
      (py4cl:python-call "abs" -42)))

(deftest call-one-arg-list (pytests)
  (assert-equalp 9
      (py4cl:python-call "sum" '(3 2 4))))

(deftest call-one-arg-string (pytests)
  (assert-equalp #("h" "e" "l" "l" "o")
      (py4cl:python-call "list" "hello")))

(deftest call-dotted-function (pytests)
  (py4cl:python-exec "import math")
  (assert-equalp (sqrt 42)
      (py4cl:python-call "math.sqrt" 42)))

(deftest call-lambda-function (pytests)
  (assert-equalp 16
      (py4cl:python-call "lambda x: x*x" 4)))

(deftest call-lambda-function-two-args (pytests)
  (assert-equalp 10
      (py4cl:python-call "lambda x, y: x*y - y" 3 5)))

(deftest call-lambda-keywords (pytests)
  (assert-equalp -1
      (py4cl:python-call "lambda a=0, b=1: a-b" :b 2 :a 1))
  (assert-equalp 1
      (py4cl:python-call "lambda a=0, b=1: a-b" :a 2 :b 1)))

(deftest call-with-lambda-callback (pytests)
  ;; Define a function in python which calls its argument
  (py4cl:python-exec "runme = lambda f: f()")
  ;; Pass a lambda function to python-call
  (assert-equalp 42
      (py4cl:python-call "runme" (lambda () 42))))

(py4cl:import-function "sum")
(deftest import-function-sum (pytests)
  (assert-equalp 6
      (sum '(2 1 3))))

(deftest call-return-numpy-types (pytests)
  (py4cl:python-exec "import numpy as np")
  (assert-equalp 42.0
                 (py4cl:python-eval "np.float64(42.0)")))

;; Simple callback function
(defun test-func ()
  42)

(deftest callback-no-args (pytests)
  (py4cl:export-function #'test-func "test")
  (assert-equalp 42
      (py4cl:python-eval "test()")))

;; Even simpler function returning NIL
(defun nil-func ()
  nil)

(deftest callback-no-args-return-nil (pytests)
  (py4cl:export-function #'nil-func "test_nil")
  (assert-equalp nil
      (py4cl:python-eval "test_nil()")))

;; Python can't eval write-to-string's output "3.141592653589793d0"
(deftest callback-return-double (pytests)
  (py4cl:export-function (lambda () pi) "test")
  (assert-equalp 3.1415927
      (py4cl:python-eval "test()")))

(deftest callback-one-arg (pytests)
  (py4cl:export-function (lambda (x) (* 2 x)) "double")
  (assert-equalp 4
      (py4cl:python-eval "double(2)")))

(deftest callback-two-args (pytests)
  (py4cl:export-function (lambda (x y) (/ x y)) "div")
  (assert-equalp 3
      (py4cl:python-eval "div(6, 2)")))

(deftest callback-many-args (pytests)
  (py4cl:export-function #'+ "add")
  (assert-equalp 15
      (py4cl:python-eval "add(2, 4, 6, 3)")))

(deftest callback-seq-arg (pytests)
  (py4cl:export-function #'reverse "reverse")
  (assert-equalp '(3 1 2 4)
      (py4cl:python-eval "reverse((4,2,1,3))"))
  (assert-equalp #(3 1 2 4)
      (py4cl:python-eval "reverse([4,2,1,3])")))

(deftest callback-keyword-arg (pytests)
  (py4cl:export-function (lambda (&key setting) setting) "test")
  (assert-equalp nil
      (py4cl:python-eval "test()"))
  (assert-equalp 42
      (py4cl:python-eval "test(setting=42)")))


;; Call python during callback
(deftest python-during-callback (pytests)
  (py4cl:export-function
   (lambda () (py4cl:python-eval "42"))
   "test")
  (assert-equalp 42
      (py4cl:python-eval "test()")))

;; Hash-table support
(deftest hash-table-empty (pytests)
  (assert-equalp "{}"
      (py4cl:python-call "str" (make-hash-table))))

(deftest hash-table-values (pytests)
  (let ((table (make-hash-table)))
    (setf (gethash "test" table) 3
          (gethash "more" table) 42)
    (assert-equalp 42
        (py4cl:python-call "lambda d: d[\"more\"]" table))
    (assert-equalp 3
        (py4cl:python-call "lambda d: d[\"test\"]" table))
    (assert-equalp 2
        (py4cl:python-call "len" table))))

(deftest hash-table-from-dict (pytests)
  ;; Simple keys
  (let ((table (py4cl:python-eval "{1:2, 2:3}")))
    (assert-equalp 2
                   (gethash 1 table))
    (assert-equalp 3
                   (gethash 2 table)))
  
  ;; Ensure values are being lispified
  (let ((table (py4cl:python-eval "{1:[1,2,3]}")))
    (assert-equalp #(1 2 3)
                   (gethash 1 table)))
  
  ;; Ensure keys are being lispified and string keys work
  (let ((table (py4cl:python-eval "{\"test\":42}")))
    (assert-equalp 42
                   (gethash "test" table))))

;; Asyncronous functions
(deftest call-function-async (pytests)
  (let ((thunk (py4cl:python-call-async "str" 42)))
    ;; returns a function which when called returns the result
    (assert-equalp "42"
        (funcall thunk))
    ;; And returns the same when called again
    (assert-equalp "42"
        (funcall thunk)))

  ;; Check if it handles errors
  (let ((thunk (py4cl:python-call-async "len"))) ; TypeError
    (assert-condition py4cl:python-error
        (funcall thunk)))

  ;; Check that values can be requested out of order
  (let ((thunk1 (py4cl:python-call-async "str" 23))
        (thunk2 (py4cl:python-call-async "str" 12))
        (thunk3 (py4cl:python-call-async "str" 7)))
    (assert-equalp "12"
        (funcall thunk2))
    (assert-equalp "7"
        (funcall thunk3))
    (assert-equalp "23"
        (funcall thunk1))))

(deftest python-objects (pytests)
  ;; Define a simple python class containing a value
  (py4cl:python-exec
"class Test:
  pass

a = Test()
a.value = 42")

  ;; Check that the variable has been defined
  (assert-equalp 42
                 (py4cl:python-eval "a.value"))

  ;; Implementation detail: No objects stored in python dict
  (assert-equalp 0
                 (py4cl:python-eval "len(_py4cl_objects)"))
  
  ;; Evaluate and return a python object
  (let ((var (py4cl:python-eval "a")))
    ;; Implementation detail: Type of returned object
    (assert-equalp 'PY4CL::PYTHON-OBJECT
                   (type-of var))
    
    ;; Implementation detail: Object is stored in a dictionary
    (assert-equalp 1
                   (py4cl:python-eval "len(_py4cl_objects)"))

    ;; Can pass to eval to use dot accessor
    (assert-equalp 42
                   (py4cl:python-eval var ".value"))

    ;; Can pass as argument to function
    (assert-equal 84
                  (py4cl:python-call "lambda x : x.value * 2" var)))
  
  ;; Trigger a garbage collection so that VAR is finalized.
  ;; This should also delete the object in python
  (tg:gc :full t)

  ;; Implementation detail: dict object store should be empty
  ;; Note: This is dependent on the CL implementation. Trivial-garbage
  ;; doesn't seem to support ccl
  #-clozure (assert-equalp 0
                (py4cl::python-eval "len(_py4cl_objects)")))

(deftest python-del-objects (tests)
    ;; Check that finalizing objects doesn't start python
  (py4cl:python-start)
  (py4cl:python-exec
"class Test:
  pass

a = Test()")
  (let ((var (py4cl:python-eval "a")))
    ;; Implementation detail: Type of returned object
    (assert-equalp 'PY4CL::PYTHON-OBJECT
        (type-of var))
    
    (py4cl:python-stop)
    (assert-false (py4cl:python-alive-p)))
  
  ;; VAR out of scope. Make sure it's finalized
  (tg:gc :full t)
  
  (assert-false (py4cl:python-alive-p)))

(deftest python-method (pytests)
  (assert-equalp 3
      (py4cl:python-method '(1 2 3) '__len__))
  (assert-equalp "hello world"
      (py4cl:python-method "hello {0}" 'format "world")))


;; Shorter more convenient slicing
(py4cl:import-function "slice")

(deftest chain (pytests)
  (assert-equalp "Hello world"
      (py4cl:chain "hello {0}" (format "world") (capitalize)))
  (assert-equalp "hello world"
      (let ((format-str "hello {0}")
            (argument "world"))
        (py4cl:chain format-str (format argument))))
  (assert-equalp "result: 3"
      (py4cl:chain "result: {0}" (format (+ 1 2))))
  (assert-equalp 3
      (py4cl:chain (slice 3) stop))

  ;; Anything not a list or a symbol is put between [] brackets (__getitem__)
  (assert-equalp "o"
      (py4cl:chain "hello" 4))

  ;; [] operator for indexing and slicing (alias for __getitem__)
  
  (assert-equalp "l"
      (py4cl:chain "hello" ([] 3)))
  (assert-equalp 3
      (py4cl:chain #2A((1 2) (3 4))  ([] 1 0)))
  (assert-equalp #(4 5)
      (py4cl:chain #2A((1 2 3) (4 5 6))  ([] 1 (slice 0 2))))

  (let ((dict (py4cl:python-eval "{\"hello\":\"world\", \"ping\":\"pong\"}")))
    (assert-equalp "world"
        (py4cl:chain dict "hello"))
    (assert-equalp "pong"
        (py4cl:chain dict ([] "ping")))))
  
(deftest chain-keywords (pytests)
  (py4cl:python-exec
   "def test_fn(arg, key=1):
       return arg * key")

  (assert-equalp 3
      (py4cl:chain (test_fn 3)))
  (assert-equalp 6
      (py4cl:chain (test_fn 3 :key 2)))

  (py4cl:python-exec
   "class testclass:
      def run(self, dummy = 1, value = 42):
        return value")

  (assert-equalp 42
      (py4cl:chain (testclass) (run)))

  (assert-equalp 31
      (py4cl:chain (testclass) (run :value 31))))


(deftest chain-strings (pytests)
  (py4cl:python-exec
   "class TestClass:
      def doThing(self, dummy = 1, value = 42):
        return value")
  
  (assert-equalp 42
      (py4cl:chain ("TestClass") ("doThing")))

  (assert-equalp 31
      (py4cl:chain ("TestClass") ("doThing" :value 31))))

(deftest remote-objects (pytests)
  ;; REMOTE-OBJECTS returns a handle
  (assert-equalp 'py4cl::python-object
                 (type-of (py4cl:remote-objects (py4cl:python-eval "1+2"))))

  ;; REMOTE-OBJECTS* returns a value
  (assert-equalp 3
                 (py4cl:remote-objects* (py4cl:python-eval "1+2")))
    
  (assert-equalp 3
                 (py4cl:python-eval 
                  (py4cl:remote-objects (py4cl:python-eval "1+2"))))

  ;; Nested remote-object environments

  (assert-equalp 'py4cl::python-object
                 (type-of (py4cl:remote-objects
                           (py4cl:remote-objects (py4cl:python-eval "1+2"))
                           (py4cl:python-eval "1+2")))))

(deftest call-callable-object (pytests)
  (assert-equalp 6
      (py4cl:python-call (py4cl:python-eval "lambda x : 2*x") 3)))

(deftest setf-eval (pytests)
  (setf (py4cl:python-eval "test_value") 42) ; Set a variable
  (assert-equalp 42
                 (py4cl:python-eval "test_value")))  

(deftest setf-chain (pytests)
  (assert-equalp #(0 5 2 -1)
                 (py4cl:remote-objects*
                   (let ((list (py4cl:python-eval "[0, 1, 2, 3]")))
                     (setf (py4cl:chain list ([] 1)) 5
                           (py4cl:chain list ([] -1)) -1)
                     list)))

  (assert-equalp "world"
      (py4cl:remote-objects*
        (let ((dict (py4cl:python-eval "{}")))
          (setf (py4cl:chain dict ([] "hello")) "world")
          (py4cl:chain dict ([] "hello")))))
  
  ;; Define an empty class which can be modified
  (py4cl:python-exec "
class testclass:
  pass")
  
  (let ((obj (py4cl:chain (testclass))))
    (setf (py4cl:chain obj data_attrib) 21)
    (assert-equalp 21
        (py4cl:chain obj data_attrib))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Passing unknown lisp objects to python

(defstruct test-struct
  x y)

(deftest lisp-structs (pytests)
  ;; Create a struct and pass to Python
  (let ((result (py4cl:python-call
                 "lambda x: x"
                 (make-test-struct :x 1 :y 2))))

    ;; Check we got back the structure
    (assert-true (typep result 'test-struct))
    (assert-equalp 1
                   (test-struct-x result))
    (assert-equalp 2
                   (test-struct-y result))))

(defclass test-class ()
  ((value :initarg :value)
   (thing :initarg :thing)))

;; Define a method to handle slot access from python
(defmethod py4cl:python-getattr ((object test-class) slot-name)
  (cond
    ((string= slot-name "value")
     (slot-value object 'value))
    ((string= slot-name "thing")
     (slot-value object 'thing))
    ((string= slot-name "func")
     (lambda (arg) (* 2 arg)))
    (t (call-next-method))))

(deftest lisp-class-slots (pytests)
  (let ((object (make-instance 'test-class :thing 23 :value 42)))
    ;; Slot access
    (assert-equalp 23
        (py4cl:python-call "lambda x : x.thing" object))
    (assert-equalp 42
        (py4cl:chain object value))

    ;; Function (method) call
    (assert-equalp 42
        (py4cl:chain object (func 21))))
    
  ;; The handler should work for other objects of the same class (class-of)
  (let ((object2 (make-instance 'test-class :thing "hello" :value 314)))
    (assert-equalp "hello"
                   (py4cl:chain object2 thing))))


;; Class inheriting from test-class
(defclass child-class (test-class)
  ((other :initarg :other)))

;; Define method which passes to the next method if slot not recognised
(defmethod py4cl:python-getattr ((object child-class) slot-name)
  (cond
    ((string= slot-name "other")
     (slot-value object 'other))
    (t (call-next-method))))

(deftest lisp-class-inherit (pytests)
  (let ((object (make-instance 'child-class :thing 23 :value 42 :other 3)))
    (assert-equalp 23
        (py4cl:python-call "lambda x : x.thing" object))
    (assert-equalp 42
        (py4cl:chain object value))
    (assert-equalp 3
        (py4cl:chain object other))))

(deftest callback-in-remote-objects (pytests)
  ;; Callbacks send values to lisp in remote-objects environments
  (assert-equalp 6
      (py4cl:remote-objects*
        (py4cl:python-call (lambda (x y) (* x y)) 2 3))))
