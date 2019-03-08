
(defpackage #:py4cl-tests
  (:use #:cl #:clunit)
  (:export #:run))

(in-package :py4cl-tests)

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

;; This tests whether outputs to stdout mess up the return stream
(deftest eval-print (pytests)
  (unless (= 2 (first (py4cl:python-version-info)))
    (assert-equalp nil
        (py4cl:python-eval "print(\"hello\")")
      "This fails with python 2")))

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
  (assert-equalp 0
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

