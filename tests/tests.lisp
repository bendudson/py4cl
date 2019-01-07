
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
  (assert-equalp nil (py4cl:python-alive-p))
  (py4cl:python-start)
  (assert-equalp t (py4cl:python-alive-p))
  (py4cl:python-stop)
  (assert-equalp nil (py4cl:python-alive-p)))

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
  (assert-equalp nil
      (py4cl:python-eval "print('hello')")))

(deftest exec-print (pytests)
  (assert-equalp nil
      (py4cl:python-exec "print('hello')")))

(deftest call-one-arg-int (pytests)
  (assert-equalp 42
      (py4cl:python-call "abs" -42)))

(deftest call-one-arg-list (pytests)
  (assert-equalp 9
      (py4cl:python-call "sum" '(3 2 4))))

(deftest call-one-arg-string (pytests)
  (assert-equalp #("h" "e" "l" "l" "o")
      (py4cl:python-call "list" "hello")))

(py4cl:defpyfun "sum")
(deftest defpyfun-sum (pytests)
  (assert-equalp 6
      (sum '(2 1 3))))

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

(deftest callback-one-arg (pytests)
  (py4cl:export-function (lambda (x) (* 2 x)) "double")
  (assert-equalp 4
      (py4cl:python-eval "double(2)")))

(deftest callback-two-args (pytests)
  (py4cl:export-function (lambda (x y) (/ x y)) "div")
  (assert-equalp 2.5
      (py4cl:python-eval "div(5, 2)")))

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

  
