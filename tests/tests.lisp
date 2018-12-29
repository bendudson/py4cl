
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

(deftest call-one-arg-int (pytests)
  (assert-equalp 42
      (py4cl:python-call "abs" -42)))

(deftest call-one-arg-list (pytests)
  (assert-equalp 9
      (py4cl:python-call "sum" '(3 2 4))))

(py4cl:defpyfun "sum")
(deftest defpyfun-sum (pytests)
  (assert-equalp 6
      (sum '(2 1 3))))

