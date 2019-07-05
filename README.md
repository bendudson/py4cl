[[https://travis-ci.org/bendudson/py4cl][https://travis-ci.org/bendudson/py4cl.svg?branch=master]]

* Github Pages

Documentation is available on [github-pages](digikar99.github.io/py4cl/). That and the one below hasn't been unified yet - they are complementary yet.

** Tests

Tests use [[https://github.com/tgutu/clunit][clunit]], and run on [[https://travis-ci.org/][Travis]] using [[https://github.com/luismbo/cl-travis][cl-travis]]. Most development
is done under Arch linux with SBCL and Python3. To run the tests
yourself:
#+BEGIN_SRC lisp
(asdf:test-system :py4cl)
#+END_SRC
or
#+BEGIN_SRC lisp
(ql:quickload :py4cl/tests)
(py4cl/tests:run)
#+END_SRC

* Examples

#+BEGIN_SRC lisp
(ql:quickload :py4cl)

(py4cl:import-module "numpy" :as "np")
(py4cl:import-module "scipy.integrate" :as "integrate")

;; Integrate some ODEs
(defparameter *data*
  (integrate:odeint 
   (lambda (y time) 
     (vector (aref y 1)       ; dy[0]/dt = y[1]
             (- (aref y 0)))) ; dy[1]/dt = -y[0]
   #(1.0 0.0)   ; Initial state
   (np:linspace 0.0 (* 2 pi) 20)))  ; Vector of times

; (array-dimensions *data*) => (20 2)

;; Make a plot, save and show it in a window
(py4cl:import-module "matplotlib.pyplot" :as "plt")

(plt:plot *data*)
(plt:xlabel "Time")
(plt:savefig "result.pdf")
(plt:show)
#+END_SRC

More detailed examples of using python packages using =py4cl=:
 - [[./docs/numpy.org][Numpy arrays]]
 - [[./docs/matplotlib.org][Matplotlib plotting]]
 - [[./docs/scipy.org][Scipy scientific library]]

* Reference

#+BEGIN_SRC lisp
(destructuring-bind (fig ax) (plt:subplots)
  ;; fig is #S(PY4CL::PYTHON-OBJECT :TYPE "<class 'matplotlib.figure.Figure'>" :HANDLE 6)
  (py4cl:python-eval ax ".plot(" #(0 1 0 1) ")")
  (plt:show)) 
#+END_SRC

The interface to python objects is nicer using =chain= (see below):
#+BEGIN_SRC lisp
(destructuring-bind (fig ax) (plt:subplots)
  (py4cl:chain ax (plot #(0 1 0 1)))
  (plt:show)) 
#+END_SRC

** Chaining python methods: =chain=

In python it is quite common to apply a chain of method calls, data
member access, and indexing operations to an object. To make this work
smoothly in Lisp, there is the =chain= macro (Thanks to @kat-co and
[[https://common-lisp.net/project/parenscript/reference.html][parenscript]] for the inspiration). This consists of a target object,
followed by a chain of operations to apply.  For example
#+BEGIN_SRC lisp
(py4cl:chain "hello {0}" (format "world") (capitalize)) ; => "Hello world"
#+END_SRC

#+RESULTS:
: Hello world

which is converted to python 
#+BEGIN_SRC python
return "hello {0}".format("world").capitalize()
#+END_SRC

#+RESULTS:
: Hello world

The only things which are treated specially by this macro are lists
and symbols at the top level. The first element of lists are treated as
python method names, top-level symbols are treated as data
members. Everything else is evaluated as lisp before being converted
to a python value.

If the first argument is a list, then it is assumed to be a python
function to be called; otherwise it is evaluated before converting to
a python value. For example
#+BEGIN_SRC lisp
(py4cl:chain (slice 3) stop)
#+END_SRC

#+RESULTS:
: 3

is converted to the python:
#+BEGIN_SRC python
return slice(3).stop
#+END_SRC

#+RESULTS:
: 3

Symbols as first argument, or arguments to python methods, are
evaluated, so the following works:
#+BEGIN_SRC lisp
(let ((format-str "hello {0}")
      (argument "world"))
 (py4cl:chain format-str (format argument))) ; => "hello world"
#+END_SRC

#+RESULTS:
: hello world

Arguments to methods are lisp, since only the top level forms in =chain= are treated specially:
#+BEGIN_SRC lisp
(py4cl:chain "result: {0}" (format (+ 1 2))) ; => "result: 3"
#+END_SRC

#+RESULTS:
: result: 3

Indexing with =[]= brackets is commonly used in python, which calls the =__getitem__= method.
This method can be called like any other method
#+BEGIN_SRC lisp
(py4cl:chain "hello" (__getitem__ 4)) ; => "o"
#+END_SRC

#+RESULTS:
: o

but since this is a common method an alias =[]= is supported:
#+BEGIN_SRC lisp
(py4cl:chain "hello" ([] 4)) ; => "o"
#+END_SRC

#+RESULTS:
: o

which is converted to the python
#+BEGIN_SRC python
return "hello"[4]
#+END_SRC

#+RESULTS:
: o

For simple cases where the index is a value like a number or string
(not a symbol or a list), the brackets can be omitted:
#+BEGIN_SRC lisp
(py4cl:chain "hello" 4) ; => "o"
#+END_SRC

#+RESULTS:
: o

Slicing can be done by calling the python =slice= function:
#+BEGIN_SRC lisp
(py4cl:chain "hello" ([] (py4cl:python-call "slice" 2 4)))  ; => "ll"
#+END_SRC

#+RESULTS:
: ll

which could be imported as a lisp function (see below):
#+BEGIN_SRC lisp
(py4cl:import-function "slice")
(py4cl:chain "hello" ([] (slice 2 4))) ; => "ll"
#+END_SRC

#+RESULTS:
: ll

This of course also works with multidimensional arrays:
#+BEGIN_SRC lisp
(py4cl:chain #2A((1 2 3) (4 5 6))  ([] 1 (slice 0 2)))  ;=> #(4 5)
#+END_SRC

#+RESULTS:
| 4 | 5 |

Sometimes the python functions or methods may contain upper case
characters; class names often start with a capital letter. All symbols
are converted to lower case, but the case can be controlled by passing
a string rather than a symbol as the first element:
#+BEGIN_SRC lisp
;; Define a class
(py4cl:python-exec
   "class TestClass:
      def doThing(self, value = 42):
        return value")

;; Create an object and call the method
(py4cl:chain ("TestClass") ("doThing" :value 31))  ; => 31
#+END_SRC
Note that the keyword is converted, converting to lower case.

** Printing from python

** Asynchronous python functions: =python-call-async=

One of the advantages of using streams to communicate with a separate
python process, is that the python and lisp processes can run at the
same time. =python-call-async= calls python but returns a closure
immediately. The python process continues running, and the result can
be retrieved by calling the returned closure. 

#+BEGIN_SRC lisp
(defparameter thunk (py4cl:python-call-async "lambda x: 2*x" 21))

(funcall thunk)  ; => 42
#+END_SRC

#+RESULTS:
: 42

If the function call requires callbacks to lisp, then these will only
be serviced when a =py4cl= function is called. In that case the python
function may not be able to finish until the thunk is called. This
should not result in deadlocks, because all =py4cl= functions can
service callbacks while waiting for a result.

Notes:
 -  =import-module= should be used as a top-level form, to ensure that
   the package is defined before it is used.

- If using =import-module= within [[https://orgmode.org/worg/org-contrib/babel/][org-mode babel]] then the import
  should be done in a separate code block to the first use of the
  imported package, or a condition will be raised like "Package NP
  does not exist."

** Exporting a function to python: =export-function=

Lisp functions can be passed as arguments to =python-call= 
or imported functions:
#+BEGIN_SRC lisp
(py4cl:python-exec "from scipy.integrate import romberg")

(py4cl:python-call "romberg" 
                   (lambda (x) (/ (exp (- (* x x)))
                                  (sqrt pi)))
                   0.0 1.0) ; Range of integration
#+END_SRC

#+RESULTS:
: 0.4213504

Lisp functions can be made available to python code using =export-function=:
#+BEGIN_SRC lisp
(py4cl:python-exec "from scipy.integrate import romberg")

(py4cl:export-function (lambda (x) (/ (exp (- (* x x)))
                                      (sqrt pi))) "gaussian")

(py4cl:python-eval "romberg(gaussian, 0.0, 1.0)") ; => 0.4213504
#+END_SRC

#+RESULTS:
: 0.4213504

** Manipulating objects remotely: =remote-objects=

If a sequence of python functions and methods are being used to manipulate data,
then data may be passed between python and lisp. This is fine for small amounts
of data, but inefficient for large datasets.

The =remote-objects= and =remote-objects*= macros provide =unwind-protect= environments
in which all python functions return handles rather than values to lisp. This enables
python functions to be combined without transferring much data.

The difference between these macros is =remote-objects= returns a handle, but
=remote-objects*= evaluates the result, and so will return a value if possible.

#+BEGIN_SRC lisp
(py4cl:remote-objects (py4cl:python-eval "1+2")) ; => #S(PY4CL::PYTHON-OBJECT :TYPE "<class 'int'>" :HANDLE 0)
#+END_SRC

#+RESULTS:
: #S(PY4CL::PYTHON-OBJECT :TYPE "<class 'int'>" :HANDLE 4)

#+BEGIN_SRC lisp
(py4cl:remote-objects* (py4cl:python-eval "1+2")) ; => 3
#+END_SRC

#+RESULTS:
: 3

The advantage comes when dealing with large arrays or other datasets:
#+BEGIN_SRC lisp
(time (np:sum (np:arange 1000000)))
; => 3.672 seconds of real time
;    390,958,896 bytes consed
#+END_SRC

#+BEGIN_SRC lisp
(time (py4cl:remote-objects* (np:sum (np:arange 1000000))))
; => 0.025 seconds of real time
;    32,544 bytes consed
#+END_SRC
** =setf=-able places

The =python-eval= function is =setf=-able, so that python objects can
be assigned to by using =setf=. Since =chain= uses =python-eval=, it is also
=setf=-able. This can be used to set elements in an array, entries in a dict/hash-table, 
or object data members, for example:
#+BEGIN_SRC lisp
(py4cl:import-module "numpy" :as "np")
#+END_SRC

#+RESULTS:
: T

#+BEGIN_SRC lisp
(py4cl:remote-objects*
  (let ((array (np:zeros '(2 2))))
    (setf (py4cl:chain array ([] 0 1)) 1.0
          (py4cl:chain array ([] 1 0)) -1.0)
    array)) 
; => #2A((0.0 1.0)
;        (-1.0 0.0))
#+END_SRC

#+RESULTS:
: #2A((0.0 1.0) (-1.0 0.0))

Note that this modifies the value in python, so the above example only
works because =array= is a handle to a python object, rather than an
array which is stored in lisp. The following therefore does not work:
#+BEGIN_SRC lisp
(let ((array (np:zeros '(2 2))))
  (setf (py4cl:chain array ([] 0 1)) 1.0
        (py4cl:chain array ([] 1 0)) -1.0)
  array)
; => #2A((0.0 0.0)
;        (0.0 0.0))
#+END_SRC

#+RESULTS:
: #2A((0.0 0.0) (0.0 0.0))

The =np:zeros= function returned an array to lisp; the array was then
sent to python and modified in python. The modified array is not
returned, since this would mean transferring the whole array. If the
value is in lisp then just use the lisp functions:
#+BEGIN_SRC lisp
(let ((array (np:zeros '(2 2))))
  (setf (aref array 0 1) 1.0
        (aref array 1 0) -1.0)
  array)
; => #2A((0.0 1.0)
;        (-1.0 0.0))
#+END_SRC

#+RESULTS:
: #2A((0.0 1.0) (-1.0 0.0))

** Passing lisp objects to python: =python-getattr=

Lisp structs and class objects can be passed to python, put into data structures and
returned:
#+BEGIN_SRC lisp
(py4cl:import-function "dict") ; Makes python dictionaries

(defstruct test-struct 
    x y)

(let ((map (dict :key (make-test-struct :x 1 :y 2))))  ; Make a dictionary, return as hash-map
  ;; Get the struct from the hash-map, and get the Y slot
  (test-struct-y
    (py4cl:chain map "key")))  ; => 2
#+END_SRC

#+RESULTS:
: 2

In python this is handled using an object of class =UnknownLispObject=, which
contains a handle. The lisp object is stored in a hash map
=*lisp-objects*=. When the python object is deleted, a message is sent to remove
the object from the hash map.

To enable python to access slots, or call methods on a struct or class, a
handler function needs to be registered. This is done by providing a method 
for generic function =python-getattr=. This function will be called when a
python function attempts to access attributes of an object (=__getattr__=
method).

#+BEGIN_SRC lisp
;; Define a class with some slots
(defclass test-class ()
  ((value :initarg :value)))

;; Define a method to handle calls from python
(defmethod py4cl:python-getattr ((object test-class) slot-name)
  (cond
    ((string= slot-name "value") ; data member
      (slot-value object 'value))
    ((string= slot-name "func")  ; method, return a function
      (lambda (arg) (* 2 arg)))
    (t (call-next-method)))) ; Otherwise go to next method

(let ((instance (make-instance 'test-class :value 21))) 
  ;; Get the value from the slot, call the method
  ;; python: instance.func(instance.value)
  (py4cl:chain instance (func (py4cl:chain instance value))))  ; => 42
#+END_SRC

#+RESULTS:
: 42

Inheritance then works as usual with CLOS methods:
#+BEGIN_SRC lisp
;; Class inheriting from test-class
(defclass child-class (test-class)
  ((other :initarg :other)))

;; Define method which passes to the next method if slot not recognised
(defmethod py4cl:python-getattr ((object child-class) slot-name)
  (cond
    ((string= slot-name "other")
     (slot-value object 'other))
    (t (call-next-method))))

(let ((object (make-instance 'child-class :value 42 :other 3)))
  (list 
    (py4cl:chain object value) ; Call TEST-CLASS getattr method via CALL-NEXT-METHOD
    (py4cl:chain object other))) ;=> (42 3)
#+END_SRC

#+RESULTS:
| 42 | 3 |
