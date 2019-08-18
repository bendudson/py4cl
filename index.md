---
title: py4cl
---

---

# Introduction

Py4CL is a bridge between Common Lisp and Python, which enables Common
Lisp to interact with Python code. It uses streams to communicate with
a separate python process, the approach taken by [cl4py](https://github.com/marcoheisig/cl4py). This is
different to the CFFI approach used by [burgled-batteries](https://github.com/pinterface/burgled-batteries),
but has the same goal. 

This fork is available on github at [digikar99/py4cl](https://github.com/digikar99/py4cl) - much of the credits go to [bendudson/py4cl](https://github.com/bendudson/py4cl), who started this project and made it useable.

Please report the issues on github: [this fork](https://github.com/digikar99/py4cl/issues) or [the original](https://github.com/bendudson/py4cl/issues).

Documentation for almost all of these functions is available as docstrings as well.

# Highlights and Limitations of `py4cl`

- Use python functions and modules from lisp. See [Defining python functions and modules](#defining-python-functions-and-modules). An effort is made to obtain the signatures of the python functions using `inspect.signature`.
- [`pycmd`](#pycmd): Choose which python binary to use. Works with miniconda.
- About 6000 `(pycall "int" "5")` instructions per second @ 1GHz intel 8750H. 
this shouldn't be a bottleneck if you're planning to run "long" processes in python. (For example, deep learning :). )
- Large arrays (100M in 2 sec!) can be transferred using numpy-file-format (see [initialize](#initialize); large strings cannot be; though, there is the [remote-objects(*)](#remote-objects).
- See [TODO].
- Multiple python processes (not documented here) - parallel execution?
- Stderr is not returned; though errors are returned
- Tested on SBCL and CCL. 


# Installation

## Dependencies

This fork is possible due to the following (and therefore, depends on):

On the CL side:

- trivial garbage
- iterate
- cl-json
- bordeaux-threads
- [numpy-file-format](https://github.com/marcoheisig/numpy-file-format) *

\* not available on quicklisp

On python side:

- numpy (optional)

(other packages should be available in a standard python distribution - tested with CPython.)

## Installation

Clone this repository into `~/quicklisp/local-projects/` or other
location where it can be found by ASDF:
```sh
git clone https://github.com/digikar99/py4cl.git
```

Original version by [bendudson](https://github.com/bendudson/py4cl) can be found at: 
```sh
git clone https://github.com/bendudson/py4cl.git
```

However, since then, several changes have been made.

Load into Lisp with
```lisp
(ql:quickload :py4cl)
```


## Setting up

### `initialize`

On loading this library for the first time, run `initialize` and provide the necessary details.
```lisp
(py4cl:initialize)
```

(You may want to note the printed information, about the location of config-file. Of course, you can call this function again, but be sure to refill the values.)

The library uses (temporary) pickled .npy files for transferring large numpy arrays efficiently
between lisp and python. This process is IO intensive, writing as much as 100MB or even a GB each time.
Using a ram-disk is recommended for this purpose. ([How to create a ram disk on Linux?](https://unix.stackexchange.com/questions/66329/creating-a-ram-disk-on-linux))

### `*config*` / `config-var`
These values can also be accessed using `*config*` and `config-var`:

```lisp
CL-USER> py4cl:*config*
((PY4CL:PYCMD . "/home/user/miniconda3/bin/python")
 (PY4CL:NUMPY-PICKLE-LOCATION . "/home/user/ram-disk/_numpy_pickle.npy")
 (PY4CL:NUMPY-PICKLE-LOWER-BOUND . 100000))
CL-USER> (py4cl:config-var 'py4cl:numpy-pickle-location)
"/home/user/ram-disk/_numpy_pickle.npy"
CL-USER> (setf (config-var 'py4cl:pycmd) "python")
"python"
```

Complementary to `config-var` are `save-config` and `load-config`. The latter is called on startup, the config-file exists. `(setf config-var)` calls the former unless it is `pycmd`, as well as asks the python process to load the config, from the config file.


# Examples and Documentation

```lisp
CL-USER> (use-package :py4cl)
```

## Python Processes

It all starts with a python process (actually, more than one as well - this use hasn't been documented here.).

### `pycmd`

```lisp
CL-USER> (config-var 'pycmd)
"python"
```

Also see [config-var](#config--config-var).

### `pyversion-info`
```lisp
CL-USER> (pyversion-info)
(3 7 3 "final" 0)
```

### `pyinterrupt`
`(pyinterrupt &optional process)`

A simple `C-c C-c` only interrupts the lisp process from slime - the python process keeps running. `(pyinterrupt)` can be used in these cases to send a SIGINT (2) to the python process.

It is possible to have `(pyinterrupt)` called on the reception of SIGINT in SLIME:

```lisp
(when (find-package :swank)
  (defvar swank-simple-break)
  (setf (fdefinition 'swank-simple-break)
        (fdefinition (find-symbol "SIMPLE-BREAK" :swank)))
  (defun swank:simple-break
      (&optional (datum "Interrupt from Emacs") &rest args) 
    (py4cl:pyinterrupt)
    (apply (fdefinition 'swank-simple-break) datum args)))
```

Also note that if `pyinterrupt` is not called before sending the next form to `eval` or `exec`, the input-output would go out of sync. A known way to get out is to `(pystop)` the python-process.

However, I have been unable to get the code to work (by adding to `do-after-load` with as well as without SLIME. Further, people may not like a library to fiddle with their environments - so it might be better to leave it up to the user to set it.

### py-cd
`(py-cd path)`

Equivalent of `slime-cd`, since python is a separate process.

### Other useful functions

#### `pystart`
#### `pystop`
#### `python-alive-p`
#### `python-start-if-not-alive`


## Doing arbitrary things in python

Unlike lisp, python (and most other languages) make a distinction between *statements* and *expressions*: see [Quora](https://www.quora.com/Whats-the-difference-between-a-statement-and-an-expression-in-Python-Why-is-print-%E2%80%98hi%E2%80%99-a-statement-while-other-functions-are-expressions) or [stackoverflow](https://stackoverflow.com/questions/4728073/what-is-the-difference-between-an-expression-and-a-statement-in-python).

A general rule of thumb from there is: if you can print it, or assign it to a variable, then it's an expression, otherwise it's a statement.

Both `pyeval` and `pyexec` take any type of arguments. The `arg` is `pythonize`d if the `arg` is not a `string`, or it is a `string` that can be read into a `real`.

### `raw-pyeval`
`(raw-pyeval &rest strings)`

Concatenates the strings and sends them to the python process for `eval`uation. The concatenation should be a valid python expression. Returns the result of evaluating the expression.

### `raw-pyexec`
`(raw-pyexec &rest strings)`

Concatenates the strings and sends them to the python process for `exec`uation. The concatenation should be a valid python statement. Returns nil.

Note that [one limitation of `pyexec` is that modules imported on the top-level (of python) are not available inside some things](https://stackoverflow.com/questions/12505047/in-python-why-doesnt-an-import-in-an-exec-in-a-function-work). These "some things" include functions.

The following should illustrate this point:
```lisp
CL-USER> (pyexec "import time")
NIL
CL-USER> (pyeval "time.time()")
1.5623434e9
CL-USER> (pyexec "
def foo():
  return time.time()")
NIL
CL-USER> (pyeval "foo()")
; Evaluation aborted on #<PYERROR {100C24DF03}> ;; says 'time' is not defined
CL-USER> (pyeval "time.time()")
1.5623434e9
```
THe workaround in this case is to `import` inside the `def`.

Often times, the two commands above would be tedious - since you'd need to convert objects into their string representations every time. To avoid this hassle, there are the following useful functions.

### `pyeval`
`(pyeval &rest args)`

For python expressions 
```lisp
CL-USER> (pyeval "'abc'+'def'")
"abcdef"
```

There's also `(setf pyeval)`, which unlike `(pyexec)`, can return non-`nil` values.

```lisp
CL-USER> (setf (pyeval "a") "5")
"5"
CL-USER> (pyeval "a")
"5"
```

See also [Doing arbitrary things in python](#doing-arbitrary-things-in-python).

### `pyexec`
`(pyexec &rest args)`

For python statements
```lisp
CL-USER> (pyexec (join-strings-using "~%" 
                                     "if True:" 
                                     "  print(5)"
                                     "else:"
                                     "  print(10)"))
; 5
NIL
```
The `join-strings-using` is doable using `(format nil...`.

See [Doing arbitrary things in python](#doing-arbitrary-things-in-python) to learn about `pyeval` and `pyexec`.

## Defining python functions and modules

Rather, we define functions that call python functions.

Names are lispified by converting underscores hyphens, and converting CamelCase to camel-case. Also see [Name Mapping](#name-mapping). 

### `defpyfun`
```lisp
(defpyfun fun-name &optional pymodule-name &key 
  (as fun-name) (lisp-fun-name (lispify-name as))
  (lisp-package *package*) (called-from-defpymodule nil)
  rename-lisp-fun-name (safety t)
```
`lisp-fun-name` is the name of the symbol that would be `fboundp`ed to the function [that calls the python function].

Example Usage:
```lisp
CL-USER> (defpyfun "Input" "keras.layers" :lisp-fun-name "INP")
T ;; return value does not matter

CL-USER> (inp :shape '(1 2))
#S(PY4CL::PYTHON-OBJECT
   :TYPE "<class 'tensorflow.python.framework.ops.Tensor'>"
   :HANDLE 1849)
```

`safety` takes care to import the required function from the required module after python process restarts for some reason. However, this affects speed.

Refer `(describe 'defpyfun)`.

### `defpymodule`
```lisp
(defpymodule pymodule-name &optional import-submodules &key 
  (lisp-package (lispify-name (or as pymodule-name)))
  (reload nil) (safety t) (is-submodule nil)
```

`lisp-package` is the name of the symbol that the package would be bound to.

Example Usage:
```lisp
CL-USER> (defpymodule "keras.layers" t :reload t :lisp-package "KL")
T

CL-USER> (kl:input :shape '(1 2))
#S(PY4CL::PYTHON-OBJECT
   :TYPE "<class 'tensorflow.python.framework.ops.Tensor'>"
   :HANDLE 816)
   
CL-USER> (pycall (kl.advanced-activations:softmax :input-shape '(1 2))
                 (kl:input :shape '(1 2)))
#S(PY4CL::PYTHON-OBJECT
   :TYPE "<class 'tensorflow.python.framework.ops.Tensor'>"
   :HANDLE 144)
```

Note that unlike Common Lisp, python has a single namespace. Therefore, currently,
to call a callable (in Python) object, but not defined as a function in Common Lisp,
you'd need to use something like [pycall].

### `defpyfuns`

(Undocumented here.)

## `pyerror`

(Undocumented here.)

## Using functions and methods

### `pycall`
`(pycall fun-name &rest args)`

Equivalent to the lisp `(funcall function &rest arguments)`. Call a python (or lisp! See [generators and lambdas](#generators-and-lambdas)) function.

```lisp
CL-USER> (py4cl:pycall "print" "hello")
;; hello
NIL
CL-USER> (py4cl:pycall #'+ 2 3 1)
6
```

Note that `fun-name` can be a name (see [Name Mapping]), a function, or a [callable] python-object. See the example in [defpymodule](#defpymodule).

Another two variants of `pycall` are [pycall-async](#pycall-async) and [pycall-monitor](#pycall-monitor--pymethod-monitor).

### `pymethod`
`(pymethod obj method-name &rest args)`

`pymethod` always pythonizes; `method-name` is [name mapped to Python names][Name Mapping].

```lisp
SEQ2SEQ> (pymethod model 'summary) ;; for some "ready" model
__________________________________________________________________________________________________
Layer (type)                    Output Shape         Param #     Connected to                     
==================================================================================================
input_1 (InputLayer)            (None, None, 43)     0                                            
__________________________________________________________________________________________________
input_2 (InputLayer)            (None, None, 64)     0                                            
__________________________________________________________________________________________________
lstm_1 (LSTM)                   [(None, 256), (None, 307200      input_1[0][0]                    
__________________________________________________________________________________________________
lstm_2 (LSTM)                   [(None, None, 256),  328704      input_2[0][0]                    
                                                                 lstm_1[0][1]                     
                                                                 lstm_1[0][2]                     
__________________________________________________________________________________________________
dense_1 (Dense)                 (None, None, 64)     16448       lstm_2[0][0]                     
==================================================================================================
Total params: 652,352
Trainable params: 652,352
Non-trainable params: 0
__________________________________________________________________________________________________
NIL
```

See [pymethod-list](#pymethod-list).

### `pycall-monitor` / `pymethod-monitor`

One issue with barebones `pycall` is that it prints out the information printed by python, after the function returns. This can pose trouble with methods like keras.Model.predict which keep printing to stdout, without returning, for a long time.

```lisp
CL-USER> (pyexec "
def foo():
  print('hello')
  import sys
  sys.stdout.flush()
  import time
  time.sleep(5)
  return 5")
NIL

CL-USER> (pycall "foo")
;; hello ; prints after 5 seconds, after foo has returned
5

CL-USER> (pycall-monitor "foo" ()) ; note the empty argument list
;; hello ; prints immediately

5 ; returns after 5 sec
```

### `pyslot-value`
`(pyslot-value object slot-name)`

```lisp
CL-USER> (pyslot-value model 'input-shape)
#((NIL NIL 43) (NIL NIL 64))
```

See [pyslot-list](#pyslot-list)

### `pycall-async`
`(pycall-async fun-name &rest args)`

One of the advantages of using streams to communicate with a separate
python process, is that the python and lisp processes can run at the
same time. pycall-async calls python but returns a closure
immediately. The python process continues running, and the result can
be retrieved by calling the returned closure. 

```lisp
(defparameter thunk (py4cl:python-call-async "lambda x: 2*x" 21))

(funcall thunk)  ; => 42
```

If the function call requires callbacks to lisp, then these will only
be serviced when a `py4cl` function is called. In that case the python
function may not be able to finish until the thunk is called. This
should not result in deadlocks, because all `py4cl` functions can
service callbacks while waiting for a result.

Also see [Name Mapping].

### `export-function`
`(export-funtion function python-name)`

Lisp functions can be made available to python code using `export-function`:
```lisp
(py4cl:python-exec "from scipy.integrate import romberg")

(py4cl:export-function (lambda (x) (/ (exp (- (* x x)))
                                      (sqrt pi))) "gaussian")

(py4cl:python-eval "romberg(gaussian, 0.0, 1.0)") ; => 0.4213504
```

### `pyhelp`
`(pyhelp python-object)`

Calls python's `help` function on `python-object`. (NOTE: some descriptions, especially
for modules, are too big to be transferred in a reasonable time.)

## Generators and Lambdas

### `pygenerator`
`(pygenerator function stop-value)`

```lisp
CL-USER> (let ((a 0)) (defun foo () (incf a)))
FOO

CL-USER> (pyeval "[x for x in " (pygenerator #'foo 3) "]")
#(1 2)
```

### lambdas

Lisp functions are `pythonize`d to `LispCallbackObject`s. As the name suggests, python can call LispCallbackObjects (and therefore, lisp functions), just like it is any other python callable (which it is!).

```lisp
CL-USER> (py4cl::pythonize #'car)
"_py4cl_LispCallbackObject(4)"

CL-USER> (pycall (lambda (string) (concatenate 'string string " - from Lisp"))
                 "hello")
"hello - from Lisp"
```

## Slot and Method Lists

Currently, all the python objects are grouped under the class `python-object`. The list of methods 
and slots associated with these objects can be obtained using the following two functions.

### `pyslot-list`
`(pyslot-list python-object &key as-vector)`

```lisp
CL-USER> (defpyfun "Model" "keras")
NIL

CL-USER> (pyslot-list (model))
("__class__" "__delattr__" "__dict__" "__doc__" "__eq__" "__ge__"
 "__getattribute__" "__gt__" "__hash__" "__le__" "__lt__" "__module__" "__ne__"
 "__repr__" "__str__" "__weakref__" "_built" "_expects_training_arg"
 "_inbound_nodes" "_initial_weights" "_is_compiled" "_is_graph_network"
 "_layers" "_losses" "_outbound_nodes" "_per_input_losses" "_per_input_updates"
 "_updates" "_uses_inputs_arg" "built" "input_spec" "inputs" "layers" "losses"
 "name" "non_trainable_weights" "optimizer" "outputs" "state_updates"
 "stateful" "supports_masking" "trainable" "trainable_weights" "updates"
 "uses_learning_phase" "weights")

CL-USER> (pyeval (model) ".inputs")
NIL
```

Optionally, see [pyslot-value](#pyslot-value)

### `pymethod-list`
`(pymethod-list python-object &key as-vector)`

```lisp
CL-USER> (pymethod-list (model))
("__call__" "__class__" "__delattr__" "__dir__" "__eq__" "__format__" "__ge__"
 "__getattribute__" "__getstate__" "__gt__" "__hash__" "__init__"
 "__init_subclass__" "__le__" "__lt__" "__ne__" "__new__" "__reduce__"
 "__reduce_ex__" "__repr__" "__setattr__" "__setstate__" "__sizeof__" "__str__"
 "__subclasshook__" "_add_inbound_node" "_base_init"
 "_check_trainable_weights_consistency" "_get_node_attribute_at_index"
 "_init_graph_network" "_init_subclassed_network" "_make_predict_function"
 "_make_test_function" "_make_train_function" "_node_key" "_set_inputs"
 "_standardize_user_data" "_updated_config" "_uses_dynamic_learning_phase"
 "add_loss" "add_update" "add_weight" "assert_input_compatibility" "build"
 "call" "compile" "compute_mask" "compute_output_shape" "count_params"
 "evaluate" "evaluate_generator" "fit" "fit_generator" "from_config"
 "get_config" "get_input_at" "get_input_mask_at" "get_input_shape_at"
 "get_layer" "get_losses_for" "get_output_at" "get_output_mask_at"
 "get_output_shape_at" "get_updates_for" "get_weights" "load_weights" "predict"
 "predict_generator" "predict_on_batch" "reset_states" "run_internal_graph"
 "save" "save_weights" "set_weights" "summary" "test_on_batch" "to_json"
 "to_yaml" "train_on_batch")
```
Optionally, see [pymethod](#pymethod).

## `chain`
`(chain target &rest chain)`

The interface to python objects is nicer using `chain` (see below):
```lisp
(destructuring-bind (fig ax) (plt:subplots)
  (py4cl:chain ax (plot #(0 1 0 1)))
  (plt:show)) 
```

In python it is quite common to apply a chain of method calls, data
member access, and indexing operations to an object. To make this work
smoothly in Lisp, there is the `chain` macro (Thanks to @kat-co and
[[https://common-lisp.net/project/parenscript/reference.html][parenscript]] for the inspiration). This consists of a target object,
followed by a chain of operations to apply.  For example
```lisp
(py4cl:chain "hello {0}" (format "world") (capitalize)) ; => "Hello world"
```
which is converted to python `return "hello {0}".format("world").capitalize()`.

The only things which are treated specially by this macro are lists
and symbols at the top level. The first element of lists are treated as
python method names, top-level symbols are treated as data
members. Everything else is evaluated as lisp before being converted
to a python value.

If the first argument is a list, then it is assumed to be a python
function to be called; otherwise it is evaluated before converting to
a python value. For example
```lisp
(py4cl:chain (slice 3) stop) ; => 3
```

is converted to the python `return slice(3).stop`.

Symbols as first argument, or arguments to python methods, are
evaluated, so the following works:
```lisp
(let ((format-str "hello {0}")
      (argument "world"))
 (py4cl:chain format-str (format argument))) ; => "hello world"
```

Arguments to methods are lisp, since only the top level forms in `chain` are treated specially:
```lisp
(py4cl:chain "result: {0}" (format (+ 1 2))) ; => "result: 3"
```

Indexing with `[]` brackets is commonly used in python, which calls the `__getitem__` method.
This method can be called like any other method

```lisp
(py4cl:chain "hello" (__getitem__ 4)) ; => "o"
```

but since this is a common method an alias `[]` is supported:
```lisp
(py4cl:chain "hello" ([] 4)) ; => "o"
```

which is converted to the python `return "hello"[4]`.

For simple cases where the index is a value like a number or string
(not a symbol or a list), the brackets can be omitted:
```lisp
(py4cl:chain "hello" 4) ; => "o"
```

Slicing can be done by calling the python `slice` function:
```lisp
(py4cl:chain "hello" ([] (py4cl:python-call "slice" 2 4)))  ; => "ll"
```

which could be imported as a lisp function (see below):
```lisp
(py4cl:import-function "slice")
(py4cl:chain "hello" ([] (slice 2 4))) ; => "ll"
```

This of course also works with multidimensional arrays:
```lisp
(py4cl:chain #2A((1 2 3) (4 5 6))  ([] 1 (slice 0 2)))  ;=> #(4 5)
```

Sometimes the python functions or methods may contain upper case
characters; class names often start with a capital letter. All symbols
are converted to lower case, but the case can be controlled by passing
a string rather than a symbol as the first element:
```lisp
;; Define a class
(py4cl:python-exec
   "class TestClass:
      def doThing(self, value = 42):
        return value")

;; Create an object and call the method
(py4cl:chain ("TestClass") ("doThing" :value 31))  ; => 31
```
Note that the keyword is converted, converting to lower case.

Further, there is also `(setf chain)`:

```lisp
(py4cl:remote-objects*
  (let ((array (np:zeros '(2 2))))
    (setf (py4cl:chain array ([] 0 1)) 1.0
          (py4cl:chain array ([] 1 0)) -1.0)
    array)) 
; => #2A((0.0 1.0)
;        (-1.0 0.0))
```

Note that this modifies the value in python, so the above example only
works because =array= is a handle to a python object, rather than an
array which is stored in lisp. The following therefore does not work:
```lisp
(let ((array (np:zeros '(2 2))))
  (setf (py4cl:chain array ([] 0 1)) 1.0
        (py4cl:chain array ([] 1 0)) -1.0)
  array)
; => #2A((0.0 0.0)
;        (0.0 0.0))
```

## `remote-objects(*)`
`(remote-objects &body body)

If a sequence of python functions and methods are being used to manipulate data,
then data may be passed between python and lisp. This is fine for small amounts
of data, but inefficient for large datasets.

The `remote-objects` and `remote-objects*` macros provide `unwind-protect` environments
in which all python functions return handles rather than values to lisp. This enables
python functions to be combined without transferring much data.

The difference between these macros is `remote-objects` returns a handle, but
`remote-objects*` evaluates the result, and so will return a value if possible.

```lisp
(py4cl:remote-objects (py4cl:python-eval "1+2")) ; => #S(PY4CL::PYTHON-OBJECT :TYPE "<class 'int'>" :HANDLE 0)
```

```lisp
(py4cl:remote-objects* (py4cl:python-eval "1+2")) ; => 3
```

The advantage comes when dealing with large arrays or other datasets:
```lisp
(time (np:sum (np:arange 1000000)))
; => 3.672 seconds of real time
;    390,958,896 bytes consed
```

```lisp
(time (py4cl:remote-objects* (np:sum (np:arange 1000000))))
; => 0.025 seconds of real time
;    32,544 bytes consed
```

`remote-objects*` returns the result.

Besides this, (see [Setting up](#setting-up).

## `python-getattr`
`(python-getattr object slot-name)`

Lisp structs and class objects can be passed to python, put into data structures and
returned:

```lisp
(py4cl:import-function "dict") ; Makes python dictionaries

(defstruct test-struct 
    x y)

(let ((map (dict :key (make-test-struct :x 1 :y 2))))  ; Make a dictionary, return as hash-map
  ;; Get the struct from the hash-map, and get the Y slot
  (test-struct-y
    (py4cl:chain map "key")))  ; => 2
```


In python this is handled using an object of class `UnknownLispObject`, which
contains a handle. The lisp object is stored in a hash map
`*lisp-objects*`. When the python object is deleted, a message is sent to remove
the object from the hash map.

To enable python to access slots, or call methods on a struct or class, a
handler function needs to be registered. This is done by providing a method 
for generic function `python-getattr`. This function will be called when a
python function attempts to access attributes of an object (`__getattr__`
method).

```lisp
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
```
Inheritance then works as usual with CLOS methods:
```lisp
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
```

# Testing 

Tests use [clunit](https://github.com/tgutu/clunit), and run on [Travis](https://travis-ci.org/) using [cl-travis](https://github.com/luismbo/cl-travis). Most development
is done under Arch linux with SBCL and Python3. To run the tests
yourself:
```lisp
(asdf:test-system :py4cl)
```
or

```lisp
(ql:quickload :py4cl-tests)
(py4cl-tests:run)
```




# Type Mapping and Pythonize

Data is passed between python and lisp as text. The python function
`lispify` converts values to a form which can be read by the lisp
reader; the lisp function `pythonize` outputs strings which can be
`eval`'d in python. The following type conversions are done:


```
| Lisp type | Python type   |
|-----------+---------------|
| NIL       | None          |
| integer   | int           |
| ratio     | float         |
| real      | float         |
| complex   | complex float |
| string    | str           |
| hash map  | dict          |
| list      | tuple         |
| vector    | list          |
| array     | NumPy array   |
| symbol    | Symbol class  |
| function  | function      |
```

Note that python does not have all the numerical types which lisp has,
for example rational numbers or complex integers.

Because `pyeval` and `pyexec` evaluate strings as python
expressions, strings passed to them are not escaped or converted as
other types are. To pass a string to python as an argument, call `py4cl::pythonize`

```lisp
CL-USER> (py4cl::pythonize "string")
"\"string\""
CL-USER> (py4cl::pythonize #'identity)
"_py4cl_LispCallbackObject(1)"
CL-USER> (py4cl::pythonize 3.0)
"3.0"
CL-USER> (py4cl::pythonize (model)) ;; keras.Model
"_py4cl_objects[1918]"
```

If python objects cannot be converted into a lisp value, then they are
stored and a handle is returned to lisp. This handle can be used to
manipulate the object, and when it is garbage collected the python
object is also deleted (using the [trivial-garbage](https://common-lisp.net/project/trivial-garbage/) 
package).

# Name Mapping

`defpyfun` takes care to lispify argument names unless some argument has a capital letter, or there exists a keyword argument; in these cases, `(&rest args)`  is used as a parameter list. Now, `defpyfun` relies on `pycall`.

The arguments passed by `pycall` are parsed by the python process: the lisp keywords are converted to their python equivalents. This only entails downcasing the symbol-name of the keywords and replacing hyphens with underscores. Thus, if the function had arguments with capital letters, (currently) it is not possible to pass arguments to such a function as keyword arguments.

```lisp
CL-USER> (py4cl:pyexec "
def foo(A, b):
  return True")
NIL
CL-USER> (defpyfun "foo")
NIL
CL-USER> (foo :a 4 :b 3)
; Evaluation aborted on #<PYERROR {100E2AF473}>.
;; unexpected keyword argument 'a'
CL-USER> (foo 4 3)
T
```

In essence, pythonization of names means: downcasing the symbol names, and replacing the hyphens with underscores. For `fun-names` this is handled in callpython.lisp. For argument names, downcasing is done by `pythonize` on symbols, and hyphen to underscore replacement in done in python process.

Lispification of python names is conditional, in that: if the name is a function name or module name, that is to be interned, then `CamelCase` and `joint_words` are converted to `camel-case` and `joint-words` For argument names, this is only done, if they do not have capital letters. This process is handled in import-export.lisp.




# What remains?

See [TODO].


---


# Also check out

## [The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/)

---

This template was taken from [The Common Lisp Cookbook][tCLC].

[tCLC]: https://github.com/LispCookbook/cl-cookbook
[pyeval]: #expressions-pyeval-rest-args
[limitations]: #limitations-of-this-documentation
[pycall]: #pycall
[TODO]: https://github.com/digikar99/py4cl/blob/master/TODO.org
[Name Mapping]: #name-mapping
