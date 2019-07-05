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

Please report the issues on [github](https://github.com/bendudson/cl-digikar-utilities/issues).

(Shorter) documentation for almost all of these functions is available as docstrings as well.

# Limitations of this Documentation

- `pyerror` and [some more useful functions and macros](#some-more-useful-functions-and-macros) has not been documented here yet.
- No mention about using multiple python processes.
- How useful / redundant is `pysetf`, `chain`? ([Some more functions](#some-more-functions-undocumented))
- How does one use `remote-objects` or `remote-objects*`?

# Highlights and Limitations of `py4cl`

- Use python functions and modules from lisp. See [Defining python functions and modules](#defining-python-functions-and-modules).
- [`pycmd`](#pycmd): Choose which python binary to use.
- About 7500 `(pycall "int" "5")` instructions per second @ 1GHz intel 8750H. 
But this shouldn't be a bottleneck if you're planning to run "long" processes in python. (For example, deep learning :).)
- Multiple python processes (not documented here) - parallel execution?
- stderr is not returned; though errors are returned
- portability status (across CL implementations, linux distributions as well as operating systems) of this fork is not known. (It is known to work on 18.04 Ubuntu 64-bit SBCL.)
- lack of integration with CL's methods and CLOS
- pyslot-list and pymethod-list are unoptimized for use; currently, intended for use on REPL that en-masse
- large arrays (100M in 2 sec!) can be transferred using numpy-file-format (see [initialize](#initialize); large strings cannot be
- See [TODO].

--- 

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

- pkgutil

(other packages should be  available in a standard python distribution - tested with CPython.)

## Installation

Clone this repository into `~/quicklisp/local-projects/` or other
location where it can be found by ASDF:
```sh
git clone https://github.com/digikar99/py4cl.git
```

Original version by [bendudson](https://github.com/bendudson/cl-digikar-utilities/issues) can be found at: 
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

Complementary to `config-var` are `save-config` and `load-config`. The latter is called on startup, the config-file exists. `(setf config-var)` calls the former, as well as asks the python process to load the config, from the config file.

---


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

Also see [config-var](##config--config-var).

### `pyversion-info`
```lisp
CL-USER> (pyversion-info)
(3 7 3 "final" 0)
```

### `pyinterrupt`
`(pyinterrupt &optional process)`

A simple `C-c C-c` only interrupts the lisp process from slime - the python process keeps running. `(pyinterrupt)` can be used in these cases to send a SIGINT (2) to the python process.

### py-cd
`(py-cd path)`

Equivalent of `slime-cd`, since python is a separate process.

### Other useful functions

The documentation for these can be viewed by the usual `(describe 'symbol-name)`.

#### `pystart`
#### `pystop`
#### `python-alive-p`
#### `python-start-if-not-alive`


## Doing arbitrary things in python

Unlike lisp, python (and most other languages) make a distinction between *statements* and *expressions*: see [Quora](https://www.quora.com/Whats-the-difference-between-a-statement-and-an-expression-in-Python-Why-is-print-%E2%80%98hi%E2%80%99-a-statement-while-other-functions-are-expressions) or [stackoverflow](https://stackoverflow.com/questions/4728073/what-is-the-difference-between-an-expression-and-a-statement-in-python).

A general rule of thumb from there is: if you can print it, or assign it to a variable, then it's an expression, otherwise it's a statement.

Both `pyeval` and `pyexec` take any type of arguments. The `arg` is `pythonize`d if the `arg` is not a `string`, or it is a `string` that can be read into a `real`.

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
Check the link to see the work-around.

See [Doing arbitrary things in python](#doing-arbitrary-things-in-python) to learn about `pyeval` and `pyexec`.

## Defining python functions and modules

Rather, we define functions that call python functions.

Names are lispified by converting underscores hyphens, and converting CamelCase to camel-case. 

### `defpyfun`
```lisp
(defpyfun fun-name pymodule-name &key (as fun-name) 
  (import-module nil) (lisp-fun-name (lispify-name as))
  (lisp-package *package*) (called-from-defpymodule nil))
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

### `defpymodule`
```lisp
(defpymodule pymodule-name has-submodules &key as
  (lisp-package (lispify-name (or as pymodule-name)))
  (reload nil))
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

Note that `fun-name` can be a string, a function, or a [callable] python-object. See the example in [defpymodule](#defpymodule).

Another two variants of `pycall` are `pycall-async` (that is undocumented here) and [pycall-monitor](#pycall-monitor--pymethod-monitor).

### `pymethod`
`(pymethod obj method-name &rest args)`

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

CL-USER> (pycall-monitor "foo" ())
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

## Some more functions (Undocumented)

### chain

### pysetf

### remote-objects 

### remote-objects*

### export-function

### defpyfuns

### pyhelp

### python-getattr


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


---

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
