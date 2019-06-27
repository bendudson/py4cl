;;; Functions and macros for importing and exporting symbols to python

(in-package :py4cl)

(declaim (ftype (function (string) string) lispify-name))
(defun lispify-name (name)
  "Converts NAME to a lisp-like name. Specifically:
  1. Replaces underscores with hyphens.
  2. CamelCase is converted to CAMEL-CASE"
  (iter (for ch in-string name)
        (collect (cond ((and (upper-case-p ch) (not (first-iteration-p))) #\-)
                       ((char= ch #\_) #\-)
                       (t ch))
          into out-string 
          result-type string)
        (when (and (upper-case-p ch) (not (first-iteration-p))) 
          (collect (char-downcase ch) into out-string result-type string))
        (finally (return (string-upcase out-string)))))

(defmacro defpyfun (fun-name pymodule-name
                           &key (lisp-function (lispify-name fun-name))
                             (lisp-package *package*))
  "Defines a function which calls python
Example
  (py4cl:pyexec \"import math\")
  (py4cl:defpyfun \"math.sqrt\")
  (math.sqrt 42) -> 6.4807405

Keywords:
  LISP-FUNCTION is a string, denoting the symbol to which the function is assigned.
  DOCSTRING is a string which becomes the function docstring
  FROM specifies a pymodule to load the function from. This will cause the python
    pymodule to be imported into the python session.
"
  (check-type fun-name string)
  (check-type lisp-function string)
  (check-type lisp-package package)
  (when pymodule-name
    (python-start-if-not-alive)
    (check-type pymodule-name string)   
    (pyexec "from " pymodule-name " import " fun-name))
  
  (let ((fun-symbol (intern lisp-function lisp-package))
        (fullname (concatenate 'string pymodule-name "." fun-name))
        (fun-doc (pyeval pymodule-name "." fun-name ".__doc__")))
    (if (member
           (slot-value (pyeval fullname) 'type)
           '("<class 'function'>") ;;  "<class 'builtin_function_or_method'>"
           :test 'string=)
          (let* ((fun-args ;; includes all the local variables
                  (mapcar-> (pyeval fullname ".__code__.co_varnames")
                            #'lispify-name
                            #'intern))
                 (fun-argcount ;; to exclude the local variables
                  (pyeval fullname ".__code__.co_argcount"))
                 (arg-list (subseq fun-args 0 fun-argcount)))
            `(progn
               (defun ,fun-symbol (&key ,@arg-list)
                 ,(or fun-doc "Python function")
                 (funcall #'pycall ,fun-name ,@arg-list))
               (export ',fun-symbol ,lisp-package)))
          `(progn
             (defun ,fun-symbol (&rest args)
                    ,(or fun-doc "Python function")
                    (apply #'pycall ,fun-name args))
             (export ',fun-symbol ,lisp-package)))))


(defmacro defpysubmodules (pymodule-name)
    (let ((submodules
           (py4cl:pyeval "[(modname, ispkg) for importer, modname, ispkg in "
                              "pkgutil.iter_modules("
                              pymodule-name
                              ".__path__)]")))
      (iter (for (submodule has-submodules) in-vector submodules)
            (collect `(defpymodule ,(concatenate 'string
                                                   pymodule-name "." submodule)
                                     ,has-submodules)))))



(defun mapcar-> (list &rest functions)
  "Applies FUNCTIONS successively to LIST."
  (if (null (car functions))
      list
      (apply #'mapcar-> (mapcar (car functions) list) (cdr functions))))

(defmacro defpymodule (pymodule-name has-submodules
                       &key (lisp-package (lispify-name pymodule-name))
                         (reload nil))
  "Import a python module (and its submodules) lisp-package Lisp package(s). 
  Example:
    (py4cl:defpymodule \"math\" :lisp-package \"m\")
    (m:sqrt 4)   ; => 2.0
\"Package already exists.\" is returned if the package exists and :RELOAD 
is NIL."
  (check-type pymodule-name string) ; is there a way to (declaim (macrotype ...?
  (check-type lisp-package string)          

  (let ((package-sym (read-from-string lisp-package))) ;; reload
    (if (find-package package-sym)
        (if reload 
            (delete-package package-sym)
            (return-from defpymodule "Package already exists."))))
  
  (python-start-if-not-alive) ; Ensure that python is running

  (pyexec "import " pymodule-name) ; Import the required module in python

  (pyexec "import inspect")
  (pyexec "import pkgutil")
  
  ;; fn-names  All callables whose names don't start with "_" 
  (let ((fun-names (pyeval "[name for name, fn in inspect.getmembers("
                                 pymodule-name
                                 ", callable) if name[0] != '_']"))
        ;; Get the package name by passing through reader, rather than using STRING-UPCASE
        ;; so that the result reflects changes to the readtable
        ;; Setting *package* causes symbols to be interned by READ-FROM-STRING in this package
        ;; Note that the package doesn't use CL to avoid shadowing
        (exporting-package (make-package lisp-package :use '())))
    ;; (format t "Package created!~%")
    (import '(cl:nil)) ; So that missing docstring is handled
    (append '(progn)
            (if has-submodules (macroexpand `(defpysubmodules ,pymodule-name)))
            ;; (format t "Submodules imported!~%")
            (iter (for fun-name in-vector fun-names)
                  (collect (macroexpand `(defpyfun
                                          ,fun-name ,pymodule-name
                                          :lisp-package ,exporting-package)))))))



(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in python process as PYTHON-NAME"
  (pyexec (concatenate 'string
                            python-name
                            "=_py4cl_LispCallbackObject("
                            (write-to-string
                             (object-handle function))
                            ")")))
