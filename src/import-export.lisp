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

(defmacro import-function (fun-name module-name
                           &key (as (lispify-name fun-name))
                             (package *package*))
  "Defines a function which calls python
Example
  (py4cl:python-exec \"import math\")
  (py4cl:import-function \"math.sqrt\")
  (math.sqrt 42) -> 6.4807405

Keywords:
  AS is a string, denoting the symbol to which the function is assigned.
  DOCSTRING is a string which becomes the function docstring
  FROM specifies a module to load the function from. This will cause the python
    module to be imported into the python session.
"
  (check-type fun-name string)
  (check-type as string)
  (check-type package package)
  (when module-name
    (python-start-if-not-alive)
    (check-type module-name string)   
    (python-exec "from " module-name " import " fun-name))
  
  (let ((fun-symbol (intern as package))
        (fullname (concatenate 'string module-name "." fun-name))
        (fun-doc (python-eval module-name "." fun-name ".__doc__")))
    (if (member
           (slot-value (python-eval fullname) 'type)
           '("<class 'function'>") ;;  "<class 'builtin_function_or_method'>"
           :test 'string=)
          (let* ((fun-args ;; includes all the local variables
                  (mapcar-> (python-eval fullname ".__code__.co_varnames")
                            #'lispify-name
                            #'intern))
                 (fun-argcount ;; to exclude the local variables
                  (python-eval fullname ".__code__.co_argcount"))
                 (arg-list (subseq fun-args 0 fun-argcount)))
            `(progn
               (defun ,fun-symbol (&key ,@arg-list)
                 ,(or fun-doc "Python function")
                 (funcall #'python-call ,fun-name ,@arg-list))
               (export ',fun-symbol ,package)))
          `(progn
             (defun ,fun-symbol (&rest args)
                    ,(or fun-doc "Python function")
                    (apply #'python-call ,fun-name args))
             (export ',fun-symbol ,package)))))


(defmacro import-submodules (module-name)
    (let ((submodules
           (py4cl:python-eval "[(modname, ispkg) for importer, modname, ispkg in "
                              "pkgutil.iter_modules("
                              module-name
                              ".__path__)]")))
      (iter (for (submodule has-submodules) in-vector submodules)
            (collect `(import-module ,(concatenate 'string
                                                   module-name "." submodule)
                                     ,has-submodules)))))



(defun mapcar-> (list &rest functions)
  "Applies FUNCTIONS successively to LIST."
  (if (null (car functions))
      list
      (apply #'mapcar-> (mapcar (car functions) list) (cdr functions))))

(defmacro import-module (module-name has-submodules
                         &key (as (lispify-name module-name))
                           (reload nil))
  "Import a python module (and its submodules) as Lisp package(s). 
  Example:
    (py4cl:import-module \"math\" :as \"m\")
    (m:sqrt 4)   ; => 2.0
\"Package already exists.\" is returned if the package exists and :RELOAD 
is NIL."
  (check-type module-name string) ; is there a way to (declaim (macrotype ...?
  (check-type as string)          

  (let ((package-sym (read-from-string as))) ;; reload
    (if (find-package package-sym)
        (if reload 
            (delete-package package-sym)
            (return-from import-module "Package already exists."))))
  
  (python-start-if-not-alive) ; Ensure that python is running

  (python-exec "import " module-name) ; Import the required module in python

  (python-exec "import inspect")
  (python-exec "import pkgutil")
  
  ;; fn-names  All callables whose names don't start with "_" 
  (let ((fun-names (python-eval "[name for name, fn in inspect.getmembers("
                                 module-name
                                 ", callable) if name[0] != '_']"))
        ;; Get the package name by passing through reader, rather than using STRING-UPCASE
        ;; so that the result reflects changes to the readtable
        ;; Setting *package* causes symbols to be interned by READ-FROM-STRING in this package
        ;; Note that the package doesn't use CL to avoid shadowing
        (exporting-package (make-package as :use '())))
    ;; (format t "Package created!~%")
    (import '(cl:nil)) ; So that missing docstring is handled
    (append '(progn)
            (if has-submodules (macroexpand `(import-submodules ,module-name)))
            ;; (format t "Submodules imported!~%")
            (iter (for fun-name in-vector fun-names)
                  (collect (macroexpand `(import-function
                                          ,fun-name ,module-name
                                          :package ,exporting-package)))))))



(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in python process as PYTHON-NAME"
  (python-exec (concatenate 'string
                            python-name
                            "=_py4cl_LispCallbackObject("
                            (write-to-string
                             (object-handle function))
                            ")")))
