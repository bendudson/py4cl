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

(defmacro import-function (fun-name &key docstring
                                      (arg-list '(&rest args))
                                      (as (read-from-string fun-name))
                                      from)
  "Define a function which calls python
Example
  (py4cl:python-exec \"import math\")
  (py4cl:import-function \"math.sqrt\")
  (math.sqrt 42)
  -> 6.4807405

Keywords:

AS specifies the symbol to be used in Lisp. This can be a symbol
or a string. If a string is given then it is read using READ-FROM-STRING.

DOCSTRING is a string which becomes the function docstring

FROM specifies a module to load the function from. This will cause the python
module to be imported into the python session.
"
  ;; Note: a string input is used, since python is case sensitive
  (unless (typep fun-name 'string)
    (error "Argument to IMPORT-FUNCTION must be a string"))
  
  (if from
      (progn
        ;; Ensure that python is running
        (python-start-if-not-alive)
        ;; import the function into python
        (python-exec "from " (string from) " import " fun-name)))
  
  ;; Input AS specifies the Lisp symbol, either as a string or a symbol
  (let ((fun-symbol (typecase as
                      (string (read-from-string as))
                      (symbol as)
                      (t (error "AS keyword must be string or symbol")))))
    
    `(defun ,fun-symbol (&key ,@arg-list)
       ,(or docstring "Python function")
       (funcall #'python-call ,fun-name ,@arg-list))))
;; (apply #'python-call ,fun-name
;;               ,@`(iter (for arg in arg-list)
;;                        (for val in ,arg-list)
;;                        (appending `((intern ,arg 'keyword)
;;                                     (,arg)))))

(defmacro import-module (module-name &key (as module-name as-supplied-p) (reload nil))
  "Import a python module as a Lisp package. The module name should be
a string.

Example:
  (py4cl:import-module \"math\")
  (math:sqrt 4)   ; => 2.0

or using 
Keywords:
AS specifies the name to be used for both the Lisp package and python module.
   It should be a string, and if not supplied then the module name is used.

RELOAD specifies that the package should be deleted and reloaded.
       By default if the package already exists then a string is returned.
"
  (unless (typep module-name 'string)
    (error "Argument to IMPORT-MODULE must be a string"))
  (unless (typep as 'string)
    (error "Keyword argument AS to IMPORT-MODULE must be a string"))

  ;; Check if the package already exists, and delete if reload is true
  ;; This is so that it is reloaded into python
  (let ((package-sym (read-from-string as)))
    (if (find-package package-sym)
        (if reload 
            (delete-package package-sym)
            (return-from import-module "Package already exists."))))
  
  ;; Ensure that python is running
  (python-start-if-not-alive)

  ;; Import the required module in python
  (if as-supplied-p
      (python-exec (concatenate 'string
                                "import " module-name " as " as))
      (python-exec (concatenate 'string
                                "import " module-name)))

  ;; Also need to import the "inspect" module
  (python-exec "import inspect")

  ;; fn-names  All callables whose names don't start with "_"
  (let ((fn-names (python-eval (concatenate 'string
                                            "[name for name, fn in inspect.getmembers("
                                            as
                                            ", callable) if name[0] != '_']")))
        ;; Get the package name by passing through reader, rather than using STRING-UPCASE
        ;; so that the result reflects changes to the readtable
        ;; Setting *package* causes symbols to be interned by READ-FROM-STRING in this package
        ;; Note that the package doesn't use CL to avoid shadowing
        (*package* (make-package (string (read-from-string as))
                                 :use '())))
    (import '(cl:nil)) ; So that missing docstring is handled
    (append '(progn)
            (iter (for fn-name in-vector fn-names)
                  (for fn-symbol = (read-from-string (lispify-name fn-name)))
                  (for fullname = (concatenate 'string as "." fn-name)) ; Include module prefix
                  (format t "Considering ~D~%" fullname)
                  (when (member
                         (slot-value (python-eval fullname) 'type)
                         '("<class 'function'>") ;;  "<class 'builtin_function_or_method'>"
                         :test 'string=)
                    (let ((fn-args
                             (mapcar #'intern
                                     (mapcar #'lispify-name
                                             (python-eval
                                              (concatenate 'string
                                                           fullname ".__code__.co_varnames")))))
                          (fn-doc (python-eval
                                   (concatenate 'string
                                                as "." fn-name ".__doc__")))
                          (fn-argcount 
                           (python-eval
                            (concatenate 'string
                                         as "." fn-name ".__code__.co_argcount"))))
                      (format t "Exporting ~d~%" fn-symbol)
                      (appending `((import-function ,fullname
                                                    :as ,fn-symbol
                                                    :arg-list ,(subseq fn-args 0 fn-argcount)
                                                    :docstring ,fn-doc)
                                   (export ',fn-symbol ,*package*)))))))))

(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in python process as PYTHON-NAME"
  (python-exec (concatenate 'string
                            python-name
                            "=_py4cl_LispCallbackObject("
                            (write-to-string
                             (object-handle function))
                            ")")))
