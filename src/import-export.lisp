;;; Functions and macros for importing and exporting symbols to python

;;;; Things we need to achieve - in case someone wants to attempt refactorisation
;;; For defpyfun:
;;;   - For convenience, we need to be able to show the function's arguments and
;;;   default values in Slime.


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

(defun get-unique-symbol (symbol-name package-name)
  (multiple-value-bind (symbol location)
      (intern symbol-name package-name)
    (if location
	(intern (concatenate 'string
			     symbol-name "/1")
		package-name)
	symbol)))

(defun get-arg-list (fun-name)
  (let* ((signature-dict
          (pyeval "dict(inspect.signature(" fun-name ").parameters)")))
    (iter (initially (remhash "kwargs" signature-dict)
		     (remhash "args" signature-dict))
		 (for (key val) in-hashtable signature-dict)
		 (collect (list (pyeval val ".name")
				(pyeval val ".default"))))))

;; In essence, this macro should give the full power of the
;;   "from modulename import function as func"
;; to the user.

;; "from keras.layers import Input" creates only "Input" and not
;; "keras.layers.Input" in python;
;; However, this leaves open the chance of a name conflict
;; - what if two python modules have the same name?
;; import-module takes care of this, along with keeping minimal work
;; in defpymodule

(defmacro defpyfun (fun-name pymodule-name
                    &key
                      (as fun-name)
                      (import-module nil) ; see above
                      (lisp-fun-name (lispify-name as))
                      (lisp-package *package*)
                      (called-from-defpymodule nil))
  "Defines a function which calls python
Example
  (py4cl:pyexec \"import math\")
  (py4cl:defpyfun \"math.sqrt\")
  (math.sqrt 42) -> 6.4807405

Keywords:
  LISP-FUN-NAME is a string, denoting the symbol to which the function is assigned.
  DOCSTRING is a string which becomes the function docstring
  FROM specifies a pymodule to load the function from. This will cause the python
    pymodule to be imported into the python session.
"
  (check-type fun-name string)
  (check-type lisp-fun-name string)
  (check-type lisp-package package)
  (check-type pymodule-name string)
  ;; (check-type as string) ;; (or nil string)?
  (python-start-if-not-alive)
  (unless called-from-defpymodule
    (pyexec "import inspect")
    (if import-module
        (pyexec "import " pymodule-name)
        (pyexec "from " pymodule-name " import " fun-name " as " as)))
  (let* ((fullname (if (or import-module called-from-defpymodule)
                       (concatenate 'string pymodule-name "." fun-name)
                       fun-name))
         (fun-doc (pyeval fullname ".__doc__"))
         (callable-type
          (cond ((pyeval "inspect.isfunction(" fullname ")") 'function)
                ((pyeval "inspect.isclass(" fullname ")") 'class)
                (t t)))
         (fun-symbol (ecase callable-type
                       (class (intern (concatenate 'string
                                                       lisp-fun-name "/CLASS")
                                      lisp-package))
                       (function (intern lisp-fun-name lisp-package))
                       (t (get-unique-symbol lisp-fun-name lisp-package))))
         ;; later, specialize further
         (fun-args-with-defaults
          (mapcar-> (get-arg-list fullname)
                    (lambda (str-val)
                      (list (intern (lispify-name (first str-val)) lisp-package)
                            (if (listp (second str-val))
				`',(second str-val)
				;; to avoid being interpreted as a function
				;; are there other cases this misses out?
				(second str-val))))))
         (parameter-list (if fun-args-with-defaults
                             (cons '&key
                                   fun-args-with-defaults)
                             '(&rest args)))
         (pass-list (mapcar #'car fun-args-with-defaults)))
    `(progn
       (defun ,fun-symbol (,@parameter-list)
              ,(or fun-doc "Python function")
              ,(if fun-args-with-defaults
                   `(funcall #'pycall ,fullname ,@pass-list)
                   `(apply #'pycall ,fullname args)))
       (export ',fun-symbol ,lisp-package))))


(defmacro defpysubmodules (pymodule-name as)
  (let ((submodules
         (py4cl:pyeval "[(modname, ispkg) for importer, modname, ispkg in "
                       "pkgutil.iter_modules("
                       (or as pymodule-name)
                       ".__path__)]")))
    (iter (for (submodule has-submodules) in-vector submodules)
          (collect `(defpymodule ,(concatenate 'string
                                               pymodule-name "." submodule)
                        ,has-submodules
                      :as ,(when as (concatenate 'string as "." submodule))
                      :is-submodule t)))))

(defun mapcar-> (list &rest functions)
  "Applies FUNCTIONS successively to LIST."
  (if (null (car functions))
      list
      (apply #'mapcar-> (mapcar (car functions) list) (cdr functions))))

(defmacro defpymodule (pymodule-name has-submodules
                       &key
                         (is-submodule nil) ;; used by defpysubmodules
                         as
                         (lisp-package (lispify-name (or as pymodule-name)))
                         (reload nil))
  "Import a python module (and its submodules) lisp-package Lisp package(s). 
  Example:
    (py4cl:defpymodule \"math\" :lisp-package \"m\")
    (m:sqrt 4)   ; => 2.0
\"Package already exists.\" is returned if the package exists and :RELOAD 
is NIL."
  (check-type pymodule-name string) ; is there a way to (declaim (macrotype ...?
  ;; (check-type as (or nil string)) ;; this doesn't work!
  (check-type lisp-package string)
  (let ((package (find-package lisp-package))) ;; reload
    (if package
        (if reload 
            (delete-package package)
            (return-from defpymodule "Package already exists."))))
  
  (python-start-if-not-alive) ; Ensure python is running
  (unless is-submodule
    (if as
        (pyexec "import " pymodule-name " as " as)
        (pyexec "import " pymodule-name)))

  (pyexec "import inspect")
  (pyexec "import pkgutil")
  
  ;; fn-names  All callables whose names don't start with "_" 
  (let ((fun-names (pyeval "[name for name, fn in inspect.getmembers("
                           (or as pymodule-name)
                           ", callable) if name[0] != '_']"))
        ;; Get the package name by passing through reader, rather than using STRING-UPCASE
        ;; so that the result reflects changes to the readtable
        ;; Note that the package doesn't use CL to avoid shadowing
        (exporting-package (make-package lisp-package :use '())))
    (import '(cl:nil)) ; So that missing docstring is handled
    (append '(progn)
            (if has-submodules (macroexpand `(defpysubmodules ,pymodule-name ,as)))
            (iter (for fun-name in-vector fun-names)
                  (collect (macroexpand `(defpyfun
					     ,fun-name ,(or as pymodule-name)
					   :lisp-package ,exporting-package
					   :called-from-defpymodule t))))
	    (unless is-submodule
	      ;; Several symbols are introduced "somewhere" that are not functions
	      `((cl:mapc #'unintern (apropos-list,lisp-package))
		t))))) 

(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in python process as PYTHON-NAME"
  (pyexec (concatenate 'string
                       python-name
                       "=_py4cl_LispCallbackObject("
                       (write-to-string
                        (object-handle function))
                       ")")))
