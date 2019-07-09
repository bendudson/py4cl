;;; Functions and macros for importing and exporting symbols to python

;;;; Things we need to achieve - in case someone wants to attempt refactorisation
;;; For defpyfun:
;;;   - For convenience, we need to be able to show the function's arguments and
;;;   default values in Slime.
;;;   - For customizability, we ought to be able to load some "config" file
;;;   containing name, signature, documentation, call method for some functions.
;;;   This latter hasn't been attempted yet.


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
    (declare (ignore symbol))
    (if location
        (concatenate 'string
                     symbol-name "/1")
        symbol-name)))


;; possible due to python!
;; https://stackoverflow.com/questions/2677185/how-can-i-read-a-functions-signature-including-default-argument-values
(defun get-arg-list (fullname lisp-package)
  "Returns a list of two lists: PARAMETER-LIST and PASS_LIST"
  (let* ((signature (ignore-errors (pyeval "inspect.signature(" fullname ")")))
         ;; errors could be value error or type error
         (pos-only (find #\/ (pycall 'str signature)))
         ;; we are ignoring futther keyword args
         (sig-dict (if signature
                       (pyeval "dict(" signature ".parameters)")
                       (make-hash-table)))
         (default-return (list '(&rest args)
                               `(apply #'pycall ,fullname args))))
    (iter (for (key val) in-hashtable sig-dict)
          (for name = (pyeval val ".name"))
          (for default = (pyeval val ".default"))
          (when (or (some #'upper-case-p name)
                    (typep default 'python-object)
                    (find #\* (pyeval "str(" val ".default)")))
            (return-from get-arg-list default-return))
          (collect (list (intern (lispify-name name) lisp-package)
                         (if (or (symbolp default) (listp default))
                             `',default
                             default))
            into parameter-list)
          (collect (if pos-only
                       (intern (lispify-name name) lisp-package)
                       (list (intern (lispify-name name) :keyword)
                             (intern (lispify-name name) lisp-package)))
            into pass-list)
          (finally 
           (return-from get-arg-list
             (cond ((null pass-list)  default-return)
                   (pos-only (list `(&optional ,@parameter-list)
                                   `(pycall ,fullname ,@pass-list)))
                   (t (list `(&key ,@parameter-list)
                            `(pycall ,fullname ,@(apply #'append pass-list))))))))))

(defun pymethod-list (python-object &key (as-vector nil))
  (pyexec "import inspect")
  (let ((method-vector (pyeval "[name for name, ele in inspect.getmembers("
                               python-object ", callable)]")))
    (if as-vector method-vector (coerce method-vector 'list))))

(defun pyslot-list (python-object &key (as-vector nil))
  (pyexec "import inspect")
  (pyexec "
def _py4cl_non_callable(ele):
  import inspect
  return not(inspect.isroutine(ele))")
  (let ((slot-vector
         (pyeval "[name for name, ele in inspect.getmembers("
                 python-object
                 ", _py4cl_non_callable)]")))
    (if as-vector slot-vector (coerce slot-vector 'list))))

(defun builtin-p (pymodule-name)
  "Some builtin functions like 'sum' do not take keyword args."
  (or (null pymodule-name)
      (string= "" pymodule-name)))

;; In essence, this macro should give the full power of the
;;   "from modulename import function as func"
;; to the user.

;; "from keras.layers import Input" creates only "Input" and not
;; "keras.layers.Input" in python;
;; However, this leaves open the chance of a name conflict
;; - what if two python modules have the same name?
;; defpymodule takes care of this, along with keeping minimal work
;; in defpyfun

(defmacro defpyfun (fun-name &optional pymodule-name
                    &key
                      (as fun-name)
                      (lisp-fun-name (lispify-name as))
                      (lisp-package *package*)
                      (called-from-defpymodule nil)
                      (rename-lisp-fun-name nil)
                      (safety t))
  "Defines a function which calls python
Example
  (py4cl:pyexec \"import math\")
  (py4cl:defpyfun \"math.sqrt\")
  (math.sqrt 42) -> 6.4807405

Arguments:
  FUN-NAME: name of the function in python, before import
  PYMODULE-NAME: name of the module containing FUN-NAME
  AS: name of the function in python, after import
  LISP-FUN-NAME: name of the lisp symbol to which the function is bound*
  LISP-PACKAGE: package (not its name) in which LISP-FUN-NAME will be interned
  SAFETY: if T, adds an additional line in the function asking to import the 
    package or function, so that the function works even after PYSTOP is called.
    However, this increases the overhead of stream communication, and therefore,
    can reduce speed."
  (check-type fun-name string)
  (check-type lisp-fun-name string)
  (check-type lisp-package package)
  ;; (check-type pymodule-name string) ;; (or nil string)
  ;; (check-type as string) ;; (or nil string)?
  (python-start-if-not-alive)
  (pyexec "import inspect")
  (unless (or called-from-defpymodule
              (builtin-p pymodule-name))
    (pyexec "from " pymodule-name " import " fun-name " as " as))
  (let* ((fullname (if called-from-defpymodule
                       (concatenate 'string pymodule-name "." fun-name)
                       (or as fun-name)))
         (fun-doc (pyeval fullname ".__doc__"))
         (callable-type
          (cond ((pyeval "inspect.isfunction(" fullname ")") 'function)
                ((pyeval "inspect.isclass(" fullname ")") 'class)
                (t t)))
         (fun-symbol (if (not rename-lisp-fun-name)
                         (intern lisp-fun-name lisp-package)
                         (ecase callable-type
                           (class (intern (if called-from-defpymodule
                                              (concatenate 'string
                                                           lisp-fun-name "/CLASS")
                                              lisp-fun-name)
                                          lisp-package))
                           (function (intern lisp-fun-name lisp-package))
                           (t (intern (get-unique-symbol lisp-fun-name lisp-package)
                                      lisp-package)))))) ;; later, specialize further
    (destructuring-bind (parameter-list pass-list) (get-arg-list fullname (find-package lisp-package))
      `(progn
         (defun ,fun-symbol (,@parameter-list)
           ,(or fun-doc "Python function")
           ,(when safety
              (if (builtin-p pymodule-name)
                  `(python-start-if-not-alive)
                  (if called-from-defpymodule
                      `(pyexec "import " ,pymodule-name)
                      `(pyexec "from " ,pymodule-name " import " ,fun-name " as " ,as))))
           ,pass-list)
         ,(when called-from-defpymodule `(export ',fun-symbol (find-package ,lisp-package)))))))


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

(defmacro defpymodule (pymodule-name &optional import-submodules
                       &key
                         (is-submodule nil) ;; used by defpysubmodules
                         as
                         (lisp-package (lispify-name (or as pymodule-name)))
                         (reload nil))
  "Import a python module (and its submodules) lisp-package Lisp package(s). 
Example:
  (py4cl:defpymodule \"math\" :lisp-package \"M\")
  (m:sqrt 4)   ; => 2.0
\"Package already exists.\" is returned if the package exists and :RELOAD 
is NIL.
Arguments:
  PYMODULE-NAME: name of the module in python, before importing
  IMPORT-SUBMODULES: leave nil for purposes of speed, if you won't use the  
    submodules
  IS-SUBMODULE: used by internal macro defpysubmodules
  AS: name of the module after importing in python
  LISP-PACKAGE: lisp package, in which to intern (and export) the callables
  RELOAD: whether to redefine and reimport"
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
        (exporting-package
	 (or (find-package lisp-package) (make-package lisp-package :use '()))))
    `(progn
       (python-start-if-not-alive) ; Ensure python is running at execution as well!
       ,(unless is-submodule
         (if as
             `(pyexec "import " ,pymodule-name " as " ,as)
             `(pyexec "import " ,pymodule-name)))
       ,(macroexpand `(defpackage ,lisp-package (:use)))
       ,@(if import-submodules (macroexpand `(defpysubmodules ,pymodule-name ,as)))
       ,@(iter (for fun-name in-vector fun-names)
               (collect (macroexpand `(defpyfun
                                          ,fun-name ,(or as pymodule-name)
                                        :lisp-package ,exporting-package
                                        :called-from-defpymodule t))))
       ;; ,(unless is-submodule
          ;; Several symbols are introduced "somewhere" that are not functions
          ;; `(cl:mapc #'unintern (apropos-list ,lisp-package)))
       
       t)))

(defmacro defpyfuns (&rest args)
  "Each ARG is supposed to be a 2-3 element list of 
 (pyfun-name pymodule-name) or (pyfun-name pymodule-name lisp-fun-name).
In addition, when ARG is a 2-element list, then, the first element can be
a list of python function names. "
  `(progn
     ,@(iter outer
         (for arg-list in args)
         (ecase (length arg-list)
           (2 (etypecase (first arg-list)
                (list (iter
                        (for pyfun-name in (first arg-list))
                        (in outer (collect `(defpyfun ,pyfun-name
                                                ,(second arg-list))))))
                (string (collect `(defpyfun ,@arg-list)))))
           (3 (collect `(defpyfun ,(first arg-list) ,(second arg-list)
                          :lisp-fun-name ,(third arg-list))))))))

(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in python process as PYTHON-NAME"
  (pyexec (concatenate 'string
                       python-name
                       "=_py4cl_LispCallbackObject("
                       (write-to-string
                        (object-handle function))
                       ")")))
