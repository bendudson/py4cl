;; This file is divided into:
;; - Preparations for calling
;; - Raw Functions
;; - Utility Functions
;;   - eval, exec, call, method, async, monitor
;;   - chain
;;   - remote objects

(in-package :py4cl)

;; ============================ PREPARATIONS FOR CALLING ======================

(define-condition pyerror (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python error: ~a" (text condition)))))

(defun dispatch-reply (stream value)
  (write-char #\r stream)
  (stream-write-value value stream)
  (force-output stream))

(defun dispatch-messages (process)
  "Read response from python, loop to handle any callbacks"
  (setq *python-process-busy-p* t)
  (let* ((read-stream (uiop:process-info-output process))
         (write-stream (uiop:process-info-input process))
         (return-value
          (loop
             (case (read-char read-stream) ; First character is type of message
               ;; Returned value
               (#\r (return (stream-read-value read-stream)))
               ;; Error
               (#\e (error 'pyerror  
                           :text (stream-read-string read-stream)))

               ;; Delete object. This is called when an UnknownLispObject is deleted
               (#\d (free-handle (stream-read-value read-stream)))

               ;; Slot access
               (#\s (destructuring-bind (handle slot-name) (stream-read-value read-stream)
                      (let ((object (lisp-object handle)))
                        ;; User must register a function to handle slot access
                        (dispatch-reply write-stream
                                        (restart-case
                                            (python-getattr object slot-name)
                                          ;; Provide some restarts for missing handler or missing slot
                                          (return-nil () nil)
                                          (return-zero () 0)
                                          (enter-value (return-value)
                                            :report "Provide a value to return"
                                            :interactive (lambda ()
                                                           (format t "Enter a value to return: ")
                                                           (list (read)))
                                            return-value))))))
               
               ;; Callback. Value returned is a list, containing the function ID then the args
               (#\c
                (let ((call-value (stream-read-value read-stream)))
                  (let ((return-value (apply (lisp-object (first call-value)) (second call-value))))
                    ;; Send a reply
                    (dispatch-reply write-stream return-value))))

               ;; Print stdout
               (#\p
                (let ((print-string (stream-read-value read-stream)))
                  (princ print-string)))
               
               (otherwise (error "Unhandled message type"))))))
    (setq *python-process-busy-p* nil)
    return-value))


;; ============================== RAW FUNCTIONS ================================
(declaim (ftype (function (character &rest string)) raw-py))
(defun raw-py (cmd-char &rest strings)
  "Intended as an abstraction to RAW-PYEVAL and RAW_PYEXEC.
Passes strings as they are, without any 'pythonize'ation."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*))
        (str (apply #'concatenate 'string strings)))
    (write-char cmd-char stream)
    (stream-write-string str stream)
    (force-output stream)
    (dispatch-messages *python*))) ; wait for python

(declaim (ftype (function (&rest string)) raw-pyeval))
(defun raw-pyeval (&rest strings)
  "Calls python eval on the concatenation of strings, as they are, without any 
pythonization or modification."
  (apply #'raw-py #\e strings))

(declaim (ftype (function (&rest string)) raw-pyexec))
(defun raw-pyexec (&rest strings)
  "Calls python exec on the concatenation of strings, as they are, without any 
pythonization or modification.
NOTE: Like usual, there are peculiarities to exec commands.
For instance,
  import sys
  def foo:
    sys.stdout.write('hello')
  foo()
will result in 'sys' name not defined PYERROR."
  (apply #'raw-py #\x strings))

;; =========================== UTILITY FUNCTIONS ===============================

(defun pythonizep (value)
  "Determines if VALUE should be pythonized."
  (or (not (stringp value)) ; do not pythonize if
      (realp (ignore-errors (parse-number:parse-number value)))))

(defun pythonize-if-needed (value)
  (if (pythonizep value) (pythonize value) value))

(defun pyeval (&rest args)
  "Calls python eval on args; PYTHONIZEs arg if it satisfies PYTHONIZEP.
Eg.
  > (let ((a 5)) (pyeval a \"*\" a)) 
  25"
  (delete-freed-python-objects) ; delete before pythonizing
  (delete-numpy-pickle-arrays)
  (apply #'raw-pyeval (mapcar #'pythonize-if-needed args)))

(defun pyexec (&rest args)
  "Calls python exec on args; PYTHONIZEs arg if it satisfies PYTHONIZEP."
  (delete-freed-python-objects) ; delete before pythonizing
  (delete-numpy-pickle-arrays)
  (apply #'raw-pyexec (mapcar #'pythonize-if-needed args)))

;; One argument for the name (setf pyeval) is that it sets the "place" returned
;; by pyeval.
(defun (setf pyeval) (value &rest args)
  "Set an expression to a value. Just adds \"=\" and the value
to the end of the expression. Note that the result is evaluated
with exec rather than eval.
Example:
    (setf (pyeval \"a\") 2)  ; python \"a=2\"
Can be useful for modifying a value directly in python.
"
  (apply #'pyexec (append args (list "=" value))) ; would nconc be better?
  value)

(defun pythonize-name (name)
  "Returns downcased SYMBOL-NAME of SYMBOL, and hyphens replaced with underscores.
  Eg. (pythonize-name 'some-example) -> \"some_example\".
Returns the string as it is."
  (etypecase name
    (string name)
    (symbol (iter (for char in-string (format nil "~(~a~)" name))
                  (collect (if (char= char #\-)
                               #\_
                               char)
                    result-type string)))
    (t (pythonize name))))

;; Note: PYCALL does not use PYEVAL/RAW-PYEVAL
(defun pycall (fun-name &rest args)
  "Calls FUN-NAME with ARGS as arguments. Arguments can be keyword based, or 
otherwise. 
FUN-NAME can be a string, symbol or python-object.
  eval is used if FUN-NAME is not a PYTHON-OBJECT.
  If FUN-NAME is a symbol, the name of the symbol is used after calling
    PYTHONIZE-NAME on it. 
  FUN-NAME is NOT PYTHONIZEd if it is a string."
  (python-start-if-not-alive) ; should delete here? what about async?
  (delete-freed-python-objects) ; delete before pythonizing
  (delete-numpy-pickle-arrays)
  (let ((stream (uiop:process-info-input *python*)))
    (write-char #\f stream) ; function call
    (stream-write-value `(,(pythonize-name fun-name) ,args) stream)
    (force-output stream))
  (dispatch-messages *python*))

(defun pycall-async (fun-name &rest args)
  "Call a python function asynchronously. 
Returns a lambda which when called returns the result."
  (python-start-if-not-alive) ; why not delete here? - see pycall may be
  (let* ((process *python*)
         (stream (uiop:process-info-input process)))
    (write-char #\a stream) ; asynchronous function call
    (stream-write-value `(,(pythonize-name fun-name) ,args) stream)
    (force-output stream)
    (let ((handle (dispatch-messages process))
          value)
      (lambda ()
        (if handle
            ;; Retrieve the value from python
            (progn
              (write-char #\R stream)
              (stream-write-value handle stream)
              (force-output stream)
              (setf handle nil
                    value (dispatch-messages process)))
            ;; If no handle then already have the value
            value)))))

(defun pymethod (object method &rest args)
  "PYCALLs METHOD of OBJECT with ARGS
Examples:
  > (pymethod \"'hello {0}'\" 'format \"world\") 
  \"hello world\"
  > (pymethod '(1 2 3) '--len--)
  3
Note: FUN-NAME is NOT PYTHONIZEd if it is a string.
"
  (python-start-if-not-alive)
  (apply #'pycall
         (concatenate 'string
                      (pythonize object) "." (pythonize-name method))
         args))

(defun pycall-monitor (fun-name arg-list &key (interval 1) (output *standard-output*))
  "Same as PYCALL, but \"monitors\" the output of the function. Useful for
functions like keras.Model.fit."
  (python-start-if-not-alive)
  (let ((call (bt:make-thread
               (lambda ()
                 (let ((to-py (uiop:process-info-input *python*)))
                   (write-char #\m to-py)
                   (stream-write-value (list fun-name arg-list) to-py)
                   (force-output to-py)))))
        (from-py (uiop:process-info-output py4cl::*python*)))
    (iter (while (bt:thread-alive-p call))
          (write-string (read-line from-py) output)
          (terpri output)
          (force-output output)
          (sleep interval)
          (finally
           (iter (for line = (read-line from-py))
                 (when (string= line "_py4cl_monitor_error")
                   (error 'pyerror
                          :text (stream-read-value from-py)))
                 (while (not (ignore-errors ; if the string is not 18 char long
                               (string= (subseq line 0 18) "_py4cl_monitor_end"))))
                 (write-string line output)
                 (terpri output))))
    (dispatch-messages *python*)))

(defun pymethod-monitor (obj method-name arg-list &key (interval 1) (output *standard-output*))
  "Same as PYCALL-MONITOR, but handy for calling methods."
  (apply #'pycall-monitor
	 (concatenate 'string (pythonize obj)
                      "." (pythonize-name method-name))
         (cons arg-list
               `(:interval ,interval :output ,output))))

(defun pygenerator (function stop-value)
  (pycall "_py4cl_generator" function stop-value))

(defun pyslot-value (object slot-name)
  (pyeval object "." (pythonize-name slot-name)))

(defun pyhelp (object)
  (pyeval "help(" object ")"))

;; Chain -----------------------------------------------------------------------

(defun function-args (args)
  "Internal function, intended to be called by the CHAIN macro.
Converts function arguments to a list of strings and (pythonize )
function calls. Handles keywords and insertion of commas. 
Returns a list which can be passed to PYTHON-EVAL.

Examples:

  (py4cl::function-args '(1 :test 2))
  => ((PY4CL::PYTHONIZE 1) \",\" \"test\" \"=\" (PY4CL::PYTHONIZE 2))
"
  (if (not args)
      '("")
      (if (keywordp (first args))
          (append
           (list (string-downcase (first args))
                 "="
                 `(pythonize ,(second args)))
           (if (cddr args)
               (append '(",") (function-args (cddr args)))))
          
          (append
           (list `(pythonize ,(first args)))
           (if (rest args)
               (append '(",") (function-args (rest args))))))))

(defun python-eval* (cmd-char &rest args)
  "Internal function, which converts ARGS into a string to be evaluated
This handles both EVAL and EXEC calls with CMD-CHAR being different
in the two cases. 
Anything in ARGS which is not a string is passed through PYTHONIZE
"
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*))
        (str (apply #'concatenate 'string (loop for val in args
                                             collecting (if (typep val 'string)
                                                            val
                                                            (pythonize val))))))
    ;; Write "x" if exec, otherwise "e"
    (write-char cmd-char stream)
    (stream-write-string str stream)
    (force-output stream)
    ;; Wait for response from Python
    (dispatch-messages *python*)))

(defun python-eval (&rest args)
  "Evaluate an expression in python, returning the result
Arguments ARGS can be strings, or other objects. Anything which 
is not a string is converted to a python value
Examples:
 (python-eval \"[i**2 for i in range(\" 4 \")]\") => #(0 1 4 9)
 (let ((a 10) (b 2))
   (py4cl:python-eval a "*" b)) => 20
"
   (delete-freed-python-objects)
   (apply #'python-eval* #\e args))

(defun (setf python-eval) (value &rest args)
  "Set an expression to a value. Just adds \"=\" and the value
to the end of the expression. Note that the result is evaluated
with exec rather than eval.
Examples:
    (setf (python-eval \"a\") 2)  ; python \"a=2\"
"
  (apply #'python-eval* #\x (append args (list "=" (py4cl::pythonize value))))
  value)

(defun (setf chain) (value &rest args)
  "Set an expression to a value. Just adds \"=\" and the value
to the end of the expression. Note that the result is evaluated
with exec rather than eval.

Examples:

    (setf (pyeval \"a\") 2)  ; python \"a=2\"
"
  (apply #'python-eval* #\x (append args (list "=" (py4cl::pythonize value))))
  value)

(defmacro chain (target &rest chain)
  "Chain method calls, member access, and indexing operations
on objects. The operations in CHAIN are applied in order from
first to last to the TARGET object.

TARGET can be
  cons -- a python function to call, returning an object to operate on
  otherwise -- a value, to be converted to a python value

CHAIN can consist of
   cons   -- a method to call
   symbol -- a member data variable
   otherwise -- a value put between [] brackets to access an index

Keywords inside python function calls are converted to python keywords.

Functions can be specified using a symbol or a string. If a symbol is used
then it is converted to python using STRING-DOWNCASE. 

Examples:

  (chain \"hello {0}\" (format \"world\") (capitalize)) 
     => python: \"hello {0}\".format(\"world\").capitalize()
     => \"Hello world\"

  (chain (range 3) stop) 
     => python: range(3).stop
     => 3

  (chain \"hello\" 4)
     => python: \"hello\"[4]
     => \"o\"
"
  (python-start-if-not-alive)
  (delete-numpy-pickle-arrays)
  `(py4cl::python-eval
    ;; TARGET 
    ,@(if (consp target)
          ;; A list -> python function call
          `(,(let ((func (first target))) ; The function name
               (if (stringp func)
                   func  ; Leave string unmodified
                   (string-downcase func))) ; Otherwise convert to lower-case string
             "("
             ,@(function-args (rest target))
             ")")
          ;; A value
          (list (list 'py4cl::pythonize target)))
    ;; CHAIN
    ,@(loop for link in chain
         appending
           (cond
             ((consp link)
              ;; A list. Usually a method to call, but [] indicates __getitem__
              (if (string= (first link) "[]")
                  ;; Calling the __getitem__ method
                  (list "[" (list 'py4cl::pythonize  ; So that strings are escaped
                                  (if (cddr link)
                                      (append '(list) (rest link)) ; More than one -> wrap in list/tuple
                                      (cadr link))) ; Only one -> no tuple
                        "]")
                  ;; Calling a method
                  `("."
                    ,(let ((func (first link)))
                       (if (stringp func)
                           func  ; Leave string unmodified
                           (string-downcase func))) ; Otherwise convert to lower-case string
                    "("
                    ,@(function-args (rest link))
                    ")")))
             ((symbolp link) (list (format nil ".~(~a~)" link)))
             (t (list "[" (list 'py4cl::pythonize link) "]"))))))


;; Remote Objects --------------------------------------------------------------

(defmacro remote-objects (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets."
  `(progn
     (python-start-if-not-alive)
     (let ((stream (uiop:process-info-input *python*)))
       ;; Turn on remote objects
       (write-char #\O stream)
       (force-output stream)
       (unwind-protect
            (progn ,@body)
         ;; Turn off remote objects
         (write-char #\o stream)
         (force-output stream)))))

(defmacro remote-objects* (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets.

This version evaluates the result, returning it as a lisp value if possible.
"
  `(pyeval (remote-objects ,@body)))

