(in-package :py4cl)

(defvar *python-command* "python"
  "String, the Python executable to launch
e.g. \"python\" or \"python3\"")

(defvar *python* nil
  "Most recently started python subprocess")

(define-condition python-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python error: ~a" (text condition)))))

(defun python-start (&optional (command *python-command*))
  "Start a new python subprocess
This sets the global variable *python* to the process handle,
in addition to returning it.
COMMAND is a string with the python executable to launch e.g. \"python\"
By default this is is set to *PYTHON-COMMAND*
"
  (setf *python*
        (uiop:launch-program
         (concatenate 'string
                      command  ; Run python executable
                      " "
                      ;; Path *base-pathname* is defined in py4cl.asd
                      ;; Calculate full path to python script
                      (namestring (merge-pathnames #p"py4cl.py" py4cl/config:*base-directory*)))
         :input :stream :output :stream)))

(defun python-alive-p (&optional (process *python*))
  "Returns non-NIL if the python process is alive
(e.g. SBCL -> T, CCL -> RUNNING).
Optionally pass the process object returned by PYTHON-START"
  (and process
       (uiop:process-alive-p process)))

(defun python-start-if-not-alive ()
  "If no python process is running, tries to start it.
If still not alive, raises a condition."
  (unless (python-alive-p)
    (python-start))
  (unless (python-alive-p)
    (error "Could not start python process")))

(defun python-stop (&optional (process *python*))
  ;; If python is not running then return
  (unless (python-alive-p process)
    (return-from python-stop))

  ;; First ask python subprocess to quit
  ;; Could give it a few seconds to close nicely
  (let ((stream (uiop:process-info-input process)))
    (write-char #\q stream))
  ;; Close input, output streams
  (uiop:close-streams process)
  ;; Terminate
  (uiop:terminate-process process)
  ;; Mark as not alive
  (setf *python* nil))

(defun dispatch-messages (process)
  "Read response from python, loop to handle any callbacks"
  (let ((read-stream (uiop:process-info-output process))
        (write-stream (uiop:process-info-input process)))
    (loop
       (case (read-char read-stream) ; First character is type of message
         ;; Returned value
         (#\r (return-from dispatch-messages
                (stream-read-value read-stream)))
         ;; Error
         (#\e (error 'python-error  
                     :text (stream-read-string read-stream))
              (return-from dispatch-messages nil))
         ;; Callback. Value returned is a list, containing the function ID then the args
         (#\c
          (let ((call-value (stream-read-value read-stream)))
            (let ((return-value (apply (get-callback (first call-value)) (second call-value))))
              ;; Send a reply
              (write-char #\r write-stream)
              (stream-write-value return-value write-stream)
              (force-output write-stream))))
         (otherwise (error "Unhandled message type"))))))

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
  (apply #'python-eval* #\e args))

(defun python-exec (&rest args)
  "Execute (using exec) an expression in python.
This is used for statements rather than expressions.

"
  (apply #'python-eval* #\x args))

(defun python-call* (process fun-name &rest args)
  "Call a python function, given the function name as a string
and additional arguments. Keywords are converted to keyword arguments."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input process)))
    ;; Write "f" to indicate function call
    (write-char #\f stream)
    (stream-write-value (list fun-name args) stream)
    (force-output stream))
  (dispatch-messages process))
    
(defun python-call (fun-name &rest args)
  (apply #'python-call* *python* fun-name args))

(defun python-call-async (fun-name &rest args)
  "Call a python function asynchronously. 
Returns a lambda which when called returns the result."
  (python-start-if-not-alive)

  (let* ((process *python*)
         (stream (uiop:process-info-input process)))
    
    ;; Write "a" to indicate asynchronous function call
    (write-char #\a stream)
    (stream-write-value (list fun-name args) stream)
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

(defmacro import-function (fun-name &key docstring
                                      (as (read-from-string fun-name)))
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
"
  ;; Note: a string input is used, since python is case sensitive
  (unless (typep fun-name 'string)
    (error "Argument to IMPORT-FUNCTION must be a string"))

  ;; Input AS specifies the Lisp symbol, either as a string or a symbol
  (let ((fun-symbol (typecase as
                      (string (read-from-string as))
                      (symbol as)
                      (t (error "AS keyword must be string or symbol")))))
    
    `(defun ,fun-symbol (&rest args)
       ,(or docstring "Python function")
       (apply #'python-call ,fun-name args))))

(defmacro import-module (module-name &key (as module-name as-supplied-p))
  "Import a python module as a Lisp package. The module name should be
a string.

Example:
  (py4cl:import-module \"math\")
  (math:sqrt 4)   ; => 2.0

or using 
Keywords:
AS specifies the name to be used for both the Lisp package and python module.
It should be a string, and if not supplied then the module name is used.
"
  (unless (typep module-name 'string)
    (error "Argument to IMPORT-MODULE must be a string"))
  (unless (typep as 'string)
    (error "Keyword argument AS to IMPORT-MODULE must be a string"))
  
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
        (*package* (make-package (string (read-from-string as))
                                 :use '() )))
    (append '(progn)
            (loop for name across fn-names
               for fn-symbol = (read-from-string name)
               for fullname = (concatenate 'string as "." name) ; Include module prefix
               append `((import-function ,fullname :as ,fn-symbol
                            :docstring ,(python-eval (concatenate 'string
                                                                  as "." name ".__doc__")))
                        (export ',fn-symbol ,*package*))))))

(defun export-function* (process function python-name)
  "Makes a lisp FUNCTION available in python PROCESS as PYTHON-NAME"
  (let ((id (register-callback function)))
    (python-exec (concatenate 'string
                              "def " python-name "(*args, **kwargs):
    return _py4cl_callback(" (write-to-string id) ", *args, **kwargs)"))))

(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in the default python process as PYTHON-NAME"
  (export-function* *python* function python-name))

(defun python-setf (&rest args)
  "Set python variables in ARGS (\"var1\" value1 \"var2\" value2 ...) "
  ;; pairs converts a list (a b c d) into a list of pairs ((a b) (c d))
  (labels ((pairs (items)
             (when items
               (unless (stringp (first items))
                 (error "Python variable names must be strings"))
               (unless (cdr items)
                 (error "Expected an even number of inputs"))
               (cons (list (first items) (second items))
                     (pairs (cddr items))))))
    
    (python-start-if-not-alive)
    (let ((stream (uiop:process-info-input *python*)))
      ;; Write "s" to indicate setting variables
      (write-char #\s stream)
      (stream-write-value (pairs args) stream)
      (force-output stream))
    ;; Should get T returned, might be error
    (dispatch-messages *python*)))

(defun python-version-info ()
  "Return a list, using the result of python's sys.version_info."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*)))
    (write-char #\v stream)
    (force-output stream))
  (dispatch-messages *python*))
