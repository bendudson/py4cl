(in-package :py4cl)

(defvar *python* nil
  "Most recently started python subprocess")

(define-condition python-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python error: ~a" (text condition)))))

(defun python-start ()
  "Start a new python subprocess
This sets the global variable *python* to the process handle,
in addition to returning it.
"
  (setf *python*
        (uiop:launch-program
         (concatenate 'string
                      "python "  ; Run python executable
                      ;; Path *base-pathname* is defined in py4cl.asd
                      ;; Calculate full path to python script
                      (namestring (merge-pathnames #p"py4cl.py" py4cl-config:*base-directory*)))
         :input :stream :output :stream)))

(defun python-alive-p (&optional (process *python*))
  "Returns T if the python process is alive.
Optionally pass the process object returned by PYTHON-START"
  (and process
       (uiop:process-alive-p process)))

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
  (uiop:terminate-process process))

(defun python-eval* (process str &key exec)
  (let ((stream (uiop:process-info-input process)))
    ;; Write "x" if exec, otherwise "e"
    (write-char (if exec #\x #\e) stream)
    (stream-write-string str stream)
    (force-output stream))

  ;; Read response, loop to handle any callbacks
  (let ((read-stream (uiop:process-info-output process))
        (write-stream (uiop:process-info-input process)))
    (loop
       (case (read-char read-stream) ; First character is type of message
         ;; Returned value
         (#\r (return-from python-eval*
                (stream-read-value read-stream)))
         ;; Error
         (#\e (error 'python-error  
                     :text (stream-read-string read-stream))
              (return-from python-eval* nil))
         ;; Callback. Value returned is a list, containing the function ID then the args
         (#\c
          (let ((call-value (stream-read-value read-stream)))
            (let ((return-value (apply (get-callback (first call-value)) (second call-value))))
              ;; Send a reply
              (write-char #\r write-stream)
              (stream-write-value return-value write-stream)
              (force-output write-stream))))
         (otherwise (error "Unhandled message type"))))))

(defun python-eval (str)
  (python-eval* *python* str))

(defun python-exec* (process str)
  (python-eval* process str :exec t))

(defun python-exec (str)
  (python-exec* *python* str))

(defun python-call* (process fun-name &rest args)
  "Call a python function, given the function name as a string
and additional arguments. Keywords are converted to keyword arguments."
  (let ((cmdstr (with-output-to-string (stream)
                  (write-string fun-name stream)
                  (if args
                      (write-string
                       (pythonize args) stream)
                      (write-string "()" stream)))))
    (python-eval* process cmdstr)))
    
(defun python-call (fun-name &rest args)
  (apply #'python-call* *python* fun-name args))
  
(defmacro defpyfun (fun-name &key docstring in-module)
  "Define a function which calls python
Example
  (py4cl:python-exec \"import math\")
  (py4cl:defpyfun \"math.sqrt\")
  (math.sqrt 42)
  -> 6.4807405
"
  ;; Note: a string input is used, since python is case sensitive
  (unless (typep fun-name 'string)
    (error "Argument to defpyfun must be a string"))
  ;; Convert string to a symbol
  (let ((fun-sym (read-from-string fun-name))
        (fun-fullname (if in-module
                          (concatenate 'string in-module "." fun-name)
                          fun-name)))
    (unless (typep fun-sym 'symbol)
      (error "Argument to defpyfun must be read as a symbol"))
    `(defun ,fun-sym (&rest args)
       ,(or docstring "Python function")
       (apply #'python-call ,fun-fullname args))))

(defmacro python-import (module-name &key (as module-name as-supplied-p))
  ;; Ensure that python is running
  (unless (python-alive-p)
    (python-start))

  ;; Import the required module in python
  (if as-supplied-p
      (python-exec (concatenate 'string
                                "import " module-name " as " as))
      (python-exec (concatenate 'string
                                "import " module-name)))

  ;; Also need to import the "inspect" module
  (python-exec "import inspect")

  ;; fn-names  All callables whose names don't start with "_"
  ;; package-sym   The symbol for the lisp package
  (let ((fn-names (python-eval (concatenate 'string
                                            "[name for name, fn in inspect.getmembers("
                                            as
                                            ", callable) if name[0] != '_']")))
        (package-sym (read-from-string as))
        (original-package (package-name *package*)))
    `(progn
       (defpackage ,package-sym (:use ))
       (in-package ,package-sym)
       ,@(loop for name across fn-names append
              `((defpyfun ,name
                    :docstring ,(python-eval (concatenate 'string
                                                          as "." name ".__doc__"))
                    :in-module ,as)
                (export (read-from-string ,name))))
       (in-package ,original-package))))

(defun export-function* (process function python-name)
  "Makes a lisp FUNCTION available in python PROCESS as PYTHON-NAME"
  (let ((id (register-callback function)))
    (python-exec* process
                  (concatenate 'string
                               "def " python-name "(*args, **kwargs):
    return _py4cl_callback(" (write-to-string id) ", *args, **kwargs)"))))

(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in the default python process as PYTHON-NAME"
  (export-function* *python* function python-name))
