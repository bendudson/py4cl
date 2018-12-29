(in-package :py4cl)

(defvar *python* nil
  "Most recently started python subprocess")

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
  "Returns T if the python process is alive"
  (uiop:process-alive-p process))

(defun python-eval* (process str &key exec)
  (let ((stream (uiop:process-info-input process)))
    ;; Write "x" if exec, otherwise "e"
    (write-char (if exec #\x #\e) stream)
    (stream-write-value str stream)
    (force-output stream))

  ;; Read response
  (let ((stream (uiop:process-info-output process)))
    (case (read-char stream)
      ;; Returned value
      (#\r (stream-read-value stream))
      ;; Error
      (#\e (error
            (stream-read-string stream)))
      ;; Callback
      (#\c nil ))))

(defun python-eval (str)
  (python-eval* *python* str))

(defun python-exec* (process str)
  (python-eval* process str :exec t))

(defun python-exec (str)
  (python-exec* *python* str))

(defun python-stop (&optional (process *python*))
  ;; First ask python subprocess to quit
  ;; Could give it a few seconds to close nicely
  (let ((stream (uiop:process-info-input process)))
    (write-char #\q stream))
  ;; Close input, output streams
  (uiop:close-streams process)
  ;; Terminate
  (uiop:terminate-process process))


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
  
(defmacro defpyfun (fun-name)
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
  (let ((fun-sym (read-from-string fun-name)))
    (unless (typep fun-sym 'symbol)
      (error "Argument to defpyfun must be read as a symbol"))
    `(defun ,fun-sym (&rest args)
       (apply #'python-call ,fun-name args))))

