(in-package :py4cl)

(defun stream-read-string (stream)
  "Reads a string from a stream
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (let ((nchars (parse-integer (read-line stream))))
    (with-output-to-string (str)
      (loop for i from 1 to nchars do
           (write-char (read-char stream) str)))))

(defun stream-read-value (stream)
  "Get a value from a stream
Currently works by reading a string then using read-from-string
"
  (let ((str (stream-read-string stream)))
    (multiple-value-bind (value count)
        (read-from-string str)
      ;; Check if all characters were used
      (unless (eql count (length str))
        (error (concatenate 'string "unread characters in reading string \"" str "\""))) 
      value)))

(defvar *python* nil
  "Most recently started python subprocess")

(defun python-start ()
  "Start a new python subprocess
This sets the global variable *python* to the process handle,
in addition to returning it.
"
  (setf *python*
        (uiop:launch-program "python py4cl.py" :input :stream :output :stream)))

(defun python-alive-p (&optional (process *python*))
  "Returns T if the python process is alive"
  (uiop:process-alive-p process))

(defun python-eval* (process str &key exec)
  (let ((stream (uiop:process-info-input process)))
    ;; Write "x" if exec, otherwise "e"
    (write-char (if exec #\x #\e) stream)
    (format stream "~a~%~a" (length str) str)
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
