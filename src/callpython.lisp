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
