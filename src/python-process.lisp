;;; Functions to start and stop python process

(in-package :py4cl)

(defvar *python* nil
  "Most recently started python subprocess")

(defvar *current-python-process-id* 0
  "A number which changes when python is started. This
is used to prevent garbage collection from deleting objects in the wrong
python session")

(defun pystart (&optional (command (config-var 'pycmd)))
  "Start a new python subprocess
This sets the global variable *python* to the process handle,
in addition to returning it.
COMMAND is a string with the python executable to launch e.g. \"python\"
By default this is is set to *PYTHON-COMMAND*
"
  (format t "Launching ~D ~D...~%" command
	  (namestring (merge-pathnames #p"py4cl.py" py4cl/config:*base-directory*)))
  (setf *python*
        (uiop:launch-program
         (concatenate 'string
                      command  ; Run python executable
                      " "
                      ;; Path *base-pathname* is defined in py4cl.asd
                      ;; Calculate full path to python script
                      (namestring (merge-pathnames #p"py4cl.py" py4cl/config:*base-directory*)))
         :input :stream :output :stream))
  (incf *current-python-process-id*))

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
    (pystart))
  (unless (python-alive-p)
    (error "Could not start python process")))

;; Function defined in writer.lisp, which clears an object store
(declaim (ftype (function () t) clear-lisp-objects))

(defun pystop (&optional (process *python*))
  "Stop (Quit) the python process PROCESS"
  (unless (python-alive-p process)
    (return-from pystop))
  (let ((stream (uiop:process-info-input process)))
    ;; ask the python process to quit; might require a few sec?
    (write-char #\q stream))
  (uiop:close-streams process)
  (uiop:terminate-process process)
  (setf *python* nil) ;; what about multiple processes?
  (clear-lisp-objects))

(defun pyversion-info ()
  "Return a list, using the result of python's sys.version_info."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*)))
    (write-char #\v stream)
    (force-output stream))
  (dispatch-messages *python*))
