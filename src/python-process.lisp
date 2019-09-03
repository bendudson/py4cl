;;; Functions to start and stop python process

(in-package :py4cl)

(defvar *python-command* "/home/shubhamkar/miniconda3/bin/python"
  "String, the Python executable to launch
e.g. \"python\" or \"python3\"")

(defvar *python* nil
  "Most recently started python subprocess")

(defvar *current-python-process-id* 0
  "A number which changes when python is started. This
is used to prevent garbage collection from deleting objects in the wrong
python session")

(defvar *py4cl-tests* nil
  "Set to true, during testing, for obtaining output to a string.")

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
                      " -u "
                      ;; Path *base-pathname* is defined in py4cl.asd
                      ;; Calculate full path to python script
                      (namestring (merge-pathnames #p"py4cl.py" py4cl/config:*base-directory*)))
         :input :stream :output :stream :error-output :stream))
  (unless *py4cl-tests*
    (bt:make-thread (lambda ()
                      (when *python*
                        (let ((py-out (uiop:process-info-error-output *python*)))
                          (loop while (and *python* (python-alive-p *python*))
                             for char = (read-char py-out nil)
                             do (when char (write-char char))))))))
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
    (python-start))
  (unless (python-alive-p)
    (error "Could not start python process")))

;; Function defined in writer.lisp, which clears an object store
(declaim (ftype (function () t) clear-lisp-objects))

(defun python-stop (&optional (process *python*))
  ;; If python is not running then return
  (unless (python-alive-p process)
    (return-from python-stop))

  ;; First ask python subprocess to quit
  ;; Could give it a few seconds to close nicely
  (let ((stream (uiop:process-info-input process)))
    (write-char #\q stream))
  ;; Terminate
  (uiop:terminate-process process)
  ;; Mark as not alive
  (setf *python* nil)

  ;; Clear lisp objects
  (clear-lisp-objects))

(defun python-interrupt (&optional (process-info *python*))
  (when (python-alive-p process-info)
    (uiop:run-program
     (concatenate 'string "/bin/kill -SIGINT -"
		  (write-to-string (uiop:process-info-pid process-info)))
     :force-shell t)
    ;; something to do with running in separate threads! "deftest interrupt"
    (unless *py4cl-tests* (dispatch-messages process-info))))

(defun python-version-info ()
  "Return a list, using the result of python's sys.version_info."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*)))
    (write-char #\v stream)
    (force-output stream))
  (dispatch-messages *python*))
