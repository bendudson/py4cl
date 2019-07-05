(in-package :py4cl)

(defvar *config* () "Used for storing configuration at a centralized location.")
;; Refer initialize function to note which variables are included under *config*

(defun take-input (prompt default)
  (format t prompt)
  (force-output)
  (let ((input (read-line)))
    (if (string= "" input) default input)))

(defun initialize ()
  "Intended to be called first upon installation. Sets up default python command,
and numpy pickle file and lower bounds."
  (let ((pycmd (take-input "Provide the python binary to use (default python): "
                           "python"))
        (numpy-pickle-location
         (take-input "~%PY4CL uses pickled files to transfer large arrays between lisp
 and python efficiently. These are expected to have sizes exceeding 100MB 
 (this depends on the value of *NUMPY-PICKLE-LOWER-BOUND*). Therefore, choose an 
 appropriate location (*NUMPY-PICKLE-LOCATION*) for storing these arrays on disk.

Enter full file path for storage (default /tmp/_numpy_pickle.npy): "
                     "/tmp/_numpy_pickle.npy"))
        (numpy-pickle-lower-bound
         (parse-integer
          (take-input "Enter lower bound for using pickling (default 100000): "
                      "100000"))))
    (setq  *config* ;; case conversion to and from symbols is handled by cl-json
           `((pycmd . ,pycmd)
             (numpy-pickle-location . ,numpy-pickle-location)
             (numpy-pickle-lower-bound . ,numpy-pickle-lower-bound)))
    ;; to avoid development overhead, we will not bring these variables "out"
    (save-config)))

(defun save-config ()
  (let ((config-path (concatenate 'string
                                  (directory-namestring py4cl/config:*base-directory*)
                                  ".config")))
    
    (with-open-file (f config-path :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (cl-json:encode-json-alist *config* f))
    (format t "Configuration is saved to ~D.~%" config-path)))

(defun load-config ()
  (let ((config-path (concatenate 'string
                                  (directory-namestring py4cl/config:*base-directory*)
                                  ".config"))
        (cl-json:*json-symbols-package* *package*))
    (setq *config* (with-open-file (f config-path)
                     (cl-json:decode-json f)))))

(defun config-var (var) (cdr (assoc var *config*)))
(defun (setf config-var) (new-value var)
  (setf (cdr (assoc var *config*)) new-value)
  (save-config)
  (when (python-alive-p)
    (pycall "_py4cl_load_config")))
