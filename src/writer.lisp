;;; Write data to python over a stream

(in-package :py4cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object Handles

(defvar *handle-counter* 0)

(defvar *lisp-objects* (make-hash-table :test #'eql))

(defvar *numpy-pickle-location* nil
  "Preferably set this to a ramdisk. Set this using SET-NUMPY-ARRAY-LOCATION, 
since corresponding changes are required to be made in the python process.")
(defvar *numpy-pickle-lower-bound* nil)

(declaim (ftype (function (string)) set-numpy-array-location))
(defun set-numpy-pickle-location (pathname-as-string)
  "Set *NUMPY-ARRAY-LOCATION* to PATHNAME, and makes corresponding changes in
python process. (Default /tmp/_numpy_pickle.npy. For persistent change, call
SAVE-OR-LOAD-NUMPY-PICKLE-PARAMETERS with T."
  (python-start-if-not-alive)
  (when (string= "" pathname-as-string)
    (setq pathname-as-string "/tmp/_numpy_pickle.npy"))
  (setq *numpy-pickle-location* pathname-as-string)
  (pyexec "_py4cl_numpy_pickle_location = " (write-to-string pathname-as-string)))

(declaim (ftype (function ((or integer nil))) set-numpy-pickle-lower-bound))
(defun set-numpy-pickle-lower-bound (bound)
  "Set *NUMPY-PICKLE-LOWER-BOUND* to BOUND, and makes corresponding changes 
in python process. (Default 10000.) For persistent change, call
SAVE-OR-LOAD-NUMPY-PICKLE-PARAMETERS with T."
  (python-start-if-not-alive)  
  (unless bound (setq bound 100000))
  (setq *numpy-pickle-lower-bound* bound)
  (pyexec "_py4cl_numpy_pickle_lower_bound = " bound))


(defun save-or-load-numpy-pickle-parameters (&optional savep)
  (let ((config-path (concatenate 'string
                                  (directory-namestring py4cl/config:*base-directory*)
                                  ".config")))
    (if (uiop:file-exists-p config-path) ;; on first ever call
        (if savep
            (with-open-file (f config-path :direction :output :if-exists :supersede
                               :if-does-not-exist :create) 
              (write-string *numpy-pickle-location* f)
              (terpri f)
              (write-string (write-to-string *numpy-pickle-lower-bound*) f)
              (terpri f))
            (with-open-file (f config-path)
              (set-numpy-pickle-location (read-line f))
              (set-numpy-pickle-lower-bound (read f))))
        (progn
          (terpri)
          (format t " PY4CL uses pickled files to transfer large arrays between lisp
 and python efficiently. These are expected to have sizes exceeding 100MB 
 (this depends on the value of *NUMPY-PICKLE-LOWER-BOUND*). Therefore, choose an 
 appropriate location (*NUMPY-PICKLE-LOCATION*) for storing these arrays on disk.~%")
          (format t "Enter location for storage (default /tmp/_numpy_pickle.npy): ")
          (force-output)
          (set-numpy-pickle-location (read-line))
          (format t "Enter lower bound for using pickling (default 100000): ")
          (force-output)
          (set-numpy-pickle-lower-bound (parse-integer (read-line) :junk-allowed t))
          (with-open-file (f config-path :direction :output :if-exists :supersede
                             :if-does-not-exist :create) 
            (write-string *numpy-pickle-location* f)
            (terpri f)
            (write-string *numpy-pickle-lower-bound* f)
            (terpri f))
          (format t "This configuration is saved to ~D.~%" config-path)))))


(defun clear-lisp-objects ()
  "Clear the *lisp-objects* object store, allowing them to be GC'd"
  (setf *lisp-objects* (make-hash-table :test #'eql)
        *handle-counter* 0))

(defun free-handle (handle)
  "Remove an object with HANDLE from the hash table"
  (remhash handle *lisp-objects*))

(defun lisp-object (handle)
  "Get the lisp object corresponding to HANDLE"
  (or (gethash handle *lisp-objects*)
      (error "Invalid Handle.")))

(defun object-handle (object)
  "Store OBJECT and return a handle"
  (let ((handle (incf *handle-counter*)))
    (setf (gethash handle *lisp-objects*) object)
    handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert objects to a form which python can eval

(defgeneric pythonize (obj)
  (:documentation
   "Convert an object into a string which can be written to stream.
Default implementation creates a handle to an unknown Lisp object.")
  (:method (obj)
    (concatenate 'string
                 "_py4cl_UnknownLispObject(\""
                 (write-to-string
                 (type-of obj))
                 "\", "
                 (write-to-string
                  (object-handle obj))
                 ")")))

(defmethod pythonize ((obj real))
  "Write a real number. 
   Note that python doesn't handle 'd','f', 's' or 'L' exponent markers"
  (substitute-if #\e (lambda (ch)
                       (member ch '(#\d #\D #\f #\F #\s #\S #\l #\L)))
                 (write-to-string obj)))

(defmethod pythonize ((obj complex))
  "Create string of the form \"(1+2j\". 
If imaginary part is negative the output is of form \"(1+-2j\"
which is interpreted correctly by python (3.7.2)."
  (concatenate 'string
               "("
               (write-to-string (realpart obj))
               "+"
               (write-to-string (imagpart obj))
               "j)"))

(defmethod pythonize ((obj array))
  (when (> (array-total-size obj) *numpy-pickle-lower-bound*)
    (numpy-file-format:store-array obj *numpy-pickle-location*)
    (return-from pythonize
      (concatenate 'string "_py4cl_numpy.load('" *numpy-pickle-location*
                   "', allow_pickle = True)")))
  
  ;; Handle case of empty array
  (if (= (array-total-size obj) 0)
      (return-from pythonize "[]"))
  
  ;; First convert the array to 1D [0,1,2,3,...]
  (let ((array1d (with-output-to-string (stream)
                   (write-char #\[ stream)
                   (princ (pythonize (row-major-aref obj 0)) stream)
                   (do ((indx 1 (1+ indx)))
                       ((>= indx (array-total-size obj)))
                     (write-char #\, stream)
                     (princ (pythonize (row-major-aref obj indx)) stream))
                   (write-char #\] stream))))
    (if (= (array-rank obj) 1)
        ;; 1D array return as-is
        array1d
        ;; Multi-dimensional array. Call NumPy to resize
        (concatenate 'string
                     "_py4cl_numpy.resize(" array1d ", "
                     (pythonize (array-dimensions obj)) ")"))))

(defmethod pythonize ((obj cons))
  "Convert a list. This leaves a trailing comma so that python
evals a list with a single element as a tuple
"
  (with-output-to-string (stream)
    (write-char #\( stream)
    (dolist (val obj)
      (write-string (pythonize val) stream)
      (write-char #\, stream))
    (write-char #\) stream)))

(defmethod pythonize ((obj string))
  (write-to-string (coerce obj '(vector character))
                   :escape t :readably t))

(defvar *lisp-to-python-types-alist*
  '((t "True")
    (nil "None")
    (float "float")
    (boolean "bool")
    (null "type(None)")
    (integer "int")
    (complex "complex")
    (vector "list")
    (hash-table "dict")
    (string "str")))
;; leaves out inspect._empty    

(defmethod pythonize ((obj symbol))
  "Handle symbols. Need to handle NIL,
converting it to Python None, and convert T to True."
  (if (assoc obj *lisp-to-python-types-alist*)
      (second (assoc obj *lisp-to-python-types-alist*))
       (concatenate 'string
		    "_py4cl_Symbol(':" (string-downcase (string obj)) "')")))

(defmethod pythonize ((obj hash-table))
  "Convert hash-table to python map.
Produces a string {key1:value1, key2:value2,}"
  (concatenate 'string
               "{"
               (apply #'concatenate 'string
                      (loop for key being the hash-keys of obj
                         using (hash-value value)
                         appending (list (pythonize key) ":" (pythonize value) ",")))
               "}"))

(defmethod pythonize ((obj function))
  "Handle a function by converting to a callback object
The lisp function is stored in the same object store as other objects."
  (concatenate 'string
                 "_py4cl_LispCallbackObject("
                 (write-to-string
                  (object-handle obj))
                 ")"))

(defmethod pythonize ((obj python-object))
  "A handle for a python object, stored in a dict in Python"
  (concatenate 'string
               "_py4cl_objects["
               (write-to-string (python-object-handle obj))
               "]"))

(defun stream-write-string (str stream)
  "Write a string to a stream, putting the length first"
  ;; Convert the value to a string
  (princ (length str) stream)  ; Header, so length of string is known to reader
  (terpri stream)
  (write-string str stream))
    
(defun stream-write-value (value stream)
  "Write a value to a stream, in a format which can be read
by the python subprocess as the corresponding python type"
  (stream-write-string (pythonize value) stream))
