;;; Write data to python over a stream

(in-package :py4cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object Handles

(defvar *handle-counter* 0)

(defvar *lisp-objects* (make-hash-table :test #'eql))

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

(defmethod pythonize ((obj symbol))
  "Handle symbols. Need to handle NIL,
converting it to Python None, and convert T to True."
  (if obj
      (if (eq obj t)
          "True"
          (concatenate 'string
                       "_py4cl_Symbol(':" (string-downcase (string obj)) "')"))
      "None"))

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
