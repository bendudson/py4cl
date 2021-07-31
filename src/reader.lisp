;;; Code to read from python process over a stream

(in-package :py4cl)

(defstruct python-object
  "A handle for a python object
which couldn't be translated into a Lisp value.
TYPE slot is the python type string
HANDLE slot is a unique key used to refer to a value in python."
  (type "" :type string)
  handle)

(defvar *freed-python-objects* nil
  "A list of handles to be freed. This is used because garbage collection may occur in parallel with the main thread.")

(defun free-python-object (python-id handle)
  (push (list python-id handle) *freed-python-objects*))

(defun delete-freed-python-objects ()
  ;; Remove (python-id handle) pairs from the list and process
  (loop for id-handle = (pop *freed-python-objects*)
     while id-handle
     do (let ((python-id (first id-handle))
              (handle (second id-handle)))
          (if (and
               (python-alive-p) ; If not alive, python-exec will start python
               (= *current-python-process-id* python-id))  ; Python might have restarted
              ;; Call the internal function, to avoid infinite recursion or deadlock
              (python-eval* #\x "
try:
  del _py4cl_objects[" handle "]
except:
  pass"))))
  (delete-numpy-pickle-arrays))

(defun delete-numpy-pickle-arrays ()
  "Delete pickled arrays, to free space."
  (loop :while (> *numpy-pickle-index* 0)
        :do (decf *numpy-pickle-index*)
            (uiop:delete-file-if-exists
             (concatenate 'string
                          (config-var 'numpy-pickle-location)
                          ".to." (write-to-string *numpy-pickle-index*)))))

(defun make-python-object-finalize (&key (type "") handle)
    "Make a PYTHON-OBJECT struct with a finalizer.
This deletes the object from the dict store in python.

Uses trivial-garbage (public domain)
"
    (tg:finalize
     (make-python-object :type type
                         :handle handle)
     (let ((python-id *current-python-process-id*))
       (lambda () ; This function is called when the python-object is garbage collected
         (ignore-errors
           ;; Put on a list to free later. Garbage collection may happen
           ;; in parallel with the main thread, which may be executing other commands.
           (free-python-object python-id handle))))))

(defun decode-external-string (string)
  "Decode lisp string if necessary in some platform"
  #-lispworks string
  #+lispworks
  (translate-string-via-fli string :latin-1 :utf-8))

#+lispworks
(defun translate-string-via-fli (string from to)
  "The way to translate string in LispWorks."
  (fli:with-foreign-string (ptr elements bytes :external-format from)
                           string
    (declare (ignore elements bytes))
    (fli:convert-from-foreign-string ptr :external-format to)))

(defun stream-read-string (stream)
  "Reads a string from a stream
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (let ((nchars (parse-integer (read-line stream))))
    #+lispworks
    (decode-external-string
     (with-output-to-string (str)
       (loop for i from 1 to nchars
             for char1 = (read-char stream)
             for code1 = (char-code char1)
             do (write-char char1 str)
                (loop repeat (cond ((< code1 #x80) 0)   ; assumes utf-8
                                   ((< code1 #xe0) 1)
                                   ((< code1 #xf0) 2)
                                   ((< code1 #xf8) 3)
                                   (t 4))
                      do (write-char (read-char stream) str)))))
    #-lispworks
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
