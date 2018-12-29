;;; Write data to python over a stream

(in-package :py4cl)

(defgeneric pythonize (obj)
  (:documentation
   "Convert an object into a string which can be written to stream.
Default implementation returns an empty string")
  (:method (obj) 
    ""))

(defmethod pythonize ((obj real))
  (write-to-string obj))

(defmethod pythonize ((obj array))
  (with-output-to-string (stream)
    (write-char #\[ stream)
    (princ (row-major-aref obj 0) stream)
    (do ((indx 1 (1+ indx)))
        ((>= indx (array-total-size obj)))
      (write-char #\, stream)
      (princ (row-major-aref obj indx) stream))
    (write-char #\] stream)))

(defmethod pythonize ((obj string))
  obj)

(defun stream-write-value (value stream)
  "Write a value to a stream, in a format which can be read
by the python subprocess.
"
  ;; Convert the value to a string
  (let ((str (pythonize value)))
    (princ (length str) stream)  ; Header, so length of string is known to reader
    (terpri stream)
    (write-string str stream)))

    
