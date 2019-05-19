;;; Code to enable python to call lisp
;;; uses low-level routines such as python-eval and python-exec

(in-package :py4cl)

(defvar *callback-functions* (make-hash-table)
  "Hash table which maps unique IDs to functions.")

(defvar *last-callback-function-id* 0
  "The ID used for the last callback function.")

(defun register-callback (function)
  ;; Get a unique ID for this callback function
  (let ((id (incf *last-callback-function-id*)))
    (setf (gethash id *callback-functions*) function)
    id))

(defun get-callback (id)
  (gethash id *callback-functions*))


