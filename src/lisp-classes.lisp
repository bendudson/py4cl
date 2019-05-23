(in-package :py4cl)

(defvar *registered-classes* (make-hash-table)
  "Hash table which maps classes to a handler.")

(defun register-class-handler (obj handler-function)
  (setf (gethash (class-of obj) *registered-classes*) handler-function))

(defun get-class-handler (obj)
  (gethash (class-of obj) *registered-classes*))


