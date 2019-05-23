(in-package :py4cl)

(defvar *registered-classes* (make-hash-table)
  "Hash table which maps classes to a handler.")

(defun register-handler (obj handler-function)
  "Register a function HANDLER-FUNCTION which will be called
when python attempts to access attributes of object of the same
class as OBJ.

HANDLER-FUNCTION should have two arguments: (object slot-name)
where OBJECT is the object being accessed, and SLOT-NAME is a string.

Example:

  (py4cl:register-handler object
                            (lambda (object slot-name)
                              (cond
                                ;; a slot
                                ((string= slot-name \"value\")
                                 (slot-value object 'value))
                                ;; a method
                                ((string= slot-name \"func\")
                                 (lambda (arg) (* 2 arg)))
                                ;; Missing
                                (t (error \"Unknown slot ~a\" slot-name)))))

If a condition is signalled, then several restarts are provided.
"
  (setf (gethash (class-of obj) *registered-classes*) handler-function))

(defun get-handler (obj)
  (gethash (class-of obj) *registered-classes*))

(defun clear-handler (obj)
  (remhash (class-of obj) *registered-classes*))

(define-condition missing-handler-error (error)
  ((object :initarg :object :reader object)
   (slot-name :initarg :slot-name :reader slot-name))
  (:report (lambda (condition stream)
             (format stream "Missing handler for object ~a, slot name ~a." (object condition) (slot-name condition))))
  (:documentation "Python has attempted to access an attribute (slot) of a
lisp object, but no handler has been registered for this class.
Use REGISTER-HANDLER to add a handler."))


