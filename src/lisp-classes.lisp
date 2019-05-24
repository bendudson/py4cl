(in-package :py4cl)

(defvar *registered-classes* (make-hash-table)
  "Hash table which maps classes to a handler.")

(defun register-handler (obj handler-function)
  "Register a function HANDLER-FUNCTION which will be called
when python attempts to access attributes of object of the same
class as OBJ.

OBJ can be an object or STANDARD-CLASS. Typically structures
must be passed in as an object, due to variations in their implementation.
Classes can be passed in using FIND-CLASS.

HANDLER-FUNCTION should have two arguments: (object slot-name)
where OBJECT is the object being accessed, and SLOT-NAME is a string.

Example:

  (defclass test-class ()
    ((value :initarg :value)))

  ;; Register handler using an object
  (let ((obj (make-instance 'test-class :value 42)))
    (py4cl:register-handler obj
                            (lambda (object slot-name)
                              (cond
                                ;; a slot
                                ((string= slot-name \"value\")
                                 (slot-value object 'value))
                                ;; a method
                                ((string= slot-name \"func\")
                                 (lambda (arg) (* 2 arg)))
                                ;; Missing
                                (t (error \"Unknown slot ~a\" slot-name))))))

  ;; Or for classes, register using FIND-CLASS
  (py4cl:register-handler (find-class 'test-class)
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
  (unless (functionp handler-function)
    (error "Handler passed to register-handler must be a function"))
  
  (let ((class-obj (if (typep object 'standard-class)
                       object
                       (class-of obj))))
    (setf (gethash class-obj *registered-classes*) handler-function)))

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

(defun call-handler (object slot-name)
  "Call the handler function previously registered for (class-of OBJECT), passing OBJECT and SLOT-NAME to the handler. 
If no handler has been registered, signals condition MISSING-HANDLER-ERROR."
  (let ((handler (get-handler object)))
    (if handler
        (funcall handler object slot-name)
        (error 'missing-handler-error
               :object object :slot-name slot-name))))

