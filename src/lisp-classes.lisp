(in-package :py4cl)

(defgeneric python-getattr (object slot-name)
  (:documentation "Called when python accesses an object's slot (__getattr__)"))

(defgeneric python-setattr (object slot-name value)
  (:documentation "Called when python sets an object's slot (__setattr__)"))
