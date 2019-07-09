
;;; ISSUES

;;; Chain
(let ((a 1)) ; a is not used
  (py4cl:chain "hello" a))

(py4cl:pyeval "[1,2,3].__len__()") ; What is the equivalent of using chain here?
(py4cl:chain '(1 2 3) ("__len__"))    ; this does not work
(py4cl:chain "hello {0}" ("__len__")) ; however, this works

;; old eval
(pyeval 4)   ; -> 4
(pyeval "4") ; -> 4

;; new eval
(pyeval 4)   ; -> 4
(pyeval "4") ; -> "4"


;; no need for separate pysetf? - this works
(setf (py4cl:pyeval "a") 5
      (py4cl:pyeval "b") 10)

