(defparameter *shell* (uiop:launch-program "python py4cl.py" :input :stream :output :stream))

(format (uiop:process-info-input *shell*)  "x26~%abcdefghijklmnopqrstuvwxyz")
(write-line "x26abcdefghijklmnopqrstuvwxyz" (uiop:process-info-input *shell*))

(write-line "dir()" (uiop:process-info-input *shell*))
(force-output (uiop:process-info-input *shell*))

(defun process-read-string (process)
  "Reads a string returned by subprocess
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (unless (uiop:process-alive-p process)
    (return-from process-read-string nil))
  (let ((stream (uiop:process-info-output process)))
    (let ((nchars (parse-integer (read-line stream))))
      (with-output-to-string (str)
        (loop for i from 1 to nchars do
             (write-char (read-char stream) str))))))

(defun eval-string (str &key exec)
  (let ((stream (uiop:process-info-input *shell*)))
    ;; Write "x" if exec, otherwise "e"
    (write-char (if exec #\x #\e) stream)
    (format stream "~a~%~a" (length str) str)
    (force-output stream))

  ;; Read response
  (let ((stream (uiop:process-info-output *shell*)))
    (case (read-char stream)
      ;; Returned value
      (#\r (read-from-string
            (process-read-string *shell*)))
      ;; Error
      (#\e (error
            (process-read-string *shell*)))
      ;; Callback
      (#\c nil ))))

(defun exec-string (str)
  (eval-string str :exec t))
    

;; Read and print lines while available
(let ((stream (uiop:process-info-output *shell*)))
  (loop while (listen stream) do
       (princ (read-line stream))
       (terpri)))

(uiop:close-streams *shell*)
