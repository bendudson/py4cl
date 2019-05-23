(in-package :py4cl)

(define-condition python-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python error: ~a" (text condition)))))

(defun dispatch-reply (stream value)
  (write-char #\r stream)
  (stream-write-value value stream)
  (force-output stream))

(defun dispatch-messages (process)
  "Read response from python, loop to handle any callbacks"
  (let ((read-stream (uiop:process-info-output process))
        (write-stream (uiop:process-info-input process)))
    (loop
       (case (read-char read-stream) ; First character is type of message
         ;; Returned value
         (#\r (return-from dispatch-messages
                (stream-read-value read-stream)))
         ;; Error
         (#\e (error 'python-error  
                     :text (stream-read-string read-stream)))

         ;; Delete object. This is called when an UnknownLispObject is deleted
         (#\d (free-handle (stream-read-value read-stream)))

         ;; Slot access
         (#\s (destructuring-bind (handle slot-name) (stream-read-value read-stream)
                (let* ((object (lisp-object handle))
                       (handler (get-class-handler object)))
                  ;; User must register a function to handle slot access
                  (dispatch-reply write-stream
                                  (if handler
                                      (funcall handler object slot-name))))))
         
         ;; Callback. Value returned is a list, containing the function ID then the args
         (#\c
          (let ((call-value (stream-read-value read-stream)))
            (let ((return-value (apply (get-callback (first call-value)) (second call-value))))
              ;; Send a reply
              (dispatch-reply write-stream return-value))))
         (otherwise (error "Unhandled message type"))))))

(defun python-eval* (cmd-char &rest args)
  "Internal function, which converts ARGS into a string to be evaluated
This handles both EVAL and EXEC calls with CMD-CHAR being different
in the two cases. 

Anything in ARGS which is not a string is passed through PYTHONIZE
"
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*))
        (str (apply #'concatenate 'string (loop for val in args
                                             collecting (if (typep val 'string)
                                                            val
                                                            (pythonize val))))))
    ;; Write "x" if exec, otherwise "e"
    (write-char cmd-char stream)
    (stream-write-string str stream)
    (force-output stream)
    ;; Wait for response from Python
    (dispatch-messages *python*)))

(defun python-eval (&rest args)
  "Evaluate an expression in python, returning the result
Arguments ARGS can be strings, or other objects. Anything which 
is not a string is converted to a python value

Examples:

 (python-eval \"[i**2 for i in range(\" 4 \")]\") => #(0 1 4 9)

 (let ((a 10) (b 2))
   (py4cl:python-eval a "*" b)) => 20
"
  (apply #'python-eval* #\e args))

(defun (setf python-eval) (value &rest args)
  "Set an expression to a value. Just adds \"=\" and the value
to the end of the expression. Note that the result is evaluated
with exec rather than eval.

Examples:

    (setf (python-eval \"a\") 2)  ; python \"a=2\"
"
  (apply #'python-eval* #\x (append args (list "=" (py4cl::pythonize value))))
  value)

(defun python-exec (&rest args)
  "Execute (using exec) an expression in python.
This is used for statements rather than expressions.

"
  (apply #'python-eval* #\x args))

(defun python-call (fun-name &rest args)
  "Call a python function, given the function name as a string
and additional arguments. Keywords are converted to keyword arguments."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*)))
    ;; Write "f" to indicate function call
    (write-char #\f stream)
    (stream-write-value (list fun-name args) stream)
    (force-output stream))
  (dispatch-messages *python*))

(defun python-call-async (fun-name &rest args)
  "Call a python function asynchronously. 
Returns a lambda which when called returns the result."
  (python-start-if-not-alive)

  (let* ((process *python*)
         (stream (uiop:process-info-input process)))
    
    ;; Write "a" to indicate asynchronous function call
    (write-char #\a stream)
    (stream-write-value (list fun-name args) stream)
    (force-output stream)
  
    (let ((handle (dispatch-messages process))
          value)
      (lambda ()
        (if handle
            ;; Retrieve the value from python
            (progn
              (write-char #\R stream)
              (stream-write-value handle stream)
              (force-output stream)
              (setf handle nil
                    value (dispatch-messages process)))
            ;; If no handle then already have the value
            value)))))

(defun python-method (obj method-name &rest args)
  "Call a given method on an object OBJ. METHOD-NAME can be a
symbol (converted to lower case) or a string. 

Examples:
 
  (python-method \"hello {0}\" 'format \"world\") 
  ; => \"hello world\"

  (python-method '(1 2 3) '__len__)
  ; => 3
"
  (python-start-if-not-alive)
  (py4cl:python-eval
   (py4cl::pythonize obj)
   (format nil ".~(~a~)" method-name)
   (if args 
       (py4cl::pythonize args)
       "()")))

(defun function-args (args)
  "Internal function, intended to be called by the CHAIN macro.
Converts function arguments to a list of strings and (pythonize )
function calls. Handles keywords and insertion of commas. 
Returns a list which can be passed to PYTHON-EVAL.

Examples:

  (py4cl::function-args '(1 :test 2))
  => ((PY4CL::PYTHONIZE 1) \",\" \"test\" \"=\" (PY4CL::PYTHONIZE 2))
"
  (if (not args)
      '("")
      (if (keywordp (first args))
          (append
           (list (string-downcase (first args))
                 "="
                 `(pythonize ,(second args)))
           (if (cddr args)
               (append '(",") (function-args (cddr args)))))
           
          (append
           (list `(pythonize ,(first args)))
           (if (rest args)
               (append '(",") (function-args (rest args))))))))

(defmacro chain (target &rest chain)
  "Chain method calls, member access, and indexing operations
on objects. The operations in CHAIN are applied in order from
first to last to the TARGET object.

TARGET can be
  cons -- a python function to call, returning an object to operate on
  otherwise -- a value, to be converted to a python value

CHAIN can consist of
   cons   -- a method to call
   symbol -- a member data variable
   otherwise -- a value put between [] brackets to access an index

Keywords inside python function calls are converted to python keywords.

Functions can be specified using a symbol or a string. If a symbol is used
then it is converted to python using STRING-DOWNCASE. 

Examples:

  (chain \"hello {0}\" (format \"world\") (capitalize)) 
     => python: \"hello {0}\".format(\"world\").capitalize()
     => \"Hello world\"

  (chain (range 3) stop) 
     => python: range(3).stop
     => 3

  (chain \"hello\" 4)
     => python: \"hello\"[4]
     => \"o\"
"
  (python-start-if-not-alive)
  `(py4cl:python-eval
    ;; TARGET 
    ,@(if (consp target)
          ;; A list -> python function call
          `(,(let ((func (first target))) ; The function name
               (if (stringp func)
                   func  ; Leave string unmodified
                   (string-downcase func))) ; Otherwise convert to lower-case string
             "("
             ,@(function-args (rest target))
             ")")
          ;; A value
          (list (list 'py4cl::pythonize target)))
    ;; CHAIN
    ,@(loop for link in chain
         appending
           (cond
             ((consp link)
              ;; A list. Usually a method to call, but [] indicates __getitem__
              (if (string= (first link) "[]")
                  ;; Calling the __getitem__ method
                  (list "[" (list 'py4cl::pythonize  ; So that strings are escaped
                                  (if (cddr link)
                                      (append '(list) (rest link)) ; More than one -> wrap in list/tuple
                                      (cadr link))) ; Only one -> no tuple
                        "]")
                  ;; Calling a method
                  `("."
                    ,(let ((func (first link)))
                       (if (stringp func)
                         func  ; Leave string unmodified
                         (string-downcase func))) ; Otherwise convert to lower-case string
                    "("
                    ,@(function-args (rest link))
                    ")")))
             ((symbolp link) (list (format nil ".~(~a~)" link)))
             (t (list "[" (list 'py4cl::pythonize link) "]"))))))

(defun python-setf (&rest args)
  "Set python variables in ARGS (\"var1\" value1 \"var2\" value2 ...) "
  ;; pairs converts a list (a b c d) into a list of pairs ((a b) (c d))
  (labels ((pairs (items)
             (when items
               (unless (stringp (first items))
                 (error "Python variable names must be strings"))
               (unless (cdr items)
                 (error "Expected an even number of inputs"))
               (cons (list (first items) (second items))
                     (pairs (cddr items))))))
    
    (python-start-if-not-alive)
    (let ((stream (uiop:process-info-input *python*)))
      ;; Write "s" to indicate setting variables
      (write-char #\s stream)
      (stream-write-value (pairs args) stream)
      (force-output stream))
    ;; Should get T returned, might be error
    (dispatch-messages *python*)))

(defmacro remote-objects (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets."
  `(progn
     (python-start-if-not-alive)
     (let ((stream (uiop:process-info-input *python*)))
       ;; Turn on remote objects
       (write-char #\O stream)
       (force-output stream)
       (unwind-protect
            (progn ,@body)
         ;; Turn off remote objects
         (write-char #\o stream)
         (force-output stream)))))

(defmacro remote-objects* (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets.

This version evaluates the result, returning it as a lisp value if possible.
"
  `(python-eval (remote-objects ,@body)))


