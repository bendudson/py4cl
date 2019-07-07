(defun slicep (name)
  (etypecase name
    (string (string= name "slice"))
    (symbol (string= (string-downcase name) "slice"))))

(defun []p (name)
  (etypecase name
    (string (string= name "__getitem__"))
    (symbol (string= (string-downcase name) "[]"))))

(defun %%chain (link)
  (etypecase link
    (list (cond ((slicep (car link))
                 `("slice("
                   ,@(loop for ele in (cdr link)
                        collect `(pythonize ,ele)
                        collect ",")
                   ")"))
                (([]p (car link))
                 `("["
                   ,@(loop for ele in (cdr link)
                        collect `(pythonize ,ele))
                   "]"))
                ((eq 'quote (car link))
                 `(,(concatenate 'string
                                 "." (pythonize-name (second link)))))
                (t `("."
                     ,(format t "Arbitrary")
                     ,(pythonize-name (car link))
                     "("
                     ,@(loop for ele in (cdr link)
                          collect `(pythonize ,ele)
                          collect ",")
                     ")"))))
    (string (list "." link))
    (symbol `("." (pythonize-name ',link)))
    (real (list "[" (pythonize link) "]"))))

(defun %chain (chain)
  (when chain
    `(,@(%%chain (car chain))
        ,@(%chain (cdr chain)))))

(defmacro chain (target &rest chain)
  ""
  (let ((stream (gensym))
        (str (gensym)))
    `(progn
      (python-start-if-not-alive)
      (delete-numpy-pickle-arrays)
      (let ((,stream (uiop:process-info-input *python*))
            (,str (concatenate 'string (pythonize ,target)
                               ,@(funcall #'%chain chain))))
        (write-char #\e ,stream) ; eval
        (stream-write-string ,str ,stream)
        (force-output ,stream)
        (dispatch-messages *python*)))))
