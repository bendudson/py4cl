;;; probably, ASDF should have a feature for doing this
;;; this file should be called after loading all the other files

(in-package :py4cl)
(let ((config-path (concatenate 'string
                                (directory-namestring py4cl/config:*base-directory*)
                                ".config"))
      (cl-json:*json-symbols-package* *package*))
  (when (uiop:file-exists-p config-path)
    (load-config)))
