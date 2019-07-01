;;; probably, ASDF should have a feature for doing this
;;; this file should be called after loading all the other files

(in-package :py4cl)
(py4cl::save-or-load-numpy-pickle-parameters)

