(deftest pymonitor (callpython-utility)
  (py4cl:pyexec "
def foo():
  import time
  import sys
  sys.stdout.write('hello')
  sys.stdout.flush()
  time.sleep(5)
  return")
  (assert-equalp "hello"
      (with-output-to-string (output)
        (let ((mon-thread (eval-when (:execute)
                            (bt:make-thread
                             (lambda ()
                               (let ((*standard-output* output))
                                 (py4cl:pycall 'foo)))))))
          (sleep 0.5)
          (bt:destroy-thread mon-thread))))) 
