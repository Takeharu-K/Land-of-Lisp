(defparameter *big* 100)
(defparameter *small* 1)

(defun guess () 
  (print (ash (+ *big* *small*) -1))
)

(defun smaller ()
  (setf *big* (1- (guess)))
  (guess)
)

(defun bigger ()
  (setf *small* (1+ (guess)))
  (guess)
)

(defun resetWith (small big)
  (setf *big* big)
  (setf *small* small)
)

(defun main ()
  (print '(I guess what number you imagine !))
  (print '(Imagine one number between 1 to 100))
  (print '(How to play?))
  (print '-----------------------)
  (print '(Type (smaller) or (bigger)))
  (guess)
)

(main)