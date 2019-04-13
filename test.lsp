(defparameter *big* 100)
(defparameter *small* 1)

(defun guess () 
  (ash (+ *big* *small*) -1)
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

