(defparameter *nodes* '(
    (oicx (you are in OICX. sugimura is by your side.))
    (osu (you are in your town osu. some people live in Osu-kanon.))
    (nit (you are in your home college NIT. Why are you here? You love lab?))))

(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(defparameter *edges* '(
    (oicx (osu south roadbike)
        (nit east train))
    (osu (north roadbike))
    (nit (west train))))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(print (describe-location 'oicx *nodes*))
(print (describe-path '(osu south roadbike)))

(print (describe-paths 'oicx *edges*))
