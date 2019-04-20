(defparameter *nodes* '(
    (oicx (you are in OICX. yuasa is by your side.))
    (osu (you are in your town osu. some people live in Osu-kanon.))
    (nit (you are in your home college NIT. Why are you here? You love lab?))))

(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(print (describe-location 'oicx *nodes*))