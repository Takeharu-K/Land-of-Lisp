;場所node
(defparameter *nodes* '(
    (oicx (you are in OICX. sugimura is by your side.))
    (osu (you are in your town osu. some people live in Osu-kanon.))
    (nit (you are in your home college NIT. Why are you here? You love lab?))))

;移動edge
(defparameter *edges* '(
    (oicx (osu south roadbike)
        (nit east train))
    (osu (oicx north roadbike))
    (nit (oicx west train))))

;物体object
(defparameter *objects* '(
    macbookpro alexa airmax matlab))

(defparameter *object-locations* '(
    (macbookpro oicx)
    (alexa osu)
    (airmax osu)
    (matlab nit)))

;現在地location
(defparameter *location* 'osu)

(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun object-at (location objects object-locations)
    (labels ((at-location-p (objects)
        (eq (cadr (assoc objects object-locations)) location)))
        (remove-if-not #'at-location-p objects)))

(defun describe-objects (location objects object-locations)
    (labels ((describe (obj)
        `(you see a ,obj on the floor.)))
        (apply #'append (mapcar #'describe (object-at location objects object-locations)))))

(defun look ()
    (append (describe-location *location* *nodes*)
        (describe-paths *location* *edges*)
        (describe-objects *location* *objects* *object-locations*)))

(print (look))
(blabalbalabalbalb)