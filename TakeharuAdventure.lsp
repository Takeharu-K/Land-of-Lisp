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

;許可されたコマンド
(defparameter *ok-commands* '(look walkTo pickup inBag))

;describe functions
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

;action functions
(defun look ()
    (append (describe-location *location* *nodes*)
        (describe-paths *location* *edges*)
        (describe-objects *location* *objects* *object-locations*)))

(defun walkTo (direction)
    (let ((next (find direction 
                (cdr (assoc *location* *edges*)) :key #'cadr)))
            (if next
                (progn (setf *location* (car next))
                        (look))
                '(you can't go to that direction.))))

(defun pickup (object)
    (cond ((member object
                (object-at *location* *objects* *object-locations*))
        (push (list object 'body) *object-locations*)
        `(you pickuped the ,object))
        (t '(you can't get that. fuck you!))))

(defun inBag ()
    (cons 'items- (object-at 'body *objects* *object-locations*)))

;gameのREPL関数
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string 
                (concatenate 'string "(" (read-line) ")"))))
        (flet ((quote-it (x)
                    (list 'quote x)))
            (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (cmd)
    (if (member (car cmd) *ok-commands*)
        (eval cmd)
        '(I dont know that command! fuck you!)))

(defun game-print (lst)
    (princ (coerce (adjustText (coerce (string-trim "() "
                                        (prin1-to-string lst))
                                        'list)
                                t
                                nil)
                    'string))
            (fresh-line))

(defun adjustText (lst caps lit)
    (when lst
        (let ((item (car lst))
                (rest (cdr lst)))
            (cond ((eql item #\space) (cons item (adjustText rest caps lit)))
                    ((member item '(#\! #\? #\.)) (cons item (adjustText rest t lit)))
                    ((eql item #\") (adjustText rest caps (not lit)))
                    (lit (cons item (adjustText rest nil nil)))
                    (caps (cons (char-upcase item) (adjustText rest nil lit)))
                    (t (cons (char-downcase item) (adjustText rest nil nil)))))))

(defun test ()
    (print (look))
    (print (walk 'north))
    (print (pickup 'macbookpro))
    (print (bag)))
