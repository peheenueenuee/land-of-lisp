(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. GAME OVER."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a varliant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3))))))
         (unless (monsters-dead)
           (monster-hit (random-monster) 1)))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
      (random-monster)
      m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((input (read)))
    (if (not (and (integerp input) (>= input 1) (<= input *monster-num*)))
      (progn (princ "That is not a valid monster number.")
             (pick-monster))
      (let ((m (aref *monsters* (1- input))))
        (if (monster-dead m)
          (progn (princ "That monster is alread dead.")
                 (pick-monster))
          m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
             (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health m))
                    (princ ") ")
                    (monster-show m))))
         *monsters*)))

;PURE FUNCTION

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun randval (n)
  (1+ (random (max 1 n))))

;STRUCT GENERIC MONSTER
(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (progn (princ "You killed the ")
           (princ (type-of m))
           (princ "! "))
    (progn (princ "You hit the ")
           (princ (type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

;STRUCT MONSTERS

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((power (randval (orc-club-level m))))
    (princ "An orc swings his club at you knocks off ")
    (princ power)
    (princ " of your health points. ")
    (decf *player-health* power)))
