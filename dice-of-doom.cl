(load "lazy-eval")

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))
(defparameter *ai-level* 4)

;DIRTY IMPERATIVE
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ " "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex)) (second hex))))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(defun start-pvp (board-size)
  (setf *board-size* board-size)
  (setf *board-hexnum* (* *board-size* *board-size*))
  (play-vs-human (game-tree (gen-board) 0 0 t)))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
                          (unless (lazy-null moves)
                            (let* ((move (lazy-car moves))
                                   (action (car move)))
                              (fresh-line)
                              (format t "~a. " n)
                              (if action
                                (format t "~a -> ~a" (car action) (cadr action))
                                (princ "end turn. ")))
                            (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game with a tie between -a" (mapcar #'player-letter w))
      (format t "The winner is ~a !" (player-letter (car w))))))

(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
                     sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
                 (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path))
                                        (dice (cadr path))))
        (cadr move)
        (caddr move)))))

;CLEAN FUNCTIONAL CODE
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))
(let ((culc-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply culc-game-tree rest)))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons (list nil
                     (game-tree (add-new-dice board player
                                              (1- spare-dice))
                                (mod (1+ player) *num-players*)
                                0
                                t))
               moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos) (car (aref board pos)))
           (dice (pos) (cadr (aref board pos))))
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                     (lazy-mapcan (lambda (dst)
                                    (if (and (not (eq (player dst) cur-player))
                                             (> (dice src) (dice dst)))
                                      (make-lazy
                                        (list (list
                                                (list src dst)
                                                (game-tree
                                                  (board-attack board cur-player
                                                                src dst
                                                                (dice src))
                                                   cur-player
                                                   (+ spare-dice (dice dst))
                                                   nil)
                                                (game-tree
                                                  (board-attack-fail board cur-player
                                                                     src dst
                                                                     (dice src))
                                                  cur-player
                                                   (+ spare-dice (dice dst))
                                                   nil))))
                                      (lazy-nil)))
                                  (make-lazy (neighbors src)))
                     (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum* collect n)))))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))
(let ((culc-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall culc-neighbors pos)))))

;; 攻撃に成功した後のボード処理
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

;; 攻撃に失敗した後のボード処理
(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (if (eq pos src)
                               (list player 1)
                               hex))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
              (cond ((zerop n) (append (reverse acc) lst))
                    ((null lst) (reverse acc))
                    (t (let ((cur-player (caar lst))
                             (cur-dice (cadar lst)))
                         (if (and (eq cur-player player) (< cur-dice *max-dice*))
                           (f (cdr lst)
                              (1- n)
                              (cons (list cur-player (1+ cur-dice)) acc))
                           (f (cdr lst)
                              n
                              (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

(defun winners (board)
  (let* ((tally (loop for hex across board collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car (remove-if (lambda (x)
                               (not (eq (cdr x) best)))
                             totals))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
      (apply (if (eq (car tree) player)
               #'max
               #'min)
             (get-ratings tree player))
      (score-board (cadr tree) player))))
(let ((culc-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall culc-rate-position tree player))))))

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
      (if (eq (car tree) player)
        (apply #'max (ab-get-ratings-max tree player upper-limit lower-limit))
        (apply #'min (ab-get-ratings-min tree player upper-limit lower-limit)))
      (score-board (cadr tree) player))))

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
              (unless (lazy-null moves)
                (let ((x (ab-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                  (if (>= x upper-limit)
                    (list x)
                    (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
              (unless (lazy-null moves)
                (let ((x (ab-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                  (if (<= x lower-limit)
                    (list x)
                    (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
              (if (threatened pos board)
                1
                2)
              -1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                    (nplayer (car nhex))
                    (ndice (cadr nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return t))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
          (lazy-nil)
          (lazy-mapcar (lambda (move)
                         (list (car move)
                               (limit-tree-depth (cadr move) (1- depth))))
                       (caddr tree)))))

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                              (car tree)
                              most-positive-fixnum
                              most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))
