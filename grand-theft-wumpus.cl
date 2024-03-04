(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))
(defun make-nodes ()
  (loop for n from 1 to *node-num* collect n))

(defun make-city-edges ()
  (let* ((nodes (make-nodes))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (_)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms) '(glow-worms))
                                ((some (lambda (worm) (within-one n worm edge-alist))
                                       glow-worms) '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist))) '(sirens!))))))
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun draw-city ()
  (ugraph->png "city.dot" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
              (let ((known-node (assoc node *congestion-city-nodes*)))
                (if (eql node *player-pos*)
                  (append known-node '(*))
                  known-node))
              (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car (cdr (assoc node *congestion-city-edges*))))
                            *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (dest-node)
                                 (if (member (car dest-node) *visited-nodes*)
                                   dest-node
                                   (list (car dest-node))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city.dot" (known-city-nodes) (known-city-edges)))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos*
                                 *congestion-city-edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worms node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. GAME OVER."))
          ((member 'wumpus node) (if charging
                              (princ "You found the Wumpus!")
                              (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. GAME OVER."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

; PURE FUNCTION
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
                          (let* ((connected (get-connected (car nodes) edge-list))
                                 (unconnected (set-difference nodes connected)))
                            (push connected islands)
                            (when unconnected (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1 (mapcar (lambda (edge) (list (cdr edge)))
                                (remove-duplicates (direct-edges node1 edge-list)
                                                   :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edge-with-cops)
  (mapcar (lambda (item)
            (let ((node1 (car item))
                  (node1-edges (cdr item)))
              (cons node1 (mapcar (lambda (edge)
                                    (let ((node2 (car edge)))
                                      (if (intersection (edge-pair node1 node2)
                                                        edge-with-cops
                                                        :test #'equal)
                                        (list node2 'cops)
                                        edge)))
                                  node1-edges))))
          edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (node-a node-b edge-alist)
  (member node-b (neighbors node-a edge-alist)))

(defun within-two (node-a node-b edge-alist)
  (or (within-one node-a node-b edge-alist)
      (some (lambda (node-x)
              (within-one node-x node-b edge-alist))
            (neighbors node-a edge-alist))))
