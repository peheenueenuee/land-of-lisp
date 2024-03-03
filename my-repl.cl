(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defun my-repl ()
  (loop (print (eval (read)))))
