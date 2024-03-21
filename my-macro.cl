(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defun my-length-v1 (ls)
  (if ls
    (1+ (my-length (cdr ls)))
    0))

(defun my-length (ls)
  (labels ((f (ls acc)
              (if ls
                (f (cdr ls) (1+ acc))
                acc)))
    (f ls 0)))
