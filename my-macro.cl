(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defun my-length (ls)
  (if ls
    (1+ (my-length (cdr ls)))
    0))
