(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defun add (a b)
  (let1 x (+ a b) (format t "The sum is ~a" x)
        x))

(defun my-length-v1 (ls)
  (if ls
    (1+ (my-length (cdr ls)))
    0))

(defun my-length-v2 (ls)
  (labels ((f (ls acc)
              (if ls
                (f (cdr ls) (1+ acc))
                acc)))
    (f ls 0)))

(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                 (let ((head (car ,g))
                       (tail (cdr ,g)))
                   ,yes)
                 ,no))))

(defun my-length-v3 (ls)
  (labels ((f (ls acc)
              (split ls
                     (f tail (1+ acc))
                     acc)))
    (f ls 0)))

(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                     (if tail
                       (f (cdr tail) (cons (cons head (car tail)) acc))
                       (reverse acc))
                     (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
        `(labels ((self ,(mapcar #'car p) ,@body))
           (self ,@(mapcar #'cdr p)))))

(defun my-length (ls)
  (recurse (ls ls
            acc 0)
           (split ls
                  (self tail (1+ acc))
                  acc)))
