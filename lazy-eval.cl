(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (c)
  (car (force c)))

(defun lazy-cdr (c)
  (cdr (force c)))

(defun lazy-nill ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defparameter *integers*
  (labels ((f (n) (lazy-cons n (f (1+ n)))))
    (f 1)))
