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

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  (lazy (when lst (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defun lazy-mapcar (fn lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fn (lazy-car lst))
                (lazy-mapcar fn (lazy-cdr lst))))))

(defun lazy-mapcan (fn lst)
  (labels ((f (lst-cur)
              (if (lazy-null lst-cur)
                (force (lazy-mapcan fn (lazy-cdr lst)))
                (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fn (lazy-car lst)))))))

(defun make-num-ls (x)
  (make-lazy (loop for i from 1 to (1+ x) collect i)))

(defun lazy-find-if (fn lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fn x)
        x
        (lazy-find-if fn (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
    (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))))

(defmacro my-debug (sign &body body)
  `(progn (format t "there is ~a" ,sign)
          ,@body))

(defparameter *integers*
  (labels ((f (n) (lazy-cons n (f (1+ n)))))
    (f 1)))

(defparameter *lal*
  (make-lazy '(1)))

