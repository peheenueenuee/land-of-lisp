(load "my-macro")

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
             "xmlns:xlink" "http://www.w3.org/1999/xlink"
             version "1.1"
             width ,width
             height ,height)
        ,@body))

(defun brightness (color amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          color))

(defun single-style-string (att) ;; att: (name . value)
  (format nil "~a:~a" (car att) (cdr att)))

(defun styles-string (atts)
  (let1 style (mapcar #'single-style-string (pairs atts))
        (concatenate 'string
                     (car style)
                     (format nil "~{;~a~}" (cdr style)))))

(defun rgb-color-string (color)
  (format nil "~{rgb(~a,~a,~a)~}" color))

(defun style-fill-and-dark-stroke (color)
  (styles-string `("fill"
                   ,(rgb-color-string color)
                   "stroke"
                   ,(rgb-color-string (brightness color -100)))))

(defun circle (center radius style)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style style)))

