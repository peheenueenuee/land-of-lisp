(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
      (code-char code)
      default)))

(defun http-byte (c1 c2 &optional (default #.(char-code #\Space)))
  (let ((code (parse-integer
                (coerce (list (code-char c1) (code-char c2)) 'string)
                :radix 16
                :junk-allowed t)))
    (or code default)))

(defun decode-param (s)
  (labels ((f (lst)
              (when lst
                (case (car lst)
                  (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
                             (f (cdddr lst))))
                  (#.(char-code #\+) (cons #.(char-code #\space) (f (cdr lst))))
                  (otherwise (cons (car lst) (f (cdr lst))))))))
    (ext:convert-string-from-bytes
      (coerce (f (coerce (ext:convert-string-to-bytes s charset:utf-8) 'list))
              'vector)
      charset:utf-8)))
