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

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
      (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
      (cons url '()))))

(defun get-header (strm)
  (let* ((s (read-line strm))
         (h (let ((i (position #\: s)))
              (when i (cons (intern (string-upcase (subseq s 0 i)))
                            (subseq s (+ i 2)))))))
    (when h (cons h (get-header strm)))))

(defun get-content-params (strm header)
  (let ((length (cdr (assoc 'contents-length header))))
    (when length 
      (let ((content (make-string (parse-integer length))))
        (read-sequence content strm)
        (parse-params content)))))

(defun serve (request-handler)
  (let ((socket (socket-server 9010)))
    (unwind-protect
      (loop (with-open-stream (st (socket-accept socket))
              (let* ((url (parse-url (read-line st)))
                     (path (car url))
                     (header (get-header st))
                     (params (append (cdr url)
                                     (get-content-params st header)))
                     (*standard-output* st))
                (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun serve-test ()
  (let ((socket (socket-server 9010)))
    (unwind-protect
      (loop (with-open-stream (st (socket-accept socket))
              (let* ((url (parse-url (read-line st)))
                     (path (car url))
                     (header (get-header st))
                     (params (append (cdr url)
                                     (get-content-params st header)))
                     )
                (princ url)
                (princ path)
                (princ header)
                (princ params)
                (hello-request-handler path header params)
                )))
      (socket-server-close socket))))

(defparameter *pseudo-http-header* "HTTP/1.1 200 OK
Date: Wed, 13 Mar 2024 12:54:39 GMT
Server: Apache/2.4.58 (FreeBSD) OpenSSL/1.1.1t-freebsd mod_perl/2.0.12 Perl/v5.36.3
Last-Modified: Mon, 11 Apr 2005 17:34:29 GMT
Accept-Ranges: bytes
Content-Length: 2100
Keep-Alive: timeout=5, max=100
Connection: Keep-Alive
Content-Type: text/html


~a")

(defparameter *pseudo-http-header-string* "HTTP/1.1 200 OK
Date: Wed, 13 Mar 2024 12:54:39 GMT
Server: Apache/2.4.58 (FreeBSD) OpenSSL/1.1.1t-freebsd mod_perl/2.0.12 Perl/v5.36.3
Last-Modified: Mon, 11 Apr 2005 17:34:29 GMT
Accept-Ranges: bytes
Keep-Alive: timeout=5, max=100
Connection: Keep-Alive
Content-Type: text/html


")

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
    (let ((name (assoc 'name params)))
      (if (not name)
        (format t *pseudo-http-header*
                "<html><form>What is your name?<input name='name' /></form></html>" )
        (format t *pseudo-http-header*
                (format nil "<html>Nice to meet you, ~a!</html>" (cdr name)))))
    (format t *pseudo-http-header*
                "Sorry... I don't know that page." )))

#|
        (princ "<html><form>What is your name?<input name='name' /></form></html>")
HTTP/1.1 200 OK
Date: Wed, 13 Mar 2024 12:54:39 GMT
Server: Apache/2.4.58 (FreeBSD) OpenSSL/1.1.1t-freebsd mod_perl/2.0.12 Perl/v5.36.3
Last-Modified: Mon, 11 Apr 2005 17:34:29 GMT
ETag: "b0c-3f47c968bd340"
Accept-Ranges: bytes
Content-Length: 2828
Keep-Alive: timeout=5, max=100
Connection: Keep-Alive
Content-Type: text/html
|#
