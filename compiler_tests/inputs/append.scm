(append '(a b c) '())
(append '() '(a b c))
(append '() '())
(append '(a b) '(c d))
(append '(a b) 'c)
(append '(a b) #\c)
(let ((x (list 'b)))
  (eq? x (cdr (append '(a) x))))