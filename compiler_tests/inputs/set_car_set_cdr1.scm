(define p '(1 (cons 2 3) 4))
(set-car! p #t)
p
(set-cdr! p '())
p