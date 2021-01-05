(define p '(1 (cons 2 3) 4))
(set-car! p (cons #t #f))
p
(set-cdr! p (cons (+ 1 2) 10))
p