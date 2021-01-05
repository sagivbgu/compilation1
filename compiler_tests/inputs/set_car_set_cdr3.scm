(define p '(1 (cons 2 3) 4))
(set-car! p ((lambda (x) x) 'x))
p
(set-cdr! p ((lambda (x y) y) 'x 'y))
p