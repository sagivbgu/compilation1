(define f2 (lambda (x . y) ( + x (apply + y))) )
(f2 #;10 11 12 13 14 15 1.0 1 )
