(define g (lambda (x) (if #t 0 20)))
(define f (lambda (n) (g 1)))
(f 7)