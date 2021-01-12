(define c ((lambda (x) (cons (lambda () (set! x '())) (lambda () x))) 5))
(define i (cdr c))
(i)
(eq? (i) 5)
