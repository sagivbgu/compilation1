(define a '(1 2 3 4))
((lambda (x y z) ((lambda () (set! x y) x))) 1 2 3)