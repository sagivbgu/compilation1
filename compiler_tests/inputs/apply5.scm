(define f (lambda (x . y) y))
(apply f 1 '())
(apply f 1 '(2))
(apply f 1 '(2 3))
(apply f 1 2 '())
(apply f 1 2 '(3))
(apply f 1 2 '(3 4))
(apply f 1 2 3 '())
(apply f 1 2 3 '(4))
(apply f 1 2 3 '(4 5))
