(equal? 3.0 3.0)
(equal? 3.0 3)
(equal? 3.0 -3.0)
(equal? 3.0 3/1)

(equal? 3 3.0)
(equal? 3 3)
(equal? 3 -3.0)
(equal? 3 3/1)

(equal? 3/1 3.0)
(equal? 3/1 3)
(equal? 3/1 -3.0)
(equal? 3/1 3/1)

(equal? 6/2 3.0)
(equal? 6/2 3)
(equal? 6/2 -3.0)
(equal? 6/2 3/1)

(equal? 0.0 0.0)
(equal? 0.0 0)
(equal? 0.0 -0)
(equal? 0.0 -0.0)
(equal? 0.0 0/2)

(equal? 0 0.0)
(equal? 0 0)
(equal? 0 -0)
(equal? 0 -0.0)
(equal? 0 0/2)

(equal? -5/6 (/ 5 -6))

(equal? car car)
(equal? car 'car)
(equal? 'car car)
(equal? 'sym 'sym)

(equal? (lambda (x) x) (lambda (x) x))
(equal? '(lambda (x) x) '(lambda (x) x))
(equal? '() '())
(equal? '() (cons '() '()))
(equal? '() (cons 1 '()))

(equal? '(a) 'a)
(equal? #\f #\f)
(equal? #f #\f)
(equal? #\f #f)
(equal? #f #f)

(equal? '(a b c) `(a b c))
(equal? "hi" "hi")
(equal? "hi" "HI")

(equal? #f (not #t))