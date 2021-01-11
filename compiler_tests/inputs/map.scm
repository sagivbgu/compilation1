(map + '(1 -2 3 -4 5 -6))

(map (lambda (x) (cons x #t)) '(1 -2 3 -4 5 -6))

(map (lambda (x y) (* x y))
     '(1 2 3 4)
     '(8 7 6 5))
