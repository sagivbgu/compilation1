((lambda (x . y) x) 2)
((lambda (x . y) y) 2)

((lambda (x y . z) x) 2 3)
((lambda (x y . z) y) 2 3)
((lambda (x y . z) z) 2 3)