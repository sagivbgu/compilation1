(define complex-cycle '(1 (7 . 8) 6 (10 . 8)))
(define x (car (car (cdr (cdr (cdr complex-cycle)))))) ;2 
(define y (car (car (cdr complex-cycle)))) ;2
(* x y) 
