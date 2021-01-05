(letrec ((func (lambda (n) (if n (func #f) 'moshe)))) (func #t))
;((lambda (func) (begin (set! func (lambda (n) (if n (func #f) 'moshe)))  (func #t))) 'whatever)