(letrec ((func (lambda (n) (if n (func #f) 'moshe)))) (func #t))
;((lambda (func) (begin (set! func (lambda (n) (if n (func #f) 'moshe)))  (func #t))) 'whatever)
;run_semantics (List.hd (tag_parse_expressions (read_sexprs "(letrec ((func (lambda (n) (if n (func #f) 'moshe)))) (func #t))")));;