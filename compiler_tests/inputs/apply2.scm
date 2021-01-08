(define first
  (lambda (ls)
    (apply (lambda (x . y) x) ls)))
(define rest
  (lambda (ls)
    (apply (lambda (x . y) y) ls)))

(first '(a b c d))
(rest '(a b c d))