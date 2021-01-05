(define not
  (lambda (x) (if x #f #t)))
(not #t)
(not #f)
(not (not (not #t)))