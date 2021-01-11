(fold-right cons '() '(1 2 3 4))

(fold-right
  (lambda (x a) (+ a (* x x)))
  0 '(1 2 3 4 5))

(fold-right
  (lambda (x y a) (cons* x y a))
  '((with apologies))
  '(parting such sorrow go ya)
  '(is sweet gotta see tomorrow))