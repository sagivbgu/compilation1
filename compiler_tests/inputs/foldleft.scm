(fold-left cons '() '(1 2 3 4))

(fold-left
  (lambda (a x) (+ a (* x x)))
  0 '(1 2 3 4 5))

(fold-left
  (lambda (a . args) (append args a))
  '(question)
  '(that not to)
  '(is to be)
  '(the be: or))

(fold-left
  (lambda (x a) x)
  #\s
  (string->list (make-string 100000)))