(define baf (lambda (f)
               (lambda (n)
                 (if (> n 0)
                     `(* ,n ,((f f) (- n 1)))
                     "end"))))
((baf baf) 3)