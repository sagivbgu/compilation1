(define baf (lambda (f)
               (lambda (n)
                 (if (= n 5)
                     "end"
                     `(* ,n ,((f f) (+ n 1)))
                     ))))
((baf baf) 3)