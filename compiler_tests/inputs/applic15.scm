(define f (lambda (x)
            (lambda (y)
              (if (= y 3)
                "end"
                `(* ,y ,((x x) (+ y 1)))
                ))))

((f f) 1)