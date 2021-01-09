(define f (lambda (x)
            (begin
              (lambda (y)
                (if (= y 3)
                  "end"
                  `(* ,y ,((x x) (+ y 1)))
                  ))
              (lambda (y)
                (if (= y 3)
                  "end"
                  ((x x) (+ y 1))
                  )))))

((f f) 1)