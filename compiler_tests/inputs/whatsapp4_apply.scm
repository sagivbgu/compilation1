(define funny
  (lambda (n)
    (if (zero? n)
        '()
        (apply cons `(ha ,(funny (- n 1)))))))

(define funnier
  (lambda (n)
    (if (zero? n)
        '()
        `(ha ,@(apply cons `(ha ,(funny (- n 1))))))))