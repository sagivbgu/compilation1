(apply cons* 0 `((3 1) 6 ,@(append '(4 4) '(4))))
(apply cons* 0 `(,(+ 3 1) ,@(append '(4 4) '(4))))
(apply cons* 0 `(,(+ 3 1) ,(append '(4 4) '(4))))