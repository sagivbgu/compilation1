(let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
(e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
(l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a ,b ,c ,d
,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d ,e ,f ,g
,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a ,b ,c ,d
,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f
,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h
,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b ,c ,d ,e
,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v 'v) (w
'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u
,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r
,s ,t ,u ,v ,w ,(let ((x 'x)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m
,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,(let ((y 'y) (z 'z)) `(,a ,b ,c ,d
,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,y
,z))))))))))))))))))))))))))))))))