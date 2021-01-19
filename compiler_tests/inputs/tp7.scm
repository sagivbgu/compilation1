(letrec 
	((sum (lambda (acc n) (if (= n 0) acc (sum (+ acc n) (+ n -1)))))) 
	(sum 0 100000))