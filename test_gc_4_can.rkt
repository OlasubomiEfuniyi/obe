(let ()
	(displayln 
		(let ((x 1) (y 2) (z 3) (a 4) (b 500000000) (c (cons #t '())))
			(displayln (>= x y))
			(displayln (<= y x))
			(displayln (= x a))
			(displayln (= a a))
			(displayln (car c))
			z)))

