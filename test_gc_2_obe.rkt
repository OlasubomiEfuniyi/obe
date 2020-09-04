(let ()
	(let ((x 1))
		(let ((y 2) (lst (cons #t '())))
			(let ((z (box (tail lst))) (a 3))
				#t)
			(let ((b 4) (c (box y)))
				#f)))
	(for x in (..= 1 1000000 1) do
		(println x)))
