(let ()
	(for ([x (in-range 1 1000001 1)])
		(displayln x))
	;;Obe's for loop returns #t but racket does not display return values when evaluated as it is now
	(displayln #t))
