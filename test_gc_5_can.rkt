(let ()
        (let ((b1 (box #t)) (b2 (box #t)) (b3 (box #t)) (b4 (box #t))
                (b5 (box #t)) (b6 (box #t)) (b7 (box #t)) (b8 (box #t))
                (lst (cons #t '())))
        (if #t
                (displayln #t)
                (displayln #f))
        #t)

        (for ([i (in-range 1 1000000 2)])
                (displayln i))
	(displayln #t))

