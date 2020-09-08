(let ()
        (let ((x '(#t #t #t #t (box #t))) (y (box #f)))
                ;;Extract the head and the tail of the list and perform operations on them
                (let ((h (car x)) (t (cdr x)))
                        (if (car t)
                                (if (unbox y)
                                        #t
                                        (unbox y))
                                #f))
                ;;Return x
                x)      
        ;;16 bytes should now be free on the previously filled heap
        (box #f)
	(displayln (box #f)))
