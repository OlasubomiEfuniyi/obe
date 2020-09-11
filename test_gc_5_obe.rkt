;;This program fills up the heap and once the variables go out of scope, triggers garbage collection. Non of the garbage collected chunks are large enough to fufil a request to create a range. This will trigger compaction.
(let ()
	(let ((b1 (box #t)) (b2 (box #t)) (b3 (box #t)) (b4 (box #t)) 
		(b5 (box #t)) (b6 (box #t)) (b7 (box #t)) (b8 (box #t)) 
		(lst (cons #t '())))
	(if #t
		(println #t)
		(println #f))
	#t)
	
	(for i in (.. 1 1000000 2) do
		(println i)))


 
