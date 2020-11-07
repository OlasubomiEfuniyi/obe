;;heap_size=224
(let ((x (cons #t '())) (y (cons #t '())) (z (cons #t '())))
	(for x in (..= 1 1000000 1) do
		;;At this point, there should be 24 bytes left on the heap.
		;;However, there should be 3 entries on the free list, each of
		;;size 32 bytes. I will use up 24 bytes each from the 3, leaving
		;; 3 entries of size 8 bytes each on the heap. The first
		;; list will not need the free list. It will use the 24 bytes left
		;; before the end of the heap. The second and the third lists will
		;; need the free list. The increment from 1 to 2 will use up 24 bytes
		;; from the last entry on the free list. To satisfy the increment from
		;; to to 3, a compaction must take place.
		;;Then, after printing, before the next iteration of the loop,
		;;the 3 24 byte chunks used by the lists will be freed, and the 24
		;;byte chunk used by 2 will be freed, leaving 4 24 byte chunks on the heap.
		;;These will be repeatedly reused by the body of the loop, without any need 
		;;for compaction. 
		(println x)))
