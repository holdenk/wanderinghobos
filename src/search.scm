(declare (unit search))

(define (a* world steps heur score)
	(cond
	 ((= 0 steps) '(0 0 0 0))
	 (else
		(let* ((directions '(left right up down wait))
					 (move-vertices (map (lambda (x) move-robot world x) directions)))
			(map
			 (lambda (w) (+ (score w) (a* w (- steps 1) heur score)))
			 move-vertices)))))


(define (make-queue-compare h)
	(lambda (a b)
		(cond
		 ((< (h (car a)) (h (car b))) -1)
		 ((> (h (car b)) (h (car a))) 1)
		 (else 0))))

(define the-queue '())

(define (queue-iter world h)
	(set! the-queue (make-queue-compare h))
	(ph-iter '() world))

(define (ph-iter steps world)
	(let* ((directions '(left right up down wait))
				 (new (map (lambda (dir)
										 (cons (move-robot world dir) (cons dir steps)))
									 directions)))
		(set! the-queue (foldl (lambda (ph entry)
														 (pairing-heap-insert entry ph))
													 the-queue
													 new)))
	(let ((e (pairing-heap-min the-queue)))
		(ph-iter (cdr e) (car e))))
