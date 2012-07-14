(declare (uses parse-input))
(declare (uses search))
(declare (uses heuristic))
(declare (uses simulate))
(use list-utils sequences vector-lib)
(let* ((w (file->world (car (command-line-arguments))))
			 (wheight (vector-length (world-board w)))
			 (wwidth (car (vector->list
										 (vector-map (lambda (i e) (vector-length e))
																 (world-board w)))))
			 (moves (reverse (vector-ref (best-move w (* wheight wwidth)) 2))))
	(display w)
	(foldl (lambda (s m)
					 (cond
						((eq? m 'wait)
						 (let ((r (simulate s)))
							 (display r)
							 r))
						((eq? m 'abort)
						 (display s)
						 s)
						(else
						 (let ((r (simulate (move-robot s m))))
							 (display r)
							 r))))
				 w
				 moves))
(display "\n")