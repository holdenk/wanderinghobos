(declare (uses parse-input simulate heuristic search))
(use sequences vector-lib)
(let* ((world (parse-input))
			 (wheight (vector-length (world-board world)))
			 (wwidth (car (vector->list
										 (vector-map (lambda (i e) (vector-length e))
																 (world-board world)))))
			 (moves (reverse (vector-ref (best-move world (* wheight wwidth)) 2))))
	(display (output-moves moves)))

