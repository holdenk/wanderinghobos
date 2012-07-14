(declare (unit dog))
(declare (uses parse-input))

(define (passable? board x y) (member (board-ref board x y) '(empty earth hug closed-lift open-lift)))

;;Do floyd warhsall on the board directly, fuck making a graph
(define (path-cost paths x1 y1 x2 y2)
  (vector-ref paths (path-is-at x1 y1 x2 y2) )
  )

(define (hobofloydwarshall board)
   ;;Initial ninjaness
  (let* ((bh (board-height board))
	 (bw (board-width board))
	 (elementcount (* bh bw))
	 (paths (make-vector (* elementcount elementcount) +inf.0)))
    (define (path-is-at x1 y1 x2 y2)
      (+ (* elementcount (+ x1 (* bw y1)))
      (+ x2 (* bw y2)))
      )
    (define (path-update-cost x1 y1 x2 y2 cost)
      (vector-set! paths (path-is-at x1 y1 x2 y2) cost )
    )
    (define (path-get-cost x1 y1 x2 y2)
      (vector-ref paths (path-is-at x1 y1 x2 y2) )
      )
    ;;Initial setup of paths
   (for-each-board-index
     (lambda (x y)
       (path-update-cost x y x y 0)
       (when (passable? board (- x 1) y) (path-update-cost x y (- x 1) y 1))
       (when (passable? board (+ x 1) y) (path-update-cost x y (+ x 1) y 1))
       (when (passable? board x (- y 1)) (path-update-cost x y x (- y 1) 1))
       (when (passable? board x (+ y 1)) (path-update-cost x y x (+ y 1) 1)))
     board
     )
   paths
   )
  )