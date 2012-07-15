(declare (unit dog))
(declare (uses parse-input))

(define (passable? board x y) (not (not (member (board-ref board x y) '(empty earth hug closed-lift open-lift robot)))))

;;Do floyd warhsall on the board directly, fuck making a graph
(define (path-cost x1 y1 x2 y2 paths board)
  (vector-ref paths (path-is-at-board x1 y1 x2 y2 board) )
  )
(define (path-is-at-board x1 y1 x2 y2 board)
  (let* ((bh (board-height board))
	 (bw (board-width board))
	 (elementcount (* bh bw))
	 (elementpos   (+ (* elementcount (+ x1 (* bw y1)))
			  (+ x2 (* bw y2))))
	 )
    elementpos
    )
)

(define (hobofloydwarshall board)
   ;;Initial ninjaness
  (let* ((bh (board-height board))
	 (bw (board-width board))
	 (elementcount (* bh bw))
	 (paths (make-vector (* elementcount elementcount) +inf.0)))
    (define (path-is-at x1 y1 x2 y2)
      (+
       (* elementcount (edge-number x1 y1))
       (edge-number x2 y2)))
    (define (edge-number x y)
      (+ x (* bw y))
   )
    (define (path-update-cost x1 y1 x2 y2 cost)
      (vector-set! paths (path-is-at x1 y1 x2 y2) cost )
    )
    (define (path-update-cost-p i j cost)
      (vector-set! paths (+ (* elementcount i) j) cost )
    )
    (define (path-get-cost x1 y1 x2 y2)
      (vector-ref paths (path-is-at x1 y1 x2 y2) )
      )
    (define (path-get-cost-p i j)
      (vector-ref paths (+ (* elementcount i) j))
      )
   (define (makeitsexy k i j)
     (cond
      ((eq? k elementcount) (makeitsexy 0 (+ 1 i) j))
      ((eq? i elementcount) (makeitsexy 0 0 (+ 1 j)))
      ((eq? j elementcount) "done")
      (else 
       (if (and 
	    (eq? i (edge-number 1 0))
	    (eq? j (edge-number 3 0))
	    (eq? k (edge-number 2 0)))
	   #f
;;	   (display "considering the sexyness line")
	   #f
       )
       (path-update-cost-p i j (min (path-get-cost-p i j) 
				  (+ (path-get-cost-p i k) (path-get-cost-p k j))))
       (makeitsexy (+ 1 k) i j)
       )
      )
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
   (makeitsexy 0 0 0)
   paths
   )
  )