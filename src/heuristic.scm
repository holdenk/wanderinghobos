;;yay heuristics!
;;they taste like rasins
(declare (unit heuristic))
(declare (uses parse-input))
(use vector-lib)

(define heuristic-world fairly-simple-heuristic-world)

(define (fairly-simple-heuristic-world initialhugs path world)
  (define MANHATTANDISTCOST 1.1)
  (simple-heuristic-world initialhugs path world MANHATTANDISTCOST)
    )
(define (very-simple-heuristic-world initialhugs path world)
  (define MANHATTANDISTCOST 1)
  (simple-heuristic-world initialhugs path world MANHATTANDISTCOST)
    )
(define (simple-heuristic-world initialhugs path world MANHATTANDISTCOST)
  ;;Also they give you a lolipop after! OMG Ponies
  (if (eq? 0 (count-hugs world))
      ;;We got all of teh hugs
      (- 
       (* MANHATTANDISTCOST (manhattan-dist-to-lift world))
       (score-world initialhugs path world)
       )
      ;;We still have hugs
      (- 
       (* MANHATTANDISTCOST (manhattan-dist-to-hug world))
       (score-world initialhugs path world)
       )
    )
)

(define (score-world initialhugs path world)
  (let ((hugvalue (if (and (not (null? path)) 
			   (eq? 'abort (car path)))
		      50
		      (if (escaped? world)
			  75
			  25
			  )
		      ))
	(path-length (moves-in-path path))
	(board (world-board world))
	)
    ;;Section 3.1 resource limits
   (if (>= path-length (* (board-height board) (board-width board)))
	-inf.0
	(score-world-with-hug-value initialhugs path world hugvalue path-length))
    )
)

(define (score-world-with-hug-value initialhugs path world hugvalue path-length)
    (-
     (* hugvalue (- initialhugs (count-hugs world)))
     path-length
     (add-death-cost world)
     )
    )

;;Todo: make sure "wait" and "todo" don't count to moves-in-path  eh
(define (moves-in-path path)
  (length (filter (lambda (x) (not (or (eq? x 'wait)
				       (eq? x 'abort)))) path))  
)
;;We don't want to die unless we abort, abortions are FREE
(define (add-death-cost world)
  (if (i-am-dead? world)
      +inf.0
      0
      )
)

(define (count-hugs world)
  (count-obj 'hug world)
)
(define (count-earth world)
  (count-obj 'earth world)
)
(define (count-obj obj world)
  (vector-fold (lambda (i count v)
		(+ count (vector-count (lambda (i e) (eq? obj e)) v))
		)
	       0
	       (world-board world)
	       )
)

(define (manhattan-dist-to-hug world)
  (let
      (
       (robot (find-robot world))
       (hugs (find-hugs world))
      )
    (fold (lambda (hug currentdist) (if (eq? currentdist 0)
					(mdist robot hug)
					(min currentdist (mdist robot hug))
					)) 0 hugs)
      )
  )
(define (manhattan-dist-to-lift world)
  (let
      (
       (robot (find-robot world))
       (lift (find-lift world))
       )
    (mdist robot lift)
    )
  )



(define (mdist a b)
  (+ (abs (- (car a) (car b)))
   (abs (- (cadr a) (cadr b)))
   )
)
