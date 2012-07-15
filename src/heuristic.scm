;;yay heuristics!
;;they taste like rasins
(declare (unit heuristic))
(declare (uses parse-input dog))
(use vector-lib)
(define flw #f)

(define (fairly-simple-hobofloydwarshall-world score initialhugs path world)
  (define DISTCOST 1.1)
  (simple-hobofloydwarshall-world score initialhugs path world DISTCOST (lambda (w) (or (world-fuckedrocks w) (not (null? (world-rocks w))))) #t))

(define (once-hobofloydwarshall-world score initialhugs path world)
  (define DISTCOST 1.1)
  (simple-hobofloydwarshall-world score initialhugs path world DISTCOST (lambda (w) #f) #f))


;; (define (once-hobo-world initialhugs path world)
;;   (define DISTCOST 1.1)
;;   (simple-hobofloydwarshall-world initialhugs path world DISTCOST
(define (simple-hobofloydwarshall-world score initialhugs path world DISTCOST fn doreachabletest)
(define cost
 (if (escaped? world)
     (- score)
     ;;Also they give you a lolipop after! OMG Ponies
     (begin
       (if (or
	    (fn world)
	    (not flw))
	   (set! flw (hobofloydwarshall (world-board world)))
	   #f
	   )
       (if (eq? 0 (count-hugs world))
	   ;;We got all of teh hugs
	   (- 
	    (* DISTCOST (floyd-dist-to-lift flw world))
	    score
	    )
	   ;;We still have hugs
	   (- 
	    (* DISTCOST (floyd-dist-to-hug flw world))
	    score
	    (if doreachabletest
		(* 5 (reachable-hugs flw world))
		;;remember kids, drugs arefun!
		0;;we can't do reachability if we don't update the happy pandas
		;;so yah sad panda face
		)
	    )
	   )
       )))
cost
)

(define (fairly-simple-fifty-heuristic-world score initialhugs path world)
  (define MANHATTANDISTCOST 1.1)
  (simple-minhugvalue-heuristic-world score initialhugs path world MANHATTANDISTCOST 50)
    )
(define (fairly-simple-heuristic-world score initialhugs path world)
  (define MANHATTANDISTCOST 1.1)
  (simple-heuristic-world score initialhugs path world MANHATTANDISTCOST)
    )
(define (very-simple-heuristic-world score initialhugs path world)
  (define MANHATTANDISTCOST 1)
  (simple-heuristic-world score initialhugs path world MANHATTANDISTCOST)
    )
(define (simple-heuristic-world score initialhugs path world MANHATTANDISTCOST)
 (if (escaped? world)
     (- score)
     ;;Also they give you a lolipop after! OMG Ponies
     (if (eq? 0 (count-hugs world))
         ;;We got all of teh hugs
         (- 
          (* MANHATTANDISTCOST (manhattan-dist-to-lift world))
	  score
          )
         ;;We still have hugs
         (- 
          (* MANHATTANDISTCOST (manhattan-dist-to-hug world))
	  score
          )
         )
     )
 )

(define (simple-minhugvalue-heuristic-world score initialhugs path world MANHATTANDISTCOST minhugvalue)
 (if (escaped? world)
     (- score)
     ;;Also they give you a lolipop after! OMG Ponies
     (if (eq? 0 (count-hugs world))
         ;;We got all of teh hugs
         (- 
          (* MANHATTANDISTCOST (manhattan-dist-to-lift world))
          (score-world-with-min-hug-value initialhugs path world minhugvalue)
          )
         ;;We still have hugs
         (- 
          (* MANHATTANDISTCOST (manhattan-dist-to-hug world))
          (score-world-with-min-hug-value initialhugs path world minhugvalue)
          )
         )
     )
 )


;;Hugs remaining and makes sexy
(define (crazy-heuristic-world score initialhugs path world)
  (let ((hugcount (count-hugs world)))
    (if (escaped? world)
	(- (score-world initialhugs path world))
	;;Also they give you a lolipop after! OMG Ponies
	(if (eq? 0 hugcount)
	    ;;We got all of teh hugs
	    (- 
	     (* MANHATTANDISTCOST (manhattan-dist-to-lift world))
	     (score-world-with-min-hug-value initialhugs path world 50)
	     )
	    ;;We still have hugs
	    (- 
	     (/ hugcount (manhattan-dist-to-hug world))
	     (score-world-with-min-hug-value initialhugs path world 50)
	     )
	    )
	)
    )
)

;;Currently we use fairly simple
;;(define heuristic-world fairly-simple-heuristic-world)
(define heuristic-world fairly-simple-heuristic-world)
;;(define heuristic-world fairly-simple-hobofloydwarshall-world)

(define heuristic-list-test (list fairly-simple-heuristic-world  very-simple-heuristic-world once-hobofloydwarshall-world))
;;(define heuristic-list-test (list fairly-simple-hobofloydwarshall-world))
(define heuristic-list-prod (list fairly-simple-heuristic-world  very-simple-heuristic-world once-hobofloydwarshall-world))

(define (score-world initialhugs path world)
  (score-world-with-min-hug-value initialhugs path world 25)
)

;;For most cases we want the minhugvalue to be 25, but for our heuristic we might want
;;to try and use different values (like 50) since we can abort randomly.
(define (score-world-with-min-hug-value initialhugs path world minhugvalue)
  (let ((hugvalue (max minhugvalue 
		       (if (and (not (null? path)) 
				(eq? 'abort (car path)))
			   50
			   (if (escaped? world)
			       75
			       minhugvalue
			       )
			   )))
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
  (length (filter (lambda (x) (not (eq? x 'abort)))
		  path))
)
;;We don't want to die unless we abort, abortions are FREE
(define (add-death-cost world)
  (if (i-am-dead? world)
      +inf.0
      0
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

(define (floyd-dist-to-lift ftw world)
  (let
      (
       (robot (find-robot world))
       (lift (find-lift world))
       )
    (path-cost (car robot) (cadr robot) (car lift) (cadr lift) ftw (world-board world))
    )
  )

(define (floyd-dist-to-hug ftw world)
  (let
      (
       (robot (find-robot world))
       (hugs (find-hugs world))
      )
    (fold (lambda (hug currentdist) (if (eq? currentdist 0)
					(path-cost (car robot) (cadr robot) (car hug) (cadr hug) ftw (world-board world))
					(min currentdist (path-cost (car robot) (cadr robot) (car hug) (cadr hug) ftw (world-board world)))
					)) 0 hugs)
      )
  )

;;Its a me MARIO!
;;by the way the K stands for kwality
;;and remember your drug testing kits from dance safe kids!
(define (reachable-hugs ftw world)
  (let
      (
       (robot (find-robot world))
       (hugs (find-hugs world))
      )
    (length (filter (lambda (h) (not (eq? +inf.0 (path-cost (car robot) (cadr robot) (car h) (cadr h) ftw (world-board world))))) hugs))
      )
  )



(define (mdist a b)
  (+ (abs (- (car a) (car b)))
   (abs (- (cadr a) (cadr b)))
   )
)
