;;yay heuristics!
;;they taste like rasins
(declare (unit heuristic))
(require-library parse-input)

(define (score-world world)
  (define HUGCOST 10000)
  (define MANHATTANDISTCOST 10)
  (define DEATHCOST +inf.0);;We don't want to die unless we abort, abortions are FREE
  ;;Also they give you a lolipop after! OMG Ponies
 (+ (* HUGCOST (count-hugs world))
     (* MANHATTANDISTCOST (manhattan-dist-to-hug world))
     (* DEATHCOST (i-am-dead? world))
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
	       world
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

(define (mdist a b)
  (+ (abs (- (car a) (car b)))
   (abs (- (cadr a) (cadr b)))
   )
)