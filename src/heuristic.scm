;;yay heuristics!
;;they taste like rasins
(declare (unit heuristic))
(require-library parse-input)

(define (score-world world)
  (define HUGCOST 10000)
  (define MANHATTANDISTCOST 10)
  (define DEATHCOST 200000)
 (+ (* HUGCOST (count-hugs world))
     (* MANHATTANDISTCOST (manhattan-dist-to-hug world))
     (* DEATHCOST (i-am-dead? world))
    )
)

(define (count-hugs world)
  (count 'hug world)
)
(define (count-earth world)
  (count 'earth world)
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
      (list 1 2)
      )
  )