(declare (unit heuristictest))
(declare (uses parse-input))
(declare (uses simulate))
(declare (uses heuristic))
(use test)

(define world-test-1
 (string->world
  "######
#. *R#
#  \\.#
#\\ * #
L  .\\#
######"))

(define world-test-2
 (string->world
  "######
#. * #
#  \\R#
#\\ * #
L  .\\#
######"))

(define world-test-3
 (string->world
  "######
#. * #
#  R #
#\\ * #
L  .\\#
######"))



(define (dry-world board) (make-world board +inf.0 +inf.0 +inf.0 0 0 '()))
(define heuristictestworld1
 (dry-world
  '#(#(wall rock earth empty wall)
     #(wall hug robot empty wall)
     #(wall wall wall wall wall))))

(define heuristictestworld2
 (dry-world
  '#(#(wall rock earth empty wall)
     #(wall robot hug empty wall)
     #(wall wall wall wall wall))))

(define heuristictestworld3
 (dry-world
  '#(#(wall rock earth empty robot)
     #(wall wall hug empty wall)
     #(wall wall wall wall wall))))

(define heuristictestworld4
 (dry-world
  '#(#(wall rock earth empty robot)
     #(wall wall wall empty wall)
     #(wall wall wall wall wall))))

(define heuristictestworld5
 (dry-world
  '#(#(hug rock earth empty robot)
     #(wall wall wall empty hug)
     #(wall wall wall wall wall))))
(define heuristictestworldDead
  (simulate (move-robot  
	     (dry-world
	      '#(#(wall rock earth hug wall)
		 #(wall empty empty rock wall)
		 #(wall empty robot empty wall)
		 #(wall wall wall wall wall)))
	     'left))
)
(define heuristictestworldNoHugsNotEscaped
  (dry-world
   '#(#(wall rock earth rock wall)
      #(wall empty empty rock wall)
      #(wall empty robot open-lift wall)
      #(wall wall wall wall wall)))
)
(define heuristictestworldNoHugsEscaped
  (dry-world
   '#(#(wall rock earth rock wall)
      #(wall empty empty rock wall)
      #(wall empty robot empty wall)
      #(wall wall wall wall wall)))
)



(test-group "find-the-robot"
	    ;;It is not Zoidbergs turn
	    (test (list 2 1) (find-robot heuristictestworld1))
	    (test (list 1 1) (find-robot heuristictestworld2))
)
(test-group "find-the-hug"
	    (test (list (list 1 1)) (find-hugs heuristictestworld1))
	    (test (list (list 2 1)) (find-hugs heuristictestworld2))
)
(test-group "dist-to-hug-tests"
	    (test 1 (manhattan-dist-to-hug heuristictestworld1))
	    (test 1 (manhattan-dist-to-hug heuristictestworld2))
	    (test 3 (manhattan-dist-to-hug heuristictestworld3))
	    (test 0 (manhattan-dist-to-hug heuristictestworld4))
	    (test 1 (manhattan-dist-to-hug heuristictestworld5))
	    (test 1 (manhattan-dist-to-hug world-test-2))
	    (test 2 (manhattan-dist-to-hug world-test-1))
)
(test-group "score-world"
	    (test #t (number? (score-world 1 (list ) heuristictestworld1)))
	    (test 1 (- (score-world 1 (list ) heuristictestworld1)
		  (score-world 1 (list 'up) heuristictestworld1))
		  )
	    (test 0 (- (score-world 1 (list ) heuristictestworld1)
		  (score-world 1 (list 'left) heuristictestworld1))
		  )
	    (test +inf.0 (heuristic-world 1 (list ) heuristictestworldDead))
	    (test 75 (score-world 1 (list ) heuristictestworldNoHugsEscaped))
	    (test 25 (score-world 1 (list ) heuristictestworldNoHugsNotEscaped))
	    (test 50 (score-world 1 (list 'abort) heuristictestworldNoHugsNotEscaped))
	    (test 0 (score-world 3 (list ) world-test-1))
	    (test -1 (score-world 3 (list 'up) world-test-2))
	    (test 2.2 (fairly-simple-heuristic-world 3 (list ) world-test-1))
	    (test 2.1 (fairly-simple-heuristic-world 3 (list 'up) world-test-2))
	    (test -20.7 (fairly-simple-heuristic-world 3 (list 'left) world-test-3))
	    (test 2 (very-simple-heuristic-world 3 (list ) world-test-1))
	    (test 2 (very-simple-heuristic-world 3 (list 'up) world-test-2))
	    (test -21 (very-simple-heuristic-world 3 (list 'left) world-test-3))

)
