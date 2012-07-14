(declare (unit heuristictest))
(require-library parse-input)
(require-library simulate)
(require-library heuristic)
(use test)

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
)
(test-group "score-world"
	    (test #t (number? (score-world 1 (list ) heuristictestworld1)))
	    (test 1 (- (score-world 1 (list ) heuristictestworld1)
		  (score-world 1 (list 'up) heuristictestworld1))
		  )
	    (test 0 (- (score-world 1 (list ) heuristictestworld1)
		  (score-world 1 (list 'wait) heuristictestworld1))
		  )
	    (test +inf.0 (heuristic-world 1 (list ) heuristictestworldDead))
)