(declare (unit simulatetest))
(declare (uses parse-input simulate heuristic search))
(use list-utils sequences)
(use srfi-1) 
(use test)


(define quantum-world
  (dry-world
   '#(
      #(wall empty robot empty wall)
      #(wall rock  empty rock wall)
      #(wall rock  empty rock wall)
      #(wall wall  wall  wall wall))))

(define smashed-world
  (dry-world
   '#(
      #(wall empty robot empty wall)
      #(wall empty empty empty wall)
      #(wall rock  rock  rock  wall)
      #(wall wall  wall  wall  wall))))

(test-group "test-simulator"
	    (test (world-board smashed-world) (world-board  (simulate quantum-world))))



 (define (foo x)
   ;;fuckit
   (set! *best-node-so-far* #f)
   (set! *best-node* #f)
   ;;i've got a time machine
   (let* 
 		((world (file->world x))
		 (board (world-board world))
		 (the-world (best-move-random-with-no-repeats (file->world x) (+ 100 (* 3 (board-height board) (board-width board))) 3)))
		(display (output-moves (vector-ref the-world 2)))
;;		(display (vector-ref the-world 1))
 	      (score-world (count-hugs (file->world x)) (vector-ref the-world 2)  (vector-ref the-world 1))))

(test-group "flood4" 
	    (test 1 (file->world "tests/contest1.map")))

;;(test-group "con1" 
;;	    (test 1 (output-moves (vector-ref (best-move (file->world "tests/contest1.map") 1000) 2))))


(test-group "some-runs" 
	    (test 212 (foo "tests/contest1.map"))
	    (test 281 (foo "tests/contest2.map"))
	    (test 275 (foo "tests/contest3.map"))
	    (test 575 (foo "tests/contest4.map"))
	    (test 1303 (foo "tests/contest5.map"))
	    (test 1177 (foo "tests/contest6.map"))
	    (test 869 (foo "tests/contest7.map"))
	    (test 1973 (foo "tests/contest8.map"))
	    (test 3093 (foo "tests/contest9.map"))
	    (test 3643 (foo "tests/contest10.map"))
	    (test 945  (foo "tests/flood1.map"))
	    (test 281  (foo "tests/flood2.map"))
	    (test 1303  (foo "tests/flood3.map"))
	    (test 1592  (foo "tests/flood4.map"))
	    (test 575  (foo "tests/flood5.map")))
