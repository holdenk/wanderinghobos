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

(test-group "some-runs"
	    (test 212 (vector-ref (best-move (file->world "tests/contest1.map") 100) 0))
	    (test 281 (vector-ref (best-move (file->world "tests/contest2.map") 100) 0))
	    (test 275 (vector-ref (best-move (file->world "tests/contest3.map") 100) 0))
	    (test 575 (vector-ref (best-move (file->world "tests/contest4.map") 100) 0))
	    (test 1303 (vector-ref (best-move (file->world "tests/contest5.map") 100)0))
	    (test 1177 (vector-ref (best-move (file->world "tests/contest6.map") 100)0))
	    (test 869 (vector-ref (best-move (file->world "tests/contest7.map") 100)0))
	    (test 1973 (vector-ref (best-move (file->world "tests/contest8.map") 100)0))
	    (test 3093 (vector-ref (best-move (file->world "tests/contest9.map") 100)0))
	    (test 3643 (vector-ref (best-move (file->world "tests/contest10.map") 100)0))
)
