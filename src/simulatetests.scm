(declare (unit simulatetest))
(declare (uses parse-input))
(declare (uses simulate))
(declare (uses heuristic))
(declare (uses search))
(declare (uses list-utils))
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
	    (test 0 (best-moves heuristic-world (file->world "../tests/contest1.map") 100)))
