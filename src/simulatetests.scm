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
      #(open-lift rock  empty rock wall)
      #(wall wall  wall  wall wall))))

(define smashed-world
  (dry-world
   '#(
      #(wall empty robot empty wall)
      #(wall empty empty empty wall)
      #(open-lift rock  rock  rock  wall)
      #(wall wall  wall  wall  wall))))

(test-group "test-simulator"
	    (test (world-board smashed-world) (world-board  (simulate quantum-world))))



 (define (foo x)
   ;;fuckit
   (set! *best-node-so-far* #f)
   (set! *best-node* #f)
   (set! flw #f)
   ;;i've got a time machine
   (let* 
 		((world (file->world x))
		 (board (world-board world))
 		 (the-world (fuckerquest-test (file->world x) (+ 250 (* 5 (board-height board) (board-width board))) 3)))
		(display (output-moves (vector-ref the-world 2)))
;;		(display (vector-ref the-world 1))
 	      (score-world (count-hugs (file->world x)) (vector-ref the-world 2)  (vector-ref the-world 1))))

;; (test-group "flood4" 
;; 	    (test 1 (file->world "tests/contest1.map")))

;;(test-group "con1" 
;;	    (test 1 (output-moves (vector-ref (best-move (file->world "tests/contest1.map") 1000) 2))))


(define meworld 
  (string->world "###########
#....     #
#.******* #
#.\\\\\\\\\\\\\\R#
#.       .#
#..*\\\\\\*..#
#.#*\\\\\\*#.#
#########L#
"))

(test-group "should-die"
	    (test #t (i-am-dead? (simulate (move-robot meworld 'down)))))

(test-group "some-runs"
						(let ((the-cases (list (cons 212    "contest1")
																	 ;; (cons 281    "contest2")
																	 ;; (cons 275    "contest3")
																	 (cons 575    "contest4")
																	 ;; (cons 1303   "contest5")
																	 ;; (cons 1177   "contest6")
																	 ;; (cons 869    "contest7")
																	 ;; (cons 1973   "contest8")
																	 ;; (cons 3093   "contest9")
																	 ;; (cons 3643   "contest10")
																	 (cons 945    "flood1")
																	 ;; (cons 281    "flood2")
																	 ;; (cons 1303   "flood3")
																	 ;; (cons 1592   "flood4")
																	 (cons 575    "flood5")
																	 (cons 860    "beard1")
																	 (cons 4519   "beard2")
																	 (cons 1789   "beard3")
																	 (cons 3103   "beard4")
																	 (cons 665    "beard5"))))
							(foldl (lambda (acc the-case)
											 (let* ((score (car the-case))
															(name (cdr the-case))
															(file (string-append (string-append "tests/" name)
																									 ".map")))
												 (test name score (foo file))))
										 '()
										 the-cases)))
