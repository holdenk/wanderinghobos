(declare (unit search))
(declare (uses simulate pairing-heap))
(use srfi-1)
(use list-utils random-bsd sequences)

;; Use signal handler, process until interrupt
;; Don't go somewhere twice

(define (remq x y)
    (cond ((null? y) y)
	  ((eq? x (car y)) (remq x (cdr y)))
	  (else (cons (car y) (remq x (cdr y))))))

(define (remq! x y)
    (cond ((null? y) y)
	  ((eq? x (car y)) (remq! x (cdr y)))
	  (else (let loop ((prev y))
		     (cond ((null? (cdr prev))
			    y)
			   ((eq? (cadr prev) x)
			    (set-cdr! prev (cddr prev))
			    (loop prev))
			   (else (loop (cdr prev))))))))

(define (map-n f n)
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (n-random-elements-without-replacement n x)
 (when (< (length x) n) (panic "Not enough elements"))
 (let loop ((x (map list x)) (l (length x)) (n n) (c '()))
  (if (zero? n)
      c
      (let ((e (list-ref x (random-integer l))))
       (loop (remq! e x) (- l 1) (- n 1) (cons (first e) c))))))

(define (random-integer n) (inexact->exact (floor (* (random-real) n))))

(define (deal x) (n-random-elements-without-replacement (length x) x))

(define moves-directions '(left right up down))
(define moves-other '(abort wait))
(define moves (append moves-directions moves-other))

(define *best-node-so-far* #f)
(define *best-node* #f)

(define (bestest-best! heap)
 (unless (pairing-heap-empty? heap)
  (let ((min (pairing-heap-min heap)))
   (when (or (not *best-node-so-far*) (> (vector-ref min 3) (vector-ref *best-node-so-far* 3)))
    (set! *best-node-so-far* min)))))


(define (get-neighbors node world)
  (filter (list (first moves) world) (lambda (p) (not (equal? (second p) #f))) (map (lambda (move) (list move (move-robot world move))) moves)))

(define (init-dist-inf world)
  (vector-unfold (lambda (x) (list +inf.0 '())) (world-length world)))

(define (min-dist-node dist unvisited)
  (let ((get-min-dist (lambda (index best x) (if (and (not (equal? #f (memq index unvisited))) (> (first (vector-ref dist best)) (first (vector-ref dist index)))) index best))))
     (vector-fold get-min-dist 0 dist)))

(define (remove-node unvisited node)
  (filter unvisited (lambda (p) (not (equal? p node))) unvisited))

(define (dist-between evaluator world node robot-node)
  (evaluator (list (world-ref world node) (world-ref world robot-node )) world))

(define (fuckfuck dist node)
  (first (vector-ref dist node)))

(define (dijkstra evaluator world source-node)
  (letrec ((initial-hugs (count-hugs world))
         (dist (init-dist-inf world))
         (evaluator1 (lambda (path world) (evaluator initial-hugs path world)))
         (check-nodes (lambda (evaluator2 unvisited world2)
            (if (empty? unvisited) dist
	        (let* ((node (min-dist-node dist unvisited))
                       (neighbors (get-neighbors node world2)))
                  (if (equal? (first (vector-ref dist node)) +inf) dist
                    (begin
                      (map (lambda (candidate)
                         (update-best! (vector (first candidate) (second candidate)))
                         (let* ((robot-node (find-robot (second candidate)))
                                (robot-node-index (* (first robot-node) (second robot-node)))
                                (alt (+ (dist-between evaluator2 (second candidate) node robot-node-index)
                                        (first (vector-ref dist node)) )))
			      (if (< alt (first (vector-ref dist robot-node-index )))
                                  (vector-set! dist robot-node-index (list alt (first candidate)))
                                  '()))) neighbors)
                  (check-nodes evaluator2 (remove-node unvisited node) world2))))))))
        (vector-set! dist source-node (list 0 '()))
        (check-nodes evaluator1 (unfold (lambda (p) (> p (* (board-width (world-board world)) (board-height (world-board world))))) (lambda (x) x) (lambda (x) (+ x 1)) 0) world)))

;; get the next move for the min cost target
(define (get-min-path targets)
  (print targets)
  (second (vector-ref targets
                      (vector-fold (lambda (index best x)
                        (if (> (first (vector-ref targets index)) (first (vector-ref targets best))) best index)) 0 targets))))

(define (cartesian->index lst)
   (* (first lst) (second lst)))

;; compute dijkstras to get costs to go from the source node to every other node, then return the next move
(define (best-moves3 world)
  (let* ((source-node-index (cartesian->index (find-robot world)))
        (target-nodes (dijkstra heuristic-world world source-node-index)))
          (get-min-path target-nodes)))


(define (update-best! tehnode)
 (when (or (not *best-node-so-far*) (< (vector-ref tehnode 0) (vector-ref *best-node-so-far* 0)))
  (set! *best-node-so-far* tehnode)))


(define (best-moves1 evaluator fuckingscoreit heap random? already-seen)
 (if (pairing-heap-empty? heap)
     heap
     (let ((cost&world&moves (pairing-heap-min heap))
           (heap1 (pairing-heap-remove-min heap)))
      (foldl
       (lambda (heap move)
        (let ((world1 (move-robot (vector-ref cost&world&moves 1) move))
              (moves (cons move (vector-ref cost&world&moves 2))))
         (if world1
             (let* ((world2 (simulate world1))
		    (score (fuckingscoreit moves world2))
                    (e (evaluator score moves world2))
                    (robot (find-robot world2))
                    (previous (if already-seen
                                  (board-ref already-seen (car robot) (cadr robot))
                                  +inf.0)))
              (cond ((i-am-dead? world2) heap)
                    ((done? world2 moves)
                     (update-best! (vector (evaluator score moves world2) world2 moves score))
                     heap)
                    ((< previous e) heap)
                    (else
                     (when already-seen (board-set! already-seen (car robot) (cadr robot) e))
                     (pairing-heap-insert (vector e world2 moves score) heap))))
             heap)))
       heap1
       (if random?
           (append (deal moves-directions) moves-other)
           moves)))))

(define (best-moves0 evaluator world n random? already-seen)
 (set! *best-node-so-far* #f)
 (let ((heap 
        (let* ((initial-hugs (count-hugs world))
               (evaluator1 (lambda (score path world) (evaluator score initial-hugs path world))) 
	       (fuckingscoreit (lambda (path world) (score-world initial-hugs path world)))
	       )
         (let loop ((n n) (heap (pairing-heap-insert
                                 (vector (evaluator1 0 '() world) world '() 0)
                                 (pairing-heap-empty
                                  (lambda (a b)
                                   (cond
                                    ((< (vector-ref a 0) (vector-ref b 0)) -1)
                                    ((= (vector-ref a 0) (vector-ref b 0)) 0)
                                    (else 1)))))))
          (bestest-best! heap)
          (if (= n 0)
              heap
              (loop (- n 1) (best-moves1 evaluator1 fuckingscoreit heap random? already-seen)))))))
  heap))

(define (best-moves evaluator world n random? already-seen)
 (let ((heap (best-moves0 evaluator world n random? already-seen)))
  (if *best-node-so-far*
      (pairing-heap-insert *best-node-so-far* heap)
      heap)))

;;Todo: Die quickly in a hugless world
(define (best-move world n) 
 (pairing-heap-min (best-moves heuristic-world world n #f #f)))

(define (best-move-with-dijkstras world)
   (best-moves3 world))

(define (world->seen-map world)
 (map-matrix (lambda (e) +inf.0) (world-board world)))

(define (best-move-with-no-repeats world n) 
 (pairing-heap-min (best-moves heuristic-world world n #f (world->seen-map world))))

;;Todo: Die quickly in a hugless world
(define (best-move-random world n restarts) 
 (for-each-n (lambda _ 
              (let ((r (pairing-heap-min (best-moves heuristic-world world n #t #f))))
               (if (or (not *best-node*) (< (vector-ref r 0) (vector-ref *best-node* 0)))
                   (set! *best-node* r)
                   #f)))
  restarts)
 *best-node*)

(define (best-move-random-with-no-repeats world n restarts) 
 (for-each-n (lambda _ 
              (let ((r (pairing-heap-min (best-moves heuristic-world world n #t (world->seen-map world)))))
               (if (or (not *best-node*) (< (vector-ref r 0) (vector-ref *best-node* 0)))
                   (set! *best-node* r)
                   #f)))
  restarts)
 *best-node*)

(define (best-move-random-with-no-repeats-and-list world n restarts heuristic-list) 
 (for-each-n (lambda _ 
	       (map (lambda (heuristic) 
		      ;;Fuckit
		      (set! flw #f)
              (let ((r (pairing-heap-min (best-moves heuristic world n #t (world->seen-map world)))))
               (if (or (not *best-node*) (< (vector-ref r 0) (vector-ref *best-node* 0)))
                   (set! *best-node* r)
                   #f))) heuristic-list)
	      )
  restarts)
 *best-node*)

(define (fuckerquest-test world n restarts)
  (best-move-random-with-no-repeats-and-list world n restarts heuristic-list-test)
)

(define (fuckerquest-prod world n restarts)
  (best-move-random-with-no-repeats-and-list world n restarts heuristic-list-prod)
)


(define (pairing-heap->list heap)
 (let loop ((heap heap) (r '()))
  (if (pairing-heap-empty? heap)
      r
      (loop (pairing-heap-remove-min heap)
            (cons (pairing-heap-min heap) r)))))

(define w1
 (dry-world
  '#(#(wall rock earth empty wall)
     #(closed-lift earth robot hug wall)
     #(wall wall wall wall wall))))

(define (test-search)(best-move-with-no-repeats (file->world "../tests/contest1.map") 30)
 (for-each 
   (lambda (a) 
    (format #t "Node ~a ~a~%" (vector-ref a 0) (vector-ref a 2))
    (world-pp (vector-ref a 1)))
  (pairing-heap->list (best-moves (lambda _ (display _)(newline) (apply heuristic-world _)) w1 10 #f #f))))

(define (test1)
 (best-moves (lambda _ (display (cons (apply heuristic-world _) _))(newline)
                (apply heuristic-world _))
             (file->world "../tests/contest1.map")
             10
             #f
             #f))

(define (test2) (best-move (file->world "../tests/contest1.map") 30))
(define (test3) (best-move (file->world "../tests/contest2.map") 30))

;; (best-move-with-no-repeats (file->world "../tests/contest1.map") 30)
