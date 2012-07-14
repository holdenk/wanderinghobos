(declare (unit search))
(declare (uses simulate pairing-heap))
(use srfi-1)
(use list-utils sequences random-bsd)

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
   (when (or (not *best-node-so-far*) (< (vector-ref min 0) (vector-ref *best-node-so-far* 0)))
    (set! *best-node-so-far* min)))))

(define (update-best! world)
 (when (or (not *best-node-so-far*) (< (vector-ref world 0) (vector-ref *best-node-so-far* 0)))
  (set! *best-node-so-far* world)))

(define (best-moves1 evaluator heap random? already-seen)
 (if (pairing-heap-empty? heap)
     heap
     (let ((cost&world&moves (pairing-heap-min heap))
           (heap1 (pairing-heap-remove-min heap)))
      (foldl
       (lambda (heap move)
        (let ((world1 (move-robot (vector-ref cost&world&moves 1) move))
              (moves (cons move (vector-ref cost&world&moves 2))))
         (if (and world1 (not (i-am-dead? world1)))
             (if (done? world1 moves)
                 (begin (update-best! (vector (evaluator moves world1) world1 moves)) heap)
                 (let* ((world2 (simulate world1))
                        (e (evaluator moves world2))
                        (robot (find-robot world2))
                        (previous (if already-seen
                                      (board-ref already-seen (car robot) (cadr robot))
                                      +inf.0)))
                  (if (< previous e)
                      heap
                      (begin 
                       (when already-seen (board-set! already-seen (car robot) (cadr robot) e))
                       (pairing-heap-insert (vector e world2 moves) heap)))))
             heap)))
       heap1
       (if random?
           (append (deal moves-directions) moves-other)
           moves)))))

(define (best-moves0 evaluator world n random? already-seen)
 (set! *best-node-so-far* #f)
 (let ((heap 
        (let* ((initial-hugs (count-hugs world))
               (evaluator1 (lambda (path world) (evaluator initial-hugs path world))))
         (let loop ((n n) (heap (pairing-heap-insert
                                 (vector (evaluator1 '() world) world '())
                                 (pairing-heap-empty
                                  (lambda (a b)
                                   (cond
                                    ((< (vector-ref a 0) (vector-ref b 0)) -1)
                                    ((= (vector-ref a 0) (vector-ref b 0)) 0)
                                    (else 1)))))))
          (bestest-best! heap)
          (if (= n 0)
              heap
              (loop (- n 1) (best-moves1 evaluator1 heap random? already-seen)))))))
  heap))

(define (best-moves evaluator world n random? already-seen)
 (let ((heap (best-moves0 evaluator world n random? already-seen)))
  (if *best-node-so-far*
      (pairing-heap-insert *best-node-so-far* heap)
      heap)))

;;Todo: Die quickly in a hugless world
(define (best-move world n) 
 (pairing-heap-min (best-moves heuristic-world world n #f #f)))

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
