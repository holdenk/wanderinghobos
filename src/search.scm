(declare (unit search))
(declare (uses simulate pairing-heap))
(use srfi-1)
(use list-utils)
(use sequences)


(define moves '(left right up down wait))

(define *best-node-so-far* #f)

(define (bestest-best! heap)
 (let ((min (pairing-heap-min heap)))
  (when (or (not *best-node-so-far*) (< (vector-ref min 0) (vector-ref *best-node-so-far* 0)))
   (set! *best-node-so-far* min))))

(define (best-moves1 evaluator heap)
 (let ((cost&world&moves (pairing-heap-min heap))
       (heap1 (pairing-heap-remove-min heap)))
  (foldl
   (lambda (heap move)
    (let ((world1 (move-robot (vector-ref cost&world&moves 1) move))
          (moves (cons move (vector-ref cost&world&moves 2))))
     (if world1
         (let ((world2 (simulate world1)))
          (pairing-heap-insert (vector (evaluator world2 moves) world2 moves) heap))
         heap)))
   heap1
   moves)))

(define (best-moves evaluator world n)
 ;; evaluator :: hugs path world
 (set! *best-node-so-far* #f)
 (let* ((initial-hugs (count-hugs world))
        (evaluator1 (lambda (path world) (evaluator initial-hugs path world))))
  (let loop ((n n) (heap (pairing-heap-insert
                          (vector (evaluator1 '() world) world '())
                          (pairing-heap-empty (lambda (a b) (< (vector-ref a 0) (vector-ref b 0)))))))
   (bestest-best! heap)
   (if (= n 0)
       heap
       (loop (- n 1) (best-moves1 evaluator1 heap))))))

(define (pairing-heap->list heap)
 (let loop ((heap heap) (r '()))
  (if (pairing-heap-empty? heap)
      (reverse r)
      (loop (pairing-heap-remove-min heap)
            (cons (pairing-heap-min heap) r)))))

(define w1
 (dry-world
  '#(#(wall rock earth empty wall)
     #(closed-lift earth robot hug wall)
     #(wall wall wall wall wall))))

(define (test-search)
 (for-each 
   (lambda (a) 
    (format #t "Node ~a ~a~%" (vector-ref a 0) (vector-ref a 2))
    (world-pp (vector-ref a 1)))
  (pairing-heap->list (best-moves (lambda _ (display _)(newline) (apply heuristic-world _)) w1 10))))

(define (test1)
 (best-moves (lambda _ (display _)(newline) (apply heuristic-world _)) 
             (file->world "../tests/contest1.map")
             10))
