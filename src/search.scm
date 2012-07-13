(use srfi-1)
(use simulate pairing-heap)
(require-library simulate pairing-heap)

(define moves '(left right up down wait))

(define (best-moves1 evaluator heap)
 (let ((cost&world&moves (pairing-heap-min heap))
       (heap1 (pairing-heap-remove-min heap)))
  (foldl
   (lambda (heap move)
    (let ((world1 (move-robot (vector-ref cost&world&moves 1) move)))
     (if world1
         (pairing-heap-insert
          (vector (evaluator (vector-ref cost&world&moves 0))
                  world1
                  (cons move (vector-ref cost&world&moves 2)))
          heap)
         heap)))
   heap
   moves)))

(define (best-moves evaluator world)
 (let loop ((n 20) (heap (pairing-heap-insert
                          (vector (evaluator world) world '())
                          (pairing-heap-empty (lambda (a b) (< (vector-ref a 0) (vector-ref b 0)))))))
  (best-moves1 evaluator heap)))

(define (pairing-heap->list heap)
 (let loop ((heap heap) (r '()))
  (if (pairing-heap-empty? heap)
      (reverse r)
      (loop (pairing-heap-remove-min heap)
            (cons (pairing-heap-min heap) r)))))

(define (test-search)
 (for-each 
   (lambda (a) 
    (format #t "Node ~a ~a~%" (vector-ref a 0) (vector-ref a 2))
    (world-pp (vector-ref a 1)))
  (pairing-heap->list (best-moves (lambda _ 1) faq-2-1))))

