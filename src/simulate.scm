(declare (unit simulate))

(require-library parse-input)
(use test srfi-1 posix)

(define (for-each-n f n)
 (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (for-each-vector f v)
 (for-each-n (lambda (i) (f (vector-ref v i))) (vector-length v)))

(define (map-n1 f n)
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-vector f v)
 (let ((u (make-vector (vector-length v))))
  (for-each-n (lambda (i) (vector-set! u i (f (vector-ref v i))))
   (vector-length v))
  u))

(define (const a) (lambda _ a))

(define (map-matrix f m) (map-vector (lambda (v) (map-vector f v)) m))

(define (map-indexed-matrix f m)
 (map-indexed-vector (lambda (r i) (map-indexed-vector (lambda (c j) (f c j i)) r)) m))

(define (map-indexed-vector f v)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (f (vector-ref v i) i)))
   (vector-length v))
  u))

(define (reverse-vector v) (list->vector (reverse (vector->list v))))

(define (for-each-board-index f board)
 (for-each-n (lambda (y)
              (for-each-n (lambda (x) (f x y))
               (board-width board)))
  (board-height board)))
(define (board-height board) (vector-length board))
(define (board-width board) (vector-length (vector-ref board 0)))

(define (board-ref board x y)
 (if (or (< y 0) (< x 0) (>= y (board-height board)) (>= x (board-width board)))
     #f
     (vector-ref (vector-ref board y) x)))

(define (board-set! board x y contents)
 (vector-set! (vector-ref board y) x contents)
 board)

(define (robot? board x y) (equal? (board-ref board x y) 'robot))
(define (rock? board x y) (equal? (board-ref board x y) 'rock))
(define (empty? board x y) (equal? (board-ref board x y) 'empty))
(define (closed-lift? board x y) (equal? (board-ref board x y) 'closed-lift))
(define (open-lift? board x y) (equal? (board-ref board x y) 'open-lift))
(define (earth? board x y) (equal? (board-ref board x y) 'earth))
(define (wall? board x y) (equal? (board-ref board x y) 'wall))
(define (hug? board x y) (equal? (board-ref board x y) 'hug))
(define (lift? board x y) (or (open-lift? board x y) (closed-lift? board x-y)))

(define (not-exists? board x y) (not (board-ref board x y)))
(define (exists? board x y) (board-ref board x y))

(define (some-board p board)
 (call-with-current-continuation
  (lambda (k)
   (for-each-vector
    (lambda (v) (for-each-vector (lambda (e) (when (p e) (k #t))) v))
    board)
   #f)))

(define (copy-board board) (map-matrix identity board))

(define (execute-square board x y board-out)
 (cond ((and (rock? board x y) (empty? board x (- y 1)))
        (board-set! board-out x y 'empty)
        (board-set! board-out x (- y 1) 'rock)
        (list (list x (- y 1))))
       ((and (rock? board x y) (rock? board x (- y 1))
           (empty? board (+ x 1) y) (empty? board (+ x 1) (- y 1)))
        (board-set! board-out x y 'empty)
        (board-set! board-out (+ x 1) (- y 1) 'rock)
        (list (list (+ x 1) (- y 1))))
       ((and (rock? board x y)
           (rock? board x (- y 1))
           (or (and (exists? board (+ x 1) y) (not (empty? board (+ x 1) y)))
              (and (exists? board (+ x 1) (- y 1)) (not (empty? board (+ x 1) (- y 1)))))
           (empty? board (- x 1) y)
           (empty? board (- x 1) (- y 1)))
        (board-set! board-out x y 'empty)
        (board-set! board-out (- x 1) (- y 1) 'rock)
        (list (list (- x 1) (- y 1))))
       ((and (rock? board x y) (hug? board x (- y 1))
           (empty? board (+ x 1) y) (empty? board (+ x 1) (- y 1)))
        (board-set! board-out x y 'empty)
        (board-set! board-out (+ x 1) (- y 1) 'rock)
        (list (list (+ x 1) (- y 1))))
       ((and (closed-lift? board x y) (not (some-board hug? board)))
        (board-set! board-out x y 'open-lift)
        '())
       (else
        (board-set! board-out x y (board-ref board x y))
        '())))

(define (simulate-board board)
 ;; Simulate runs in top-down rather than bottom-up order!
 (let ((board (reverse-vector board)))
  (let ((new-board (map-matrix (const 'empty) board))
        (falling-rocks '()))
   (for-each-board-index 
    (lambda (x y) (set! falling-rocks 
                   (append (execute-square board x y new-board)
                           falling-rocks)))
    board)
   (list (reverse-vector new-board)
         (map (lambda (rock) (list (car rock) (cadr rock)))
              falling-rocks)))))

(define (robot-underwater? world)
 (> (cadr (find-robot world))
    (- (board-height (world-board world))
       (world-water world))))

(define (simulate world)
 (let ((new-board (simulate-board (world-board world))))
  (make-world (car new-board)
              (+ (world-water world)
                 (+ (if (= (modulo (world-iteration world) (world-flooding world)) 0)
                        1
                        0)))
              (world-flooding world)
              (world-waterproof world)
              (+ (world-underwater world) (if (robot-underwater? world) 1 0))
              (+ (world-iteration world) 1)
              (map (lambda (position)
                    (list (car position) 
                          (- (board-height (car new-board)) (cadr position) 1)))
                   (cadr new-board)))))

(define (i-am-dead? world)
 (let ((robot (find-robot world)))
  (any 
   (lambda (location) 
    (and (= (car location) (car robot))
       (= (car location) (- (cadr robot) 1))))
   (world-rocks world))))

(define (move-robot-board board direction)
 (let* ((location (find-robot-board board))
        (destination
         (case direction
          ;; TODO abort
          ((left) (list (- (car location) 1) (cadr location)))
          ((right) (list (+ (car location) 1) (cadr location)))
          ((up) (list (car location) (- (cadr location) 1)))
          ((down) (list (car location) (+ (cadr location) 1)))
          ((wait) location)
          (else (error "Unsupported move"))))
        (l-x (car location)) (l-y (cadr location))
        (d-x (car destination)) (d-y (cadr destination)))
  (define (move-it)
   (board-set! (board-set! (copy-board board) d-x d-y 'robot) l-x l-y 'empty))
  (cond ((and (exists? board d-x d-y)
            (or (hug? board d-x d-y)
               (empty? board d-x d-y)
               (open-lift? board d-x d-y)
               (earth? board d-x d-y)))
         (move-it))
        ((and (= d-x (+ l-x 1)) (= d-x d-y) (rock? board d-x d-y) (empty? board (+ d-x 2) d-y))
         (board-set! (move-it) (+ d-x 2) d-y 'rock))
        ((and (= d-x (- l-x 1)) (= d-x d-y) (rock? board d-x d-y) (empty? board (- d-x 2) d-y))
         (board-set! (move-it) (- d-x 2) d-y 'rock))
        (else #f))))

(define (find-robot-board board)
 (call-with-current-continuation
  (lambda (k)
   (for-each-board-index 
    (lambda (x y) (when (robot? board x y) (k (list x y))))
    board)
   (error "AIN'T GOT NO ROBOT?!?"))))

(define (find-lift-board board)
 (call-with-current-continuation
  (lambda (k)
   (for-each-board-index 
    (lambda (x y) (when (lift? board x y) (k (list x y))))
    board)
   (error "AIN'T GOT NO LIFT?!? WE SHOULD BE DONE!"))))


(define (find-hugs-board board)
  (vector-fold (lambda (y hugs vector) 
		 (vector-fold (lambda (x hugs element)
				(if (eq? element 'hug)
				    (cons (list x y) hugs)
				    hugs
				    )
				)
			      hugs
			      vector
			      )

		 )
	       (list ) board)
  )

(define (find-robot world)
 (find-robot-board (world-board world)))

(define (find-hugs world)
 (find-hugs-board (world-board world)))

(define (find-lift world)
 (find-lift-board (world-board world)))


(define (move-robot world direction)
 (let ((board (move-robot-board (world-board world) direction)))
  (if board
      (make-world board
                  (world-water world)
                  (world-flooding world)
                  (world-waterproof world)
                  (world-underwater world)
                  (world-iteration world)
                  (world-rocks world))
      #f)))

;; #*. #
;; # R #
;; #####

(define (dry-world board) (make-world board +inf.0 +inf.0 +inf.0 0 0 '()))

(define faq-2
 (dry-world
  '#(#(wall rock earth empty wall)
     #(wall empty robot empty wall)
     #(wall wall wall wall wall))))

(define faq-2-1
 (dry-world
  '#(#(wall rock earth empty wall)
     #(wall empty empty rock wall)
     #(wall empty robot empty wall)
     #(wall wall wall wall wall))))

;; (world-board faq-2)
;; (world-board (simulate faq-2))
;; (i-am-dead? (simulate (move-robot faq-2 'left)))
;; (i-am-dead? (simulate (move-robot faq-2-1 'left)))

(define (run-test)
 (and (equal? (i-am-dead? (simulate (move-robot faq-2 'left))) #f)
    (equal? (i-am-dead? (simulate (move-robot faq-2-1 'left))) #t)
    (equal? (i-am-dead? (simulate (move-robot faq-2-1 'right))) #f)))
