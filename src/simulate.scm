; -*- indent-tabs-mode: nil -*- ;
(declare (unit simulate))

(declare (uses parse-input))
(use list-utils test srfi-1 posix vector-lib sequences)

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

(define (for-each-matrix f m) (for-each-vector (lambda (v) (for-each-vector f v)) m))

(define (map-indexed-matrix f m)
  (map-indexed-vector (lambda (r i) (map-indexed-vector (lambda (c j) (f c j i)) r)) m))

(define (for-each-indexed-vector f v)
  (for-each-n (lambda (i) (f (vector-ref v i) i)) (vector-length v)))

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

(define (for-each-indexed-matrix f m)
  (for-each-indexed-vector
   (lambda (r i) (for-each-indexed-vector (lambda (c j) (f c i j)) r))
   m))

(define (reverse-vector v) (list->vector (reverse (vector->list v))))

(define (for-each-board-index f board)
  (let ((width (board-width board)))
    (for-each-n (lambda (y)
                  (for-each-n (lambda (x) (f x y))
                              width))
                (board-height board))))
(define (board-height board) (vector-length board))
(define (board-width board) (vector-length (vector-ref board 0)))
(define (board-length board) (* (board-height board) (board-width board)))

(define (board-ref board x y)
  (if (or (< y 0) (< x 0) (>= y (board-height board)) (>= x (board-width board)))
      #f
      (vector-ref (vector-ref board y) x)))

(define (board-ref-unchecked board x y)
  (vector-ref (vector-ref board y) x))

(define (board-set! board x y contents)
  (vector-set! (vector-ref board y) x contents)
  board)

(define (world-ref world n)
  (let ((b (world-board world)))
    (board-ref b (quotient n (board-height b)) (modulo n (board-height b))))) 

(define (world-length world)
  (board-length (world-board world)))

(define (is-robot? item) (equal? item 'robot))
(define (is-rock-like? item) (or (is-rock? item) (is-horock? item)))
(define (is-horock? item) (equal? item 'horock))
(define (is-rock? item) (equal? item 'rock))
(define (is-empty? item) (equal? item 'empty))
(define (is-closed-lift? item) (equal? item 'closed-lift))
(define (is-open-lift? item) (equal? item 'open-lift))
(define (is-lift? item) (or (is-closed-lift? item) (is-open-lift? item)))
(define (is-earth? item) (equal? item 'earth))
(define (is-wall? item) (equal? item 'wall))
(define (is-hug? item) (equal? item 'hug))
(define (is-beard? item) (equal? item 'beard))

(define (robot? board x y) (is-robot? (board-ref board x y)))
(define (rock? board x y) (is-rock? (board-ref board x y)))
(define (horock? board x y) (is-horock? (board-ref board x y)))
(define (empty? board x y) (equal? (board-ref board x y) 'empty))
(define (closed-lift? board x y) (equal? (board-ref board x y) 'closed-lift))
(define (open-lift? board x y) (equal? (board-ref board x y) 'open-lift))
(define (earth? board x y) (equal? (board-ref board x y) 'earth))
(define (wall? board x y) (equal? (board-ref board x y) 'wall))
(define (hug? board x y) (equal? (board-ref board x y) 'hug))
(define (lift? board x y) (or (open-lift? board x y) (closed-lift? board x y)))
(define (trampoline-in? board x y) (is-trampoline-in? (board-ref board x y)))
(define (trampoline-out? board x y) (is-trampoline-out? (board-ref board x y)))
(define (beard? board x y) (is-beard? (board-ref board x y)))

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

(define (execute-square board x y board-out nr-hugs grow-beard moving-rocks)
  (let ((xy (board-ref-unchecked board x y)))
    (cond ((is-rock-like? xy)
           '()
           (let ((xy-1 (board-ref board x (- y 1)))
                 (x+1y (board-ref board (+ x 1) y))
                 (x+1y-1 (board-ref board (+ x 1) (- y 1)))
                 (x-1y-1 (board-ref board (- x 1) (- y 1)))
                 (x-1y (board-ref board (- x 1) y))
                 (x-1y+1 (board-ref board (- x 1) (+ y 1)))
                 (xy+1 (board-ref board x (+ y 1)))
                 (x+1y+1 (board-ref board (+ x 1) (+ y 1))))
             (cond 
              ((and (is-rock-like? xy) (equal? 'empty xy-1))
               (board-set! board-out x y 'empty)
               (board-set! board-out x (- y 1) xy)
               (list (list x (- y 1))))
              ((and (is-rock-like? xy) (is-rock-like? xy-1)
                    (equal? x+1y 'empty) (equal? x+1y-1 'empty))
               (board-set! board-out x y 'empty)
               (board-set! board-out (+ x 1) (- y 1) xy)
               (list (list (+ x 1) (- y 1))))
              ((and (is-rock-like? xy)
                    (is-rock-like? xy-1)
                    (or (and x+1y (not (equal? 'empty x+1y)))
                        (and x+1y-1 (not (equal? 'empty x+1y-1))))
                    (equal? x-1y 'empty)
                    (equal? x-1y-1 'empty))
               (board-set! board-out x y 'empty)
               (board-set! board-out (- x 1) (- y 1) xy)
               (list (list (- x 1) (- y 1))))
              ((and (is-rock-like? xy) (equal? xy-1 'hug)
                    (equal? x+1y 'empty) (equal? x+1y-1 'empty))
               (board-set! board-out x y 'empty)
               (board-set! board-out (+ x 1) (- y 1) xy)
               (list (list (+ x 1) (- y 1))))
              (else (board-set! board-out x y xy) '()))))
          ((and (equal? xy 'closed-lift) (= nr-hugs 0))
           (board-set! board-out x y 'open-lift)
           '())
          ((and (equal? 'beard xy) grow-beard)
           (let ((xy-1 (board-ref board x (- y 1)))
                 (x+1y (board-ref board (+ x 1) y))
                 (x+1y-1 (board-ref board (+ x 1) (- y 1)))
                 (x-1y-1 (board-ref board (- x 1) (- y 1)))
                 (x-1y (board-ref board (- x 1) y))
                 (x-1y+1 (board-ref board (- x 1) (+ y 1)))
                 (xy+1 (board-ref board x (+ y 1)))
                 (x+1y+1 (board-ref board (+ x 1) (+ y 1))))
             (when (equal? 'empty x-1y-1)
                   (board-set! board-out (- x 1) (- y 1) 'beard))
             (when (equal? 'empty xy-1)
                   (board-set! board-out x (- y 1) 'beard))
             (when (equal? 'empty x+1y-1)
                   (board-set! board-out (+ x 1) (- y 1) 'beard))
             (when (equal? 'empty x-1y)
                   (board-set! board-out (- x 1) y 'beard))
             (when (equal? 'empty x+1y)
                   (board-set! board-out (+ x 1) y 'beard))
             (when (equal? 'empty x-1y+1)
                   (board-set! board-out (- x 1) (+ y 1) 'beard))
             (when (equal? 'empty xy+1)
                   (board-set! board-out x (+ y 1) 'beard))
             (when (equal? 'empty x+1y+1)
                   (board-set! board-out (+ x 1) (+ y 1) 'beard))
             (board-set! board-out x y xy)
             '()))
          (else (if (equal? 'empty (board-ref board-out x y))
                    (board-set! board-out x y xy))
                '()))))

(define (simulate-board board nr-hugs grow-beard moving-rocks)
  ;; Simulate runs in top-down rather than bottom-up order!
  (let ((board (reverse-vector board)))
    (let ((new-board (map-matrix (const 'empty) board))
          (falling-rocks '()))
      (for-each-board-index 
       (lambda (x y) (set! falling-rocks 
                           (append (execute-square board
                                                   x
                                                   y
                                                   new-board
                                                   nr-hugs
                                                   grow-beard
                                                   moving-rocks)
                                   falling-rocks)))
       board)
      (list (reverse-vector new-board)
            (map (lambda (rock) (list (car rock) (cadr rock)))
                 falling-rocks)))))

(define (robot-underwater? world)
  (= (cadr (find-robot world))
     (- (board-height (world-board world))
        (world-water world))))

(define (simulate world)
  (let ((new-board (simulate-board (world-board world)
                                   (world-hug-count world)
                                   (if (not (zero? (world-beard world)))
                                       (= (modulo (world-iteration world)
                                                  (world-beard world))
                                          (- (world-beard world)
                                             1))
                                       #f)
                                   (world-rocks world))))
    (make-world (car new-board)
                (+ (world-water world)
                   (+ (if (and ;; Fuck, Chicken isn't IEEE 754 complaint
                           (not (zero? (world-flooding world)))
                           (= (modulo (world-iteration world) (world-flooding world)) 0))
                          1
                          0)))
                (world-flooding world)
                (world-waterproof world)
                (if (robot-underwater? world)
                    (+ (world-underwater world) 1)
                    0)
                (+ (world-iteration world) 1)
                (map (lambda (position)
                       (list (car position) 
                             (- (board-height (car new-board)) (cadr position) 1)))
                     (cadr new-board))
                (world-robot-location world)
                (world-hugs world)
                (count-obj-board 'hug (car new-board))
                (world-lift-location world)
                (world-fuckedrocks world)
                (world-beard world)
                (+ (world-razors world)
                   (- (count-obj-board 'razor
                                       (world-board world))
                      (count-obj-board 'razor
                                       (car new-board)))))))

(define (i-am-dead? world)
  (let ((robot (find-robot world)))
    (or (any (lambda (location) 
               (and (= (car location) (car robot))
                    (= (cadr location) (- (cadr robot) 1))))
             (world-rocks world))
        (< (world-waterproof world) (world-underwater world)))))

(define (find-trampolines board)
  (let ((trampolines '()))
    (for-each-indexed-matrix 
     (lambda (e y x) (when (is-trampoline? e) (set! trampolines (cons (list x y) trampolines))))
     board)
    trampolines))

(define (find-anus-for-mouth board mouth)
  (call-with-current-continuation
   (lambda (k)
     (for-each-indexed-matrix 
      (lambda (e y x) (when (and (is-trampoline-out? e) (member  mouth (vector-ref e 2)))
                            (k (list x y))))
      board)
     #f)))

(define (find-mouths-for-anus board anus)
  (let ((trampolines '()))
    (for-each-indexed-matrix 
     (lambda (e y x) (when (and (is-trampoline-in? e) (equal? (vector-ref e 2) anus))
                           (set! trampolines (cons (list x y) trampolines))))
     board)
    trampolines))

(define (move-robot-board board location direction)
  (cond ((member direction '(abort wait))
         (list board location #f))
        ((equal? direction 'shave)
         (list (let* ((board-out (copy-board board))
                      (x (car location))
                      (y (cadr location))
                      (neighbours (list (cons x (- y 1))
                                        (cons (+ x 1) y)
                                        (cons (+ x 1) (- y 1))
                                        (cons (- x 1) (- y 1))
                                        (cons (- x 1) y)
                                        (cons (- x 1) (+ y 1))
                                        (cons x (+ y 1))
                                        (cons (+ x 1) (+ y 1)))))
                 (foldl (lambda (cur-board point)
                          (if (beard? board (car point) (cdr point))
                              (board-set! cur-board
                                          (car point)
                                          (cdr point)
                                          'empty)
                              cur-board))
                        board-out
                        neighbours))
               location
               #f))
        (else
         (let* ((destination
                 (case direction
                   ((left) (list (- (car location) 1) (cadr location)))
                   ((right) (list (+ (car location) 1) (cadr location)))
                   ((up) (list (car location) (- (cadr location) 1)))
                   ((down) (list (car location) (+ (cadr location) 1)))
                   (else (error "Unsupported move" board direction))))
                (l-x (car location)) (l-y (cadr location))
                (d-x (car destination)) (d-y (cadr destination))
                (dxy (board-ref board d-x d-y)))
           (define (move-it)
             (board-set! (board-set! (copy-board board) d-x d-y 'robot) l-x l-y 'empty))
           (cond ((and dxy (member dxy '(hug empty open-lift earth razor)))
                  (list (move-it) (list d-x d-y) #f))
                 ((equal? dxy 'trampoline-in)
                  (let ((f (find-anus-for-mouth board (vector-ref dxy 1))))
                    (list
                     (foldl (lambda (board l) (board-set! board (car l) (cadr l) 'empty))
                            (board-set!
                             (board-set! 
                              (board-set! 
                               (copy-board board)
                               d-x d-y 'empty)
                              l-x l-y 'empty)
                             (car f) (cadr f) 'robot)
                            (find-mouths-for-anus board (vector-ref dxy 2)))
                     (list d-x d-y)
                     #f)))
                 ((and (= d-x (+ l-x 1))
                       (= d-x d-y)
                       (is-rock-like? dxy)
                       (empty? board (+ l-x 2) l-y))
                  (list (board-set! (move-it) (+ l-x 2) l-y 'dxy) (list d-x d-y)
                        #t))
                 ((and (= d-x (- l-x 1))
                       (= d-x d-y)
                       (is-rock-like? dxy)
                       (empty? board (- l-x 2) l-y))
                  (list (board-set! (move-it) (- l-x 2) l-y 'dxy) (list d-x d-y)
                        #t))
                 (else #f))))))

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
     #f)))

(define (find-hugs-board board)
  (vector-fold 
   (lambda (y hugs vector) 
     (vector-fold (lambda (x hugs element) (if (eq? element 'hug)
                                               (cons (list x y) hugs)
                                               hugs))
                  hugs vector))
   '() board))

(define (find-robot world) (world-robot-location world))

(define (find-hugs world) (world-hugs world))

(define (find-lift world) (world-lift-location world))

(define (move-robot world direction)
	(if (or (> (world-razors world) 0)
					(not (equal? 'shave direction)))
			(let ((board&location 
						 (move-robot-board (world-board world)
															 (world-robot-location world) direction)))
				(if board&location
						(let ((board (car board&location)))
							(make-world board
													(world-water world)
													(world-flooding world)
													(world-waterproof world)
													(world-underwater world)
													(world-iteration world)
													(world-rocks world)
													(cadr board&location)
													(find-hugs-board board)
													(count-obj-board 'hug board)
													(let ((l (world-lift-location world)))
														(if (and l (member (board-ref-unchecked board
																																		(car l)
																																		(cadr l))
																							 '(open-lift closed-lift)))
																l
																#f))
													(caddr board&location)
													(world-beard world)
													(if (equal? 'shave direction)
															(- (world-razors world) 1)
															(world-razors world))
													;;#f
													))
						#f))
			#f))

;; #*. #
;; # R #
;; #####

(define (dry-world board) 
  (make-world board +inf.0 +inf.0 +inf.0 0 0 '() 
              (find-robot-board board)
              (find-hugs-board board)
              (count-obj-board 'hug board)
              (find-lift-board board)
              #f
              0
              0))

(define faq-2
  (dry-world
   '#(#(wall rock earth empty wall)
      #(open-lift empty robot empty wall)
      #(wall wall wall wall wall))))

(define faq-2-1
  (dry-world
   '#(#(wall rock earth empty wall)
      #(wall empty empty rock wall)
      #(open-lift empty robot empty wall)
      #(wall wall wall wall wall))))

;; (world-board faq-2)
;; (world-board (simulate faq-2))
;; (i-am-dead? (simulate (move-robot faq-2 'left)))
;; (i-am-dead? (simulate (move-robot faq-2-1 'left)))

(define (run-test)
  (and (equal? (i-am-dead? (simulate (move-robot faq-2 'left))) #f)
       (equal? (i-am-dead? (simulate (move-robot faq-2-1 'left))) #t)
       (equal? (i-am-dead? (simulate (move-robot faq-2-1 'right))) #f)))

(define (count-lifts world) 
  (+ (count-obj 'closed-lift world) (count-obj 'open-lift world)))

(define (escaped? world) (not (world-lift-location world)))

(define (done? world path)
  (if (or (eq? 'abort (car path)) (escaped? world))
      #t
      #f))
