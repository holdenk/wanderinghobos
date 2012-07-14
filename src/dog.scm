(declare (unit wat))
(declare (uses parse-input))

(define-gs-record path cost edges)
(define-gs-record edge end weight)
(define-gs-record graph adj)

(define-record-printer (path path port)
 (begin
  (format port "(#<path> ")
  (format port "~s " (path-cost path))
  (format port "~s)" (path-edges path))))

(define-record-printer (edge edge port)
 (begin
  (format port "(#<edge> ")
  (format port "~s " (edge-end edge))
  (format port "~s)" (edge-weight edge))))

(define (path-min p0 p1)
  (if (<= (path-cost p0) (path-cost p1))
      p0
      p1))

(define (path-add p0 p1)
  (make-path 
   (+ (path-cost p0) (path-cost p1))
   (append (path-edges p0) (path-edges p1))))

(define (for-each-n-matrix f i j)
 (for-each-n (lambda (i) (for-each-n (lambda (j) (f i j)) j)) i))

(define (matrix-ref a i j) (vector-ref (vector-ref a i) j))

(define (matrix-set! a i j x) (vector-set! (vector-ref a i) j x))

(define (find-if p l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (car l)) (car l))
	(else (loop (cdr l))))))
(define (map-n-matrix f i j)
 (map-n-vector (lambda (i) (map-n-vector (lambda (j) (f i j)) j)) i))
(define (map-n-vector f n)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (when (< i n)
    (vector-set! v i (f i))
    (loop (+ i 1))))
  v))

(define (shortest-paths n- edge-cost)
 (define n (add1 n-))
 (define m (map-n-matrix edge-cost n n))
 (for-each-n
   (lambda (k)
    (for-each-n
      (lambda (i)
       (for-each-n
         (lambda (j)
          (matrix-set! 
           m i j
           (path-min (matrix-ref m i j)
                     (path-add (matrix-ref m i k)
                               (matrix-ref m k j)))))
        n))
     n))
  n)
 m)

(define (create-graph)
  (make-graph (make-hash-table)))

(define (graph-add! g start end weight)
 (hash-table-update! (graph-adj g)
                     start
                     (lambda (old) (cons (make-edge end weight) old))
                     (lambda () '())))


(define (graph-shortest-paths g)
 (define adj (graph-adj g))
 (shortest-paths 
  (apply max (hash-table-map adj (lambda (k v) k)))
  (lambda (i j)
   (if (= i j)
       (make-path 0 '())
       (let ((e (find-if (lambda (e) (= (edge-end e) j)) (hash-table-ref adj i (lambda () '())))))
        (if e
            (make-path (edge-weight e) (list e))
            (make-path +inf.0 '())))))))

(define (passable? board x y) (member (board-ref board x y) '(empty earth hug closed-lift open-lift)))

(define (board2shit board)
 (let ((g (create-graph))
       (width (board-width board)))
  (for-each-board-index
   (lambda (x y)
    (when (passable? board (- x 1) y) (graph-add! g (+ (* y width) x) (+ (* y width) (- x 1)) 1))
    (when (passable? board (+ x 1) y) (graph-add! g (+ (* y width) x) (+ (* y width) (+ x 1)) 1))
    (when (passable? board x (- y 1)) (graph-add! g (+ (* y width) x) (+ (* (- y 1) width) x) 1))
    (when (passable? board x (+ y 1)) (graph-add! g (+ (* y width) x) (+ (* (+ y 1) width) x) 1)))
   board)
  g))
