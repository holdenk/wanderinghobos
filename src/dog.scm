(declare (unit wat))
(declare (uses parse-input))
(define-gs-record path (cost edges))

(define (path-min p0 p1)
  (if (<= (path-cost p0) (path-cost p1))
      p0
      p1))

(define (path-add p0 p1)
  (make-path 
   (+ (path-cost p0) (path-cost p1))
   (append (path-edges p0) (path-edges p1))))

(define (shortest-paths n- edge-cost)
  (define n (add1 n-))
  (define m
    (for*/matrix 
     n n 
     ([i (in-range 0 n)]
      [j (in-range 0 n)])
     (edge-cost i j)))
  (for* ([k (in-range 0 n)]
         [i (in-range 0 n)]
         [j (in-range 0 n)])
    (matrix-set! 
     m i j
     (path-min (matrix-ref m i j)
               (path-add (matrix-ref m i k)
                         (matrix-ref m k j)))))
  m)

(define-gs-record edge (end weight))

(define-gs-record graph (adj))

(define (create-graph)
  (make-graph (make-hasheq)))

(define (graph-add! g start end weight)
  (hash-update! (graph-adj g)
                start
                (lambda (old)
                  (list* (make-edge end weight) old))
                empty))

(define (graph-shortest-paths g)
  (define adj (graph-adj g))
  (shortest-paths 
   (apply max (hash-map adj (lambda (k v) k)))
   (lambda (i j)
     (if (= i j)
         (make-path 0 empty)
         (local 
           [(define e
              (findf (lambda (e) (= (edge-end e) j))
                     (hash-ref adj i empty)))]
           (if e
               (make-path (edge-weight e) (list e))
               (make-path +inf.0 empty)))))))

