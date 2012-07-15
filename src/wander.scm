;; -*- indent-tabs-mode: nil -*- ;;
(declare (unit wander))
(declare (uses simulate parse-input dog))

(use list-utils)

(define (world-width world)
  (board-width (world-board world)))
(define (world-height world)
  (board-height (world-board world)))

(define (wander-compute-fw world)
  (let* ((board (world-board world))
         (fw-data (hobofloydwarshall board)))
    (cons fw-data board)))

(define (wander-fw-distance fw-data a b)
  (path-cost (car a) (cadr a)
             (car b) (cadr b)
             (car fw-data)
             (cdr fw-data)))

;; Finds the closest point to the target in the provided list of points, and
;; places it at the head of the list (the rest may be re-ordered).
(define (point-closest-to-in points target fw-data)
  (foldl (lambda (cur-m next)
           (if (< (wander-fw-distance fw-data next target)
                  (wander-fw-distance fw-data (car cur-m) target))
               (cons next cur-m)
               (cons (car cur-m) (cons next (cdr cur-m)))))
         (cons (car points) '())
         (cdr points)))


(define (wander-hugs world)
  (let* ((fw-data (wander-compute-fw world))
         (hugs-locs (find-hugs world))
         (robot-loc (find-robot world))
         (lift-loc (find-lift world)))
    '()))


(define (clusters-merge a b)
  (list a b))


(define (contains-cluster? haystack needle)
  (cond ((eq? haystack '()) #f)
        ((eq? needle (car haystack)) #t)
        (else (contains-cluster? (cdr haystack) needle))))


(define (find-nearest-cluster fw-data clusters target)
  (foldl (lambda (curr next)
           (if (< (cluster-distance next target)
                  (cluster-distance curr target))
               next
               curr))
         (car clusters)
         (cdr clusters)))


(define (cluster-extract-points cluster)
  (if (number? (car cluster))
      cluster
      (list (cluster-extract-points (car cluster))
            (cluster-extract-points (cdr cluster)))))


(define (cluster-distance fw-data a b)
  (let ((a-points (cluster-extract-points a))
        (b-points (cluster-extract-points b)))
    (apply min
           (map (lambda (ap)
                  (apply min (map (lambda (bp)
                                    (wander-fw-distance fw-data
                                                        ap
                                                        bp))
                                  b-points)))))))


;; Execute one step in the nearest-neighbour-chain algorithm.
(define (nearest-neighbour-chain1 active-clusters
                                  clusters-stack1
                                  fw-data)
  (if (> (length active-cluster) 1)
      (apply nearest-neighbour-chain1
             (let* ((clusters-stack (if (> (length clusters-stack) 0)
                                        (list (car active-clusters))
                                        (clusters-stack1)))
                    (c (car clusters-stack))
                    (d (find-nearest-cluster fw-data active-clusters c)))
               (if (contains-cluster? clusters-stack d)
                   (list (cdr clusters-stack)
                         (cons (clusters-merge d (car clusters-stack))
                               (filter (lambda (x) (eq? d x) active-clusters)))
                         fw-data)
                   (list (cons d (clusters-stack)) active-clusters)) fw-data))
      active-clusters))


(define (nearest-neighbour-chain points fw-data)
  (nearest-neighbour-chain1 points '() fw-data))
