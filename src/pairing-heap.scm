#| pairing-heap.scm

Copyright 2007 Will M. Farr <farr@mit.edu>.

Provided under a BSD license:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. The name of the author may not be used to endorse or promote
products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.  
|# 
(declare
 (unit pairing-heap)
 (inline)
 (lambda-lift)
 (usual-integrations)
 (export pairing-heap?
         pairing-heap-empty
         pairing-heap-empty?
         pairing-heap-min 
         pairing-heap-merge
         pairing-heap-insert
         pairing-heap-remove-min
         pairing-heap-fold
         pairing-heap-sort))

(use srfi-1)

;; Each pairing heap stores a suspension (i.e. (delay ...)) of its own
;; remove-min.  This is necessary because (pairing-heap-remove-min h)
;; has amortized bounds: on average it takes O(log(n)) time, but in
;; rare cases it may take O(n) time.  Because heaps are persistent,
;; the (pairing-heap-remove-min operation may be called multiple times
;; on *the same heap*.  Therefore, we must ensure that the result is
;; memoized, and subsequent calls take O(1) time to avoid repeatedly
;; doing O(n) work.
(define-record %ph compare elt remove-min-heap)
(define-record %ph-elt min sub-heaps)

(define *empty-elt* (gensym 'empty-elt))

(define pairing-heap? %ph?)

(define (pairing-heap-empty compare)
  (make-%ph compare *empty-elt* (delay (error 'pairing-heap-remove-min
                                              "cannot remove min from empty heap"))))

(define (pairing-heap-empty? h)
  (eq? *empty-elt* (%ph-elt h)))

(define (pairing-heap-min h)
  (%ph-elt-min (%ph-elt h)))

(define (<? compare obj1 obj2)
  (fx< (compare obj1 obj2) 0))

(define (sub-heaps h)
  (%ph-elt-sub-heaps (%ph-elt h)))

(define (pairing-heap-merge h1 h2)
  (cond
   ((pairing-heap-empty? h2) h1)
   ((pairing-heap-empty? h1) h2)
   (else
    (let ((compare (%ph-compare h1))
          (m1 (pairing-heap-min h1))
          (m2 (pairing-heap-min h2)))
      (if (<? compare m1 m2)
          (let ((h (make-%ph compare
                             (make-%ph-elt m1 (cons h2 (sub-heaps h1)))
                             #f)))
            (%ph-remove-min-heap-set! h (delay (%remove-min h)))
            h)
          (let ((h (make-%ph compare
                             (make-%ph-elt m2 (cons h1 (sub-heaps h2)))
                             #f)))
            (%ph-remove-min-heap-set! h (delay (%remove-min h)))
            h))))))

(define (pairing-heap-insert elt ph)
  (let ((compare (%ph-compare ph)))
    (pairing-heap-merge
     (make-%ph compare
               (make-%ph-elt elt '())
               (delay (pairing-heap-empty compare)))
     ph)))

;; %remove-min does the actual work (pairing-heap-remove-min only
;; forces a suspension of %remove-min).  We first merge adjascent
;; pairs of sub-heaps, and then merge the entire list of merged-pairs
;; into a single heap.
(define (%remove-min h)
  (let ((merged-pairs
         (let pair-loop ((hs (sub-heaps h))
                         (merged-hs '()))
           (cond
            ((null? hs) merged-hs)
            ((null? (cdr hs)) (cons (car hs) merged-hs))
            (else
             (pair-loop
              (cddr hs)
              (cons (pairing-heap-merge (car hs) (cadr hs))
                    merged-hs)))))))
    (fold pairing-heap-merge (pairing-heap-empty (%ph-compare h)) merged-pairs)))

;; Just force (%remove-min h).
(define (pairing-heap-remove-min h)
  (force (%ph-remove-min-heap h)))

(define (pairing-heap-fold kons knil h)
  (if (pairing-heap-empty? h)
      knil
      (fold (lambda (sub-heap acc)
              (pairing-heap-fold kons acc sub-heap))
            (kons (%ph-elt-min (%ph-elt h)) knil)
            (sub-heaps h))))

(define (pairing-heap-sort compare list-or-vector)
  (if (list? list-or-vector)
      (sort-list compare list-or-vector)
      (sort-vector compare list-or-vector)))

(define (sort-list compare list)
  (let ((rev-compare (lambda (obj1 obj2) (fx* -1 (compare obj1 obj2)))))
    (let ((h (fold pairing-heap-insert (pairing-heap-empty rev-compare) list)))
      (let loop ((sorted-elts '())
                 (h h))
        (if (pairing-heap-empty? h)
            sorted-elts
            (loop (cons (pairing-heap-min h) sorted-elts)
                  (pairing-heap-remove-min h)))))))

(define (sort-vector compare vec)
  (let* ((n (vector-length vec))
         (h (let h-loop ((h (pairing-heap-empty compare))
                         (i 0))
              (if (fx>= i n)
                  h
                  (h-loop (pairing-heap-insert (vector-ref vec i) h)
                          (fx+ i 1))))))
    (let ((result (make-vector n)))
      (let result-loop ((i 0)
                        (h h))
        (if (fx>= i n)
            result
            (begin
              (vector-set! result i (pairing-heap-min h))
              (result-loop (fx+ i 1) (pairing-heap-remove-min h))))))))

#|
Tests:

(define my-< (lambda (n1 n2)
               (cond
                ((fx< n1 n2) -1)
                ((fx= n1 n2) 0)
                (else 1))))

(define my-> (lambda (n1 n2) (fx* -1 (my-< n1 n2))))

(pairing-heap? (fold pairing-heap-insert (pairing-heap-empty my-<) '(1 2 3 4)))

(let ((h (fold pairing-heap-insert (pairing-heap-empty my-<) '(1 2 3 4))))
  (lset= = 
         '(1 2 3 4)
         (pairing-heap-fold cons '() h)))

(let ((h (fold pairing-heap-insert (pairing-heap-empty my-<) '(1 2 3 4))))
  (= (pairing-heap-min h) 1))

(let ((h (fold pairing-heap-insert (pairing-heap-empty my-<) '(4 3 2 1))))
  (and (= (pairing-heap-min h) 1)
       (= (pairing-heap-min
           (pairing-heap-remove-min h))
          2)))

(let ((heap-sort
       (lambda (list)
         (let ((h (fold pairing-heap-insert (pairing-heap-empty my-<) list)))
           (let loop ((result '()) (h h))
             (if (pairing-heap-empty? h)
                 (reverse result)
                 (loop (cons (pairing-heap-min h) result)
                       (pairing-heap-remove-min h))))))))
  (apply < (heap-sort '(10 9 8 5 7 6 4 2 1 3))))

(let ((h (fold pairing-heap-insert (pairing-heap-empty my-<) '(10 9 8 5 7 6 4 2 1 3))))
  (eq? (pairing-heap-remove-min h) (pairing-heap-remove-min h)))

(let ((heap-sort
       (lambda (list)
         (let ((h (fold pairing-heap-insert (pairing-heap-empty my->) list)))
           (let loop ((result '()) (h h))
             (if (pairing-heap-empty? h)
                 (reverse result)
                 (loop (cons (pairing-heap-min h) result)
                       (pairing-heap-remove-min h))))))))
  (apply > (heap-sort '(10 9 8 5 7 6 4 2 1 3))))

(apply > (pairing-heap-sort my-> '(10 9 8 5 7 6 4 2 1 3)))
(apply > (vector->list (pairing-heap-sort my-> (vector 10 9 8 5 7 6 4 2 1 3))))
(apply < (pairing-heap-sort my-< '(10 9 8 5 7 6 4 2 1 3)))
(apply < (vector->list (pairing-heap-sort my-< (vector 10 9 8 5 7 6 4 2 1 3))))
|#
