(declare (unit fail-safe))
(use posix)

;;Trigger when we have 10 seconds
(define (failsafe)
 (cond ((and *best-node* *best-node-so-far*) 
        (if (< (vector-ref *best-node* 0) (vector-ref *best-node-so-far* 0))
            (vector-ref *best-node* 2)
            (vector-ref *best-node-so-far* 2)))
       (*best-node* (vector-ref *best-node* 2))
       (*best-node-so-far* (vector-ref *best-node-so-far* 2))
       (else '(abort))))

(set-signal-handler! signal/int failsafe)
