(declare (unit failsafe))
(declare (uses search))
(use posix)

(define (failsafe s)
  (define moves (cond ((and *best-node* *best-node-so-far*) 
		       (if (> (vector-ref *best-node* 3) (vector-ref *best-node-so-far* 3))
			   (vector-ref *best-node* 2)
			   (vector-ref *best-node-so-far* 2)))
		      (*best-node* (vector-ref *best-node* 2))
		      (*best-node-so-far* (vector-ref *best-node-so-far* 2))
		      (else '(abort))
		      ))
 (display (output-moves moves))
 (exit 0)
)
(define register-failsafe
  (lambda ()
    (set-signal-handler! signal/int (lambda (s) (failsafe s)))
    )
)