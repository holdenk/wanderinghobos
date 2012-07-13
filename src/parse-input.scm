;;Parse the initial input
;;Its a me MARIO! I make the pizza pie
(require-extension srfi-13)

(define-record map-info maplines water flooding waterproof)


(define (parse-input)
  (define (convert-to-symbol char)
	   (cond 
	    ((eq? #\R) 'ROBOT)
	    ((eq? #\#) 'WALL)
	    ((eq? #\*) 'ROCK)
	    ((eq? #\\) 'HUG);;Fuck calling this shit a Lambda
	    ((eq? #\L) 'LIFT)
	    ((eq? #\.) 'EARTH)
	    ((eq? #\space) 'SPACE)
	    (else (fatal "fuck you bad input"))
	   )
	   )
 (let* (
	 ;;Read the lines, split on empty line
	 (thelines (span (lambda (x) (string=? "" x))
		   (read-lines)))
         (mineinfo (cdr thelines))
	 )
    (map-info (list->vector (map (lambda (line)
				   (map convert-to-symbol (string->list line))) (car thelines))) 0 0 10)
    )
)
