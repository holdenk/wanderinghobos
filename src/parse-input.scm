;;Parse the initial input
;;Its a me MARIO! I make the pizza pie
(declare (unit parse-input))

(use srfi-13)
(use list-utils)
(use srfi-1)
(require-extension records)
(require-extension srfi-17)
(require-extension srfi-9)

(define-syntax (define-gs-record x r c)
  (let ((type (cadr x))
        (fields (cddr x))
 (%begin (r 'begin))
 (%define-record (r 'define-record))
 (%define (r 'define))
 (%getter-with-setter (r 'getter-with-setter)))
    `(,%begin
      (,%define-record ,type ,@fields)
      ,@(map (lambda (f)
	(let* ((getter (string->symbol
			(string-append
			 (symbol->string 
			  (strip-syntax type))
			 "-"
			 (symbol->string 
			  (strip-syntax f)))))
	       (setter (string->symbol
			(string-append
			 (symbol->string
			  (strip-syntax getter))
			 "-set!"))))
	  (list %define getter (list %getter-with-setter getter setter))))
      fields))))

(define-gs-record map-info maplines water flooding waterproof)


(define (parse-input)
  (define (convert-to-symbol char)
	   (cond 
<<<<<<< HEAD
	    ((eq? char #\R) 'ROBOT)
	    ((eq? char #\#) 'WALL)
	    ((eq? char #\*) 'ROCK)
	    ((eq? char #\\) 'HUG);;Fuck calling this shit a Lambda
	    ((eq? char #\L) 'LIFT)
	    ((eq? char #\.) 'EARTH)
	    ((eq? char #\space) 'SPACE)
	    (else (fatal "fuck you bad input"))
=======
	    ((eq? #\R) 'ROBOT)
	    ((eq? #\#) 'WALL)
	    ((eq? #\*) 'ROCK)
	    ((eq? #\\) 'HUG);;Fuck calling this shit a Lambda
	    ((eq? #\L) 'LIFT)
	    ((eq? #\.) 'EARTH)
	    ((eq? #\space) 'SPACE)
	    (else (fatal "Bad input"))
>>>>>>> 09a5a7e5033a75b8e4844ff94ad6a7448c14f218
	   )
	   )
 (let* (
	 ;;Read the lines, split on empty line
	 (thelines (let-values (((a b) (span (lambda (x) (string=? x "")) (read-lines)))) (cons a b))
			 )
         (mineinfo (cdr thelines))
	 )
    (make-map-info (list->vector (map (lambda (line)
				   (list->vector (map convert-to-symbol (string->list line)))) (car thelines))) 0 0 10)
    )
)