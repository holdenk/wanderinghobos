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

(define-gs-record world board water flooding waterproof underwater iteration rocks)

(define (parse-input)
  (define (convert-to-symbol char)
	   (cond 
	    ((eq? char #\R) 'robot)
	    ((eq? char #\#) 'wall)
	    ((eq? char #\*) 'rock)
	    ((eq? char #\\) 'hug);;Fuck calling this shit a Lambda
	    ((eq? char #\L) 'closed-lift)
	    ((eq? char #\O) 'open-lift)
	    ((eq? char #\.) 'earth)
	    ((eq? char #\space) 'space)
	    (else (fatal "fuck you bad input"))
	   )
	   )
 (let* (
	 ;;Read the lines, split on empty line
	 (thelines (let-values (((a b) (span (lambda (x) (string=? x "")) (read-lines)))) (cons a b))
			 )
         (mineinfo (cdr thelines))
	 )
  (make-world (list->vector (map (lambda (line)
				   (list->vector (map convert-to-symbol (string->list line)))) (car thelines))) 0 0 10 0 0 '())
    )
)
