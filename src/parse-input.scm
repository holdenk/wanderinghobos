;;Parse the initial input
;;Its a me MARIO! I make the pizza pie
(declare (unit parse-input))

(use srfi-13)
(use srfi-69)
(use list-utils)
(use srfi-1)
(use vector-lib)
(require-extension records)
(require-extension srfi-17)
(require-extension srfi-9)

(define (fatal x)
  (display "fatal \"")
  (display x)
  (display "\"")
)

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

(define (symbol-to-char symbol)
 (case symbol
  ((robot) #\R)
  ((wall) #\#)
  ((rock) #\*)
  ((hug) #\\)
  ((closed-lift) #\L)
  ((open-lift) #\O)
  ((earth) #\.)
  ((empty) #\space)
  ((hug) #\\)
  (else (display symbol)(newline)(error "bad world"))))

(define (world-pp world)
 (vector-for-each (lambda (i l)
                   (vector-for-each (lambda (i l2) (display (symbol-to-char l2))) l)
                   (display "\n"))
                  (world-board world))
 (display (format "Water ~A\nFlooding ~A\nWaterproof ~A\nIteration ~A\nRocks ~A\n"
                  (world-water world)
                  (world-flooding world)
                  (world-waterproof world)
                  (world-iteration world)
                  (world-rocks world))))

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
	    ((eq? char #\space) 'empty)
	    (else (fatal char))
	   )
	   )
 (let* (
	 ;;Read the lines, split on empty line
	 (thelines (let-values (((a b) (span (lambda (x) (not (string=? "" x))) (read-lines)))) (cons a b))
			 )
         (mineinfo (alist->hash-table 
		    (map (lambda (s) 
			   (cons (read s) 
				 (read s))) 
			 (map open-input-string 
			      (cdr thelines)
			      ))))
	 )
    (make-world (list->vector (map (lambda (line)
				   (list->vector (map convert-to-symbol (string->list line)))) (car thelines)))
		   (hash-table-ref/default mineinfo "Water" 0)
		   (hash-table-ref/default mineinfo "Flooding" 0)
		   (hash-table-ref/default mineinfo "Waterproof" 10)
                   0
                   0
                   '()
		   )
    )
)
