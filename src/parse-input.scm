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

(define (number-of-fields string)
 (let loop ((n 0) (chars (string->list string)))
  (if (null? chars)
      n
      (if (char-whitespace? (first chars))
	  (loop n (cdr chars))
	  (loop (+ n 1)
		(let loop ((chars chars))
		 (if (or (null? chars) (char-whitespace? (first chars)))
		     chars
		     (loop (cdr chars)))))))))

(define (map-n f n)
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (field-ref string n)
 (let loop ((n n) (chars (string->list string)))
  (if (char-whitespace? (first chars))
      (loop n (cdr chars))
      (if (zero? n)
	  (let loop ((chars chars) (field '()))
	   (if (or (null? chars) (char-whitespace? (first chars)))
	       (list->string (reverse field))
	       (loop (cdr chars) (cons (first chars) field))))
	  (loop (- n 1)
		(let loop ((chars chars))
		 (if (char-whitespace? (first chars))
		     chars
		     (loop (cdr chars)))))))))

(define (fields string)
 (map-n (lambda (i) (field-ref string i)) (number-of-fields string)))

(define (for-each-n f n)
 (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (for-each-vector f v)
 (for-each-n (lambda (i) (f (vector-ref v i))) (vector-length v)))

(define (map-n1 f n)
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-vector f v)
 (let ((u (make-vector (vector-length v))))
  (for-each-n (lambda (i) (vector-set! u i (f (vector-ref v i))))
   (vector-length v))
  u))

(define (const a) (lambda _ a))

(define (map-matrix f m) (map-vector (lambda (v) (map-vector f v)) m))

(define (for-each-matrix f m) (for-each-vector (lambda (v) (for-each-vector f v)) m))

(define (map-indexed-matrix f m)
 (map-indexed-vector (lambda (r i) (map-indexed-vector (lambda (c j) (f c j i)) r)) m))

(define (for-each-indexed-vector f v)
 (for-each-n (lambda (i) (f (vector-ref v i) i)) (vector-length v)))

(define (map-indexed-vector f v)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (f (vector-ref v i) i)))
   (vector-length v))
  u))

(define (for-each-indexed-matrix f m)
 (for-each-indexed-vector
  (lambda (r i) (for-each-indexed-vector (lambda (c j) (f c i j)) r))
  m))

(define (reverse-vector v) (list->vector (reverse (vector->list v))))

(define (for-each-board-index f board)
 (for-each-n (lambda (y)
              (for-each-n (lambda (x) (f x y))
               (board-width board)))
  (board-height board)))
(define (board-height board) (vector-length board))
(define (board-width board) (vector-length (vector-ref board 0)))

(define (fatal x)
  (display "fatal \"")
  (display x)
  (display "\"")
)

(define (output-moves list)
  (define (sym-to-char sy)
           (cond
            ((eq? sy 'down) #\D)
            ((eq? sy 'left) #\L)
            ((eq? sy 'right) #\R)
            ((eq? sy 'up) #\U )
            ((eq? sy 'wait) #\W)
            ((eq? sy 'abort) #\A)
						((eq? sy 'shave) #\S)))
  (list->string (map sym-to-char (reverse list))))

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

(define-gs-record world board water flooding waterproof underwater iteration rocks robot-location hugs hug-count lift-location fuckedrocks beard razors)

(define (world-trampoline-connections world)
 (let ((r '()))
  (for-each-matrix
   (lambda (a) (when (is-trampoline-in? a) (set! r (cons (cons (vector-ref a 1) (vector-ref a 2)) r))))
   (world-board world))
  r))

(define-record-printer (world world port)
 (begin
  (format port "(~%")
  (vector-for-each (lambda (i l)
                    (vector-for-each (lambda (i l2) (display (symbol-to-char l2) port)) l)
                    (display "\n" port))
                   (world-board world))
  (format port
          "water ~A, flooding ~A, waterproof ~A, iteration ~A, underwater ~A, rocks ~A, tramps ~A, robot ~A"
          (world-water world)
          (world-flooding world)
          (world-waterproof world)
          (world-iteration world)
          (world-underwater world)
          (world-rocks world)
          (world-trampoline-connections world)
          (world-robot-location world))
  (format port ")")))

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
  (else 
   (cond ((is-trampoline-in? symbol) (trampoline-name symbol))
         ((is-trampoline-out? symbol) (trampoline-name symbol))
         (else (display symbol)(newline)(error "bad world"))))))

(define (world-pp port world)
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

(define (make-trampoline-in name out) (vector 'trampoline-in name out))
(define (make-trampoline-out name in) (vector 'trampoline-out name in))
(define (trampoline-name t) (vector-ref t 1))
(define (is-trampoline-in? obj) (and (vector? obj) (equal? (vector-ref obj 0) 'trampoline-in)))
(define (is-trampoline-out? obj) (and (vector? obj) (equal? (vector-ref obj 0) 'trampoline-out)))
(define (is-trampoline? obj) (or (is-trampoline-in? obj) (is-trampoline-out? obj)))
(define (set-trampoline-in-out! obj out) (vector-set! obj 2 out))
(define (set-trampoline-out-in! obj in) (vector-set! obj 2 in))

(define (memq/default obj alist default)
 (let ((mem (assq obj alist))) (if mem (cdr mem) default)))

(define (find-robot-board board)
 (call-with-current-continuation
  (lambda (k)
   (for-each-board-index 
    (lambda (x y) (when (robot? board x y) (k (list x y))))
    board)
   (error "AIN'T GOT NO ROBOT?!?"))))

(define (find-lift-board board)
 (call-with-current-continuation
  (lambda (k)
   (for-each-board-index 
    (lambda (x y) (when (lift? board x y) (k (list x y))))
    board)
   (error "AIN'T GOT NO LIFT?!? WE SHOULD BE DONE!"))))

(define (find-hugs-board board)
 (vector-fold 
  (lambda (y hugs vector) 
   (vector-fold (lambda (x hugs element) (if (eq? element 'hug)
                                        (cons (list x y) hugs)
                                        hugs))
                hugs vector))
  '() board))

(define (copy-board board) (map-matrix identity board))

(define (count-obj-board obj board)
 (vector-fold (lambda (i count v) (+ count (vector-count (lambda (i e) (eq? obj e)) v)))
              0 board))

(define (count-hugs world) (count-obj 'hug world))
(define (count-earth world) (count-obj 'earth world))
(define (count-obj obj world)
 (vector-fold 
  (lambda (i count v) (+ count (vector-count (lambda (i e) (eq? obj e)) v)))
  0 (world-board world)))

(define (parse-input . port)
 (define (convert-to-symbol char)
  (cond ((eq? char #\R) 'robot)
        ((eq? char #\#) 'wall)
        ((eq? char #\*) 'rock)
        ((eq? char #\\) 'hug) ;;Fuck calling this shit a Lambda
        ((eq? char #\L) 'closed-lift)
        ((eq? char #\O) 'open-lift)
        ((eq? char #\.) 'earth)
				((eq? char #\W) 'beard)
				((eq? char #\!) 'razor)
				((eq? char #\@) 'horock)
        ((eq? char #\space) 'empty)
        ((member char (string->list "ABCDEFGHI")) (make-trampoline-in char #f))
        ((member char (string->list "123456789")) (make-trampoline-out char '()))
        (else (fatal char))))
 (let* ( ;;Read the lines, split on empty line
        (thelines (let-values (((a b) (span (lambda (x) (not (string=? "" x)))
                                            (apply read-lines port))))
                   (cons a b)))
        (mineinfo (map (lambda (s)
                        (cons
                         (string->symbol (field-ref s 0))
                         (case (string->symbol (field-ref s 0))
                          ((Water) (string->number (field-ref s 1)))
                          ((Flooding) (string->number (field-ref s 1)))
                          ((Waterproof) (string->number (field-ref s 1)))
                          ((Trampoline) (cons (car (string->list (field-ref s 1))) (car (string->list (field-ref s 3)))))
													((Growth) (string->number (field-ref s 1)))
													((Razors) (string->number (field-ref s 1)))
                          (else (error "Bad")))))
                       (remove (lambda (a) (equal? a "")) (cdr thelines))))
        (wwidth (apply max (map (lambda (s) (string-length s)) (car thelines))))
        (trampolines (map (lambda (a) (cdr a)) (filter (lambda (e) (equal? (car e) 'Trampoline)) mineinfo))))
  (let ((board 
         (map-matrix
          (lambda (e)
           (cond  ((is-trampoline-in? e)
                   (let ((trampoline (find (lambda (t) (equal? (car t) (trampoline-name e))) trampolines)))
                    (unless trampoline (error "Missing trampoline" (trampoline-name e) trampolines))
                    (set-trampoline-in-out! e (cdr trampoline))
                    e))
                  ((is-trampoline-out? e)
                   (let ((trampolines (filter (lambda (t) (equal? (cdr t) (trampoline-name e))) trampolines)))
                    (unless trampolines (error "Missing trampoline" (trampoline-name e) trampolines))
                    (set-trampoline-out-in! e (map car trampolines))
                    e))
                  (else e)))
          (list->vector (map (lambda (line)
                              (list->vector
                               (map convert-to-symbol
                                    (string->list
                                     (string-pad-right line wwidth))))) (car thelines))))))
   (make-world board
               (memq/default 'Water mineinfo 0)
               (memq/default 'Flooding mineinfo 0)
               (memq/default 'Waterproof mineinfo 10)
               0
               0
               '()
               (find-robot-board board)
               (find-hugs-board board)
               (count-obj-board 'hug board)
               (find-lift-board board)
							 #f
							 (memq/default 'Growth mineinfo 0)
							 (memq/default 'Razors mineinfo 0)))))

(define (string->world string)
        (call-with-input-string string parse-input))
(define (file->world file-name)
        (call-with-input-file file-name parse-input))
