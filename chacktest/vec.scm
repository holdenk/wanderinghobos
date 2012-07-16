(import foreign)
(use srfi-4)
#>
char ** hobopass(int a, char * vec, int vecsize, char b) {
		      printf("called with %d e0 (%d) size %d bool %d\n",a,vec[0],vecsize,b);
		      vec[1] = 2;
		      char ** m = malloc(3*sizeof(char *));
		      m[0] = malloc(5*sizeof(char));
		      m[0][0] = 'a';
		      m[0][1] = 'b';
		      m[0][2] = 0;
		      m[1] = 0;
		      return m;
}
<#
(define a (make-s8vector 4))
(s8vector-set! a 0 2)
(define passtest (foreign-lambda c-string-list "hobopass" integer s8vector integer bool))
(passtest 1 a (s8vector-length a) #f)
(display "rlist:")
(display (passtest 1 a (s8vector-length a) #t))
(display "\n")
(display "ilike:")
(display (s8vector-ref a 1))
(display "sexy\n")