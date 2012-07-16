(import foreign)
(use srfi-4)
#>
int hobopass(int a, char * vec, int vecsize, char b) {
		      printf("called with %d e0 (%d) size %d bool %d\n",a,vec[0],vecsize,b);
		      vec[1] = 2;
		      return 0;
}
<#
(define a (make-s8vector 4))
(s8vector-set! a 0 2)
(define passtest (foreign-lambda integer "hobopass" integer s8vector integer bool))
(passtest 1 a (s8vector-length a) #f)
(passtest 1 a (s8vector-length a) #t)
(display "ilike:")
(display (s8vector-ref a 1))
(display "sexy\n")