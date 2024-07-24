
(module tvfae-types (lib "slideshow.ss" "texpict")

  (require (lib "code.ss" "slideshow")
           "colors.ss"
	   "utils.ss"
	   "alg.ss"
           "types.ss"
	   (lib "list.ss"))

  (provide tyid-def E-prime-def
	   withtype-rule
	   cases-rule
           N Q)

  (define N
    (case-lambda
     [() (colorize (bt "x") BlueColor)]
     [(s) (hbl-append (N) (subscript s))]))

  (define Q
    (case-lambda
     [() (colorize (bt "D") BlueColor)]
     [(s) (hbl-append (N) (subscript s))]))

  (define (tyid-def)
    (hbl-append (Q) (t " = ") 
                (cbind (N 1) (T 1)) (t "+") 
                (cbind (N 2) (T 2))))
  

  (define (E-prime-def)
    (hbl-append (E-prime)
                (t "=")
                (E+ (E) (inenv (hbl-append
                                (tyid-def)
                                (t ", ")
                                (tbind (N 1) (-> (T 1) (Q)))
                                (t ", ")
                                (tbind (N 2) (-> (T 2) (Q))))))))
                 
  (define (withtype-rule extras)
    (scale/improve-new-text
     (apply
      infer (code #,(E) #,(ts) {let-type {#,(Q) [#,(N 1) #,(T 1)] [#,(N 2) #,(T 2)]} #,(M)} : #,(T 0))
      (E-prime-def)
      (append
       (list
	(ante-append (code #,(E-prime) #,(ts) #,(T 1))
		     (code #,(E-prime) #,(ts) #,(T 2))
		     (code #,(E-prime) #,(ts) #,(M) : #,(T 0))))
       (extras)))
     0.85))

  (define (cases-rule extras)
    (scale/improve-new-text
     (let ([line (lambda (n)
		   (code [#,(N n) {#,(N (+ n 2))} #,(M n)]))])
       (apply
	infer (code #,(E) #,(ts) {type-case #,(Q) #,(M 0) #,(line 1) #,(line 2)} : #,(T 0))
	(hbl-append (E)
		    (t "=")
		    (inenv (code #,(t "...") #,(tyid-def) #,(t "..."))))
	(append
	 (extras)
	 (list
	  (ante-append 
	   (code #,(E) #,(ts) #,(M 0) : #,(Q))
	   (code #,(E+ (E) (inenv (tbind (N 3) (T 1)))) #,(ts) #,(M 1) : #,(T 0))
	   (code #,(E+ (E) (inenv (tbind (N 4) (T 2)))) #,(ts) #,(M 2) : #,(T 0)))))))
     0.85)))


  
