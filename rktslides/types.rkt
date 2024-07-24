(module types (lib "slideshow.ss" "texpict")

  (require (lib "code.ss" "slideshow")
           "colors.ss"
	   "utils.ss"
	   "alg.ss"
	   (lib "list.ss"))
  
  (provide type-tree type-tree/formal
           no-type
           _ __ open-paren close-paren
           subscript subnt
           ts crs arg-crs mtenv inenv
           tbind cbind -> Env forall tsubs
           E E+ E-prime M T A X prime)
  
  (define type-tree
    (case-lambda
     [(prepre pre a sep b sep2 c post post2 atype btype ctype etype wtype tv-append aenv benv cenv eenv wenv close?)
      (type-tree prepre pre a sep b sep2 c post post2 atype btype ctype etype wtype tv-append)]
     [(prepre pre a sep b sep2 c post post2 atype btype ctype etype wtype tv-append)
      (let ([prog (lambda (s)
		    (cond
		     [(string? s) (alg-code s)]
		     [s s]
		     [else #f]))])
	(let ([pre (prog pre)]
	      [prepre (launder (prog prepre))]
	      [a (prog a)]
	      [sep (prog sep)]
	      [b (prog b)]
	      [sep2 (prog sep2)]
	      [c (prog c)]
	      [post (launder (prog post))]
	      [post2 (launder (prog post2))]
	      [at atype]
	      [atype (if atype (prog atype) (blank))]
	      [bt btype]
	      [btype (if btype (prog btype) (blank))]
	      [ct ctype]
	      [ctype (and ctype (prog ctype))]
	      [et etype]
	      [etype (prog etype)]
	      [wtype (and wtype (prog wtype))])
	  (let* ([expr (hbl-append pre a sep (or b (blank)) (or sep2 (blank)) (or c (blank)) post)]
		 [expr+ (hbl-append prepre expr post2)]
		 [types (let ([ab (hbl-append (* 3 font-size) atype btype)])
			  (if ctype
			      (hbl-append (* 3 font-size) ab ctype)
			      ab))])
	    (let ([whole (let* ([sep (/ font-size 2)]
				[p (vc-append sep
					      (tv-append sep
							 expr+
							 types)
					      (inset etype gap-size 0 0 0))])
			   (if wtype 
			       (vc-append sep p wtype)
			       p))])
	      (let ([base (ghost whole)]
		    [show (lambda (whole p)
                            (pin-over whole p lt-find p))]
		    [tline (lambda (whole e t dy)
			     (let-values ([(ec eb) (find-cb whole e)]
					  [(tc tt) (find-ct whole t)]
					  [(w/2) (/ (pict-width e) 2)])
                               (cons-colorized-picture
                                whole
                                GreenColor
                                `((connect ,tc ,tt ,ec ,(+ eb dy))
                                  (connect ,(- ec w/2) ,(+ eb dy) ,(+ ec w/2) ,(+ eb dy))))))])
		(let* ([start (show (show (show base expr) prepre) post2)]
		       [atype (if at
				  (tline (show start atype)
					 a atype 0)
				  start)]
		       [btype (if bt
				  (tline (show atype btype)
					 b btype 0)
				  atype)]
		       [ctype (if ctype
				  (tline (show btype ctype)
					 c ctype 0)
				  btype)]
		       [all (if et
				(tline (show ctype etype)
				       expr etype -5)
				ctype)])
		  (map list
		       (append
			(list start)
			(if at (list atype) null)
			(if bt (list btype) null)
			(if ct (list ctype) null)
			(if et (list all) null)
			(if wtype 
			    (list (tline (show all wtype) expr+ wtype -10)) 
			    null)))))))))]))

  (define type-tree/formal
    (case-lambda
     [(prepre pre a sep b sep2 c post post2 atype btype ctype etype wtype tv-append aenv benv cenv eenv wenv close?)
      (let* ([prog (lambda (s)
                     (cond
                       [(string? s) (alg-code s)]
                       [s s]
                       [else #f]))]
             [typed (lambda (e a t) (let ([j (hbl-append a __ (code :) __ t)])
                                      (if e
                                          (hbl-append (prog e) __ (ts) __ j)
                                          j)))])
	(let ([pre (prog pre)]
	      [prepre (prog prepre)]
	      [a (prog a)]
	      [sep (prog sep)]
	      [b (prog b)]
	      [sep2 (prog sep2)]
	      [c (prog c)]
	      [post (prog post)]
	      [post2 (prog post2)]
	      [at atype]
	      [atype (if atype (prog atype) (blank))]
	      [bt btype]
	      [btype (if btype (prog btype) (blank))]
	      [ct ctype]
	      [ctype (and ctype (prog ctype))]
	      [et etype]
	      [etype (prog etype)]
	      [wtype (and wtype (prog wtype))])
	  (let* ([expr (hbl-append pre a sep b (or sep2 (blank)) (or c (blank)) post)]
		 [expr+ (hb-append prepre expr post2)])
	    (let ([base (infer (typed eenv expr etype)
			       (apply
				(if close? ante-append/close ante-append)
				(typed aenv a atype)
				(append
				 (if bt
				     (list (typed benv b btype))
				     null)
				 (if ctype
				     (list (typed cenv c ctype))
				     null))))])
	      (list (list
		     (if wtype
			 (infer (typed wenv expr+ wtype) base)
			 base)))))))]
     [(prepre pre a sep b sep2 c post post2 atype btype ctype etype wtype tv-append)
      (type-tree/formal prepre pre a sep b sep2 c post post2 atype btype ctype etype wtype tv-append #f #f #f #f #f #f)]))

  (define no-type (colorize (bit "no type") RedColor))

  (define _ (tt ""))
  (define __ (tt " "))
  
  (define (open-paren) (colorize (tt "{") (current-base-color)))
  (define (close-paren) (colorize (tt "}") (current-base-color)))

  (define (subscript n)
    (text (format "~a" n) `(subscript . ,(current-main-font)) (current-font-size)))
  
  (define (subnt s n)
    (hbl-append (nonterm s) 
                (subscript n)))

  (define (ts) (text "\u22A2" 'symbol (current-font-size)))

  (define arg-crs 
    (case-lambda
      [() (text "\u00D7" (current-main-font) (current-font-size))]
      [(a b) (hbl-append a (t " ") b)]))

  (define crs 
    (case-lambda
      [() (text "\u00D7" (current-main-font) (current-font-size))]
      [(a b) (hbl-append (t "(") a (t " ") (crs) (t " ") b (t ")"))]))
  (define (mtenv) (text "\u2205" 'symbol (current-font-size)))
  (define (inenv s)
    (hbl-append (t "[ ") s (t " ]")))
  (define (tbind e ty)
    (hbl-append e (text "\u2190" 'symbol (current-font-size)) ty))
  (define (cbind e ty)
    (hbl-append e (t "@") ty))
  (define ->
    (case-lambda
      [() sym:rightarrow]
      [(a b) (hbl-append (t "(") a (t " ") (->) (t " ") b (t ")"))]))
  (define (forall var expr)
    (hbl-append (t "(")
                (text "\u2200" 'symbol (current-font-size))
		(t " (") var (t ") ") 
		expr
                (t ")")))

  (define (tsubs ty id idty)
    (E+ ty (inenv (tbind id idty))))
  
  (define (Env) (nonterm "Env"))

  (define (E) (colorize (text "\u0393" 'symbol (current-font-size)) BlueColor))
  
  (define (E-prime)
    (hbl-append (E) (t "ʹ")))

  (define (E+ env binds)
    (hbl-append line-sep env binds))

  (define (prime F)
    (case-lambda
     [() (hbl-append (F) (t "ʹ"))]
     [(n) (let ([p (F n)])
            (refocus
             (lbl-superimpose p
                              (hbl-append (ghost (F)) (t "ʹ")))
             p))]))
  
  (define M
    (case-lambda
      [() (colorize (bt "e") BlueColor)]
      [(n) (hbl-append (M) (subscript n))]))

  (define X
    (case-lambda
      [() (colorize (bt "x") BlueColor)]
      [(n) (hbl-append (X) (subscript n))]))

  (define A
    (case-lambda
      [() (colorize (bt "a") BlueColor)]
      [(n) (hbl-append (A) (subscript n))]))

  (define T
    (case-lambda
      [() (colorize (text "\u03C4" 'symbol (current-font-size)) BlueColor)]
      [(n) (hbl-append (T) (subscript n))])))

