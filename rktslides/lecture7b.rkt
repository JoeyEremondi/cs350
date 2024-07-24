#lang slideshow
(require slideshow/code
         slideshow/play
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss"
         "utils/crate.rkt"
         "utils/encodings-checklist.rkt")

(define (align . l) (apply para #:width (* 0.95 (get-client-w #:aspect 'fullscreen)) l))

;; ------------------------------------------------------------
(part 1)

(encodings-checklist
 'next
 (colorize (para #:align 'right (t "... and recursive functions?"))
           "blue"))

(define fac-title "Factorial")
(define fac-title/plait "Factorial in Plait")
(define fac+rec-title "Make-Recursive and Factorial")

(define fac-layout 'top)

(slide
 #:title fac-title/plait
 #:layout fac-layout
 (code (local [(define fac
                 (lambda (n)
                   (if (zero? n)
                       1
                       (* n (fac (- n 1))))))]
         (fac 10)))
 'next
 (blank)
 (blank)
 (para #:align 'center
       (code local) "binds both in the body"
       "expression and in the"
       "binding expression"))

(slide
 #:title fac-title/plait
 #:layout fac-layout
 (code (letrec ([fac
                 (lambda (n)
                   (if (zero? n)
                       1
                       (* n (fac (- n 1)))))])
         (fac 10)))
 'next
 (blank)
 (blank)
 (para #:align 'center
       (code letrec) "has the shape of" (code let)
       "but the binding structure of" (code local)))

(slide
 #:title fac-title/plait
 #:layout fac-layout
 (code (let ([fac
              (lambda (n)
                (if (zero? n)
                    1
                    (* n (fac (- n 1)))))])
         (fac 10)))
 'next
 (blank)
 (para #:align 'center
       "Doesn't work, because" (code let) "binds" (code fac)
       "only in the body"))

(define (goal-slide [next 'nothing]
                    #:hilite1 [hilite1 values]
                    #:hilite2 [hilite2 values]
                    #:hilite3 [hilite3 values])
  (slide
   #:title fac-title
   (para "Overall goal: Implement" (code letrec) "as syntactic sugar for Curly")
   (code {letrec {[_name _rhs]}
           _name})
   next
   (blank)
   (para (hilite1 (bt "Step 1:")) "Implement" (code fac) "in Plait without" (code letrec))
   next
   (blank)
   (para (hilite2 (bt "Step 2:")) "Isolate the" (code _rhs))
   (code .... {lambda {n}
                {if {zero? n}
                    1
                    {* n {fac {- n 1}}}}} ....)
   next
   (blank)
   (para (hilite3 (bt "Step 3:")) "Surrounding as a" (code parse) "transformation for Curly")))

(define (hilite p)
  (refocus (cc-superimpose
            (colorize (filled-rectangle (+ (pict-width p) gap-size)
                                        (+ (pict-height p) gap-size))
                      "yellow")
            p)
           p))

(goal-slide 'next)

(define seurat (bitmap "utils/seurat.png"))

(slide
 #:title "This is Difficult..."
 (let ([p (scale seurat 20)])
   (clip (inset p
                (* -0.45 (pict-width p))
                (* -0.48 (pict-height p)))))
 'next
 (blank (* 2 gap-size))
 (bitmap "utils/seurat.png"))

;; ----------------------------------------
(part 2)

(goal-slide #:hilite1 hilite)

(slide
 #:title fac-title
 #:layout fac-layout
 (code (let ([fac
              (lambda (n)
                (if (zero? n)
                    1
                    (* n (fac (- n 1)))))])
         (fac 10)))
 'next
 (blank)
 (para #:align 'center
       "At the point that we call" (code fac)
       ", obviously we have a binding for" (code fac)
       "...")
 'next
 (colorize (para #:align 'right "... so pass it as an argument!") BlueColor))

(define-syntax hcode
  (syntax-rules ()
    [(_ v ...) (parameterize ([code-colorize-enabled #t])
                 (code v ...))]))

(define-syntax hylite
  (syntax-rules ()
    [(_ v) (colorize
            (parameterize ([code-colorize-enabled #f])
              (code v))
            RedColor)]))

(define-syntax hylite2
  (syntax-rules ()
    [(_ v) (colorize
            (parameterize ([code-colorize-enabled #f])
              (code v))
            BlueColor)]))

(define-syntax hylite3
  (syntax-rules ()
    [(_ v) (colorize
            (parameterize ([code-colorize-enabled #f])
              (code v))
            "purple")]))

(slide
 #:title fac-title
 #:layout fac-layout
 'alts
 (list
  (list
   (code (let ([facX
                (lambda (#,(hylite facX) n)
                  (if (zero? n)
                      1
                      (* n (fac (- n 1)))))])
           (facX #,(hylite facX) 10))))
  (list
   (code (let ([facX
                (lambda (#,(hylite facX) n)
                  (if (zero? n)
                      1
                      (* n (facX #,(hylite facX) (- n 1)))))])
           (facX #,(hylite facX) 10)))))
 'next
 (blank)
 (colorize (para #:align 'right "Wrap this to get" (code fac) "back...") BlueColor))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode #,(hylite
           (let ([fac
                  (lambda (n)
                    #,(hcode
                       (let ([facX
                              (lambda (facX n)
                                (if (zero? n)
                                    1
                                    (* n (facX facX (- n 1)))))])
                         (facX facX #,(hylite n)))))])
             #,(hcode (fac 10))))))

;; ------------------------------------------------------------
(part 3)

(goal-slide #:hilite2 hilite)

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               (lambda (n)
                 (let ([facX
                        (lambda (facX n)
                          (if (zero? n)
                              1
                              (* n (facX facX (- n 1)))))])
                   (facX facX  n)))])
          (fac 10)))
 'next
 (blank)
 (para "But Curly has only single-argument functions..."))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               (lambda (n)
                 (let ([facX
                        (lambda (facX)
                          (lambda (n)
                            (if (zero? n)
                                1
                                (* n (#,(hylite (facX facX)) (- n 1))))))])
                   (#,(hylite (facX facX)) n)))])
          (fac 10)))
 'next
 (blank)
 (colorize
  (vr-append
   (current-line-sep)
   (para #:width client-w #:align 'right "Simplify:" (code (lambda (n) (let ([f ...]) ((f f) n)))))
   (para #:width client-w #:align 'right sym:implies (code (let ([f ...]) (f f))) "..."))
  BlueColor))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               (let ([facX
                      (lambda (facX)
                        (lambda (n)
                          (if (zero? n)
                              1
                              (* n ((facX facX) (- n 1))))))])
                 (facX facX))])
          (fac 10))))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               (let ([facX
                      (lambda (facX)
                        (code:comment "Almost looks like original fac:")
                        (lambda (n)
                          (if (zero? n)
                              1
                              (* n ((facX facX) (- n 1))))))])
                 (facX facX))])
          (fac 10)))
 'next
 (blank)
 (colorize
  (para #:align 'right
        "More like original: introduce a local binding for"
        (code (facX facX)) "...")
  BlueColor))


(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               (let ([facX
                      (lambda (facX)
                        #,(hylite
                           (let ([fac (facX facX)])
                             #,(hcode
                                (code:comment "Exactly like original fac:"))
                             #,(hcode
                                (lambda (n)
                                  (if (zero? n)
                                      1
                                      (* n (#,(hylite fac) (- n 1)))))))))])
                 (facX facX))])
          (fac 10)))
 'next
 (blank)
 (vl-append
  (current-line-sep)
  (para #:fill? #f
        (bt "Oops!") sym:emdash "this is an infinite loop")
  (para #:fill? #f
        "We used to evaluate" (code (facX facX)) "only when"
        (code n) "is non-zero"))
 'next
 (blank)
 (colorize
  (para #:align 'right "Delay" (code (facX facX)) "...")
  BlueColor))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               (let ([facX
                      (lambda (facX)
                        (let ([fac #,(hylite
                                      (lambda (x) 
                                        (#,(hcode (facX facX)) x)))])
                          (code:comment "Exactly like original fac:")
                          (lambda (n)
                            (if (zero? n)
                                1
                                (* n (fac (- n 1)))))))])
                 (facX facX))])
          (fac 10))))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               #,(hylite
                  (let ([facX
                         (lambda (facX)
                           (let ([fac (lambda (x) 
                                        ((facX facX) x))])
                             #,(hcode
                                (code:comment "Exactly like original fac:")
                                (lambda (n)
                                  (if (zero? n)
                                      1
                                      (* n (fac (- n 1))))))))])
                    (facX facX)))])
          (fac 10))))

(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               #,(hylite
                  (let ([facX
                         (lambda (facX)
                           (let ([fac (lambda (x) 
                                        ((facX facX) x))])
                             #,(hylite3
                                (#,(hylite2
                                    (lambda (fac)
                                      #,(hcode
                                         (code:comment "Exactly like original fac:")
                                         (lambda (n)
                                           (if (zero? n)
                                               1
                                               (* n (fac (- n 1))))))))
                                 fac))))])
                    (facX facX)))])
          (fac 10))))

(define (combined n)
  (define (mk-lifted inside)
    (hylite
     (let ([fX
            (lambda (fX)
              (let ([f (lambda (x) 
                         ((fX fX) x))])
                #,(hylite3 (#,inside
                            f))))])
       (fX fX))))
  (define (top body)
    (inset
     (scode (define mk-rec
              (lambda (#,(hylite2 body-proc))
                #,body))
            code:blank)
     0 0 0 (current-code-line-sep)))
  (define (bottom rhs)
    (scode
     (let ([fac
            #,rhs])
       (fac 10))))
  (define fac
    (hylite2 
     (lambda (fac)
       #,(hcode
          (code:comment "Exactly like original fac:")
          (lambda (n)
            (if (zero? n)
                1
                (* n (fac (- n 1)))))))))

  (define fac-g1 (launder (ghost fac)))
  (define fac-g2 (launder (ghost fac)))

  (define pre-lifted-dot (blank 0))
  (define lifted
    (mk-lifted (fade-pict n fac-g1 (hylite2 body-proc))))
  (define lifted-g2 (launder (ghost lifted)))

  (define pre-rhs lifted-g2)
  (define post-rhs (code (mk-rec
                          #,fac-g2)))

  (let* ([p (vl-append
             (fade-pict n (blank) (scale (top lifted-g2) (+ 0.2 (* 0.8 n))))
             (bottom (let ([rhs (fade-pict n pre-rhs post-rhs)])
                       (refocus (lt-superimpose
                                 pre-lifted-dot
                                 rhs)
                                rhs))))]
         [p (slide-pict p (scode #,lifted) pre-lifted-dot lifted-g2 n)]
         [p (slide-pict p (scode #,fac) fac-g1 fac-g2 n)])
    (scale p (- 1.0 (* 0.1 n)))))
                        
#;
(slide
 #:title fac-title
 #:layout fac-layout
 (scode (let ([fac
               #,(hylite
                  (let ([fX
                         (lambda (fX)
                           (let ([f (lambda (x) 
                                      ((fX fX) x))])
                             #,(hylite3
                                (#,(hylite2
                                    (lambda (fac)
                                      #,(hcode
                                         (code:comment "Exactly like original fac:")
                                         (lambda (n)
                                           (if (zero? n)
                                               1
                                               (* n (fac (- n 1))))))))
                                 f))))])
                    (fX fX)))])
          (fac 10))))

(play-n
 #:title fac-title
 #:layout fac-layout
 combined)

#;
(slide
 #:title fac-title
 (scode (define mk-rec
          (lambda (#,(hylite2 body-proc))
            #,(hylite
               (let ([fX
                      (lambda (fX)
                        (let ([f (lambda (x) 
                                   ((fX fX) x))])
                          #,(hylite3 (#,(hylite2 body-proc) f))))])
                 (fX fX)))))
        code:blank
        (let ([fac (mk-rec
                    #,(hylite2 
                       (lambda (fac)
                         #,(hcode
                            (code:comment "Exactly like original fac:"))
                         #,(hcode
                            (lambda (n)
                              (if (zero? n)
                                  1
                                  (* n (fac (- n 1)))))))))])
          (fac 10))))

(slide
 #:title fac-title
 (scode (let ([fac
               (mk-rec
                (lambda (fac)
                  (code:comment "Exactly like original fac:")
                  (lambda (n)
                    (if (zero? n)
                        1
                        (* n (fac (- n 1)))))))])
          (fac 10))))

(slide
 #:title "Fibonnaci"
 (scode (let ([fib
               (mk-rec
                (lambda (fib)
                  (code:comment "Usual fib:")
                  (lambda (n)
                    (if (or (= n 0) (= n 1))
                        1
                        (+ (fib (- n 1))
                           (fib (- n 2)))))))])
          (fib 5))))

(slide
 #:title "Sum"
 (scode (let ([sum
               (mk-rec
                (lambda (sum)
                  (code:comment "Usual sum:")
                  (lambda (l)
                    (if (empty? l)
                        0
                        (+ (fst l)
                           (sum (rest l)))))))])
          (sum `(1 2 3 4)))))

;; ----------------------------------------
(part 4)

(goal-slide #:hilite3 hilite)

(define (subscript p n)
  (hbl-append p (text (format "~a" n) `(subscript . ,(current-main-font)) (current-font-size))))

(define fae-1 (code _rhs))
(define fae-2 (code _body))
(define name-1 (code _name))
(define mk-rec (text  "mk-rec" `(italic . modern) (current-font-size)))

(define (ralign .  x)
  (apply item #:bullet (ghost bullet) x))

(define Y
  (code
   {lambda {body-proc}
     {{lambda {fx} {fX fX}}
      {lambda {fX}
        {{lambda {f} {body-proc f}}
         {lambda {x}
           {{fX fX} x}}}}}}))

(define letrec-source
  (code {letrec {[#,name-1 #,fae-1]}
          #,fae-2}))

(define letrec-expansion
  (scale
   (code {{lambda {#,name-1} #,fae-2}
          {{lambda {body-proc}
             {let {[fX {fun {fX}
                            {let {[f {lambda {x}
                                       {{fX fX} x}}]}
                              {body-proc f}}}]}
               {fX fX}}}
           {lambda {#,name-1} #,fae-1}}})
   0.45))

(slide
 #:title "Implementing Recursion"
 'alts
 (list
  (list
   (scode {letrec {[fac {lambda {n}
                          {if0 n
                               1
                               {* n
                                  {fac {- n 1}}}}}]}
            {fac 10}})
   (para #:width (get-client-w #:aspect 'fullscreen) "could be parsed the same as")
   'alts
   (let* ([proc (sscode {let {[fac 
                               {#,mk-rec
                                {lambda {fac}
                                  {lambda {n}
                                    {if0 n
                                         1
                                         {* n
                                            {fac {- n 1}}}}}}}]}
                          {fac 10}})]
          [def (code
                #,mk-rec #,(t "=") {lambda {body-proc}
                                     {let {[fX
                                            {lambda {fX}
                                              {let {[f {lambda {x}
                                                         {{fX fX} x}}]}
                                                {body-proc f}}}]}
                                       {fX fX}}})]
          [def2 (code
                 #,mk-rec #,(t "=") #,Y)]
          [wrap (lambda (def)
                  (refocus
                   (ht-append proc (let ([p (scale (inset def (/ gap-size 2)) 0.7)])
                                     (inset
                                      (cc-superimpose (colorize
                                                       (filled-rectangle (pict-width p)
                                                                         (pict-height p))
                                                       "lightblue")
                                                      p)
                                      (* -10 gap-size)
                                      (* -3 gap-size)
                                      0
                                      0)))
                   proc))])
     (list
      (list proc)
      (list (wrap def))
      (list (wrap def2)))))
  (list
   (ralign
    letrec-source)
   (para #:width (get-client-w #:aspect 'fullscreen) "could be parsed the same as")
   (ralign
    (code {let {[#,name-1 {#,mk-rec {lambda {#,name-1} #,fae-1}}]}
            #,fae-2}))
   (para #:width (get-client-w #:aspect 'fullscreen) "which is really")
   (ralign
    (code {{lambda {#,name-1} #,fae-2}
           {#,mk-rec {lambda {#,name-1} #,fae-1}}}))
   (vc-append
    (current-line-sep)
    (para #:width (get-client-w #:aspect 'fullscreen) "which, writing out" mk-rec ", is really")
    (ralign
     letrec-expansion)))))

;; ----------------------------------------
(part 5)

(slide
 #:title "The Big Picture"
 (hc-append
  (* 6 gap-size)
  (vc-append
   gap-size
   letrec-source
   (colorize (arrow gap-size (/ pi -2)) "forestgreen")
   (scale letrec-expansion 1.6))
  seurat))

(slide
 #:title "Y Combinator"
 (para (code mk-rec) "is better known as the" (bit "Y combinator"))
 'alts
 (list
  (list Y)
  (list
   (table 3
          (list (code Y)
                def
                (code
                 (λ (g)
                   {(λ (fx) {fX fX})
                    (λ (fX)
                      {(λ (f) {g f})
                       (λ (x)
                         {{fX fX} x})})})))
          ltl-superimpose ltl-superimpose
          gap-size gap-size)))
 'next
 'alts
 (list
  (list
   (blank)
   (para "a.k.a. the" (bit "fixpoint operator"))
   (code {Y #,(hylite2 (lambda (#,(hcode f_in)) #,(hcode f_out)))}))
  (list
   (blank)
   (blank)
   (para "See also" (it "The Why of Y") "(Gabriel) or" (it "The Little Schemer") "(Friedman & Felleisen)"))))

;; ----------------------------------------
(part 6)

(slide
 #:title "Example with Quasiquote Escapes"
 (code
  (define (parse [s : S-Exp]) : Exp
    ....
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (let ([name (first bs)]
             [rhs (second bs)]
             [body (third (s-exp->list s))])
         (parse `{{lambda {,name} ,body}
                  ,rhs})))]
    ....)))
