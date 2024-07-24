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

(let ([rhs (code 5)]
      [body (code {+ x 6})]
      [name (code x)])
  (slide
   #:title "Binding Constructs"
   (code
    {let {[#,name #,rhs]}
      #,body}
    code:blank
    code:blank
    {let {[f {lambda {#,name}
               #,body}]}
      {f #,rhs}})))

(define (same-progs #:rhs [rhs (code 5)]
                    #:body [body (code {+ x 6})]
                    #:name [name (code x)])
  (list
   (code
    {let {[#,name #,rhs]}
      #,body}
    code:blank
    code:blank
    {{lambda {#,name}
       #,body}
     #,rhs})))

(define let-desugar
  (code (test (parse `{let {[x 5]} {+ x 6}})
              (appE (lamE 'x (plusE (idE 'x) (numE 6)))
                    (numE 5)))))

(slide
 #:title (hbl-append (titlet "Converting ") (code let) (titlet " to ") (code lambda))
 (para "These programs are the same:")
 (blank)
 'alts
 (list
  (same-progs)
  (same-progs #:body (code _body))
  (same-progs #:body (code _body) #:rhs (code _rhs))
  (same-progs #:body (code _body) #:rhs (code _rhs) #:name (code _name)))
 'next
 (blank)
 (blank)
 let-desugar)

;; ------------------------------------------------------------
(part 2)

(slide
 #:title "Syntactic Sugar and Libraries"
 (para "We can add some features to Curly by changing only" (code parse))
 let-desugar
 (para "Language features that can be implemented this way are"
       (bit "syntactic sugar"))
 'next
 (blank)
 (para "Another example:")
 (code
  (test (parse `{neg 3})
        (multE (numE 3) (numE -1))))
 'next
 (para "... but that one might be better as just a function in a" (bit "library") ":")
 (code
  {let {[neg {lambda {n}
               {* n -1}}]}
    ....}))

(slide
 #:title "Encodings"
 (para "Syntactic sugar and library extensions are both are forms of" (bit "encoding"))
 'next
 (blank)
 'alts
 (list
  (list
   (item "Mutable variables encoded as boxes:")
   (scale
    (code
     (test (parse '{lambda {x} {begin {set! x 1} x}})
           (lamE 'x (beginE (setboxE (idE 'x) (numE 1))
                            (unboxE (idE 'x)))))
     code:blank
     (test (parse '{f 1})
           (appE (unboxE (idE 'f)) (boxE (numE 1)))))
    0.9))
  (list
   (item "Boxes encoded with mutable variables:")
   'alts
   (list
    (list
     (scale (crate-code (code ....)) 0.75))
    (list
     (scale (crate-code (code .... {{set-crate! b} 5} ....)) 0.75))))))

(slide
 #:title "Syntactic Sugar in Libraries"
 (para "Some languages, like Racket and Plait, support sugar via libraries")
 (blank)
 (scale
  (code
   (define-syntax-rule (with [(v-id sto-id) call]
                         body)
     (type-case Result call
       [(v*s v-id sto-id) body]))
   code:blank
   code:blank
   (define (interp [a : Exp] [env : Env] [sto : Store])
     (type-case Exp a
       ....
       [(plusE l r)
        (with [(v-l sto-l) (interp l env sto)]
          (with [(v-r sto-r) (interp r env sto-l)]
            (v*s (num+ v-l v-r) sto-r)))]
       ....)))
  0.9))

(slide
 #:title "Encodings and Expressiveness"
 (para "Existing constructs determine what you can encode")
 (blank)
 (item "No state...")
 (colorize (para #:align 'right "... no way to encode boxes or variables")
           RedColor)
 'next
 (blank)
 (item "Just" (code define) "for single-argument functions...")
 (colorize (para #:align 'right "... no way to encode " (code lambda))
           RedColor)
 (colorize (para #:align 'right "... no way to encode boxes")
           RedColor))

(slide
 #:title "Encodings"
 (para "Why study encodings:")
 (blank)
 (item #:width (* 0.9 (current-para-width))
       "To identify language constructs that are fundamentally expressive")
 (para #:align 'right "e.g., boxes in contrast to" (code let))
 'next
 (blank)
 (item #:width (* 0.9 (current-para-width))
       "To simplify" (code interp))
 (para #:align 'right "e.g., no" (code letE))
 'next
 (colorize (para #:align 'right
                 (scale (t "... but performance considerations may dominate")
                        0.8))
           "blue"))

;; ------------------------------------------------------------
(part 3)

(define (mult-args #:body [body (code {+ x y})]
                   #:x [x (code x)]
                   #:y [y (code y)])
  (list
   (code
    {let {[f {lambda {#,x #,y}
               #,body}]}
      {f 1 2}}
    code:blank
    code:blank
    {let {[f {lambda {#,x}
               {lambda {#,y}
                 #,body}}]}
      {{f 1} 2}})))

(slide
 #:title "Encoding Multiple Arguments"
 'alts
 (list
  (mult-args)
  (mult-args #:body (code _body)))
 'next
 (blank)
 (para #:align 'center
       "This transformation is called" (bit "currying")))

;; ------------------------------------------------------------
(part 4)

(slide
 #:title (hbl-append (titlet "Encoding ") (code if))
 'alts
 (list
  (list
   (code {if _tst
             _thn
             _els}))
  (list
   (code {if* _tst
              {lambda {d} _thn}
              {lambda {d} _els}}))
  (list
   (code {{if* _tst
               {lambda {d} _thn}
               {lambda {d} _els}}
          0})
   'next
   (blank)
   (blank)
   (code true #,def {lambda {x} {lambda {y} x}}
         false #,def {lambda {x} {lambda {y} y}})
   'next
   (blank)
   (code {{{{_tst 
             {lambda {d} _thn}}
            {lambda {d} _els}}}
          0}))))

;; ------------------------------------------------------------
(part 5)

(define sym::equals
  (lbl-superimpose (ghost sym:implies) (t "=")))

(slide
 #:title "Encoding Pairs"
 'alts
 (list
  (list (code {cons 1 empty}))
  (list (code {pair 1 0}))
  (list (code {pair _f _s}))
  (list (code {lambda .... _f _s}))
  (list (code {lambda {sel} {{sel _f} _s}})
        'next
        (blank)
        (code pair #,def {lambda {x}
                           {lambda {y}
                             {lambda {sel} {{sel x} y}}}}
              fst #,def {lambda {p} {p true}}
              snd #,def {lambda {p} {p false}})
        'next
        (blank)
        (code {fst {{pair 1} 0}}
              #,sym:implies {fst {lambda {sel} {{sel 1} 0}}}
              #,sym:implies {{lambda {sel} {{sel 1} 0}} true}
              #,sym:implies {{true 1} 0}
              #,sym::equals {{{lambda {x} {lambda {y} x}} 1} 0}
              #,sym:implies {{lambda {y} 1} 0}
              #,sym:implies 1))))


;; ------------------------------------------------------------
(part 6)

(slide
 #:title "λ-Calculus Grammar"
 'alts
 (list
  (list
   (grammar-table
    (list (nonterm "Exp") eqls (nonterm "Symbol") (blank)
          (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank))))
  (list
   (grammar-table
    (list (nonterm "Exp") eqls (nonterm "Symbol") (blank)
          (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code (λ (#,(nonterm "Symbol")) #,(nonterm "Exp"))) (blank)))))
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (code true #,def (λ (x) (λ (y) x))
         false #,def (λ (x) (λ (y) y))))
  (list
   (code {{true a} b}))
  (list
   (code {{(λ (x) (λ (y) x)) a} b}))))

;; ------------------------------------------------------------
(part 7)

(define NWIDTH (* (get-client-w #:aspect 'fullscreen) 0.9))

(define (nalign p) (para #:width NWIDTH p))

(define (_1 p)
  (hbl-append p (text "1" `(subscript . ,(current-main-font)) (current-font-size))))
  
(define f_1 (_1 (code f)))
(define f_N (hbl-append (code f) (text "N" `(italic subscript . ,(current-main-font)) (current-font-size))))

(define (note p . r)
  (ltl-superimpose p (apply para #:align 'right #:width NWIDTH r)))

(slide
 #:title "Encoding Numbers"
 'alts
 (list
  (list (nalign (code zero #,def (λ (x) (λ (y) y)))))
  (list (note (nalign (code zero #,def (λ (f) (λ (x) x))))
              "applies" (code f) "to" (code x) "zero times")))
 'next
 (note (nalign (code one #,def (λ (f) (λ (x) {f x}))))
       "applies" (code f) "to" (code x) "one time")
 'next
 (nalign (code two #,def (λ (f) (λ (x) {f {f x}}))))
 'next
 (nalign (code three #,def (λ (f) (λ (x) {f {f {f x}}}))))
 'next
 (blank)
 (note (nalign (code _N #,def (λ (f) (λ (x) {#,f_1 ... {#,f_N x}}))))
       (code f) "to" (code x) (code _N) "times")
 'next
 (blank)
 (para #:align 'center "This encoding is called" (bit "Church numerals")))

(define zero
  (frame
   (hbl-append (scale (code (λ (f) (λ (x) x))) 0.85)
               (let ([c (ghost (code x))])
                 (inset c 0 0 (- (pict-width c)) 0)))))

(slide
 #:title "Incrementing a Number"
 'alts
 (list
  (list (code add1 #,def (λ (n)
                           ...)))
  (list (code add1 #,def (λ (n)
                           (λ (f)
                             (λ (x) ...)))))
  (list (code add1 #,def (λ (n)
                           (λ (f)
                             (λ (x) ... {{n f} x} ...)))))
  (list (code add1 #,def (λ (n)
                           (λ (f)
                             (λ (x) {f {{n f} x}}))))))
 'next
 (blank)
 (code {add1 zero}
       #,sym:implies (λ (f)
                       (λ (x) {f {{zero f} x}}))
       #,sym::equals (λ (f)
                       (λ (x) {f {{#,zero f} x}}))
       #,sym:implies (λ (f)
                       (λ (x) {f x}))
       #,sym::equals one))

;; ----------------------------------------
(part 8)

(define (_m p)
  (hbl-append p (text "m" `(subscript . ,(current-code-font)) (current-font-size))))

(define add1_1 (_1 (code add1)))
(define add1_m (_m (code add1)))

(slide
 #:title "Adding Numbers"
 (nalign (code add2 #,def (λ (n) {add1 {add1 n}})))
 'next
 (nalign (code add3 #,def (λ (n) {add1 {add1 {add1 n}}})))
 'next
 (blank)
 'alts
 (list
  (list
   (nalign (code add #,def (λ (n) (λ (m) {#,add1_1 ... {#,add1_m n}})))))
  (list
   (nalign (code add #,def (λ (n) (λ (m) {{m add1} n}))))
   (para "... because a number" (it "m") "applies some function"
       (it "m") "times to an argument")))
 'next
 (blank)
 (code {{add one} two}
       #,sym:implies {{two add1} one}
       #,sym:implies {add1 {add1 one}}
       #,sym:implies three))

(define addn_1 (_1 (code {add n})))
(define addn_m (_m (code {add n})))
       
(slide
 #:title "Multiplying Numbers"
 'alts
 (list
  (list
   (nalign (code mult #,def (λ (n) (λ (m) {#,addn_1 
                                           ... 
                                           {#,addn_m zero}})))))
  (list
   (nalign (code mult #,def (λ (n) (λ (m) {{m {add n}} zero}))))
   (blank)
   (para "... because" (code {add #,(it "n")}) "is a function that adds" (it "n")
         "to any number")
   (para "... and a number" (it "m") "applies some function"
       (it "m") "times to an argument"))))

(slide
 #:title "Testing for Zero"
 'alts
 (list
  (list
   (code iszero #,def (λ (n) ... true ... false ...)))
  (list
   (code iszero #,def (λ (n) {{n (λ (x) false)} 
                              true}))
   (blank)
   (para "because applying" (code (λ (x) false)) "zero"
         "times to" (code true) "produces" (code true) ", and"
         "applying it any other number of times produces" (code false))))
 'next
 (blank)
 'alts
 (list
  (list
   (code {iszero zero}
         #,sym:implies {{zero (λ (x) false)} true}
         #,sym:implies true))
  (list
   (code {iszero one}
         #,sym:implies {{one (λ (x) false)} true}
         #,sym:implies {(λ (x) false) true}
         #,sym:implies false))))

(define decrement-title "Decrementing a Number")

(slide
 #:title decrement-title
 'alts
 (list
  (list
   (code sub1 #,def (λ (n)
                      (λ (f)
                        (λ (x) ...)))))
  (list
   (code sub1 #,def (λ (n)
                      (λ (f)
                        (λ (x) ... {{n f} x} ...))))
   'next
   (blank)
   (para #:align 'center
         "Too late! No way to undo a call to" (code f)))
  (list
   (code ... {{pair zero} zero}
         ... {{pair zero} one}
         ... {{pair one} two}
         ... {{pair two} three}
         ...
         ... {{pair #,(it "n-1")} #,(it "n")}))))

(slide
 #:title decrement-title
 'alts
 (list
  (list
   (table 3
          (list (code shift) def (code (λ (p)
                                         {{pair {snd p}} {add1 {snd p}}})))
          ltl-superimpose ltl-superimpose
          gap-size gap-size)
   'next
   (blank)
   (table 3
          (list (code {shift {{pair zero} zero}}) sym:implies (code {{pair zero} one})
                (code {shift {{pair zero} one}})  sym:implies (code {{pair one} two})
                (code {shift {{pair #,(it "n-2")} #,(it "n-1")}}) sym:implies (code {{pair #,(it "n-1")} #,(it "n")}))
          ltl-superimpose ltl-superimpose
          gap-size gap-size)
   'next
   (blank)
   (blank)
   (table 3
          (list (code sub1) def (code (λ (n)
                                        {fst
                                         {{n shift} {{pair zero} zero}}})))
          ltl-superimpose ltl-superimpose
          gap-size gap-size)
   'next
   (blank)
   (para #:align 'center
         "And then subtraction is obvious..."))))

;; ----------------------------------------
(part 9)

(slide
 #:title "More Numbers"
 (para (it "Negative integers") ": pair non-negative integer with a sign boolean")
 (blank)
 (blank)
 (para (it "Rational numbers") ": pair numerator and denominator")
 (blank)
 (blank)
 (para (it "Complex numbers") ": pair real and imaginary parts"))
       

(encodings-checklist)

