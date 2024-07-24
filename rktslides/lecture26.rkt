#lang slideshow
(require slideshow/code
         slideshow/balloon
         "utils/utils.rkt"
         "utils/alg.ss")

(define WIDE (* (get-client-w #:aspect 'fullscreen) 0.9))
(define NARROW (*  (get-client-w #:aspect 'fullscreen) 0.4))

(current-para-width ((get-current-para-width #:aspect 'fullscreen)))

(current-keyword-list (cons "if0" (remove* `("delay")
                                           (current-keyword-list))))

;; ----------------------------------------
(part 1)

(define functions+if0-lines
  (list (nonterm "Exp")  eqls (nonterm "Number") (blank)
        (blank)          -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code #,(nonterm "Symbol")) (blank)
        (blank)          -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {if0 #,(nonterm "Exp") #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)))
(define let-lines
  (list (blank)          -or- (code {let {[#,(nonterm "Symbol") #,(nonterm "Exp")]} #,(nonterm "Exp")}) (blank)))
(define add1-lines
  (list (blank)          -or- (code {add1 #,(nonterm "Exp")}) (blank)))

(define functions+if0 (grammar-table functions+if0-lines))
(define functions+if0+let (grammar-table (append functions+if0-lines
                                                 let-lines)))
(define functions+if0+let+add1 (grammar-table (append functions+if0-lines
                                                      let-lines
                                                      add1-lines)))

(define case-production
  (code {case #,(nonterm "Exp")
          [{#,(nonterm "Number")} #,(nonterm "Exp")]
          ...
          [else #,(nonterm "Exp")]}))

(slide
 #:title "Languages and Sugar"
 'alts
 (list 
  (list (lt-superimpose functions+if0
                        (ghost functions+if0+let)
                        (ghost functions+if0+let+add1)))
  (list (lt-superimpose functions+if0+let
                        (ghost functions+if0+let+add1)))
  (list functions+if0+let+add1)
  (list (lt-superimpose functions+if0+let
                        (ghost functions+if0+let+add1))
        (blank)
        (code {let {[add1 {lambda {n}
                           {+ n 1}}]}
               .... {add1 x} ....}))
  (list (para #:width WIDE "Potential sugar:")
        (blank gap-size)
        (para (code {- #,(nonterm "Exp") #,(nonterm "Exp")}))
        'next
        (blank gap-size)
        (para case-production)
        'next
        (blank gap-size)
        (para (code {delay #,(nonterm "Exp")}))
        (para (code {force #,(nonterm "Exp")})))))

;; ----------------------------------------
(part 2)

(slide
 #:title "S-Expressions with and without Types"
 (para #:width WIDE "Plait:")
 (code (s-exp->number (first (s-exp->list `{1 2}))))
 (code `{1 ,(number->s-exp (+ 2 3))})
 (blank)
 (blank)
 (para #:width WIDE "Racket:")
 (code (first `{1 2}))
 (code `{1 ,(+ 2 3)}))

;; ----------------------------------------
(part 3)

(define simple-expand-delay
  (code
   (define (expand-delay s)
     `{lambda {dummy} ,(second s)})))

(slide
 #:title "S-Expressions"
 (para "In Plait:"
       #:width (get-client-w #:aspect 'fullscreen))
 (para
  (scode
   (define (expand-delay [s : S-Exp])
     `{lambda {dummy} ,(second (s-exp->list s))})
   code:blank
   (test (expand-delay `{delay {+ 1 2}})
         `{lambda {dummy} {+ 1 2}})))
 (blank)
 'next
 (para "In Racket:"
       #:width (get-client-w #:aspect 'fullscreen))
 (para
  (scode
   #,simple-expand-delay
   code:blank
   (require (only-in plai test))
   (test (expand-delay `{delay {+ 1 2}})
         `{lambda {dummy} {+ 1 2}}))))

(define (expand-body [dummy (code '{dummy})]
                     #:second [second #f])
  (code
   (cons 'lambda
         (cons #,dummy
               (cons #,(or second (code (first (rest s))))
                     '{})))))

(slide
 #:title "Primitive S-Expression Construction"
 (blank)
 (para (scode #,simple-expand-delay))
 'next
 (para "=")
 (para
  (scode (define (expand-delay s)
           (list 'lambda
                 `{dummy}
                 (second s)))))
 'next
 (para "=")
 'alts
 (let ([mk (lambda (dummy [second (code (second s))])
             (list
              (para
               (scode (define (expand-delay s)
                        #,(expand-body dummy
                                       #:second second))))))])
   (list
    (mk (code `{dummy}))
    (mk (code (list 'dummy)))
    (mk (code '{dummy}))
    (mk (code '{dummy}) #f))))

(define (in str p)
  (htl-append (tt str) p))

(slide
 #:title "Expansion Function"
 (blank)
 'alts
 (list
  (list
   (para
    (scode code:blank
           #,(in "  "
                 (code (define (expand-delay s)
                         #,(in "     " (expand-body)))))
           #,(in "  " (code (expand-delay `{delay {+ 1 2}}))))))
  (list
   (para
    (scode #,(in "  "
                 (code (define expand-delay
                         #,(in "   " (code (lambda (s)
                                             #,(expand-body)))))))
           #,(in "  " (code (expand-delay `{delay {+ 1 2}}))))))
  (list
   (para
    (scode (let ([expand-delay
                  (lambda (s)
                    #,(expand-body))])
             (expand-delay `{delay {+ 1 2}})))))
  (list
   (para
    (scode (let ([#,(code delay)
                  (lambda (s)
                    #,(expand-body))])
             (delay `{delay {+ 1 2}})))))))

;; ----------------------------------------
(part 4)

(define (case-three-four #:val [val values]
                         #:three [three values]
                         #:three-answer [three-answer values]
                         #:rest [rest values])
  (code {case #,(val (code {+ 1 2}))
          [{#,(three (code 3))} #,(three-answer (code 'three))]
          #,(rest (code [{4} 'four]
                        [else 'no]))}))

(define (case-three-four-expand #:val [val values]
                                #:three [three values]
                                #:three-answer [three-answer values]
                                #:rest [rest values])
  (code {let {[x #,(val (code {+ 1 2}))]}
          {if0 {- x #,(three (code 3))}
               #,(three-answer (code 'three))
               {case x
                 #,(rest
                    (code
                     [{4} 'four]
                     [else 'no]))}}}))

(define (bg col)
  (lambda (p)
    (let ([p2 (inset p 2)])
      (refocus
       (cc-superimpose (colorize (filled-rectangle (pict-width p2)
                                                   (pict-height p2))
                                 col)
                       p2)
       p))))

(slide
 #:title "Recursive Expansion"
 case-production
 (blank)
 (blank)
 'next
 'alts
 (let ([combine
        (lambda (lhs exp #:single? [single? #t])
          (htl-append (* 2 gap-size)
                      lhs
                      (t "⇒")
                      (let ([p1 (code {let {[tmp {+ 1 2}]}
                                        {if0 {- tmp 3}
                                             'three
                                             {if0 {- tmp 4}
                                                  'four
                                                  'no}}})])
                        (lt-superimpose
                         ((if single? ghost values) p1)
                         ((if single? values ghost) exp)))))]
       [val (bg "lightblue")]
       [three (bg "lightgreen")]
       [three-answer (bg "pink")]
       [rest (bg "yellow")])
   (list
    (list (combine (case-three-four) (case-three-four-expand) #:single? #f))
    (list (combine (case-three-four) (case-three-four-expand)))
    (list
     (combine (case-three-four #:val val
                               #:three three
                               #:three-answer three-answer
                               #:rest rest)
              (case-three-four-expand #:val val
                                      #:three three
                                      #:three-answer three-answer
                                      #:rest rest)))
    (list
     (htl-append (* 2 gap-size)
                 (code {case #,(val (code x))
                         [else #,(three-answer (code 'no))]})
                 (t "⇒")
                 (code {let {[tmp #,(val (code x))]}
                         #,(three-answer (code 'no))}))))))

(define (with-case body)
  (para
   (sscode (let ([case (lambda (s)
                         (if (and (symbol? (first (third s)))
                                  (symbol=? 'else (first (third s))))
                             `{let {[tmp ,(second s)]}
                                ,(second (third s))}
                             `{let {[tmp ,(second s)]}
                                {if0 {- tmp ,(first (first (third s)))}
                                     ,(second (third s))
                                     ,(cons 'case
                                            (cons 'tmp
                                                  (rest (rest (rest s)))))}}))])
             #,body))))

(slide
 #:title (hbl-append (code case) (titlet " Expander"))
 'alts
 (list
  (list
   (with-case
     (code
      (test (case `{case x
                     [else 'no]})
            `{let {[tmp x]}
               'no}))))
  (list
   (with-case
     (code
      (test (case `{case {+ 1 2}
                     [{3} 'three]
                     [{4} 'four]
                     [else 'no]})
            `{let {[tmp (+ 1 2)]}
               {if0 {- tmp 3}
                    'three
                    {case tmp
                      [{4} 'four]
                      [else 'no]}}}))))))

;; ----------------------------------------
(part 5)

(slide
 #:title "Replacing a Parser"
 (para #:width WIDE
       "We can add sugar to a language by replacing the"
       (tt "parse") "function while reusing" (tt "interp"))
 (blank)
 (sscode
  (require "core.rkt")
  code:blank  
  (define (parse2 [s : S-Exp]) : Exp
    (cond
     [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
     ....
     [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
      (let ([bs ....])
        (parse2 `{{lambda {,(first bs)}
                   ,(third (s-exp->list s))}
                  ,(second bs)}))]
     [(s-exp-match? `{case ANY ....} s)
      ....]
     ....))))

;; ----------------------------------------

(define expand-of-s (code (expand s)))

(define parse*-defn
   (sscode
    (define (parse* [s : S-Exp]
                    [expand : (S-Exp -> (Optionof S-Exp))])
      (type-case (Optionof S-Exp) #,expand-of-s
        [(some e) (parse* e expand)]
        [(none )
         (cond
           [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
           ....)]))
    code:blank
    (define (parse [s : S-Exp]) : Exp
      (parse* s (lambda (s) (none))))))

(slide
 #:title "Extending a Parser"
 (para #:width WIDE
       "Better: Set up an" (bit "extensible") 
       "parser to support new forms:")
 (blank)
 'alts
 (list
  (list parse*-defn)
  (list (pin-balloon (wrap-balloon (para #:fill? #f
                                         #:width NARROW
                                         (code expand)
                                         "gets first shot at rewriting each"
                                         "S-expression")
                                   's 0 gap-size)
                     parse*-defn
                     expand-of-s ct-find))
  (list
   (sscode
    (define (parse2 [s : S-Exp]) : Exp
      (parse* s try-let+case))
    code:blank
    (define (try-let+case [s : S-Exp])
      (cond
       [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
        (let ([bs ....])
          (some `{{lambda {,(first bs)}
                    ,(third (s-exp->list s))}
                  ,(second bs)}))]
       [(s-exp-match? `{case ....} s)
        (some ....)]
       [else (none)]))))))

;; ----------------------------------------
(part 6)

(slide
 #:title "Extensible Languages"
 (para "An" (bit "extensible parser") "provides hooks for a language" (colorize (it "implementer") "blue") "to add sugar")
 (item "Programs that use the new sugar must be run with the extended implementation")
 (item "Different sets of extensions don't work together")
 'next
 (blank)
 (para "An" (bit "extensible language") "provides hooks for a language" (colorize (it "user") "blue") "to add sugar")
 (item "Programs can use new sugar with the existing implementation")
 (item "Different sets of extensions can work together"))

(define use-let
  (code {let {[x 5]}
          {+ x x}}))

(define let-macro-rhs (code ....))

(define def-and-use-let
  (code {let-macro {[let #,let-macro-rhs]}
          #,use-let}))

(slide
 #:title "Language Extension via Macros"
 'alts
 (list
  (list (pin-over (ghost def-and-use-let)
                  use-let lt-find
                  use-let))
  (list def-and-use-let)
  (list (pin-balloon (wrap-balloon (para #:fill? #f
                                         #:width NARROW
                                         "a function that acts like"
                                         (code expand) "for forms that start with"
                                         (code let))
                                   's 0 gap-size)
                     def-and-use-let
                     let-macro-rhs ct-find)
        'next
        (blank)
        (para "Our extensible language needs")
        (item (code let-macro))
        (item "S-expression values and operations")
        (item (code quote)))))

(slide
 #:title "S-Expression Values"
 (para "Plait S-expressions are distinct from lists and symbols")
 'next
 (blank)
 (para "In untyped Racket or Curly, an S-expression is one of")
 (item "number")
 (item "symbol")
 (item "list of S-expressions")
 'next
 (colorize (para #:align 'right "So, we need to add symbols and lists")
           "blue"))

;; ----------------------------------------
(part 7)

(define add-letmacro-title (hbl-append (titlet "Adding ") (code let-macro)))

(define delay-example
  (code {let-macro {[delay {lambda {s}
                             {cons 'lambda ....}}]}
          {delay 7}}))

(slide
 #:title add-letmacro-title
 (grammar-table (append
                 functions+if0-lines
                 (list
                  (blank)          -or- (code {cons #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
                  (blank)          -or- (t "...") (blank)
                  (blank)          -or- (code {let-macro {[#,(nonterm "Symbol") #,(nonterm "Exp")]} #,(nonterm "Exp")}) (blank))))
 'next
 (blank)
 'alts
 (list
  (list delay-example)
  (list def-and-use-let)))

(start-at-recent-slide)

(define inte (code interp))
(define mt-env (code mt-env))

(define (mk-let-macro-parse #:show-interp? [show-interp? #f]
                            #:show-rhs? [show-rhs? show-interp?]
                            #:env? [env? show-rhs?]
                            #:show-sym? [show-sym? env?]
                            #:show-bind? [show-bind? show-sym?]
                            #:show-recur? [show-recur? show-bind?])
  (define header
    (if env?
        (code (parse* [s : S-Exp] [env : Env]))
        (code (parse [s : S-Exp]))))
  (define (add-sub s)
    (define p
      (code
       (let ([bs (s-exp->list (first
                               (s-exp->list (second
                                             (s-exp->list s)))))])
         #,((if show-bind? values ghost) s))))
    (if show-bind?
        p
        (pin-over (ghost p) s lt-find s)))
  (define extract-sym
    (code (s-exp->symbol (first bs))))
  (define rhs-parse
    ((if show-rhs? values ghost)
     (code (parse (second bs)))))
  (define rhs-interp
    (pin-over ((if show-interp? values ghost)
               (code (#,inte #,(ghost rhs-parse)
                             #,mt-env)))
              rhs-parse lt-find
              rhs-parse))
  (let ([p
         (code
          (define #,header : Exp
            (cond
              [(s-exp-match? `{let-macro {[SYMBOL ANY]} ANY} s)
               #,(add-sub
                  (lt-superimpose
                   ((if (or env? (not show-recur?)) ghost values)
                    (code (parse (third (s-exp->list s)))))
                   (pin-over ((if env? values ghost)
                              (code
                               (parse* (third (s-exp->list s))
                                       (extend-env (bind #,(ghost extract-sym)
                                                         #,rhs-interp)
                                                   env))))
                             extract-sym lt-find
                             ((if show-sym? values ghost) extract-sym))))]
              ....)))])
    (refocus (vl-append
              gap-size
              p
              (let ([p (inset (scale delay-example 0.8) 5)])
                (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p))
                                          balloon-color)
                                p)))
             p)))


(define (mk-macro-app-parse #:parse? [parse? #f]
                            #:expand? [expand? parse?]
                            #:lookup? [lookup? expand?]
                            #:try? [try? lookup?]
                            #:bind? [bind? try?]
                            #:maybe-app? [maybe-app? #f])
  (define app-pattern (code `{ANY ANY ...}))
  (define lookup ((if lookup? values ghost)
                  (code (lookup (s-exp->symbol rator) env))))
  (define expand (pin-over ((if expand? values ghost)
                            (code (apply-macro #,(ghost lookup)
                                               s)))
                           lookup lt-find
                           lookup))
    
  (define expand-case
    (pin-over ((if parse? values ghost)
               (code
                (parse* #,(ghost expand)
                        env)))
              expand lt-find
              expand))
  (define try-body
    (code (try
           #,expand-case
           (lambda ()
             (if (= (length (s-exp->list s)) 2)
                 (appE (parse* rator env)
                       (parse* (second (s-exp->list s)) env))
                 (error 'parse "invalid input"))))))
  (define body
    (code (let ([rator (first (s-exp->list s))])
            #,(if try? try-body (code code:blank)))))
  (define p
    (lt-superimpose
     (code
      (define (parse* [s : S-Exp] [env : Env]) : Exp
        (cond
         ....
         [(s-exp-match? #,app-pattern s)
          #,(if bind? body (code code:blank))])))
     (ghost (mk-let-macro-parse #:show-interp? #t))))
  (let* ([p (if maybe-app?
                (pin-balloon (wrap-balloon (para #:fill? #f
                                                 #:width NARROW
                                                 "Maybe a function call, maybe a macro use")
                                           'sw 0 gap-size)
                             p
                             app-pattern lt-find)
                p)])
    (refocus (vl-append
              gap-size
              p
              (let ([p (inset (scale (code {delay 7}) 0.8) 5)])
                (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p))
                                          balloon-color)
                                p)))
             p)))
  
(slide
 #:title add-letmacro-title
 'alts
 (list
  (list (mk-let-macro-parse))
  (list (mk-let-macro-parse #:show-recur? #t))
  (list (mk-let-macro-parse #:show-bind? #t))
  (list (mk-let-macro-parse #:show-sym? #t))
  (list (mk-let-macro-parse #:env? #t))
  (list (mk-let-macro-parse #:show-rhs? #t))
  (list (mk-let-macro-parse #:show-interp? #t))
  (list (pin-balloon (wrap-balloon (para #:fill? #f
                                         (code parse) "calls" (code interp) "!")
                                   'n 0 (* -3 gap-size))
                     (mk-let-macro-parse #:show-interp? #t)
                     inte cb-find))
  (list (pin-balloon (wrap-balloon (para #:fill? #f
                                         #:width NARROW
                                         "Parse-time" (code interp) "cannot see"
                                         "run-time variables")
                                   'n 0 (* -1 gap-size))
                     (mk-let-macro-parse #:show-interp? #t)
                     mt-env cb-find))
  (list (mk-macro-app-parse))
  (list (mk-macro-app-parse #:maybe-app? #t))
  (list (mk-macro-app-parse #:bind? #t))
  (list (mk-macro-app-parse #:try? #t))
  (list (mk-macro-app-parse #:lookup? #t))
  (list (mk-macro-app-parse #:expand? #t))
  (list (mk-macro-app-parse #:parse? #t))
  (list
   (code
    (define (apply-macro [transformer : Value] [s : S-Exp])
      (type-case Value transformer
        [(closV arg body env)
         (value->s-exp
          (interp body (extend-env
                        (bind arg (s-exp->value s))
                        env)))]
        [else (error 'apply-macro "not a function")]))))))

(slide
 #:title (hbl-append (titlet "Example Use of ") (code let-macro))
 (code
  (parse `{let-macro {[delay {lambda {s}
                              {cons 'lambda
                                    {cons '{dummy}
                                          {cons {first {rest s}}
                                                '{}}}}}]}
           {let-macro {[force {lambda {s}
                                 {cons {first {rest s}}
                                      '{0}}}]}
             {force {delay 7}}}})))

;; ----------------------------------------
(part 8)

(slide
 #:title "Accidental Capture"
 (code
  (parse `{let-macro {[delay {lambda {s}
                              {cons 'lambda
                                    {cons '{dummy}
                                          {cons {first {rest s}}
                                                '{}}}}}]}
           {let-macro {[force {lambda {s}
                                 {cons {first {rest s}}
                                      '{0}}}]}
             {let {[dummy 8]}
               {force {delay dummy}}}}}))
 'next
 (blank)
 (para #:width WIDE "Macros must be careful to invent names for new variables")
 (para #:width WIDE "The" (code gensym) "function makes a fresh symbol"))
