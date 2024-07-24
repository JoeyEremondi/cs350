#lang slideshow
(require slideshow/code
         slideshow/balloon
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss")

(define (align p)
  (para #:width (get-client-w #:aspect 'fullscreen) p))

;; ----------------------------------------
(part 1)

(define error-title "Implementing Errors")

(slide
 #:title error-title
 (code (test/exn (interp #,(parsed {+ 1 {1 1}}))
                 "not a function"))
 'next
 (blank)
 (para "Change to")
 (code (test (interp #,(parsed {+ 1 {1 1}}))
             (errorV "not a function"))))

(slide
 #:title error-title
 (code
  (define (continue k v)
    (type-case Cont k
      ...
      [(doAppK v-f next-k)
       (type-case Value v-f
         [closV ...]
         [else (errorV "not a function")])])))
 (blank)
 (para "Return" (code errorV) "directly, dropping" (code k)))

(slide
 #:title error-title
 (code
  (define (lookup [n : Symbol] [env : Env] [#,(hilite (code k)) : Cont]) : Value
    (type-case (Listof Binding) env
      [empty (#,(hilite (code errorV)) "free variable")]
      [(cons b rst-env)
       (cond
         [(symbol=? n (bind-name b))
          (#,(hilite (code continue k)) (bind-val b))]
         [else (lookup n rst-env #,(hilite (code k)))])]))))

(slide
 #:title error-title
 (code
  (define (num-op op l r #,(hilite (code k)))
    (cond
     [(and (numV? l) (numV? r))
      (#,(hilite (code continue k)) (numV (op (numV-n l) (numV-n r))))]
     [else
      (#,(hilite (code errorV)) "not a number")]))
  code:blank
  (define (num+ l r #,(hilite (code k)))
    (num-op + l r #,(hilite (code k))))
  (define (num* l r #,(hilite (code k)))
    (num-op * l r #,(hilite (code k))))))

 
;; ----------------------------------------
(part 2)

(slide
 #:title "Catching Exceptions"
 'alts
 (list
  (list (code (/ 1 0)
              code:blank
              #,sym:implies #,(colorize (it "division by zero") "red")))
  (list
   (code
    (try (/ 1 0)
         (lambda () +inf.0))
    code:blank
    #,sym:implies +inf.0))
  (list
   (code
    (try (+ 1 0)
         (lambda () +inf.0))
    code:blank
    #,sym:implies 1))
  (list
   (code
    (try (list 1 (/ 1 0) 3)
         (lambda () empty))
    code:blank
    #,sym:implies empty))
  (list
   (code
    (cons 10
          (try (list 1 (/ 1 0) 3)
               (lambda () empty)))
    code:blank
    #,sym:implies (cons 10 empty)))
  (list
   (code
    (try (try (list 1 (/ 1 0) 3)
              (lambda () empty))
         (lambda () (list 10)))
    code:blank
    #,sym:implies empty))
  (list
   (code
    (try (try (list 1 (/ 1 0) 3)
              (lambda () (list (/ 1 0))))
         (lambda () (list 10)))
    code:blank
    #,sym:implies (list 10)))))

(slide
 #:title (hbl-append (titlet "Language with ") (code try))
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)          -or- (code #,(nonterm "Symbol")) (blank)
        (blank)          -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {try #,(nonterm "Exp") {lambda {} #,(nonterm "Exp")}}) new-label))
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (code (test #,(parsed {try 0 {lambda {} 1}})
               (numV 0))))
  (list
   (code (test #,(parsed {try {0 0} {lambda {} 1}})
               (numV 1))))
  (list
   (code (test #,(parsed {+ {try 2 {lambda {} 1}}
                            3})
               (numV 5))))
  (list
   (code (test #,(parsed {+ {try {2 2} {lambda {} 1}}
                            3})
               (numV 4))))
  (list
   (code (test #,(parsed {try {try {0 0}
                                   {lambda {} 1}}
                              {lambda {} 2}})
               (numV 1))))
  (list
   (code (test #,(parsed {try {try {0 0}
                                   {lambda {} {1 1}}}
                              {lambda {} 2}})
               (numV 2))))))

;; ----------------------------------------
(part 3)

(slide
 #:title "Expression and Parse"
 (grammar-table
  (list (nonterm "Exp") eqls (t "...") (blank)
        (blank)          -or- (code {try #,(nonterm "Exp") {lambda {} #,(nonterm "Exp")}}) new-label))
 'next
 (blank (* 3 gap-size))
 (code
  (define-type Exp
    ....
    (tryE [body : Exp]
          [handle : Exp])))
 'next
 (blank (* 3 gap-size))
 (code
  (test (parse `{try {+ 1 2} {lambda {} 8}})
        (tryE (addE (numE 1) (numE 2))
              (numE 8)))))

(slide
 #:title "Interp"
 (align
  (code
   (define (interp a env k)
     (type-case Exp a
       ...
       [(tryE body handler)
        (interp body env (tryK handler env k))]))))
 'next
 (align
  (code
   code:blank
   (define (continue k v)
     (type-case Cont k
       ...
       [(tryK h env next-k)
        (continue next-k v)])))))
  
(slide
 #:title "Throwing Errors"
 (para #:width (get-client-w #:aspect 'fullscreen)
       "Instead of just returning an" (code errorV) ", look for a" (code tryK) ":")
 (blank)
 'alts
 (list
  (list
   (item #:bullet (ghost bullet) "Change")
   (code (errorV "not a number"))
   (item #:bullet (ghost bullet) "to")
   (code (escape k (errorV "not a number"))))
  (list
   (code (test (escape (doPlusK (numV 3)
                                (doneK))
                       (errorV "fail"))
               (errorV "fail"))))
  (list
   (code (test (escape (doPlusK (numV 1)
                                (tryK (numE 2) mt-env
                                      (doneK)))
                       (errorV "fail"))
               (numV 2))))
  (list
   (code (test (escape (doPlusK (numV 1)
                                (tryK (numE 2) mt-env
                                      (doPlusK (numV 3)
                                               (doneK))))
                       (errorV "fail"))
               (numV 5))))
  (list
   (align
    (code
     (define (escape [k : Cont] [v : Value]) : Value
       (type-case Cont k
         [(doneK ) v]
         [(plusSecondK r env next-k) (escape next-k v)]
         [(doPlusK v-l next-k) (escape next-k v)]
         [(multSecondK r env next-k) (escape next-k v)]
         [(doMultK v-l next-k) (escape next-k v)]
         [(appArgK a env next-k) (escape next-k v)]
         [(doAppK v-f next-k) (escape next-k v)]
         ...)))))
  (list
   (align
    (code
     (define (escape [k : Cont] [v : Value]) : Value
       (type-case Cont k
         ...
         [(tryK h env next-k) (interp h env next-k)])))))))

;; ----------------------------------------
(part 4)

(define hole (filled-ellipse 18 18))

(define jumps-title "Continuation Jumps")

(slide
 #:title jumps-title
 (para "The" (code try) "form lets a programmer jump out"
       "to an enclosing context:")
 (code (+ 1
          (try (+ 2
                  (+ 3
                     (+ 4
                        (1 5))))
               (lambda () 0))))
 'next
 (para "jumps to")
 (code (+ 1 #,hole))
 (para "with code" (code 0)))

(slide
 #:title jumps-title
 (para "The" (code let/cc) "form lets a programmer jump out"
       "to any target context, and supply a value:")
 'alts
 (list
  (list
   (code (+ 1
            (let/cc k1
              (+ 2
                 (+ 3
                    (let/cc k2
                      (+ 4
                         (#,(hilite (code k1)) 5))))))))
   'next
   (para "jumps to")
   (code (+ 1 #,hole))
   (para "with code" (code 5)))
  (list
   (code (+ 1
            (let/cc k1
              (+ 2
                 (+ 3
                    (let/cc k2
                      (+ 4
                         (#,(hilite (code k2)) 5))))))))
   'next
   (para "jumps to")
   (code (+ 1 (+ 2 (+ 3 #,hole))))
   (para "with code" (code 5))))
 'next
 (colorize
  (para #:align 'right
        "Does it ever make sense to jump" (it "in") "?")
  "blue"))

(define (jump-slide #:lambda? [lambda? #f]
                    #:balloon? [balloon? lambda?])
  (define k (code k))
  (define k+ (refocus (pin-balloon
                       (wrap-balloon (if lambda?
                                         (scode (lambda (v)
                                                  (+ 1
                                                     (+ 2
                                                        (+ 3
                                                           (+ 4
                                                              v))))))
                                         (scode (+ 1
                                                   (+ 2
                                                      (+ 3
                                                         (+ 4
                                                            #,hole))))))
                                     'sw 0 gap-size)
                       k k ct-find)
                      k))
  (slide
   #:title jumps-title
   (code (define continue (lambda (n) n))
         code:blank
         (let/cc esc
           (+ 1
              (+ 2
                 (+ 3
                    (+ 4
                       (let/cc #,(if balloon?
                                     k+
                                     k)
                         (begin
                           (set! continue k)
                           (esc 0))))))))
         code:blank
         (continue 5))))

(jump-slide)
(jump-slide #:balloon? #t)
(jump-slide #:lambda? #t)

;; ----------------------------------------
(part 5)

(slide
 #:title (hbl-append (titlet "Language with ") (code let/cc))
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)          -or- (code #,(nonterm "Symbol")) (blank)
        (blank)          -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {let/cc #,(nonterm "Symbol") #,(nonterm "Exp")}) new-label)))


(define implementing-title "Implementing Continuations as Values")

(slide
 #:title implementing-title
 (code (define-type Value
         (numV [n : Number])
         (closV [arg : Symbol]
                [body : Exp]
                [env : Env])
         (contV [k : Cont]))))

(slide
 #:title implementing-title
 (code
  (define (interp a env k)
    (type-case Exp a
      ...
      [(let/ccE n body)
       (interp body
               (extend-env 
                (bind n (contV k))
                env)
               k)]))))

(slide
 #:title implementing-title
 (code
  (define (continue k v)
    (type-case Cont k
      ...
      [(doAppK v-f next-k)
       (type-case Value v-f
         [(closV n body c-env) ...]
         [(contV k-v) (continue k-v v)]
         [else (error ...)])]
      ...))))

;; ----------------------------------------
(part 6)

(slide
 #:title "Using Continuations"
 (para "Few programs use" (code let/cc) "...")
 'next
 (blank)
 (para "Continuations are mostly useful for building other constructs:")
 (item "exception handling")
 (item "threads")
 (item "generators")
 (item "..."))

;; ----------------------------------------
(part 7)

(slide
 #:title "Generators"
 (code
  (define (make-numbers start-n)
    (generator
     yield (code:comment "<- binds for use below")
     (local [(define (numbers n)
               (begin
                 (yield n) (code:comment "<- yield a value")
                 (numbers (+ n 1))))]
       (numbers start-n))))
  code:blank
  (define g (make-numbers 0))
  (g) (code:comment "=> 0")
  (g) (code:comment "=> 1")
  (g) (code:comment "=> 2"))
 (para #:align 'right "see" (tt "generator.rkt")))

;; ----------------------------------------
(part 8)

(slide
 #:title "Cooperative Threads"
 (code
  (define (count label n)
    (begin
      (pause) (code:comment "allows others to run")
      (display label)
      (display (to-string n))
      (display "\n")
      (count label (+ n 1))))
  code:blank
  (thread (lambda (vd) (count "a" 0)))
  (thread (lambda (vd) (count "b" 0)))
  (swap))
 (para #:align 'right "see" (tt "thread.rkt")))
