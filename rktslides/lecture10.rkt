#lang slideshow
(require (lib "code.ss" "slideshow")
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss"
         slideshow/play
         slideshow/balloon)

;; ----------------------------------------
(part 1)

(slide
 #:title "Defining a Language's Evaluation"
 (para (tt "lambda.rkt") ":")
 (item "With" (tt "#lang plait") sym:implies "eager Curly")
 (item "With" (tt "#lang plait #:lazy") sym:implies "lazy Curly")
 'next
 (blank)
 (para (tt "more-lazy.rkt") ":")
 (item "With" (tt "#lang plait") sym:implies "lazy Curly")
 (item "With" (tt "#lang plait #:lazy") sym:implies "lazy Curly")
 'next
 (blank)
 (para "Let's make eager evaluation order explicit"))

(define (todo p)
  (frame
   (inset
    (vl-append (current-line-sep)
               (scale (t "To do:") 0.8)
               p)
    2)))

(define (plus-todo q p)
  (rtl-superimpose
   (scale
    (todo p)
    0.8)
   q))

(define hole (filled-ellipse 18 18))

(define (align p)
  (para #:width (* (get-client-w #:aspect 'fullscreen) 0.95) (scale p 0.8)))

(slide
 #:title "Evaluation and ``To do'' Lists"
 (align (code (interp (plusE (numE 1) (numE 2)) mt-env)))
 'next
 (align (code #,sym:implies (num+ (interp (numE 1) mt-env)
                                  (interp (numE 2) mt-env))))
 'next
 (plus-todo (align (code #,sym:implies (interp (numE 1) mt-env)))
            (code (num+ #,hole 
                        (interp (numE 2) mt-env))))
 'next
 (plus-todo (align (code #,sym:implies (numV 1)))
            (code (num+ #,hole 
                        (interp (numE 2) mt-env))))
 'next
 (plus-todo (align (code #,sym:implies (interp (numE 2) mt-env)))
            (code (num+ (numV 1)
                        #,hole)))
 'next
 (plus-todo (align (code #,sym:implies (numV 2)))
            (code (num+ (numV 1)
                        #,hole)))
 'next
 (align (code #,sym:implies (num+ (numV 1) (numV 2)))))

(slide
 #:title "Continuations"
 (para "A ``to do'' list is a" (bit "continuation"))
 'next
 (blank)
 'alts
 (list
  (list
   (todo (code (num+ #,hole 
                     (interp (numE 2) mt-env)))))
  (list
   (todo (code (+ 3
                  (* #,hole
                     (f (rest ls))))))))
 'next
 (blank)
 (para "A" (bit "stack") "is one way to implement continuations")
 (todo
  (code (* #,hole (f (rest ls)))
        (+ 3 #,hole)))
 'next
 (blank)
 (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
   (para #:width (* 0.9 client-w)
         #:align 'right
         "The terms" (bit "stack") "and" (bit "continuation") "are sometimes used interchangably")))

;; ----------------------------------------
(part 2)

(define proto (code doPlusK : (Value -> Cont)))

(slide
 #:title "Representing Continuations"
 'alts
 (list
  (list
   (todo (code {+ 3 #,hole}))
   'next
   (blank)
   'alts
   (list
    (list
     (code (define-type Cont
             ....)))
    (list
     (code (define-type Cont
             (doPlusK [v : Value])
             ....))))
   'next
   (blank)
   (code (doPlusK (numV 3))))
  (list
   (todo
    (code {+ #,hole {f 0}}))
   'next
   (blank)
   'alts
   (list
    (list
     (code (define-type Cont
             (doPlusK [v : Value])
             ....)))
    (list
     (code (define-type Cont
             (plusSecondK [r : Exp]
                          [e : Env])
             (doPlusK [v : Value])
             ....))))
   'next
   (blank)
   (code (plusSecondK (appE (idE 'f) (numE 0))
                      mt-env)))
  (list
   (todo
    (code {+ #,hole {f 0}}
          {+ 3 #,hole}))
   'next
   (blank)
   'alts
   (list
    (list
     (scode (define-type Cont
              (plusSecondK [r : Exp]
                           [e : Env])
              (doPlusK [v : Value])
              ....)))
    (list
     (scode (define-type Cont
              (plusSecondK [r : Exp]
                           [e : Env]
                           [k : Cont])
              (doPlusK [v : Value]
                       [k : Cont])
              ....)))
    (list
     (scode (define-type Cont
              (doneK)
              (plusSecondK [r : Exp]
                           [e : Env]
                           [k : Cont])
              (doPlusK [v : Value]
                       [k : Cont])
              ....))))
   'next
   (blank)
   (code (plusSecondK (appE (idE 'f) (numE 0))
                     mt-env
                     (doPlusK (numV 3)
                              (doneK)))))))

(define (make-interp #:next-n [next-n 0]
                     #:k-n [k-n 0]
                     #:add-n [add-n k-n]
                     #:lift-n [lift-n add-n]
                     #:add-todo-n [add-todo-n 0]
                     #:abstract-todo-n [abstract-todo-n 0])
  (define k-sep (fade-pict k-n (blank) (tt " ")))
  (define k-arg (fade-pict k-n (blank) (code k)))
  (define env
    (hbl-append (code env)
                k-sep
                k-arg))
  (define env0 (code l env))
  (define env01 (ghost (launder env0)))
  (define env02 (ghost (launder env0)))
  (define Env
    (hbl-append (code Env)
                k-sep
                (fade-pict k-n (blank) (code Cont))))
  (define r (code r))
  (define r1 (ghost (launder r)))
  (define r2 (ghost (launder r)))
  (define env1 (code env))
  (define env11 (ghost (launder env1)))
  (define env12 (hbl-append (ghost (launder env1))
                            k-sep
                            k-arg))
  (define plusK (cellophane (code (plusSecondK #,r2 #,env12)) add-n))
  (define plusK1 (ghost (launder plusK)))
  (define plusK2 (ghost (launder plusK)))
  (define env+k (fade-pict next-n
                           env01
                           (code #,env02
                                 #,plusK2)))
  (define l (code (interp #,env+k)))
  (define l1 (ghost (launder l)))
  (define l2 (ghost (launder l)))
  (define l21 (ghost (launder l2)))
  (define l22 (ghost (launder l2)))
  (define add (code (num+ #,(lbl-superimpose l1 
                                             (cellophane hole lift-n))
                          (interp #,r1 #,env11))))
  (define add1 (ghost (launder add)))
  (define add2 (ghost (launder add)))
  (define (add-todo p)
    (cond
      [(zero? add-todo-n) p]
      [else (refocus (vl-append gap-size
                                p
                                (cellophane
                                 (todo (fade-pict
                                        abstract-todo-n
                                        (code {+ #,hole #,(nonterm "Exp")})
                                        (vl-append
                                         (code (num+ #,hole
                                                     (interp r env)))
                                         (fade-pict k-n (blank) (code ....)))))
                                 add-todo-n))
                     p)]))
  (define rhs (fade-pict next-n
                         (fade-pict lift-n
                                    add1
                                    (code #,l21
                                          #,(add-todo (fade-pict add-n add2 plusK1))))
                         l22))
  (define base
    (code (define interp : (Exp #,Env -> Value)
            (lambda (a #,env)
              (type-case Exp a
                ...
                [(plusE l r) #,rhs]
                ...)))))
  (let* ([p (slide-pict base
                        (cellophane add (- 1 add-n)) add1 add2
                        lift-n)]
         [p (slide-pict p
                        l2 l21 l22
                        next-n)]
         [p (slide-pict p
                        l l1 l2
                        lift-n)]
         [p (slide-pict p
                        plusK plusK1 plusK2
                        next-n)]
         [p (slide-pict p
                        r r1 r2
                        add-n)]
         [p (slide-pict p
                        env1 env11 env12
                        add-n)]
         [p (slide-pict p
                        env0 env01 env02
                        next-n)])
         
    p))

;; ----------------------------------------
(part 3)

(define using-title (hbl-append (code interp) (titlet " with Continuations")))

(play-n (lambda (n) (make-interp #:lift-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-interp #:lift-n 1 #:add-todo-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-interp #:lift-n 1 #:add-todo-n 1 #:abstract-todo-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-interp #:lift-n 1 #:add-n n #:add-todo-n 1 #:abstract-todo-n 1))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-interp #:add-n 1 #:k-n n #:add-todo-n 1 #:abstract-todo-n 1))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-interp #:lift-n 1 #:add-n 1 #:k-n 1 #:add-todo-n (- 1 n) #:abstract-todo-n 1))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-interp #:k-n 1 #:next-n n))
        #:title using-title)

(define (make-continue #:do-add-n [do-add-n 0]
                       #:do-add-todo-change-n [do-add-todo-change-n 0]
                       #:do-add-todo-n [do-add-todo-n (if (zero? do-add-todo-change-n)
                                                                 0
                                                                 1)]
                       #:add-k-n [add-k-n 0]
                       #:add-n [add-n add-k-n]
                       #:do-n [do-n add-n]
                       #:func-n [func-n do-n]
                       #:cont-n [cont-n func-n])
  (define r (code (numV n)))
  (define r1 (launder (ghost r)))
  (define r2 (launder (ghost r)))
  (define (add-todo p)
    (cond
      [(zero? do-add-todo-n) p]
      [else
       (refocus (ht-append (* 3 gap-size)
                           p
                           (cellophane (scale
                                        (todo (code
                                               (num+ #,(fade-pict do-add-todo-change-n
                                                                  hole
                                                                  (code v))
                                                     #,(fade-pict do-add-todo-change-n
                                                                  (code (interp r env))
                                                                  hole))
                                               ....))
                                        0.8)
                                       do-add-todo-n))
                p)]))
  (define (add-do-todo p)
    (refocus (ht-append (* 10 gap-size)
                        p
                        (scale (todo (code (num+ v-l
                                                 #,hole)
                                           ....))
                               0.8))
             p))
  (slide-pict
   (code (define interp : (Exp Env Cont -> Value)
           (lambda (a env k)
             (type-case Exp a
               ...
               [(numE n) #,(fade-pict cont-n
                                      r1
                                      (code (continue k #,r2)))]
              ...)))
         code:blank
         #,(cellophane
            (code
             (define continue : (Cont Value -> Value)
               (lambda (k v)
                 (type-case Cont k
                   ...
                   #,(fade-pict
                      add-n
                      (cellophane (code [(doneK) v]) do-n)
                      (fade-pict
                       do-add-n
                       (code
                        [#,(add-todo (code (plusSecondK r env next-k)))
                         (interp r env
                                 #,(fade-pict
                                    add-k-n
                                    (code ...)
                                    (code (doPlusK v next-k))))])
                       (code
                        [#,(add-do-todo (code (doPlusK v-l next-k)))
                         (continue next-k (num+ v-l v))])))
                   ...))))
            func-n))
   r r1 r2
   cont-n))

(play-n (lambda (n) (make-continue #:cont-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:cont-n 1 #:func-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:func-n 1 #:do-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:do-n 1 #:add-n n))
        #:steps 5
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:add-n 1 #:do-add-todo-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:add-n 1 #:do-add-todo-n 1 #:do-add-todo-change-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:add-n 1 #:do-add-todo-change-n 1 #:add-k-n n))
        #:title using-title
        #:skip-last? #t)
(play-n (lambda (n) (make-continue #:add-k-n 1 #:do-add-todo-change-n 1 #:do-add-n n))
        #:steps 1
        #:title using-title)

;; ----------------------------------------
(part 4)

(define (interp+continue i c)
  (scale
   (code
    (define (interp [a : Exp] [env : Env] [k : Cont]) : Value
      (type-case Exp (code:line a ...)
        #,i
        ...))
    code:blank
    (define (continue [k : Cont] [v : Value]) : Value
      (type-case Cont (code:line k ...)
        #,c
        ...)))
   0.8))

(slide
 #:title using-title
 (interp+continue
  (code [(numE n) (continue k (numV n))])
  (code [(doneK) v]))
 'next
 (blank)
 (align (code (interp (numE 5) mt-env (doneK))))
 'next
 (align (code #,sym:implies (continue (doneK) (numV 5))))
 'next
 (align (code #,sym:implies (numV 5))))

(define (simple-steps n l)
  (if ((length l) . <= . n)
      (list (add-between l 'next))
      (append
       (simple-steps n (take l n))
       (simple-steps n (list-tail l (sub1 n))))))

(slide
 #:title using-title
 (interp+continue
  (code [(plusE l r) (interp l env (plusSecondK r env k))])
  (code [(plusSecondK r env next-k)
         (interp r env (doPlusK v next-k))]
        [(doPlusK v-l next-k)
         (continue next-k (num+ v-l v))]))
 'next
 (blank)
 'alts
 (simple-steps
  3
  (list
   (align (code (interp (plusE (numE 5) (numE 2)) mt-env (doneK))))
   (align (code #,sym:implies (interp (numE 5)
                                      (plusSecondK (numE 2) mt-env (doneK)))))
   (align (code #,sym:implies (continue (plusSecondK (numE 2) mt-env (doneK))
                                        (numV 5))))
   (align (code #,sym:implies (interp (numE 2) mt-env
                                      (doPlusK (numV 5) (doneK)))))
   (align (code #,sym:implies (continue (doPlusK (numV 5) (doneK))
                                        (numV 2))))
   (align (code #,sym:implies (continue (doneK)
                                        (numV 7)))))))

;; ----------------------------------------
(part 5)

(slide
 #:title using-title
 (interp+continue
  (code [(lamE n body)
         (continue k (closV n body env))])
  (blank)))

(slide
 #:title using-title
 (interp+continue
  (code [(appE fun arg) (interp fun env (appArgK arg env k))])
  (code [(appArgK a env next-k)
         (interp a env (doAppK v next-k))]
        [(doAppK v-f next-k)
         (type-case Value v-f
           [(closV n body c-env)
            (interp body (extend-env
                          (bind n v)
                          c-env)
                    next-k)]
           [else (error ...)])])))

(start-at-recent-slide)

(slide
 #:title using-title
 #:layout 'tall
 'alts
 (let ([p (scale
           (interp+continue
            (code [(appE fun arg) (interp fun env (appArgK arg env k))])
            (code [(appArgK a env next-k)
                   (interp a env (doAppK v next-k))]
                  [(doAppK v-f next-k)
                   (type-case Value v-f
                     [(closV n body c-env)
                      (interp body (extend-env
                                    (bind n v)
                                    c-env)
                              next-k)]
                     [else (error ...)])]))
           0.75)])
   (list
    (list p)
    (list (rb-superimpose
           p
           (inset (scale
                   (code E_1 = (extend-env
                                (bind 'f (closV 'x
                                                (idE 'x)
                                                mt-env))
                                mt-env))
                   0.8)
                  0 0 (* gap-size -10) 0)))))
 'next
 'alts
 (simple-steps
  2
  (list
   (align (code (interp (appE (idE 'f) (numE 1))
                        E_1
                        (doneK))))
   (align (code #,sym:implies (interp (idE 'f)
                                      E_1
                                      (appArgK (numE 1) E_1 (doneK)))))
   (align (code #,sym:implies (continue (appArgK (numE 1) E_1 (doneK))
                                        (closV 'x (idE 'x) mt-env))))
   (align (code #,sym:implies (interp (numE 1)
                                      E_1
                                      (doAppK (closV 'x (idE 'x) mt-env) (doneK)))))
   (align (code #,sym:implies (continue (doAppK (closV 'x (idE 'x) mt-env) (doneK))
                                        (numV 1))))
   (align (code #,sym:implies (interp (idE 'x)
                                      (extend-env (bind 'x (numV 1)) mt-env)
                                      (doneK))))
   (align (code #,sym:implies (continue (doneK)
                                        (numV 1)))))))

;; ----------------------------------------
(part 6)

(define (aalign p)
  (para #:width (* (get-client-w #:aspect 'fullscreen) 0.95) p))

(define (anim-steps #:title title . l)
  (let loop ([l l])
    (define pa (aalign
                (if (procedure? (car l))
                    ((car l) 1)
                    (car l))))
    (define sym (if (eq? (cadr l) '=)
                    (t "=")
                    sym:implies))
    (define next-l (if (eq? (cadr l) '=)
                       (cddr l)
                       (cdr l)))
    (define pb (aalign
                (if (procedure? (car next-l))
                    ((car next-l) 0)
                    (car next-l))))
    (when (procedure? (car l))
      (play-n #:title title
              #:layout 'top
              (lambda (n)
                (aalign ((car l) n)))
              #:skip-last? #t))
    (play-n #:title title
            #:layout 'top
            (lambda (n)
              (vc-append
               pa
               (cellophane (aalign sym) n)
               (cellophane pb n)))
            #:skip-last? (pair? (cdr next-l)))
    (cond
     [(null? (cdr next-l)) (void)]
     [else
      (play-n #:title title
              #:layout 'top
              (lambda (n)
                (define g (aalign pb))
                (define g1 (ghost (launder g)))
                (define g2 (ghost (launder g)))
                (slide-pict (lt-superimpose
                             (vc-append
                              (cellophane pa (- 1 n))
                              (cellophane (aalign sym) (- 1 n))
                              g1)
                             g2)
                            g g1 g2
                            n))
              #:skip-last? #t)
      (loop next-l)])))

(slide
 #:title "Infinite Loop"
 (tt "while (1) { }"))

(slide
 #:title "Space-Bounded Loop"
 (tt "int f() { return f(); }"))

(define book-loop-title "Curly Loop")

(slide
 #:title book-loop-title
 (code {let {[f {lambda {f}
                  {f f}}]}
         {f f}})
 'next
 (blank)
 (blank)
 (para #:align 'center "infinite or space-bounded?"))

(anim-steps
 #:title book-loop-title
 (code {let {[f {lambda {f} {f f}}]}
         {f f}})
 (code {{lambda {f} {f f}}
        {lambda {f} {f f}}})
 (code {{lambda {f} {f f}}
        {lambda {f} {f f}}}))

(define (assign p e n)
  (lt-superimpose 
   p
   (cellophane
    (pin-balloon (wrap-balloon e 'ne gap-size 0)
                 (ghost p)
                 p (lambda (p q)
                     (define-values (x y) (lt-find p q))
                     (values x (+ y 14))))
    n)))

(anim-steps
 #:title book-loop-title
 (code (interp #,(parsed {{lambda {f} {f f}}
                          {lambda {f} {f f}}})
               mt-env
               (doneK)))
 (code (interp #,(parsed {lambda {f} {f f}})
               mt-env
               (appArgK #,(parsed {lambda {f} {f f}})
                        mt-env
                        (doneK))))
 (code (continue (appArgK #,(parsed {lambda {f} {f f}})
                          mt-env
                          (doneK))
                 (closV 'f #,(parsed {f f}) mt-env)))
 (code (interp #,(parsed {lambda {f} {f f}})
               mt-env
               (doAppK (closV 'f #,(parsed {f f}) mt-env)
                       (doneK))))
 (code (continue (doAppK (closV 'f #,(parsed {f f}) mt-env)
                         (doneK))
                 (closV 'f #,(parsed {f f}) mt-env)))
 (lambda (n)
   (code (interp #,(parsed {f f})
                 #,(assign (code (extend-env 
                                  (bind 'f (closV 'f #,(parsed {f f}) mt-env))
                                  mt-env))
                           (code E_1)
                           n)
                 (doneK))))
 '=
 (code (interp #,(parsed {f f})
               E_1
               (doneK)))
 (code (interp #,(parsed f)
               E_1
               (appArgK #,(parsed f) E_1
                        (doneK))))
 (code (continue (appArgK #,(parsed f) E_1
                          (doneK))
                 (closV 'f #,(parsed {f f}) mt-env)))
 (code (interp #,(parsed f)
               E_1
               (doAppK (closV 'f #,(parsed {f f}) mt-env)
                       (doneK))))
 (code (continue (doAppK (closV 'f #,(parsed {f f}) mt-env)
                         (doneK))
                 (closV 'f #,(parsed {f f}) mt-env)))
 (code (interp #,(parsed {f f})
               E_1
               (doneK))))

;; ----------------------------------------
(part 7)

(anim-steps
 #:title (string-append book-loop-title "?")
 (code {let {[f {lambda {f} {+ 1 {f f}}}]}
         {f f}})
 (code {{lambda {f} {+ 1 {f f}}}
        {lambda {f} {+ 1 {f f}}}})
 (code {+ 1 {{lambda {f} {+ 1 {f f}}}
             {lambda {f} {+ 1 {f f}}}}})
 (code {+ 1 {+ 1 {{lambda {f} {+ 1 {f f}}}
                  {lambda {f} {+ 1 {f f}}}}}}))

(slide
 #:title "Tail Calls"
 (code
  (define (forever x)
    (forever (not x))))
 'next
 (blank)
 (blank)
 (para #:aspect 'fullscreen
       "Call to" (code forever) "is a" (bit "tail call") ","
       "because there's no work to do after" (code forever) "returns"))

(slide
 #:title "Non-Tail Calls"
 (code
  (define (run-out-of-memory x)
    (not (run-out-of-memory x))))
 'next
 (blank)
 (blank)
 (para #:aspect 'fullscreen
       "The call to" (code run-out-of-memory) "is" (it "not")
       "a tail call, because there's work to do after it returns"))

(slide
 #:title "Tail Calls"
 (code
  (define (forever x)
    (if x
        (forever #t)
        (forever #f))))
 'next
 (blank)
 (blank)
 (para #:aspect 'fullscreen
       "Even though the call to" (code forever) "is wrapped in" (code if) ","
       "there's no work to do after" (code forever) "returns")
 'next
 (para #:aspect 'fullscreen
       "The branches of" (code if) "are in" (bit "tail position")
       "with respect to the" (code if)))

(slide
 #:title "Non-Tail Calls"
 (code
  (define (run-out-of-memory x)
    (if (run-out-of-memory x)
        #t
        #f)))
 'next
 (blank)
 (blank)
 (para #:aspect 'fullscreen
       "The call to" (code run-out-of-memory) "is" (it "not")
       "a tail call, because there's work to do after it returns")
 'next
 (para #:aspect 'fullscreen
       "The test position" (code if) "is" (it "not") "in tail position"
       "with respect to the" (code if)))

(slide
 #:title (hbl-append (code interp) (titlet " and ") (code continue))
 (para "In" (tt "lambda-k.rkt") ":")
 'next
 (item (code interp) "calls" (code continue) "only as a tail call")
 'next
 (item (code continue) "calls" (code interp) "only as a tail call")
 'next
 (item (code lookup) "calls" (code lookup) "only as a tail call")
 'next
 (item "nothing else is recursive")
 'next
 (para "∴ the Plait continuation is always small"))
