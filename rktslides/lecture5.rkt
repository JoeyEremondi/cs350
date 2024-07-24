#lang slideshow
(require slideshow/code
         slideshow/play
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss")

(define WIDE (* 0.9 (get-client-w #:aspect 'fullscreen)))

;; ----------------------------------------

(slide (titlet "Part 1"))

;; ----------------------------------------

(slide
 #:title "State"
 (para "Substitution relies on an identifier having a fixed value")
 'next
 (blank)
 (code {let {[x 5]}
         {let {[f {lambda {y} {+ x y}}]}
           ...
           {f 1}}}
       #,(bt "=")
       {let {[f {lambda {y} {+ 5 y}}]}
         ...
         {f 1}})
 (para #:align 'right "because" (code x) "cannot change"))

(slide
 #:title "State"
 (para "In Plait, a variable's value" (it "can") "change")
 'next
 (blank)
 (code > (let ([x 5])
           (let ([f (lambda (y) (+ x y))])
             (begin
               (set! x 6)
               (f 1))))
       #,(colorize (tt "- number") "blueviolet")
       #,(colorize (tt "7") (current-id-color)))
 'next
 (blank)
 (para "A variable has" (bit "state"))
 'next
 (blank)
 (para "Assignment to variables in Plait is strongly discouraged, but in other languages..."))

(slide
 #:title "Inessential State: Summing a List"
 (para #:width WIDE "The Java way:")
 (scale
  (vl-append
   (current-code-line-sep)
   (tt "int sum(List<Integer> l) {")
   (tt "  int t = 0;")
   (tt "  for (Integer n : l) {")
   (tt "    t = t + n;")
   (tt "  }")
   (tt "  return t;")
   (tt "}"))
  0.9)
 'next
 (blank)
 (para #:width WIDE "The Plait way:")
 'alts
 (list
  (list
   (scode
    (define (sum [l : (Listof Number)]) : Number
      (cond
       [(empty? l) 0]
       [else (+ (first l) (sum (rest l)))]))))
  (list
   (scode
    (define (sum [l : (Listof Number)] [t : Number])
      (cond
       [(empty? l) t]
       [else (sum (rest l) (+ (first l) t))]))))
  (list
   (scode
    (define (sum [l : (Listof Number)]) : Number
      (foldl + 0 l))))))



(slide
 #:title "Inessential State: Feeding Fish"
 (para #:width WIDE "The Java way:")
 (scale
  (vl-append
   (current-code-line-sep)
   (tt "void feed(int[] aq) {")
   (tt "  for (int i = 0; i < aq.length; i++) {")
   (tt "    aq[i]++;")
   (tt "  }")
   (tt "}"))
  0.9)
 'next
 (blank)
 (para #:width WIDE "The Plait way:")
 'alts
 (list
  (list
   (scode
    (define feed : ((Listof Number) -> (Listof Number))
      (lambda (l)
        (map (lambda (x) (+ x 1)) l)))))))

(slide
 #:title "Reasons to Avoid State"
 (code
  (test (feed (list 4 3 7 1))
        (list 5 4 8 2))
  code:blank
  code:blank
  code:blank
  (define today (list 4 3 7 1))
  (define tomorrow (feed today))
  (compare today tomorrow)))

(slide
 #:title "When State is Essential"
 (bitmap "fish-gui.png")
 'next
 (scale
  (code
   (define weight 0)
   code:blank
   (define total-message (make-message (to-string weight)))
   code:blank
   (define (make-feed-button label amt)
     (make-button label
                  (lambda (evt)
                    (begin
                      (set! weight (+ weight amt))
                      (draw-message total-message 
                                    (to-string weight))))))
   code:blank
   (create-window (list (list total-message)
                        (list (make-feed-button "Feed 3" 3)
                              (make-feed-button "Feed 7" 7)))))
  0.82))

(slide
 #:title "State as a Side Channel"
 (para "State is a" (bit "side channel") "for parts of a program to communicate")
 (let* ([b1 (code {f 1})]
        [b2 (code {g 2})])
   (pin-arrow-line 16
                   (code ... #,b1 ...
                         code:blank ...
                         code:blank code:blank code:blank code:blank ... #,b2 ...)
                   b1 rt-find
                   b2 ct-find
                   #:color "red"
                   #:line-width 2
                   #:start-pull 1/2
                   #:start-angle (* pi 1/4)
                   #:end-angle (* pi -1/2)))
 (blank)
 (blank)
 'next
 (item #:bullet (colorize (tt "+") "forestgreen")
       "Programmer can add new channels at will")
 'next
 (item #:bullet (colorize (tt "-") "red")
       "Channels of communication may not be apparent"))

;; ----------------------------------------

(slide (titlet "Part 2"))

;; ----------------------------------------

(slide
 #:title "Variables vs. Boxes"
 'alts
 (list
  (list
   (code
    (define weight 0)
    code:blank
    (define (feed!) : void
      (set! weight (+ 1 weight)))
    code:blank
    (define (get-size) : Number
      weight)))
  (list
   (code
    (define weight (box 0))
    code:blank
    (define (feed!) : void
      (set-box! weight (+ 1 (unbox weight))))
    code:blank
    (define (get-size) : Number
      (unbox weight)))))
 'next
 (blank)
 (blank)
 (code
  box : ('a -> (Boxof 'a))
  unbox : ((Boxof 'a) -> 'a)
  set-box! : ((Boxof 'a) 'a -> void)))

(slide
 #:title "Boxes as Simple Objects"
 (hb-append
  (* 3 gap-size)
  (code
   (let ([b (box 0)])
     (begin
       (set-box! b 10)
       (unbox b))))
  (vl-append
   (current-line-sep)
   (tt "class Box<T> {")
   (tt "  T v;")
   (tt "  Box(T v) {")
   (tt "    this.v = v;")
   (tt "  }")
   (tt "}")
   (tt " ")
   (tt " ")
   (tt "Box b = new Box(0);")
   (tt " ")
   (tt "b.v = 10;")
   (tt "return b.v;"))))

(define box-example-1
  (hbl-append gap-size
              (scode {let {[b {box 0}]}
                       {begin
                         {set-box! b 10}
                         {unbox b}}})
              sym:implies
              (scode 10)))

(slide
 #:title "Boxes"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {- #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code #,(nonterm "Symbol")) (blank)
        (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {box #,(nonterm "Exp")}) new-label
        (blank)         -or- (code {unbox #,(nonterm "Exp")}) new-label
        (blank)         -or- (code {set-box! #,(nonterm "Exp") #,(nonterm "Exp")}) new-label
        (blank)         -or- (code {begin #,(nonterm "Exp") #,(nonterm "Exp")}) new-label))
 'next
 (blank)
 (para
  #:align 'center
  box-example-1))

(slide
 #:title "Implementing Boxes"
 (code (define-type Exp
         ...
         (boxE [arg : Exp])
         (unboxE [arg : Exp])
         (setboxE [bx : Exp]
                  [val : Exp])
         (beginE [l : Exp]
                 [r : Exp]))))

;; ----------------------------------------

(slide (titlet "Part 3"))

(define box-impl-title
  "Implementing Boxes with Boxes")

(slide
 #:title box-impl-title
 'alts
 (let ([bx (lambda (show)
             (code {box 0}
                   #,(show sym:implies) #,(show (para #:fill? #f "... a box containing" (code (numV 0)) "..."))))])
   (list
    (list
     (un-s-scale box-example-1))
    (list (bx ghost))
    (list (bx values)))))

(slide
 #:title box-impl-title
 'alts
 (list
  (list
   (code (define-type Value
           (numV [n : Number])
           (closV [arg : Symbol]
                  [body : Exp]
                  [env : Env])
           (boxV [b : (Boxof Value)]))))
  (list
   (scale
    (code
     (define (interp [a : Exp] [env : Env]) : Value
       (type-case Expr a
         ...
         [(boxE a)
          (boxV (box (interp a env)))]
         [(unboxE a)
          (type-case Value (interp a env)
            [(boxV b) (unbox b)]
            [else (error 'interp "not a box")])]
         [(setboxE bx val)
          (type-case Value (interp bx env)
            [(boxV b) (let ([v (interp val env)])
                        (begin (set-box! b v)
                               v))]
            [else (error 'interp "not a box")])]
         [(beginE l r) (begin
                         (interp l env)
                         (interp r env))])))
    0.8)
   'next
   (colorize
    (para #:align 'center "This doesn't explain anything about boxes!")
    BlueColor))))

;; ----------------------------------------

(slide (titlet "Part 4"))

(slide
 #:title (hbl-append (titlet "State and ") (code interp))
 (para "We don't need state to" (code interp) "state")
 'next
 (item "We control all the channels of communication")
 (item "Communicate the current values of boxes explicitly"))

(define empty-box-content (code 99))

(define (example-store v)
  (let* ([f (frame (ghost (inset empty-box-content (/ gap-size 2))))]
         [row (hc-append f f f f f)]
         [mid-row (hc-append f f f (cc-superimpose f v) f)])
    (vl-append
     (it "Memory:")
     (vc-append row row mid-row row row))))

(define (mk-step expr store)
  (lb-superimpose
   (vl-append
    gap-size
    expr
    store)
   (blank (* client-w 2/5) 0)))

(define step-a
  (mk-step
   (code {let {[b {box 7}]}
           ...})
   (example-store (blank))))

(define step-b1
  (mk-step
   (code ...
         code:blank)
   (example-store (code 7))))

(define step-b
  (mk-step
   (code ... {set-box! b 10}
         #||#...)
   (example-store (code 7))))

(define step-c
  (mk-step
   (code ... {unbox b}
         #||#...)
   (example-store (code 10))))

(slide
 #:title "Boxes and Memory"
 'alts 
 (list (list
        (htl-append
         (* 2 gap-size)
         step-a sym:implies step-b1))
       (list
        (htl-append
         (* 2 gap-size)
         step-b sym:implies step-c))))

(define communicating-memory-title "Communicating Memory")

(define (pre-mem-pin p pre-memory interp 
                     #:val? [val? #f]
                     #:bottom? [bottom? val?])
  (pin-arrow-line 16
                  p
                  pre-memory (if bottom? cb-find rc-find)
                  interp (lambda (p q)
                           (define-values (x y) (rt-find p q))
                           (values (- x gap-size) y))
                  #:color (if val? "purple" "forestgreen")
                  #:line-width 2
                  #:start-angle (if bottom? (* pi -1/2) 0)
                  #:end-angle (* pi -1/2)))

(define (comm-slide #:result? [result? #f]
                    #:post-arg? [post-arg? result?]
                    #:post-out? [post-out? post-arg?]
                    #:post? [post? post-out?]
                    #:pre-arg? [pre-arg? post?]
                    #:type? [type? pre-arg?]
                    #:pre-in? [pre-in? type?]
                    #:pre? [pre? pre-in?])
  (define pre-memory ((if pre? values ghost)
                      (scale (example-store (code 7)) 0.5)))
  (define post-memory ((if post? values ghost)
                       (scale (example-store (code 10)) 0.5)))
  (define interp (code (interp .... code:blank)))
  (define out (code ...))
  (slide
   #:title communicating-memory-title
   (let* ([p
           (vr-append
            gap-size
            (vl-append
             gap-size
             pre-memory
             (code #,interp #,sym:implies #,out #,(ghost (code ...))))
            post-memory)]
          [p (if pre-in?
                 (pre-mem-pin p pre-memory interp)
                 p)]
          [p (if post-out?
                 (pin-arrow-line 16
                                 p
                                 out rc-find
                                 post-memory ct-find
                                 #:color "forestgreen"
                                 #:line-width 2
                                 #:start-angle 0
                                 #:end-angle (* pi -1/2))
                 p)])
     p)
   (blank)
   (vl-append
    gap-size
    (cond
     [post-arg?
      (code interp : (Expr Env Store -> Result))]
     [pre-arg? 
      (code interp : (Expr Env Store -> Value))]
     [else
      ((if type? values ghost) (code interp : (Expr Env -> Value)))])
    ((if result? values ghost)
     (code (define-type Result
             (v*s [v : Value] [s : Store])))))))

(comm-slide)
(comm-slide #:pre? #t)
(comm-slide #:pre-in? #t)
(comm-slide #:type? #t)
(comm-slide #:pre-arg? #t)
(comm-slide #:post? #t)
(comm-slide #:post-out? #t)
(comm-slide #:post-arg? #t)
(comm-slide #:result? #t)

(define (chain-slide #:seq-n [seq-n 1.0]
                     #:code? [code? #f]
                     #:add? [add? code?]
                     #:combine? [combine? add?]
                     #:combine-val? [combine-val? combine?]
                     #:post? [post? combine-val?]
                     #:post-val? [post-val? post?]
                     #:pre? [pre? post-val?])
  (define pre-memory ((if pre? values ghost)
                      (scale (example-store (code 7)) 0.25)))
  (define (bottom-align p)
    (drop-below-ascent p (- (pict-ascent p) (pict-height p))))
  (define post-memory ((if post-val? values ghost)
                       (bottom-align (scale (example-store (code 10)) 0.25))))
  (define last-memory ((if combine-val? values ghost)
                       (bottom-align (scale (example-store (code 11)) 0.25))))
  (define interp-l (code (interp l env)))
  (define interp-r (code (interp r env)))
  (define interp-l-1 (launder (ghost interp-l)))
  (define interp-l-2 (launder (ghost interp-l)))
  (define interp-r-1 (launder (ghost interp-r)))
  (define interp-r-2 (launder (ghost interp-r)))
  (define dots-a1 (code ...))
  (define dots-a2 (code ...))
  (define num+ (code (num+ #,(fade-pict seq-n interp-l-1 dots-a1) #,(fade-pict seq-n interp-r-1 dots-a2))))
  (define num+1 (launder (ghost num+)))
  (define num+2 (launder (ghost num+)))
  (define last-dots (code ...))
  (define dots-v1 (code ...))
  (define dots-v2 (code ...))
  (let* ([p (fade-pict seq-n
                       #:combine lc-superimpose
                       num+1
                       (vl-append
                        (current-line-sep)
                        (inset pre-memory 0 0 0 (- (* 2 gap-size) (pict-height pre-memory)))
                        ((if post-val? values ghost)
                         (code #,interp-l-2 #,sym:implies (v*s #,dots-v1 #,post-memory)))
                        (blank gap-size)
                        ((if combine-val? values ghost)
                         (code #,interp-r-2 #,sym:implies (v*s #,dots-v2 #,last-memory)))
                        (blank gap-size)
                        ((if combine? values ghost)
                         (let ([p (code (v*s #,num+2 #,last-dots))])
                           (define-values (x y) (lt-find p num+2))
                           (inset p (- x) 0 0 0)))))]
         [p (slide-pict p num+ num+1 num+2 seq-n)]
         [p (slide-pict p interp-l interp-l-1 interp-l-2 seq-n)]
         [p (slide-pict p interp-r interp-r-1 interp-r-2 seq-n)]
         [p (if pre?
                (pre-mem-pin p pre-memory interp-l)
                p)]
         [p (if post?
                (pre-mem-pin p post-memory interp-r #:bottom? #t)
                p)]
         [p (if combine?
                (pre-mem-pin p last-memory last-dots #:bottom? #t)
                p)]
         [p (if add?
                (pre-mem-pin p dots-v1 dots-a1 #:val? #t)
                p)]
         [p (if add?
                (pre-mem-pin p dots-v2 dots-a2 #:val? #t)
                p)])
    (vc-append
     (* 2 gap-size)
     (scale p 0.95)
     ((if code? values ghost)
      (code (type-case Result (interp l env sto)
              [(v*s v-l sto-l)
               (type-case Result (interp r env sto-l)
                 [(v*s v-r sto-r)
                  (v*s (num+ v-l v-r) sto-r)])]))))))

(define chain-title "Communicating the Store")
(play-n #:title chain-title
        (lambda (n) (chain-slide #:seq-n n)))
(slide #:title chain-title (chain-slide #:pre? #t))
(slide #:title chain-title (chain-slide #:post-val? #t))
(slide #:title chain-title (chain-slide #:post? #t))
(slide #:title chain-title (chain-slide #:combine-val? #t))
(slide #:title chain-title (chain-slide #:combine? #t))
(slide #:title chain-title (chain-slide #:add? #t))
(slide #:title chain-title (chain-slide #:code? #t))

(slide
 #:title "The Store"
 (scode
  (define-type-alias Location Number)
  code:blank
  (define-type Storage
    (cell [location : Location] [val : Value]))
  code:blank
  (define-type-alias Store (Listof Storage))
  (define mt-store empty)
  (define override-store cons))
 'next
 (hc-append
  gap-size
  (scale (example-store (code 10)) 0.8)
  (scode (override-store (cell 13 (numV 10))
                         mt-store))))

;; ----------------------------------------

(slide (titlet "Part 5"))

(define examples-title "Store Examples")

(define (align p) (para #:width WIDE p))

(define (sto-exp p)
  (list
   (align
    (code interp : (Exp Env Store -> Result)
          code:blank
          #,p))))


(slide
 #:title examples-title
 'alts
 (list
  (list (align
         (code interp : (Exp Env -> Value)
               code:blank
               (test (interp (numE 5) mt-env)
                     (numV 5)))))
  (list (align
         (code interp : (Exp Env Store -> Value)
               code:blank
               (test (interp (numE 5) mt-env mt-store)
                     (numV 5)))))
  (sto-exp (code (test (interp (numE 5) mt-env mt-store)
                       (v*s (numV 5) mt-store))))
  (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                       ...)))
  (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                       (v*s ...
                            ...))))
  (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                       (v*s (boxV ...)
                            ...))))
  (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                       (v*s (boxV ...)
                            ... (numV 5) ...))))
  (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                       (v*s (boxV ...)
                            ... (cell 1 (numV 5)) ...))))
  (append
   (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                        (v*s (boxV 1)
                             ... (cell 1 (numV 5)) ...))))
   (list
    'next
    (code
     code:blank
     (define-type Value
       (numV [n number?])
       (closV [arg : Symbol]
              [body : Exp]
              [env : Env])
       (boxV [l : Location])))))
  (sto-exp (code (test (interp (boxE (numE 5)) mt-env mt-store)
                       (v*s (boxV 1)
                            (override-store 
                             (cell 1 (numV 5))
                             mt-store)))))
  (sto-exp (code (test (interp (parse `{set-box! {box 5} 6})
                               mt-env 
                               mt-store)
                       (v*s ...
                            ...))))
  (sto-exp (code (test (interp (parse `{set-box! {box 5} 6})
                               mt-env 
                               mt-store)
                       (v*s (numV 6)
                            ...))))
  (sto-exp (code (test (interp (parse `{set-box! {box 5} 6})
                               mt-env 
                               mt-store)
                       (v*s (numV 6)
                            ...
                            ...
                            (override-store
                             (cell 1 (numV 5))
                             mt-store) ...))))
  (sto-exp (code (test (interp (parse `{set-box! {box 5} 6})
                               mt-env 
                               mt-store)
                       (v*s (numV 6)
                            (override-store
                             (cell 1 (numV 6))
                             (override-store
                              (cell 1 (numV 5))
                              mt-store))))))))

;; ----------------------------------------

(slide (titlet "Part 6"))

(define impl-title (hbl-append (code interp) (titlet " with a Store")))

(define (interp p #:suffix [suffix #f])
  (list
   (align
    (scale
     (let ([c (code (define interp : (Exp Env Store -> Result)
                      (lambda (a env sto)
                        ...
                        #,p
                        ...)))])
       (if suffix
           (code #,c
                 #,suffix)
           c))
     0.85))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(numE n) ...]))
  (interp
   (code [(numE n) ... (numV n) ...]))
  (interp
   (code [(numE n) (v*s (numV n) sto)]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(idE s) (v*s (lookup s env) sto)]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(plusE l r) ...]))
  (interp
   (code [(plusE l r) (num+ (interp l env)
                            (interp r env))]))
  (interp
   (code
    [(plusE l r)
     (type-case Result (interp l env sto)
       [(v*s v-l sto-l)
            (type-case Result (interp r env sto-l)
              [(v*s v-r sto-r)
                   (v*s (num+ v-l v-r) sto-r)])])]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(boxE a)
          ... (interp a env sto) ...]))
  (interp
   (code [(boxE a)
          (type-case Result (interp a env sto)
            [(v*s v sto-v)
                      ...])]))
  (interp
   (code [(boxE a)
          (type-case Result (interp a env sto)
            [(v*s v sto-v)
             ... (new-loc sto-v)
             ...])])
   #:suffix
   (code
    code:blank
    (code:contract new-loc : (Store -> Location))))
  (interp
   (code [(boxE a)
          (type-case Result (interp a env sto)
            [(v*s v sto-v)
             (let ([l (new-loc sto-v)])
               ...)])]))
  (interp
   (code [(boxE a)
          (type-case Result (interp a env sto)
            [(v*s v sto-v)
             (let ([l (new-loc sto-v)])
               ... (boxV l)
               ...)])]))
  (interp
   (code [(boxE a)
          (type-case Result (interp a env sto)
            [(v*s v sto-v)
             (let ([l (new-loc sto-v)])
               ... (boxV l) 
               ... (override-store (cell l v)
                                   sto-v)
               ...)])]))
  (interp
   (code [(boxE a)
          (type-case Result (interp a env sto)
            [(v*s v sto-v)
             (let ([l (new-loc sto-v)])
               (v*s (boxV l) 
                    (override-store (cell l v) 
                                    sto-v)))])]))))

(slide
 #:title impl-title
 #:layout 'top
 (scode (define (new-loc [sto : Store]) : Location
          (+ 1 (max-address sto)))
        code:blank
        (define (max-address [sto : Store]) : Location
          (type-case (Listof Storage) sto
            [empty 0]
            [(cons c rst-sto) (max (cell-location c)
                                   (max-address rst-sto))]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code
    [(unboxE a) ...]))
  (interp
   (code
    [(unboxE a)
     ... (interp a env sto) ...]))
  (interp
   (code
    [(unboxE a)
     (type-case Result (interp a env sto)
       [(v*s v sto-v)
        ...])]))
  (interp
   (code
    [(unboxE a)
     (type-case Result (interp a env sto)
       [(v*s v sto-v)
        (type-case Value v
          [(boxV l) ...]
          [else (error 'interp
                       "not a box")])])]))
  (interp
   (code
    [(unboxE a)
     (type-case Result (interp a env sto)
       [(v*s v sto-v)
        (type-case Value v
          [(boxV l) ... (fetch l sto-v)
                    ...]
          [else (error 'interp
                                  "not a box")])])])
   #:suffix (code code:blank
                  fetch : (Location Store -> Value)))
  (interp
   (code
    [(unboxE a)
     (type-case Result (interp a env sto)
       [(v*s v sto-v)
        (type-case Value v
          [(boxV l) (v*s (fetch l sto-v) 
                         sto-v)]
          [else (error 'interp
                       "not a box")])])]))))

(define-syntax hilite-red
  (syntax-rules ()
    [(_ st)
     (un-ss-scale
      (parameterize ([code-colorize-enabled #f])
        (colorize (sscode st) RedColor)))]))

(define interp-two
  (code
   (define (interp-two l r env sto handle)
     (type-case Result (interp l env sto)
       [(v*s v-l sto-l)
            (type-case Result (interp r env sto-l)
              [(v*s v-r sto-r)
                   (handle v-l v-r sto-r)])]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(setboxE bx val)
          ...]))
  (interp
   (code [(setboxE bx val)
          ... (interp bx env sto) ...
          ... (interp val env sto-b) ...
          ...]))
  (interp
   (code [(setboxE bx val)
          (type-case Result (interp bx env sto)
            [(v*s v-b sto-b)
             (type-case Result (interp val env sto-b)
               [(v*s v-v sto-v)
                ...])])]))
  (interp
   (code [(setboxE bx val)
          (type-case Result (interp bx env sto)
            [(v*s v-b sto-b)
             (type-case Result (interp val env sto-b)
               [(v*s v-v sto-v)
                (type-case Value v-b
                  [(boxV l) ...]
                  [else (error 'interp
                               "not a box")])])])]))
  (interp
   (code [(setboxE bx val)
          (type-case Result (interp bx env sto)
            [(v*s v-b sto-b)
             (type-case Result (interp val env sto-b)
               [(v*s v-v sto-v)
                (type-case Value v-b
                  [(boxV l) ...
                            (override-store
                             (cell l v-v)
                             sto-v)]
                  [else (error 'interp 
                               "not a box")])])])]))
  (interp
   (code [(setboxE bx val)
          (type-case Result (interp bx env sto)
            [(v*s v-b sto-b)
             (type-case Result (interp val env sto-b)
               [(v*s v-v sto-v)
                (type-case Value v-b
                  [(boxV l)
                   (v*s v-v
                        (override-store
                         (cell l v-v)
                         sto-v))]
                  [else (error 'interp
                               "not a box")])])])]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(beginE l r)
          (type-case Result (interp l env sto)
            [(v*s v-l sto-l)
             (interp r env sto-l)])]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 7"))

(define pat
  (code (with [(_v-id _sto-id) _call]
          _body)))

(define tmpl
  (code (type-case Result _call
          [(v*s _v-id _sto-id)
           _body])))

(slide
 #:title "Awkward Syntax"
 (para
  #:width WIDE
  (code
   (type-case Result (interp l env sto)
     [(v*s v-l sto-l)
      (type-case Result (interp r env sto-l)
        [(v*s v-r sto-r)
         (v*s (num+ v-l v-r) sto-r)])])))
 'next
 (blank)
 (blank)
 (para #:width WIDE tmpl))

(define (better-syntax #:pat? [pat? #t]
                       #:move-n [move-n 0]
                       #:ds-n [ds-n 0])
  (define tmpl-1 (launder (ghost tmpl)))
  (define tmpl-2 (launder (ghost tmpl)))
  (define pat-1 (launder (ghost pat)))
  (define pat-2 (launder (ghost pat)))
  (define orig (vl-append
                (* 3 gap-size)
                tmpl-1
                pat-1))
  (define ds (code (define-syntax-rule #,pat-2
                     #,tmpl-2)))
  (let* ([p (fade-pict move-n orig (cellophane ds ds-n))]
         [p (slide-pict p tmpl tmpl-1 tmpl-2 move-n)]
         [p (slide-pict p ((if pat? values ghost) pat) pat-1 pat-2 move-n)])
    p))

(define better-title "Better Syntax")

(slide #:title better-title (better-syntax #:pat? #f))
(void (play #:title better-title (lambda (n) (better-syntax #:move-n n))))
(play-n #:title better-title (lambda (n) (better-syntax #:move-n 1 #:ds-n n)))

(define (example-use show)
  (slide
   #:title "Better Syntax"
   (code (define-syntax-rule #,pat
           #,tmpl)
         code:blank
         code:blank
         (with [(v-r sto-r) (interp r env sto-l)]
           (v*s (num+ v-l v-r) sto-r))
         #,(show sym:implies)
         #,(show (code
                  (type-case Result (interp r env sto-l)
                    [(v*s v-r sto-r)
                     (v*s (num+ v-l v-r) sto-r)]))))))
(example-use ghost)
(example-use values)

(slide
 #:title "Better Syntax"
 (code (with [(v-l sto-l) (interp l env sto)]
         (with [(v-r sto-r) (interp r env sto-l)]
           (v*s (num+ v-l v-r) sto-r)))))
