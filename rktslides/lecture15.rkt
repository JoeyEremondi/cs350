#lang slideshow

(require slideshow/code
         slideshow/balloon
         "utils/colors.ss"
         "utils/utils.ss"
         "utils/alg.ss"
         "utils/types.ss")

(define type-var bt)

(define WIDER (* (get-client-w #:aspect 'fullscreen) 0.8))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 1)

(define type-grammar
 (grammar-table
  (list
   (nonterm "Type") eqls (code num) (blank)
   (blank)          -or- (code bool) (blank)
   (blank)          -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank)
   (blank)           -or- (code ?) (blank))))

(slide
 #:title "Expressions and Types"
 (para "What is the type of the following expression?")
 (blank)
 (code {lambda {x} {+ x 1}})
 'next
 (blank)
 (para (colorize (bt "Answer: ") BlueColor)
       "Yet another trick question; it's not an expression"
       "in our typed language, because the argument type is missing")
 'next
 (blank)
 (para "But it seems like the answer" (it "should") "be" (-> (code _num) (code _num))))

(slide
 #:title "Type Inference"
 (para (dt "Type inference") "is the process of inserting type annotations"
       "where the programmer omits them")
 (blank)
 (para "We'll use explicit question marks, to make it clear where types are"
       "omitted")
 (blank)
 (code {lambda {[x : ?]} {+ x 1}})
 'next
 (blank)
 type-grammar)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 2)

(define (sub a b)
  (hbl-append (text a `(bold . ,(current-main-font)) (current-font-size))
              (subscript b)))

(define (implies a b)
  (hbl-append (* 2/3 (current-font-size))
              a
              (text (string (integer->char 222)) 'symbol (current-font-size))
              b))

(define (intx-t-is-int i n)
  (let ([a (hb-append (sub "T" n)
                      (colorize (t " = ") "red")
                      (code _num))])
    (hbl-append (current-font-size)
                (ghost a)
                (if (string? i) (alg-code i) i)
                a)))

(slide
 #:title "Type Inference"
 'alts~
 (type-tree (hbl-append (open-paren) (code lambda {[x : ?]}) __)
            (hbl-append (open-paren) (code +) __) (code x) __ (code 1) #f #f (close-paren) (close-paren)
            (sub "T" 1)
            (code _num) #f (intx-t-is-int (code _num) 1)
            (-> (code _num) (code _num)) vr-append)
 'next
 'alts
 (list
  (list (blank)
        (blank)
        (item "Create a new type variable for each" (code ?))
        (item "Change type comparison to install type equivalences"))
  (list (blank)))
 'alts~
 (type-tree (hbl-append (open-paren) (code lambda {[x : ?]}) __) 
            (hbl-append (open-paren) (code if) __)
            (code #t) __ (code 1) __ (code x) (close-paren) (close-paren) 
            (code _bool) (code _num) (sub "T" 1)
            (intx-t-is-int (code _num) 1)
            (-> (code _num) (code _num)) vr-append))

(slide
 #:title "Type Inference: Impossible Cases"
 'alts~
 (type-tree (hbl-append (open-paren) (code lambda {[x : ?]}) __) 
            (hbl-append (open-paren) (code if) __)
            (code x) __ (code 1) __ (code x) (close-paren) (close-paren) 
            (sub "T" 1) (code _num) (sub "T" "1")
            (colorize
             (hbl-append
              (colorize (bit "no type: ") "red")
              (sub "T" 1)
              (t " can't be both ")
              (code _bool)
              (t " and ")
              (code _num))
             "black")
            #f vr-append))

(define poly
  (type-tree _ (hbl-append (open-paren) (code lambda {[y : ?]}) __) (code y) _ #f #f #f (close-paren) _
             (sub "T" 1) #f #f
             (-> (sub "T" 1)
                 (sub "T" 1))
             #f
             vr-append))

(slide
 #:title "Type Inference: Many Cases"
 'alts~
 poly
 'next
 (item "Sometimes, more than one type works")
 (subitem (-> (code _num) (code _num)))
 (subitem (-> (code _bool) (code _bool)))
 (subitem (-> (-> (code _num) (code _bool))
                   (-> (code _num) (code _bool))))
 (item #:bullet (ghost bullet) "so the type checker leaves variables in the reported type"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 3)

(slide
 #:title "Type Inference: Function Calls"
 'alts~
 (let* ([first-t1 (sub "T" 1)]
        [i->i (-> (code _num) (code _num))]
        [stages
         (type-tree _ (open-paren) (code {lambda {[y : ?]} y}) __ (code {lambda {[x : ?]} {+ x 1}}) #f #f (close-paren) _
                    (-> first-t1 (sub "T" 1)) i->i #f
                    (launder i->i)
                    #f vc-append)])
   (let loop ([l stages])
     (if (null? (cddr l))
         (cons
          (car l)
          (map
           (lambda (s)
             (list
              (vr-append
               (current-line-sep)
               (cc-superimpose
                (car s)
                (linewidth
                 2
                 (colorize
                  (pin-line (pin-line
                             (ghost (car s))
                             first-t1 lb-find
                             first-t1 rb-find)
                            i->i lb-find
                            i->i rb-find)
                  "red")))
               (hb-append (sub "T" 1)
                          (colorize (t " = ") "red")
                          (launder i->i)))))
           l))
         (cons (car l) (loop (cdr l)))))))

(slide
 #:title "Type Inference: Function Calls"
 'alts~
 (type-tree (hbl-append (open-paren) (code lambda {[y : ?]}) __)
            (open-paren) (code y) __ (code 7) #f #f (close-paren) (close-paren)
            (sub "T" 1) (code _num) #f
            (let ([a (hb-append (sub "T" 1)
                                (colorize (t " = ") "red")
                                (->
                                 (code _num)
                                 (sub "T" 2)))])
              (hbl-append (current-font-size)
                          (ghost a)
                          (sub "T" 2)
                          a))
            (-> (-> (code _num) (sub "T" 2))
                (sub "T" 2))
            vr-append)
 'next
 (blank)
 (item #:width (* (get-client-w #:aspect 'fullscreen) 0.8)
       "In general, create a new type variable record for the result of a function call"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 4)

(define self-app (type-tree (hbl-append (open-paren) (code lambda {[x : ?]}) __)
                            (open-paren) (code x) __ (code x) #f #f (close-paren) (close-paren)
                            (sub "T" 1) (sub "T" 1) #f
                            (colorize
                             (hbl-append
                              (sub "T" 1)
                              (t " = ")
                              (->
                               (sub "T" 1)
                               (sub "T" 2))
                              (bt " ??"))
                             "black")
                            #f vr-append))

(slide
 #:title "Type Inference: Cyclic Equations"
 'alts~
 self-app
 'next
 (blank)
 (blank)
 (para (sub "T" 1) "=" (-> (sub "T" 1) (sub "T" 2)) "=" (-> (-> (sub "T" 1) (sub "T" 2)) (sub "T" 2)) " ... no solution")
 'next
 (blank)
 (blank)
 (para "The" (dt "occurs check") ":")
 (item "When installing a type equivalence,"
       "make sure that the new type for" (type-var "T")
       "doesn't already contain" (type-var "T")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 5)

(define (uni-item . l) (apply item #:width (* (get-client-w #:aspect 'fullscreen) 3/4) l))
(define (uni-subitem . l) (apply subitem #:width (* (get-client-w #:aspect 'fullscreen) 3/4) l))

(slide
 #:title "Type Unification"
 (para "For comparing types, replace")
 (blank)
 (code equal? : (Type Type -> Boolean))
 'next
 (blank)
 (para "with")
 (blank)
 'alts
 (list
  (list
   (code unify! : (Type Type -> ())))
  (list
   (code unify! : (Type Type Exp -> ()))))
 (blank)
 (blank)
 'next
 (para "To simplify by substituting with discovered equivalences:")
 (blank)
 (code resolve : (Type -> Type)))

(slide
 #:title "Type Unification"
 (item (code resolve) (sub "T" 1) "⇒" (sub "T" 1))
 'next
 (blank)
 (blank)
 (item (code unify!) (sub "T" 1) "with" (code num))
 (item #:bullet (ghost bullet) "Then," (code resolve) "of" (sub "T" 1) (t " = ") (code num))
 'next
 (blank)
 (blank)
 (item "So far," (code resolve) "of" (-> (sub "T" 1) (sub "T" 2)) (t " = ") (-> (code num) (sub "T" 2)))
 'next
 (item #:bullet (ghost bullet) (code unify!) (sub "T" 1) "with" (sub "T" 2))
 (item #:bullet (ghost bullet) "Then," (code resolve) "of" (sub "T" 2) (t " = ") (code num)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 6)

(slide
 #:title "Type Grammar, Again"
 type-grammar)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (align p) (para p))

(slide
 #:title "Representing Type Variables"
 (align
  (vl-append
   0
   (code
    (define-type Type
      (numT)
      (boolT)
      (arrowT [arg : Type]
              [result : Type])
      (varT [is : (Boxof (Optionof Type))])))))
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (align (code (varT (box (none))))))
  (list
   (align (code (varT (box (some (numT)))))))
  (list
   (align
    (code (define (unify! [t1 : Type] [t2 : Type] [expr : Exp])
            ....
            (type-case Type t1
              ...
              [(varT b)
               .... (set-box! b (some (resolve t2))) ....]
              ...)
            ....))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 7)

(slide
 #:title "Unification Examples"
 'alts
 (list
  (list (code
         (test (unify! (numT)
                       (numT))
               (values))))
  (list (code
         (test (unify! (boolT)
                       (boolT))
               (values))))
  (list (code
         (test/exn (unify! (numT) 
                           (boolT))
                   "no type")))
  (list (code
         (test (unify! (varT (box (none)))
                       (numT))
               (values))))
  (list (code
         (test (unify! (varT (box (some (numT))))
                       (numT))
               (values))))
  (list (code
         (test/exn (unify! (varT (box (some (boolT))))
                           (numT))
                   "no type")))
  (list (code
         (test/exn (let ([t (varT (box (none)))])
                     (begin
                       (unify! t
                               (numT))
                       (unify! t
                               (boolT))))
                   "no type")))
  (list (code
         (test (let ([t (varT (box (none)))])
                 (begin
                   (unify! t
                           (numT))
                   (unify! t
                           (numT))))
               (values))))
  (list (code
         (test (let ([t (varT (box (none)))])
                 (begin
                   (unify! (arrowT t (boolT))
                           (arrowT (numT) (boolT)))
                   (unify! t
                           (numT))))
               (values))))
  (list (code
         (test/exn (let ([t (varT (box (none)))])
                     (unify! (arrowT t (boolT))
                             t))
                   "no type")))
  (list (code
         (test (let ([t1 (varT (box (none)))]
                     [t2 (varT (box (none)))])
                 (unify! t1
                         t2))
               (values))))
  (list (code
         (test/exn (let ([t1 (varT (box (none)))]
                         [t2 (varT (box (none)))])
                     (begin
                       (unify! t1
                               t2)
                       (unify! t1
                               (numT))
                       (unify! t2
                               (boolT))))
                   "no type")))
  (list (code
         (test/exn (let ([t1 (varT (box (none)))]
                         [t2 (varT (box (none)))])
                     (begin
                       (unify! t1
                               t2)
                       (unify! t2
                               (boolT))
                       (unify! t1
                               (numT))))
                   "no type")))
  (list (code
         (test/exn (let ([t1 (varT (box (none)))]
                         [t2 (varT (box (none)))])
                     (begin
                       (unify! t1
                               (arrowT t2 (boolT)))
                       (unify! t1
                               (arrowT (numT) t2))))
                   "no type")))))
  

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 8)

(slide
 #:title "Type Unification"
 #:gap-size (/ gap-size 2)
 (blank)
 (para #:width WIDER (code unify!) "a type variable" (type-var "T") "with a type" (T 2) ":")
 'next
 (uni-item "If" (type-var "T") "is set to" (T 1) "," (code unify!) (T 1) "with" (T 2))
 'next
 'alts~
 (let ([already (t "already equivalent")]
       [tvar (type-var "T")])
   (let ([p (uni-item "If" (T 2) "is" already "to" tvar
                         ", succeed")]
         [line-1 (para #:fill? #f (T 2) "=" (type-var "T"))])
     (define (add b #:spike [spike 'n])
       (pin-balloon (wrap-balloon b spike
                                  (case spike
                                    [(sw) (* -3 gap-size)]
                                    [else 0])
                                  (case spike
                                    [(sw) (* 1/2 gap-size)]
                                    [else (- gap-size)]))
                    p 
                    (case spike
                      [(sw) tvar]
                      [else already])
                    (case spike
                      [(sw) rt-find]
                      [else cb-find])))
     (list
      (list p)
      (list (add (vl-append
                  (current-line-sep)
                  line-1
                  (para #:fill? #f "or ..."))))
      (list (add (vl-append
                  (current-line-sep)
                  line-1
                  (para #:fill? #f "or" (T 2) "=" (sub "T" 3) "and" (sub "T" 3) "=" (type-var "T"))
                                (para #:fill? #f "or ..."))))
      (list (add (para #:fill? #f (code (resolve #,(T 2))) "is" (type-var "T") "?")
                 #:spike 'sw)))))
 'next
 'alts~
 (let ([fl (t "fail")])
   (let ([p (uni-item "If" (T 2) "contains" (type-var "T") ", then" fl)])
     (define (add b)
       (pin-balloon (wrap-balloon b 'w (- gap-size) 0
                                  balloon-color
                                  20)
                    p fl rc-find))
     (list
      (list p)
      (list (add (code (occurs? #,(type-var "T") #,(T 2)))))
      (list (add (code (occurs? #,(type-var "T") (resolve #,(T 2)))))))))
 'next
 (uni-item "Otherwise, set" (type-var "T") "to" (T 2) "and succeed")
 'next
 (blank)
 (para #:width WIDER (code unify!) "a type" (T 1) "to type" (T 2) ":")
 'next
 (uni-item "If" (T 2) "is a type variable" (type-var "T")
           ", then" (code unify!) (type-var "T") "and" (T 1))
 'next
 (uni-item "If" (T 1) "and" (T 2) "are both" (code _num) "or"
           (code _bool) ", succeed")
 'next
 (vl-append
  (current-line-sep)
  (uni-item "If" (T 1) "is" (-> (T 3) (T 4)) "and"
            (T 2) "is" (-> (T 5) (T 6)) ", then")
  (uni-subitem (code unify!) (T 3) "with" (T 5))
  (uni-subitem (code unify!) (T 4) "with" (T 6)))
 'next
 (uni-item "Otherwise, fail"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 9)

(slide
 #:title "Type Unification"
 #:layout 'top
 (para
  #:width (get-client-w #:aspect 'fullscreen)
  (sscode
   (define (unify! [t1 : Type] [t2 : Type] [expr : Exp])
     (type-case Type t1
       [(varT is1)
        ...]
       [else
        (type-case Type t2
          [(varT is2) (unify! t2 t1 expr)]
          [(numT) (type-case Type t1
                    [(numT) (values)]
                    [else (type-error expr t1 t2)])]
          [(boolT) (type-case Type t1
                     [(boolT) (values)]
                     [else (type-error expr t1 t2)])]
          [(arrowT a2 b2) (type-case Type t1
                            [(arrowT a1 b1)
                             (begin
                               (unify! a1 a2 expr)
                               (unify! b1 b2 expr))]
                            [else (type-error expr t1 t2)])])])))))

(define (unify-slide rhs)
  (slide
   #:title "Type Unification"
   #:layout 'top
   (para
    #:width (get-client-w #:aspect 'fullscreen)
    (sscode
     (define (unify! [t1 : Type] [t2 : Type] [expr : Exp])
       (type-case Type t1
         [(varT is1) #,(un-ss-scale rhs)]
         [else ...]))))))

#;
(if condense?
    (skip-slides 1)
    (unify-slide (sscode ...)))

(let ([steps
       (list
        (lambda (e)
          (sscode
           (type-case (Optionof Type) (unbox is1)
             [(some t3) (unify! t3 t2 expr)]
             [(none ) #,(un-ss-scale e)])))
        (lambda (e)
          (sscode
           (local [(define t3 (resolve t2))]
             #,(un-ss-scale e))))
        (lambda (e)
          (sscode
           (if (eq? t1 t3)
               (values)
               #,(un-ss-scale e))))
        (lambda (e)
          (sscode
           (if (occurs? t1 t3)
               (type-error expr t1 t3)
               #,(un-ss-scale e))))
        (lambda (e)
          (sscode
           (begin
             (set-box! is1 (some t3))
             (values)))))])
  (let loop ([steps steps][accum values])
    (if (null? steps)
        (unify-slide (accum (blank)))
        (if condense?
            (loop (cdr steps) (lambda (e) (accum ((car steps) e))))
            (begin
              (unify-slide (accum (sscode ...)))
              (loop (cdr steps)
                    (lambda (e) (accum ((car steps) e)))))))))

(slide
 #:title "Type Unification Helpers"
 (sscode
  (define (resolve [t : Type]) : Type
    (type-case Type t
      [(varT is)
       (type-case (Optionof Type) (unbox is)
         [(none) t]
         [(some t2) (resolve t2)])]
      [else t]))
  code:blank
  (define (occurs? [r : Type] [t : Type]) : Boolean
    (type-case Type t
      [(numT ) #f]
      [(boolT ) #f]
      [(arrowT a b)
       (or (occurs? r a)
           (occurs? r b))]
      [(varT is) (or (eq? r t)
                     (type-case (Optionof Type) (unbox is)
                       [(none) #f]
                       [(some t2) (occurs? r t2)]))]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 10)

(define (typecheck-slide body)
  (slide
   #:title "Type Checker with Inference"
   #:layout 'top
   (para
    #:width (get-client-w #:aspect 'fullscreen)
    (xscode
     (define typecheck : (Exp Type-Env -> Type)
       (lambda (a tenv)
         (type-case Exp a
           ...
           #,(un-xs-scale body)
           ...)))))))

(define (typecheck-slide/no-print body)
  (if condense?
      (skip-slides 1)
      (typecheck-slide body)))

(typecheck-slide
 (xscode
  [(numE n) (numT)]))

(typecheck-slide/no-print
 (xscode
  [(plusE l r) 
   ... (typecheck l env) ...
   ... (typecheck r env) ...]))

(typecheck-slide
 (xscode
  [(plusE l r)
   (begin
     (unify! (typecheck l env) (numT) l)
     (unify! (typecheck r env) (numT) r)
     (numT))]))

(typecheck-slide
 (xscode
  [(idE name) (get-type name env)]
  [(lamE n arg-type body)
   (arrowT arg-type
           (typecheck body (aBind name
                                  arg-type
                                  env)))]))

(typecheck-slide/no-print
 (xscode
  [(appE fn arg)
   ... (typecheck fn env) ... 
   ... (typecheck arg env) ...]))



(typecheck-slide/no-print
 (xscode
  [(appE fn arg)
   (local [(define result-type (varT (box (none))))]
     ... (arrowT (typecheck arg env)
                 result-type)
     ... (typecheck fn env) ...)]))

(typecheck-slide
 (xscode
  [(appE fn arg)
   (local [(define result-type (varT (box (none))))]
     (begin
       (unify! (arrowT (typecheck arg env)
                       result-type)
               (typecheck fn env)
               fn)
       result-type))]))

#|

(typecheck-slide/no-print
 (xscode
  [if0 (test-expr then-expr else-expr)
       ... (typecheck test-expr env) ...
       ... (typecheck then-expr env) ...
       ... (typecheck else-expr env) ...]))

(typecheck-slide/no-print
 (xscode
  [if0 (test-expr then-expr else-expr)
       (begin
         (unify! (typecheck test-expr env) (numT) test-expr)
         ... (typecheck then-expr env) ...
         ... (typecheck else-expr env) ...)]))

(typecheck-slide
 (xscode
  [if0 (test-expr then-expr else-expr)
       (begin
         (unify! (typecheck test-expr env) (numT) test-expr)
         (local [(define test-ty (typecheck then-expr env))]
           (begin 
             (unify! test-ty (typecheck else-expr env) else-expr)
             test-ty)))]))
|#

#;
(typecheck-slide
 (xscode
  [rec (name ty rhs-expr body-expr)
       (local [(define rhs-ty (parse-type ty))
               (define new-ds (aBind name
                                     rhs-ty
                                     env))]
         (begin
           (unify! rhs-ty (typecheck rhs-expr new-ds) rhs-expr)
           (typecheck body-expr new-ds)))]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 11)

(slide
 #:title "Type Errors"
 (para "Checking --- report that an expression doesn't have an expected type (expressed as a string):")
 (code
  type-error : (Exp String -> ...))
 'next
 (blank)
 (blank)
 (para "Inference --- report that, near some expression, two types are incompatible:")
 (code
  type-error : (Exp Type Type -> ...)))

