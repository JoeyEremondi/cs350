#lang slideshow
(require slideshow/code
         slideshow/balloon
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss"
         "utils/lexaddr.rkt"
         "utils/posn3d-obj.rkt"
         "utils/classbox.rkt")

;; ----------------------------------------
(part 1)

(mk-addr-slide (scode {let {[x 88]}
                        {+ x y}})
               (scode #,(parsed+sub
                         {+ x y}
                         ...))
               (code x) 
               "at the beginning"
               (balloon-code x = 88 code:blank ...))

(mk-addr-slide (scode {let {[y 1]}
                        {+ x y}})
               (scode #,(parsed+sub
                         {+ x y}
                         ...))
               (code y) 
               "at the beginning"
               (balloon-code y = 1 code:blank ...))

(mk-addr-slide (scode {let {[y 1]} 
                        {let {[x 2]}
                          {+ x y}}})
               (scode #,(parsed+sub
                         {+ x y}
                         ...))
               (code y) 
               "second"
               (balloon-code x = 2 code:blank y = 1 code:blank ...))

(mk-addr-slide (scode {let {[y 1]}
                        {let {[x 88]}
                          {* {+ x y} 17}}})
               (scode #,(parsed+sub
                         {+ x y}
                         ...))
               (hbl-append (code x) (t " and ") (code y))
               "first and second"
               (balloon-code x = 88 code:blank y = 1 code:blank ...))

(mk-addr-slide (scode {let {[y 1]}
                        {let {[w 10]}
                          {let {[z 9]}
                            {let {[x 0]}
                              {+ x y}}}}})
               (scode #,(parsed+sub
                         {+ x y}
                         ...))
               (hbl-append (code x) (t " and ") (code y))
               "first and fourth"
               (balloon-code x = 0 code:blank z = 9 code:blank w = 10 code:blank y = 1 code:blank ...))

(mk-addr-slide (scode {let {[y {let {[r 9]} {* r 8}}]}
                        {let {[w 10]}
                          {let {[z {let {[q 9]} q}]}
                            {let {[x 0]}
                              {+ x y}}}}})
               (scode #,(parsed+sub
                         {+ x y}
                         ...))
               (hbl-append (code x) (t " and ") (code y))
               "first and fourth"
               (balloon-code x = 0 code:blank z = 9 code:blank w = 10 code:blank y = 1 code:blank ...))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Lexical Scope"
 (item "For any expression, we can tell which identifiers will"
       "be in the environment at run time")
 (item "The order of the environment is predictable"))

;; ----------------------------------------
(part 2)

(slide
 #:title "Compilation of Variables"
 (para "A" "compiler" "can transform an" (code Exp)
       "expression to an expression without identifiers"
       sym:emdash "only lexical addresses")
 'next
 (blank)
 (code (code:contract compile : Exp ... -> ExpD))
 (blank)
 (htl-append
  (* 4 gap-size)
  (sscode
   (define-type Exp
     (numE [n : Number])
     (addE [l : Exp]
           [r : Exp])
     (multE [l : Exp]
            [r : Exp])
     (idE [n : Symbol])
     (lamE [n : Symbol]
           [body : Exp])
     (appE [fun : Exp]
           [arg : Exp])))
  (sscode
   (define-type ExpD
     (numD [n Number])
     (addD [l : ExpD]
           [r : ExpD])
     (multD [l : ExpD]
            [r : ExpD])
     (atD [pos : Number])
     (lamD [body : ExpD])
     (appD [fun : ExpD]
           [arg : ExpD])))))

(define (align . c) (apply para #:width (* 1.0 (get-client-w #:aspect 'fullscreen)) c))

(slide
 #:title "Compile Examples"
 (align
  (code
   (compile #,(parsed 1) ...) #,sym:implies #,(cparsed 1)))
 (align
  (code
   (compile #,(parsed {+ 1 2}) ...) #,sym:implies #,(cparsed {+ 1 2})))
 'next
 (align
  (code
   (compile #,(parsed x) ...) #,sym:implies #,(it "compile: free identifier")))
 'next
 (align
  (code
   (compile #,(parsed {lambda {x} {+ 1 x}}) ...) 
   #,sym:implies #,(cparsed {lambda {+ 1 {at 0}}})))
 'next
 (blank)
 (align
  (code
   (compile #,(parsed {lambda {y} {lambda {x} {+ x y}}}) ...) 
   code:blank #,sym:implies #,(cparsed {lambda {lambda {+ {at 0} {at 1}}}}))))


(slide
 #:title "Implementing the Compiler"
 'alts~
 (list
  (list
   (para
    (scode
     (define (compile [a : Exp] ...)
       (type-case Exp a
         [(numE n) (numD n)]
         [(plusE l r) (plusD (compile l)
                             (compile r))]
         [(multE l r) (multD (compile l)
                             (compile r))]
         [(idE n) ...]
         [(lamE n body-expr)
          ... (compile body-expr) ...]
         [(appE fun-expr arg-expr)
          (appD (compile fun-expr)
                (compile arg-expr))])))))
  (list
   (para
    (scode
     (define (compile [a : Exp] [env : EnvC])
       (type-case Exp a
         [(numE n) (numD n)]
         [(plusE l r) (plusD (compile l env)
                             (compile r env))]
         [(multE l r) (multD (compile l env)
                             (compile r env))]
         [(idE n) ...]
         [(lamE n body-expr)
          ... (compile body-expr ...) ...]
         [(appE fun-expr arg-expr)
          (appD (compile fun-expr env)
                (compile arg-expr env))])))))
  (list
   (para
    (scode
     (define (compile [a : Exp] [env : EnvC])
       (type-case Exp a
         [(numE n) (numD n)]
         [(plusE l r) (plusD (compile l env)
                             (compile r env))]
         [(multE l r) (multD (compile l env)
                             (compile r env))]
         [(idE n) (atD (locate n env))]
         [(lamE n body-expr)
          ... (compile body-expr ...) ...]
         [(appE fun-expr arg-expr)
          (appD (compile fun-expr env)
                (compile arg-expr env))])))))
  (list
   (para
    (scode
     (define (compile [a : Exp] [env : EnvC])
       (type-case Exp a
         [(numE n) (numD n)]
         [(plusE l r) (plusD (compile l env)
                             (compile r env))]
         [(multE l r) (multD (compile l env)
                             (compile r env))]
         [(idE n) (atD (locate n env))]
         [(lamE n body-expr)
          (lamD
           (compile body-expr
                    (extend-env (bindE n)
                                env)))]
         [(appE fun-expr arg-expr)
          (appD (compile fun-expr env)
                (compile arg-expr env))])))))))

(slide
 #:title "Compile-Time Environment"
 (para #:width (get-client-w #:aspect 'fullscreen)
       "Mimics the run-time environment, but without values:")
 (scode
  (define-type BindingC
    (bindE [name : Symbol]))
  code:blank
  (define-type-alias EnvC (Listof BindingC))
  code:blank
  (define (locate name env)
    (cond
     [(empty? env) (error 'locate "free variable")]
     [else (if (symbol=? name (bindC-name (first env)))
               0
               (+ 1 (locate name (rest env))))]))))

(slide
 #:title (hbl-append (code interp) (titlet " for Compiled"))
 (para #:width (get-client-w #:aspect 'fullscreen) 
       "Almost the same as" (code interp) "for" (code Exp) ":")
 (sscode
  (define (interp a env)
    (type-case ExpD a
      [(numD n) (numV n)]
      [(plusD l r) (num+ (interp l env)
                         (interp r env))]
      [(multD l r) (num* (interp l env)
                         (interp r env))]
      [(atD pos) (list-ref env pos)]
      [(lamD body-expr)
       (closV body-expr env)]
      [(appD fun-expr arg-expr)
       (let ([fun-val (interp fun-expr env)]
             [arg-val (interp arg-expr env)])
         (interp (closV-body fun-val)
                 (cons arg-val
                       (closV-env fun-val))))]))))

(define WIDE (* 0.9 (get-client-w #:aspect 'fullscreen)))

(parameterize ([current-para-width ((get-current-para-width #:aspect 'fullscreen))])
  (slide
   #:title "Timing Effect of Compilation"
   (para #:width WIDE "Given")
   (para
    (sscode (define c #,(parsed
                         {{{{lambda {x}
                              {lambda {y} 
                                {lambda {z} {+ {+ x x} {+ x x}}}}}
                            1}
                           2}
                          3}))
            (define d (compile c mt-env))))
   (para #:width WIDE "then")
   (para (sscode (interp d empty)))
   (para #:width WIDE "is significantly faster than")
   (para (sscode (interp c mt-env)))
   'next
   (blank)
   (para
    #:align 'right
    (scale/improve-new-text
     (para "Using the built-in" (code list-ref)
           "simulates machine array indexing,"
           "but don't take timings too seriously")
     0.75))))

;; ----------------------------------------
(part 3)

(define prog-file
  (let ([p (code {...})])
    (cc-superimpose
     (file-icon (+ (pict-width p) gap-size)
                (* 1.25 (+ (pict-width p) gap-size))
                #t)
     p)))


(define computer (inset (scale (bitmap "computer.png") 1/4) 
                        0 (* -1/2 gap-size)))
(define racket (scale (bitmap "racket.png") 1/6))

(define steps-title "From Plait to Machine Code")

(define (steps-slide n)
  (define (at i p) ((if (n . >= . i) values ghost) p))
  (slide
   #:title steps-title
   (let ([p (vc-append
             gap-size
             (scale prog-file 0.75)
             (colorize (arrow gap-size (* pi -1/2)) "forestgreen")
             racket
             (at 1 (colorize (arrow gap-size (* pi -1/2)) "forestgreen"))
             (at 1 computer))])
     (refocus (hc-append
               (* 4 gap-size)
               p
               (vl-append
                gap-size
                (at 2 (item #:fill? #f "Everything must be a number"))
                (at 3 (item #:fill? #f  "No" (code define-type) "or" (code type-case)))
                (at 4 (item #:fill? #f  "No implicit continuations"))
                (at 5 (item #:fill? #f  "No implicit allocation"))))
              p))))

(for ([i (in-range 6)])
  (steps-slide i))

;; ----------------------------------------
(part 4)

(define convert-arrow (let ([p (blank gap-size)])
                        (pin-arrow-line (/ gap-size 2)
                                        p
                                        p lc-find
                                        p rc-find
                                        #:color "forestgreen"
                                        #:line-width 3)))

(slide
 #:title steps-title
 (para "Step 1:")
 (htl-append gap-size
             (vr-append
              gap-size
              (code Exp)
              (code {lambda {x}
                      {+ 1 x}}))
             convert-arrow
             (vl-append
              gap-size
              (code ExpD)
              (code {(code:line lambda)
                     {+ 1 {at 0}}})))
 'next
 (blank)
 (blank)
 (para #:align 'center "Eliminates all run-time names"))

(slide
 #:title steps-title
 (para "Step 2:")
 (code interp #,convert-arrow interp #,(tt "+") continue)
 'next
 (blank)
 (blank)
 (para #:align 'center "Eliminates implicit continuations"))

(slide
 #:title steps-title
 (para #:width WIDE "Step 3:")
 'alts
 (let ([p
        (lambda (v)
          (htl-append gap-size
                      (vr-append
                       gap-size
                       (t "function calls")
                       (v
                        (sscode
                         (interp l
                                 env
                                 (plusSecondK r
                                              env
                                              k)))))
                      convert-arrow
                      (vl-append 
                       gap-size
                       (hbl-append (t "regsiters and ") (tt "goto"))
                       (v
                        (sscode 
                         (begin
                           (set! expr-reg l)
                           (set! k-reg (plusSecondK r 
                                                    env-reg 
                                                    k-reg))
                           (interp)))))))])
   (list
    (list (p ghost))
    (list (p values))))
 'next
 (blank)
 (blank)
 (para #:align 'center "Makes argument passing explicit"))

;; ----------------------------------------
(part 5)

(slide
 #:title steps-title
 (para #:width WIDE "Step 4:")
 'alts
 (let ([convert (lambda (a b)
                  (list (htl-append gap-size a convert-arrow b)))])
   (list
    (convert (scode (multSecondK r 
                                 env-reg 
                                 k-reg))
             (scode (malloc3 3
                             (ref expr-reg 2)
                             env-reg
                             k-reg)))
    (list
     (table
      3
      (list (code doneK) convert-arrow (tt "1")
            (code plusSecondK)  convert-arrow (tt "2")
            (t "...") (blank) (blank)
            (code numD) convert-arrow (tt "8")
            (code plusD) convert-arrow (tt "9")
            (t "...") (blank) (blank)
            (code numV) convert-arrow (tt "15")
            (code closV) convert-arrow (tt "16"))
      lbl-superimpose lbl-superimpose
      gap-size (current-line-sep)))
    (convert (sscode (type-case Cont k-reg
                       ...
                       [(multSecondK r env k)
                        ... r
                        ... env
                        ... k ..]
                       ...))
             (sscode (case (ref k-reg 0)
                       ...
                       [(3) ; multSecondK
                        ... (ref k-reg 1)
                        ... (ref k-reg 2)
                        ... (ref k-reg 3) ...]
                       ...)))
    (list
     (code
      (define memory (make-vector 1500 0))
      (define ptr-reg 0)
      code:blank
      (define (malloc3 tag a b c)
        (begin
          (vector-set! memory ptr-reg tag)
          (vector-set! memory (+ ptr-reg 1) a)
          (vector-set! memory (+ ptr-reg 2) b)
          (vector-set! memory (+ ptr-reg 3) c)
          (set! ptr-reg (+ ptr-reg 4))
          (- ptr-reg 4)))))))
 'next
 (blank)
 (para #:align 'center "Makes all allocation explicit")
 (para #:align 'center "Makes everything a number"))

#;{
   
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 6)

(define ticae-posn
  (code
   {class (code:line posn extends object)
     {[x : num]  [y : num]}
     {mdist : num -> num
            {+ {get this x} {get this y}}}
     {addDist : posn -> num
              {+ {send this mdist 0} {send arg mdist 0}}}}))

(define ticae-posn3d
  (ssscode {class (code:line posn3D extends posn)
             {[z : num]}
             {mdist : num -> num
                    {+ {get this z} {super mdist arg}}}}
           {send {new posn3D 7 5 3} mdist 0}))

(define-values (posn-ex posn-ex-label)
  (posn-obj+label))
(define-values (posn3d-ex posn3d-ex-label)
  (posn3d-obj+label))

(define-values (posn4d-ex posn4d-ex-label)
  (obj+label (code posn4D)
             (list (code 0) (code 0) (code 0) (code 1))))

(define posn-defn-box
  (classbox (code posn) 
            (list (code x) (code y))
            (list (hbl-append (code mdist)
                              (t " ")
                              (ssscode {+ {get this x} {get this y}}))
                  (hbl-append (code addDist)
                              (t " ")
                              (ssscode {+ {send this mdist 0} {send arg mdist 0}})))))

(define posn-pos-defn-box
  (classbox (code posn)
            null
            (list (sscode {+ {get this 0} {get this 1}})
                  (sscode {+ {send this 0 0} {send arg 0 0}}))))

(define posn3d-pos-defn-box
  (classbox (code posn3D)
            null
            (list (sscode {+ {get this 2} {ssend ...}})
                  (sscode {+ {send this 0 0} {send arg 0 0}}))))

(define posn4d-pos-defn-box
  (classbox (code posn4D)
            null
            (list (sscode {+ {get this 3} {ssend ...}})
                  (sscode {+ {send this 0 0} {send arg 0 0}})
                  (sscode {get this 3})
                  (sscode {new posn {get this 0} {get this 1}}))))

(slide
 #:title "Compiling Class-Based Programs"
 (scale
  (code
   #,ticae-posn
   code:blank
   {send {new posn 1 2} mdist 0})
  0.8)
 'next
 (blank)
 (blank)
 (blank)
 (para (code this) "and" (code arg) "--- no search")
 'next
 (para "field and method names --- search"))

(slide
 #:title "Run-Time Dispatch by Name"
 (pin-arrow-line (/ gap-size 2)
                 (ht-append
                  (* 4 gap-size)
                  (ht-append
                   (t "o = ")
                   posn-ex)
                  posn-defn-box)
                 posn-ex-label rc-find
                 posn-defn-box lt-find
                 #:line-width 0 #:color "orange")
 (blank)
 (code {send o mdist 0})
 'next
 (blank)
 'alts
 (list
  (list
   (para #:fill? #f (code send) "follows reference to class table, searches method list"))
  (list
   (hc-append gap-size
              (scale ticae-posn 0.5)
              sym:implies
              (vl-append 
               (current-line-sep)
               (t "typechecking ensures")
               (t "search will succeed")))
   'next
   (blank)
   'alts
   (list
    (list
     (para #:fill? #f
           "If we preserve method order in flattening,"
           "method will always be first in list"))
    (list
     (para #:fill? #f
           "Similarly, we can rely on field positions"
           "as long as we add subclass fields to the end"))))))

(define (by-posn-slide posn-ex posn-ex-label posn-pos-defn-box)
  (slide
   #:title "Run-Time Dispatch by Position"
   (pin-arrow-line (/ gap-size 2)
                   (ht-append
                    (* 4 gap-size)
                    (ht-append
                     (t "o = ")
                     posn-ex)
                    posn-pos-defn-box)
                   posn-ex-label rc-find
                   posn-pos-defn-box lt-find
                   #:line-width 0 #:color "orange")
   (blank)
   (code {send o 0 0})))
(by-posn-slide posn-ex posn-ex-label posn-pos-defn-box)
(by-posn-slide posn3d-ex posn3d-ex-label posn3d-pos-defn-box)
(by-posn-slide posn4d-ex posn4d-ex-label posn4d-pos-defn-box)

}
