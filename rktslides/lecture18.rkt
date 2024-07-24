#lang slideshow
(require slideshow/code
         slideshow/balloon
         slideshow/face
         racket/draw
         racket/class
         "utils/utils.rkt"
         "utils/colors.rkt"
         "utils/alg.rkt"
         "utils/types.rkt"
         "utils/fae-types.rkt")

(define WIDE (* (get-client-w #:aspect 'fullscreen) 0.95))

(set-spotlight-style! #:color (let ([c (send the-color-database find-color "pink")])
                                (make-color (send c red)
                                            (send c green)
                                            (send c blue)
                                            0.7)))

;; ----------------------------------------
(part 1)

(slide
 #:title "Typed Records"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)          -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code #,(nonterm "Symbol")) (blank)
        (blank)          -or- (code {lambda {[#,(nonterm "Symbol") : #,(nonterm "Type")]} #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {record {#,(nonterm "Symbol") #,(nonterm "Exp")} ...}) new-label
        (blank)          -or- (code {get #,(nonterm "Exp") #,(nonterm "Symbol")}) new-label
        (blank)          -or- (code {set #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Exp")}) new-label))
 (grammar-table
  (list (nonterm "Type") eqls (code num) (blank)
        (blank)          -or- (code bool) (blank)
        (blank)           -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank)
        (blank)           -or- (code {[#,(nonterm "Symbol") : #,(nonterm "Type")] ...}) new-label)))

(define (show-type ty)
  (vc-append
   gap-size
   (blank)
   (blank)
   (htl-append (t "Has type ") ty)))

(slide
 #:title "Records"
 'alts
 (list
  (list
   (code {record {x {+ 1 2}}
                 {y {* 3 4}}})
   'next
   (show-type (code {[x : num]
                     [y : num]})))
  (list
   (code {record {p {record {x {+ 1 2}}
                            {y {* 3 4}}}}})
   'next
   (show-type (code {[p : {[x : num]
                           [y : num]}]})))
  (list
   (code {get {record {x {+ 1 2}}
                      {y {* 3 4}}}
              y})
   'next
   (blank)
   (para "The subexpression type")
   (show-type (code {[x : num]
                     [y : num]}))
   (para "means that" (code get) "will succeed for" (code y)))
  (list
   (code {get 
          {get {record {p {record {x {+ 1 2}}
                                  {y {* 3 4}}}}}
               p}
          x})
   'next
   (blank)
   (para "The subexpression type")
   (show-type (code {[p : {[x : num]
                           [y : num]}]}))
   (para "means that" (code get) "will succeed for" (code p) "then" (code x)))
  (list
   (code {set {record {x {+ 1 2}}
                      {y {* 3 4}}}
              y
              0})
   'next
   (blank)
   (blank)
   (htl-append (t "Same type and value as ") (code {record {x 3}
                                                           {y 0}})))
  (list
   (code {let {[r : {[x : num]
                     [y : num]}
                  {record {x {+ 1 2}}
                          {y {* 3 4}}}]}
           {+ {get r x}
              {get r y}}}))
  (list
   (code {lambda {[r : {[x : num]}]}
           {get r x}})
   'next
   (show-type (code ({[x : num]} -> num))))
  (list
   (code {lambda {[r : {[x : num]}]}
           {set r x 1}})
   'next
   (show-type (code ({[x : num]} -> {[x : num]}))))
  (list
   (code {let {[f : ({[x : num]} -> {[x : num]})
                  {lambda {[r : {[x : num]}]}
                    {set r x 1}}]}
           {f {record {x {+ 1 1}}}}}))))

(define dots (t "..."))
(define (set-of i)
  (hbl-append (t "{") i (t "}")))
(define (seq-of X sub)
  (set-of (code #,(hbl-append (X 1) (t ",")) #,dots #,(X sub))))
(define (lt) (t "≤"))
(define (not-of lt)
  (refocus (cc-superimpose lt (t "/")) lt))

(define (set-rule subtype?)
  (infer (hbl-append
          (code #,(E) #,(ts) {set #,(M 1) #,(X "i") #,(M 2)}) __ (code :) __ 
          (code {[#,(X 1) : #,(T 1)] #,dots [#,(X "n") : #,(T "n")]}))
         (vc-append
          (hbl-append
           (code #,(E) #,(ts) #,(M 1) : {[#,(X 1) : #,(T 1)] #,dots [#,(X "n") : #,(T "n")]})
           __ __
           (code #,(E) #,(ts) #,(M 2) : #,(if subtype? ((prime T)) (T "i"))))
          (let ([p (code #,(X "i") ∈ #,(seq-of X "n"))])
            (if subtype?
                (hbl-append p __ __ (code #,((prime T)) #,(lt) #,(T "i")))
                p)))))

(slide
 #:title "Typechecking"
 (infer (hbl-append
         (code #,(E) #,(ts) {record {{#,(X 1) : #,(M 1)} #,dots {#,(X "n") : #,(M "n")}}})
         __ (code :) __ (T))
        (vc-append
         (current-line-sep)
         (hbl-append
          (code #,(E) #,(ts) #,(M 1) : #,(T 1))
          __ __
          dots
          __ __
          (code #,(E) #,(ts) #,(M "n") : #,(T "n")))
         (code #,(T) = {[#,(X 1) : #,(T 1)] #,dots [#,(X "n") : #,(T "n")]})))
 'next
 (blank) (blank)
 
 (infer (hbl-append
         (code #,(E) #,(ts) {get #,(M) #,(X "i")}) __ (code :) __ (T "i"))
        (vc-append
         (hbl-append
          (code #,(E) #,(ts) #,(M) : {[#,(X 1) : #,(T 1)] #,dots [#,(X "n") : #,(T "n")]}))
         (code #,(X "i") ∈ #,(seq-of X "n"))))
 'next
 (blank) (blank)

 (set-rule #f))

;; ----------------------------------------
(part 2)



(define records-and-fields-title "Records and Fields")

(define (fields-slides func-arg-type
                       #:prefix [prefix 0]
                       #:arg [arg-expr (code
                                        {record {x 1}
                                                {y 2}})]
                       #:arg-type [arg-type
                                   (code {[x : num] [y : num]})]
                       #:has-type? [has-type? #t]
                       #:show-sub? [show-sub? #t]
                       . extra)
  (define (extra-fields-slides #:sub? [sub? #f]
                               #:arg? [arg? sub?]
                               #:func? [func? arg?])
    (define func (hilite
                  #:on? func?
                  (code
                   {lambda {[r : #,func-arg-type]}
                     {get r x}})))
    (define arg (hilite
                 #:on? arg?
                 #:color "lightgreen"
                 arg-expr))
    (slide
     #:title records-and-fields-title
     (let* ([p (code {#,func
                      #,arg})]
            [p (if func?
                   (pin-balloon (wrap-balloon 
                                 (code #,(-> func-arg-type (code num)))
                                 's 0 gap-size)
                                p
                                func ct-find)
                   p)]
            [p (if arg?
                   (pin-balloon (wrap-balloon 
                                 arg-type
                                 'n 0 (- gap-size))
                                p
                                arg cb-find)
                   p)])
       (inset p 0 prefix 0 0))
     (vc-append
      (blank (* 4 gap-size))
      ((if sub? values ghost)
       (if show-sub?
           (code #,arg-type #,(if has-type? (lt) (not-of (lt))) #,func-arg-type)
           (apply vc-append gap-size extra))))))

  (extra-fields-slides)
  (extra-fields-slides #:func? #t)
  (extra-fields-slides #:arg? #t)
  (extra-fields-slides #:sub? #t))

(fields-slides (code {[x : num]})
               #:prefix (* 3 gap-size)
               #:arg (code {record {x 1}})
               #:arg-type (code {[x : num]})
               #:show-sub? #f
               (show-type (code num))
               (blank)
               (apply-rule))
(fields-slides (code {[x : num] [y : num]})
               #:prefix (* 3 gap-size)
               #:arg (code {record {y 1}
                                   {x 2}})
               #:arg-type (code {[y : num] [x : num]})
               #:show-sub? #f
               (para #:align 'center no-type " --- field order doesn't match")
               (blank)
               (apply-rule)
               (blank))
(fields-slides (code {[x : num]})
               #:prefix (* 3 gap-size)
               #:arg (code {record {x 1}
                                   {y 2}})
               #:arg-type (code {[x : num] [y : num]})
               #:show-sub? #f
               (para #:align 'center no-type " --- extra fields in argument")
               (blank)
               (apply-rule))
 
(define (record-subtype-rule covariant?)
 (infer
  (hbl-append (code {[#,(X 1) : #,(T 1)] #,dots [#,(X "n") : #,(T "n")]})
              __
              (lt)
              __
              (code {[#,((prime X) 1) : #,((prime T) 1)] #,dots [#,((prime X) "m") : #,((prime T) "m")]}))
  (vc-append (current-line-sep)
             (code #,(seq-of X "n") ⊇ #,(seq-of (prime X) "m"))
             (code #,(X "i") #,(t "=") #,((prime X) "j") #,(t "⇒") #,(T "i") #,(if covariant? (t "≤") (t "=")) #,((prime T) "j")))))

(define arrow-equal (code #,(-> (T 1) (T 2)) #,(lt) #,(-> (T 1) (T 2))))

(slide
 #:title "Subtypes"
 (para "If" (T) "is a" (bit "subtype") "of" ((prime T)) ",")
 (para #:align 'center (T) (lt) ((prime T)))
 (para "then an expression of type" (T) "can be used in place of an expression of type" ((prime T)))
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (code {[x : num] [y : num]} #,(lt) {[x : num]}))
  (list
   (code {[y : num] [x : num]} #,(lt) {[x : num] [y : num]}))
  (list
   (code {[x : num]} #,(lt) {[x : num]}))
  (list
   (code {[x : num]} #,(not-of (lt)) {[x : num] [y : num]}))
  (list
   (code {[x : num] [y : num]} #,(lt) {[x : num]})))
 (blank)
 (blank)
 (parameterize () ; [current-font-size (floor (* #e0.8 (current-font-size)))])
   (para #:align 'center "Intution: " (T) (lt) ((prime T)) "means that fewer values fit" (T) "than" ((prime T)))))

(slide
 #:title "Subtype Rules"
 (record-subtype-rule #f)
 'next
 (blank) (blank)
 (hc-append (* 3 gap-size) (code num #,(lt) num) (code bool #,(lt) bool))
 (blank)
 arrow-equal
 'next
 (blank) (blank)
 (apply-rule #:subtype? #t))

(fields-slides (code {[x : num]}))
(fields-slides (code {[y : num] [x : num]}))
(fields-slides (code {[x : num] [y : num]})
               #:arg (code {record {y 5}})
               #:arg-type (code {[y : num]})
               #:has-type? #f)

;; ----------------------------------------
(part 3) ; variance

(define (vs-slides title mk-code t1 t2
                   old-rule new-rule
                   . rest)
  (define (vs hi-a vs hi-b)
    (vc-append
     gap-size
     (hi-b t2)
     (vs (t "vs."))
     (hi-a t1)))
  (define (hi-a p) (hilite p))
  (define (hi-b p) (hilite #:color "lightgreen" p))
  (slide
   #:title title
   'alts
   (append
    (list
     (list
      (mk-code values values))
     (list
      (mk-code hi-a values)
      (blank)
      (blank)
      (vs hi-a ghost ghost))
     (list
      (mk-code hi-a hi-b)
      (blank)
      (blank)
      (vs hi-a values hi-b)))
    (if old-rule
        (list
         (list
          (vs values values values)
          'next
          (blank)
          (blank)
          'alts
          (if new-rule
              (list
               (cons old-rule rest)
               (list new-rule))
              (list
               (cons
                old-rule
                rest)))))
        null))))
             
(vs-slides "Subtypes in Fields"
           (lambda (hi-a hi-b)
             (code
              {let {[f : ? ; ({[p : {[x : num]}]} -> num)
                       {lambda {[r : #,(hi-a (code {[p : {[x : num]}]}))]}
                         {get {get r p} x}}]}
                {f #,(hi-b (code {record {p {record {x 5}
                                                    {y 6}}}}))}}))
           (code {[p : {[x : num]}]})
           (code {[p : {[x : num]
                        [y : num]}]})
           (record-subtype-rule #f)
           (record-subtype-rule #t))

(define field-update-and-subtype-title 
  "Field Update and Subtypes")

(slide
 #:title field-update-and-subtype-title 
 (code
  {lambda {[r : {[p : {[x : num]}]}]}
    {set r p {record {x 5}
                     {y 6}}}}))

(slide
 #:title field-update-and-subtype-title 
 (para #:width WIDE "Original rule:")
 (set-rule #f)
 'next
 (blank)
 (blank)
 (para #:width WIDE "Revised rule:")
 (set-rule #t))

;; ----------------------------------------
(part 4)

(define (people p1 p2
                #:alice [alice values]
                #:bob [bob values]
                #:mood [mood 'happy]
                #:want [want (blank)]
                #:give [give (blank)]
                #:wide? [wide? #f])
  (define (talk f s spike ty delta)
    (let ([p (if s
                 (pin-balloon (wrap-balloon (apply para #:width (* 1/3 (get-client-w #:aspect 'fullscreen))
                                                   #:fill? #f
                                                   (if (list? s)
                                                       s
                                                       (list s)))
                                            spike 0 gap-size)
                              f
                              f (lambda (p q)
                                  (define-values (x y) (ct-find p q))
                                  (values (+ x (if (eq? spike 'sw) 20 -20))
                                          y)))
                 f)])
      (refocus (vc-append (* delta gap-size) p ty) p)))
  (list
   (inset
    (hc-append
     (* 8 gap-size)
     (talk (alice (scale (face 'happy) 0.75)) p2 (if wide? 'se 'sw) give 3)
     (talk (bob (scale (if (procedure? mood) (mood) (face mood)) 0.75)) p1 (if wide? 'sw 'se) want 1))
    (* 3 gap-size))))

(define fish-color "blue")

(define color-strip
  (let ([p (lambda (c)
             (colorize (scale (filled-rectangle gap-size gap-size) 0.5)
                       c))])
    (hc-append (p "red") (p "forestgreen") (p "blue"))))

(define make-fishbowl
  (let ([bowl (new dc-path%)])
    (send bowl move-to 5 0)
    (send bowl line-to 8 3)
    (send bowl curve-to 4 3 0 17 0 20)
    (send bowl curve-to 0 35 8 40 10 40)
    (send bowl line-to 20 40)
    (send bowl curve-to 22 40 30 35 30 20)
    (send bowl curve-to 30 17 26 3 22 3)
    (send bowl line-to 25 0)
    (send bowl close)
    (send bowl scale 2.2 1.5)
    (send bowl scale 1.2 1.2)
    (lambda (#:color [color fish-color]
             #:color-bowl? [color-bowl? #f])
      (scale
       (cb-superimpose
        (cc-superimpose
         (dc (lambda (dc x y)
               (let ([op (send dc get-pen)]
                     [ob (send dc get-brush)])
                 (send dc set-pen "gray" 2 'solid)
                 (send dc set-brush "skyblue" 'solid)
                 (send dc draw-path bowl x y)
                 (send dc set-pen op)
                 (send dc set-brush ob)))
             (* 30 2.2 1.2) (* 1.5 1.2 40))
         (if color
             (inset (standard-fish 30 25 #:color color) 4 3 0 0)
             (blank)))
        (if color-bowl?
            (inset color-strip 5)
            (blank)))
       1.5))))

(define ((add-right o) p) (refocus (hc-append gap-size p o) p))
(define ((add-left o) p) (refocus (hc-append gap-size o p) p))

(define fish-type (code {[sz : num]}))
(define blue-fish-type (code {[sz : num] [col : num]}))
(define fish-result-type (-> (code num) fish-type))
(define blue-fish-result-type (-> (code num) blue-fish-type))
(define fish-arg-type (-> fish-type (code num)))
(define blue-fish-arg-type (-> blue-fish-type (code num)))

(define a-fish (scale (standard-fish 30 25 #:color "gray") 3 2))
(define a-colorfish (scale (standard-fish 30 25 #:color "blue") 3 2))

(define (background p color)
  (cc-superimpose (colorize (filled-rectangle (+ (pict-width p) 10)
                                              (+ (pict-height p) 20))
                            color)
                  p))

(define (store-of p)
  (hc-append
   gap-size
   (background (vc-append (colorize (t "Pets") "white")
                          (background (ghost (scale p 1/2)) "lightblue"))
               "brown")
   (colorize (arrow gap-size 0) "forestgreen")
   (scale p 0.5)))
  
(define (idea-of p)
  (define col "beige")
  (define c (cloud (* 2 (pict-width p))
                   (* 2 (pict-height p))
                   col))
  (vl-append 5
             (inset (cc-superimpose c p) 0 0 0 -10)
             (colorize (filled-ellipse 40 20) col)
             (colorize (filled-ellipse 20 10) col)
             (blank (pict-height c))))

(slide
 #:title "Subtypes and Functions"
 'alts
 (list
  (people #f #f)
  (people "I would like a fish." #f #:want fish-type
          #:bob (add-right (idea-of a-fish)))
  (people #f "Here is a blue fish." #:want fish-type #:give blue-fish-type
          #:bob (add-right (idea-of a-fish))
          #:alice (add-left (inset a-colorfish gap-size)))
  (append
   (people #f #f #:mood 'happier #:want fish-type #:give blue-fish-type
           #:bob (add-right (idea-of a-fish))
           #:alice (add-left (inset a-colorfish gap-size)))
   (list (blank)
         (blank)
         (blank)
         (code #,blue-fish-type #,(lt) #,fish-type)))
  (people #f #f)
  (people "Can you recommend a fish store?" #f #:want fish-result-type
          #:bob (add-right (idea-of (store-of a-fish))))
  (people #f (list "You should go to" (bt "Blue Fish R Us") "!") #:want fish-result-type #:give blue-fish-result-type
          #:bob (add-right (idea-of (store-of a-fish)))
          #:alice (add-left (inset (store-of a-colorfish) gap-size)))
  (append
   (people #f #f #:mood 'happier #:want fish-result-type #:give blue-fish-result-type
           #:bob (add-right (idea-of (store-of a-fish)))
           #:alice (add-left (inset (store-of a-colorfish) gap-size)))
   (list (blank)
         (blank)
         (blank)
         (scale
          (code #,blue-fish-result-type #,(lt) #,fish-result-type)
          0.9)))))

(define arrow-covariant
  (infer (code #,(-> (T 1) (T 2)) #,(lt) #,(-> (T 1) ((prime T) 2)))
         (code #,(T 2) #,(lt) #,((prime T) 2))))

(vs-slides "Subtypes from Functions"
           (lambda (hi-a hi-b)
             (code {let {[f : ?
                            {lambda {[g #,(hi-a (code (num -> {[x : num]})))]}
                              {get {g 10} x}}]}
                     {f #,(hi-b
                           (code {lambda {[v : num]}
                                   {record {x v}
                                           {y v}}}))}}))
           (code (num -> {[x : num]}))
           (code (num -> {[x : num] [y : num]}))
           arrow-equal
           arrow-covariant)

;; ----------------------------------------
(part 5)

(slide
 #:title "Subtype and Function Arguments"
 'alts
 (list
  (people #f #f)
  (people "I need a fish bowl." #f #:want fish-arg-type
          #:bob (add-right (idea-of (make-fishbowl #:color "gray"))))
  (people #f "Here is a bowl made specially for colorful fish." #:want fish-arg-type #:give blue-fish-arg-type
          #:bob (add-right (idea-of (make-fishbowl #:color "gray")))
          #:alice (add-left (inset (make-fishbowl #:color #f #:color-bowl? #t) gap-size)))
  (append
   (people #f #f #:mood 'unhappy #:want fish-arg-type #:give blue-fish-arg-type
           #:bob (add-right (idea-of (make-fishbowl #:color "gray")))
           #:alice (add-left (inset (make-fishbowl #:color #f #:color-bowl? #t) gap-size)))
   (list (blank)
         (blank)
         (blank)
         (scale
          (code #,blue-fish-arg-type #,(not-of (lt)) #,fish-arg-type)
          0.9)))))

(vs-slides "Subtypes and Function Arguments"
           (lambda (hi-a hi-b)
             (scode {let {[f : ?
                             {lambda {[g #,(hi-a (code ({[x : num]} -> num)))]}
                               {g {record {x 1}}}}]}
                      {f #,(hi-b
                            (code {lambda {[r : {[x : num] [y : num]}]}
                                    {get r y}}))}}))
           (code ({[x : num]} -> num))
           (code ({[x : num] [y : num]} -> num))
           arrow-covariant
           #f
           (blank)
           (blank)
           (it "Correctly rejected!"))

(slide
 #:title "Subtype and Function Arguments"
 'alts
 (list
  (people #f #f)
  (people "I need a fish bowl for my blue fish." #f #:want blue-fish-arg-type
          #:bob (add-right (idea-of (make-fishbowl #:color-bowl? #t))))
  (people #f "Here is a bowl that works for any fish." #:want blue-fish-arg-type #:give fish-arg-type
          #:bob (add-right (idea-of (make-fishbowl #:color-bowl? #t)))
          #:alice (add-left (inset (make-fishbowl #:color #f #:color-bowl? #f) gap-size)))
  (append
   (people #f #f #:mood 'happy #:want blue-fish-arg-type #:give fish-arg-type
           #:bob (add-right (idea-of (make-fishbowl #:color-bowl? #f)))
           #:alice (add-left (inset (make-fishbowl #:color #f #:color-bowl? #f) gap-size)))
   (list (blank)
         (blank)
         (blank)
         (scale
          (code #,fish-arg-type #,(lt) #,blue-fish-arg-type)
          0.9)))))

(define arrow-variants
  (infer (code #,(-> (T 1) (T 2)) #,(lt) #,(-> ((prime T) 1) ((prime T) 2)))
         (ante-append (code #,((prime T) 1) #,(lt) #,(T 1))
                      (code #,(T 2) #,(lt) #,((prime T) 2)))))

(vs-slides "Subtypes to Functions"
           (lambda (hi-a hi-b)
             (scode {let {[f : ?
                             {lambda {[g #,(hi-a (code ({[x : num] [y : num]} -> num)))]}
                               {g {record {x 1}
                                          {y 2}}}}]}
                      {f #,(hi-b
                            (code {lambda {[r : {[x : num]}]}
                                    {get r x}}))}}))
           (code ({[x : num] [y : num]} -> num))
           (code ({[x : num]} -> num))
           arrow-covariant
           arrow-variants)

;; ----------------------------------------
(part 6)

(slide
 #:title "Covariance and Contravariance"
 'alts
 (list
  (list
   arrow-variants
   (blank)
   (blank)
   (para #:align 'center "Function-result types are" (bit "covariant") "with function types")
   (blank)
   (para #:align 'center
         #:width WIDE
         "Function-argument types are" (bit "contravariant") "with function types"))
  (list
   (record-subtype-rule #t)
   (blank)
   (blank)
   (para #:align 'center "Field types are" (bit "covariant") "with record types")
   'next
   (blank)
   (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
     (para #:align 'right "... as long as" (code set) "is a functional update")))))

;; ----------------------------------------
(part 7)

(define update-title "Subtypes and Update")

(define fish-bowl-arrow-type (code code:blank
                                   ({[fish : {[sz : num]}]}
                                    -> {[fish : {[sz : num]}]})))
(define fish-bowl!-arrow-type (code code:blank
                                    ({[fish : {[sz : num]}]}
                                    -> bool)))
(define colorfish-bowl-type (code {[fish : {[sz : num]
                                            [col : num]}]}))

(slide
 #:title "Subtypes and Functions"
 'alts
 (list
  (people #f #f)
  (people "Can you take care of my fish in a bowl while I'm on vacation?."
          #f
          #:bob (add-right (make-fishbowl #:color-bowl? #t))
          #:want colorfish-bowl-type
          #:wide? #t)
  (list
   'alts
   (let ([mk
          (lambda (#:bob-fish? [bob-fish? #t] #:bob-gone? [bob-gone? #f] #:color-fish? [color-fish? #t])
            (define bowl (make-fishbowl #:color-bowl? color-fish?))
            (people #f (if (and bob-gone? (not color-fish?))
                           "Let's move the fish to this other bowl."
                           "Give me a bowl with a fish, and I'll return a bowl with a fish when you get back.")
                    #:bob (if bob-fish?
                              (lambda (p) (refocus (hc-append gap-size (if bob-gone? (ghost p) p) bowl) p))
                              (if bob-gone? ghost values))
                    #:alice (if bob-fish?
                                values
                                (lambda (p) (refocus (hc-append gap-size bowl p) p)))
                    #:want colorfish-bowl-type
                    #:give fish-bowl-arrow-type
                    #:wide? #t))])
     (list
      (mk)
      (mk #:bob-fish? #f)
      (mk #:bob-fish? #f #:bob-gone? #t)
      (mk #:bob-fish? #f #:bob-gone? #t #:color-fish? #f))))
  (people #f #f #:mood (lambda () (face* 'none 'smaller #f default-face-color 6))
          #:bob (lambda (p) (refocus (hc-append gap-size p (make-fishbowl)) p))
          #:want colorfish-bowl-type
          #:give fish-bowl-arrow-type)
  (people #f #f #:mood 'unhappy
          #:bob (lambda (p) (refocus (hc-append gap-size p (make-fishbowl #:color "gray")) p))
          #:want colorfish-bowl-type
          #:give fish-bowl-arrow-type)
  (people #f #f)
  (people "Can you take care of the fish in my bowl while I'm on vacation?."
          #f
          #:bob (lambda (p) (refocus (hc-append gap-size p (make-fishbowl #:color-bowl? #t)) p))
          #:want colorfish-bowl-type
          #:wide? #t)
  (list
   'alts
   (let ([mk
          (lambda (#:bob? [bob? #t] #:color-fish? [color-fish? #t])
            (define bowl (make-fishbowl #:color (if color-fish? fish-color "gray") #:color-bowl? #t))
            (people #f (if color-fish?
                           "Give me a bowl with a fish, and I'll take care of it."
                           "I like this gray fish better!")
                    #:bob (lambda (p) (refocus (hc-append gap-size (if bob? p (ghost p)) bowl) p))
                    #:want colorfish-bowl-type
                    #:give fish-bowl!-arrow-type
                    #:wide? #t))])
     (list
      (mk)
      (mk #:bob? #f)
      (mk #:bob? #f #:color-fish? #f))))
  (people #f #f #:mood 'sortof-unhappy
          #:bob (lambda (p) (refocus (hc-append gap-size p (make-fishbowl #:color "gray" #:color-bowl? #t)) p))
          #:want colorfish-bowl-type
          #:give fish-bowl!-arrow-type)))

(vs-slides update-title
           (lambda (hi-a hi-b)
             (code
              {let {[f : ?
                       {lambda {[r : #,(hi-a (code {[x : num]}))]}
                         {set r x 5}}]}
                {f #,(hi-b (code {record {x 1} {y 2}}))}}))
           (code {[x : num]})
           (code {[x : num] [y : num]})
           (record-subtype-rule #t)
           #f
           (blank)
           (blank)
           (it "Seems ok for both functional and imperative update..."))

(vs-slides update-title
           (lambda (hi-a hi-b)
             (code
              {let {[f : ?
                       {lambda {[r : #,(hi-a (code {[p : {[x : num]}]}))]}
                         {set r p {record {x 10}}}}]}
                {get
                 {get
                  {f #,(hi-b (code {record {p {record {x 5}
                                                      {y 6}}}}))}
                  p}
                 y}}))
           (code {[p : {[x : num]}]})
           (code {[p : {[x : num]
                        [y : num]}]})
           #f
           #f)

(vs-slides update-title
           (lambda (hi-a hi-b)
             (code
              {let {[f : ?
                       {lambda {[r : #,(hi-a (code {[p : {[x : num]}]}))]}
                         {set! r p {record {x 10}}}}]}
                {let {[r : ?
                         #,(hi-b (code {record {p {record {x 5}
                                                          {y 6}}}}))]}
                  {begin
                    {f r}
                    {get {get r p} y}}}}))
           (code {[p : {[x : num]}]})
           (code {[p : {[x : num]
                        [y : num]}]})
           (record-subtype-rule #t)
           (record-subtype-rule #f)
           (blank)
           (blank)
           (it "Wrong for imperative update!"))

(slide
 #:title "Invariance"
 (para #:width WIDE "With imperative update:")
 (blank)
 (blank)
 (record-subtype-rule #f)
 (blank)
 (blank)
 (para #:align 'center "Field types must be" (bit "invariant") "with record types"))
