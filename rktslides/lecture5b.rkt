#lang slideshow
(require slideshow/code
         slideshow/play
         slideshow/balloon
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss")

(define (align . l) (apply para #:width (* 0.95 (get-client-w #:aspect 'fullscreen)) l))
(define WIDE (* 0.9 (get-client-w #:aspect 'fullscreen)))

(fullscreen-para-width!)

;; ------------------------------------------------------------
(part 1)

(slide
 #:title "Records"
 #:layout 'top
 (blank)
 (colorize (para "Literal objects in JavaScript:") "blue")
 (blank)
 (tt* "var o = { x : 1,  y : 1+1 }"
      " "
      "o.x  ⇒ 1"
      "o.y  ⇒ 2"))

(slide
 #:title "Record Update"
 #:layout 'top
 (blank)
 (colorize (para "Field update in JavaScript:") "blue")
 (blank)
 (tt* "var o = { x : 1,  y : 1+1 }"
      " "
      "o.x = 5"
      "o.x  ⇒ 5")
 'next
 (blank)
 (blank)
 (para "This kind of update involves" (bit "state")))

(slide
 #:title "Record Functional Update"
 #:layout 'top
 (blank)
 (colorize (para "Field update" (it "different") "from JavaScript:") "blue")
 (blank)
 (tt* "var o = { x : 1,  y : 1+1 }"
      "var p = (o.x = 5)"
      " "
      "o.x  ⇒ 1"
      "p.x  ⇒ 5"
      "p.y  ⇒ 2")
  'next
  (blank)
  (blank)
  (para "This approach is" (bit "functional update"))
  (para "We'll implement functional update first for Curly"))

(slide
 #:title "Records"
 'alts
 (list
  (list
   (tt* "{ x : 1,  y : 1+1 }")
   'next
   (blank)
   (blank)
   (code {record {x 1}
                 {y {+ 1 1}}}))
  (list
   (tt* "var o = { x : 1,  y : 1+1 }"
        "....")
   'next
   (blank)
   (blank)
   (code {let {[o {record {x 1}
                          {y {+ 1 1}}}]}
           ....}))
  (list
   (tt* ""
        "o.x")
   'next
   (blank)
   (blank)
   (code code:blank
         code:blank
         {get o x}))
  (list
   (tt* "var o = { x : 1,  y : 1+1 }"
        "o.x")
   'next
   (blank)
   (blank)
   (code {let {[o {record {x 1}
                          {y {+ 1 1}}}]}
           {get o x}}))
  (list
   (tt* "(o.x = 5)")
  'next
   (blank)
   (blank)
   (code {set o x 5}))))

(slide
 #:title "Functional Record Update"
 (para
  (code {let {[r1 {record {a 2}
                          {b 4}}]}
          {let {[r2 {set r1 a 5}]}
            {+ {get r1 a} 
               {get r2 a}}}}))
 'next
 (para sym:implies (code 7))
 'next
 (blank)
 (para (code set) "creates a new record with the new field value"))

;; ------------------------------------------------------------
(part 2)

(slide
 #:title "Records"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code #,(nonterm "Symbol")) (blank)
        (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {record {#,(nonterm "Symbol") #,(nonterm "Exp")} ...}) new-label
        (blank)         -or- (code {get #,(nonterm "Exp") #,(nonterm "Symbol")}) new-label
        (blank)         -or- (code {set #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Exp")}) new-label)))

(slide
 #:title "Record Programs"
 'alts
 (list
  (list
   (para
    (code {let {[r {record {x 5}
                           {y 2}}]}
            {get r x}}
          code:blank
          #,sym:implies 5)))
  (list
   (para
    (code {let {[r {record {x 5}
                           {y 2}}]}
            {get r y}}
          code:blank
          #,sym:implies 2)))
  (list
   (para
    (code {let {[r {record {x 5}
                           {y {+ 1 1}}}]}
            {get r y}}
          code:blank
          #,sym:implies 2)))
  (list
   (para
    (code {let {[mk {lambda {v}
                      {record {x {+ v 1}}
                              {y {+ v 2}}}}]}
            {get {mk 2} x}}
          code:blank
          #,sym:implies 3)))
  (list
   (para
    (code {get {record {x 1}
                       {y 2}}
               x}
          code:blank
          #,sym:implies 1)))
  (list
   (para
    (code {record {x 1}
                  {y 2}}
          code:blank
          #,sym:implies #,(t "... a record ..."))))
  (list
   (para
    (code {set {record {x 1}
                       {y 2}}
               x
               5}
          code:blank
          #,sym:implies #,(para #:fill? #f "... a record with" (code x) "as" (code 5) "..."))))))

(slide
 #:title "Record Expressions & Values"
 (scode
  (define-type Exp
    ....
    (recordE [ns : (Listof Symbol)]
             [args : (Listof Exp)])
    (getE [rec : Exp]
          [n : Symbol])
    (setE [rec : Exp]
          [n : Symbol]
          [val : Exp]))
  code:blank
  (define-type Value
    (numV [n : Number])
    (closV [arg : Symbol]
           [body : Exp]
           [env : Env])
    (recV [ns : (Listof Symbol)]
          [vs : (Listof Value)]))))

;; ----------------------------------------
(part 3)

(slide
 #:title "Parsing Records"
 (scode
  (define (parse [s : S-Exp]) : Exp
    (cond
     ....
     [(s-exp-match? `{record {SYMBOL ANY} ...} s)
      (recordE (map (lambda (l) 
                      (s-exp->symbol
                       (first (s-exp->list l))))
                    (rest (s-exp->list s)))
               (map (lambda (l) 
                      (parse
                       (second (s-exp->list l))))
                    (rest (s-exp->list s))))]
     ....))))

(define (rinterp p #:suffix [suffix #f] #:scale [s 0.85])
  (list
   (align
    (scale
     (let ([c (code (define (interp [a : Exp] [env : Env]) : Value
                      (type-case Exp a
                        ...
                        #,p
                        ...)))])
       (if suffix
           (code #,c
                 #,suffix)
           c))
     s))))


(define rimpl-title (hbl-append (code interp) (titlet " for Records")))

(define find-suffix
  (code code:blank
        code:blank
        find : (Symbol (Listof Symbol) (Listof Value)
                       -> Value)))

(slide
 #:title rimpl-title
 #:layout 'top
 'alts~
 (list
  (rinterp
   (code
    [(recordE ns vs)
     ...]))
  (rinterp
   (code
    [(recordE ns vs)
     ... ns ...
     ... (map (lambda (v) (interp v env))
              vs) ...]))
  (rinterp
   (code
    [(recordE ns vs)
     (recV ns
           (map (lambda (v) (interp v env))
                vs))]))
  
  (rinterp
   (code
    [(getE r n)
     ...]))
  (rinterp
   (code
    [(getE r n)
     ... (interp r env)
     ... n ...]))
  (rinterp
   (code
    [(getE r n)
     (type-case Value (interp r env)
       [(recV ns vs)
        ... n ...]
       [(else error 'interp "not a record")])]))
  (rinterp
   (code
    [(getE r n)
     (type-case Value (interp r env)
       [(recV ns vs)
        (find n ns vs)]
       [(else error 'interp "not a record")])])
   #:suffix
   find-suffix)

  (rinterp
   (code
    [(setE r n v)
     ...]))
  (rinterp
   (code
    [(setE r n v)
     ... (interp r env)
     ... n
     ... (interp v env) ...]))
  (rinterp
   (code
    [(setE r n v)
     (type-case Value (interp r env)
       [(recV ns vs)
        ... n
        ... (interp v env) ...]
       [(else error 'interp "not a record")])]))
  (rinterp
   (code
    [(setE r n v)
     (type-case Value (interp r env)
       [(recV ns vs)
        ... (update n
                    (interp v env)
                    ns
                    vs) ...]
       [(else error 'interp "not a record")])])
   #:suffix
   (code code:blank
         code:blank
         update : (Symbol Value (Listof Symbol) (Listof Value) 
                          -> (Listof Value))))
  (rinterp
   (code
    [(setE r n v)
     (type-case Value (interp r env)
       [(recV ns vs)
        (recV ns
              (update n
                      (interp v env)
                      ns
                      vs))]
       [(else error 'interp "not a record")])]))))

(slide
 #:title "Updating a Record"
 (code
  (define (update [n : Symbol]
                  [v : Value]
                  [ns : (Listof Symbol)]
                  [vs : (Listof Value)]) : (Listof Value)
    (cond
      [(empty? ns) (error 'interp "no such field")]
      [else (if (symbol=? n (first ns))
                (cons v (rest vs))
                (cons (first vs) 
                      (update n v (rest ns) (rest vs))))]))))

;; ----------------------------------------
(part 4)

(define FULL (* 0.8 client-w))

(slide
 #:title "Imperative Record Update"
 (tt* "var o = { x : 1,  y : 1+1 }"
      "o.x = 5"
      " "
      "o.x  ⇒ 5")
 'next
 (blank)
 (blank)
 (vl-append
  gap-size
  (para #:width FULL #:fill? #f
        "Creating a JavaScript object allocates memory for each of its fields")
  (para #:width FULL #:fill? #f
        "Field assignment updates memory")))

(slide
 #:title "Imperative Record Update"
 (para
  (code {let {[r1 {record {a {+ 1 1}}
                          {b {+ 2 2}}}]}
          {begin
            {set! r1 a 5}
            {get r1 a}}}))
 'next
 (para sym:implies (code 5))
 'next
 (blank)
 (vl-append
  gap-size
  (para #:width FULL #:fill? #f
        "Creating a record must allocate memory for each of its fields")
  (para #:width FULL #:fill? #f
        "Curly's new" (code set!) "modifies a field's memory, instead of creating a new object")))

(slide
 #:title "Records with Allocated Fields via Boxes"
 (code
  (define-type Value
    ....
    (recV [ns : (Listof Symbol)]
          [vs : (Listof (Boxof Value))]))))

(define rimpl-mut-title (hbl-append (code interp) (titlet " for Mutable Records")))

(define new-find-suffix
  (code code:blank
        code:blank
        find : (Symbol (Listof Symbol) (Listof (Boxof Value))
                       -> (Boxof Value))))

(slide
 #:title rimpl-mut-title
 'alts
 (list
  (rinterp
   (code
    [(recordE ns vs)
     (recV ns
           (map (lambda (v) (interp v env))
                vs))]))
  (rinterp
   (code
    [(recordE ns vs)
     (recV ns
           (map (lambda (v) (box (interp v env)))
                vs))]))
  (rinterp
   (code
    [(getE r n)
     (type-case Value (interp r env)
       [(recV ns vs)
        (find n ns vs)]
       [(else error 'interp "not a record")])])
   #:suffix find-suffix)
  (rinterp
   (code
    [(getE r n)
     (type-case Value (interp r env)
       [(recV ns vs)
        (unbox (find n ns vs))]
       [(else error 'interp "not a record")])])
   #:suffix new-find-suffix)
  (rinterp
   (code
    [(setE r n v)
     (type-case Value (interp r env)
       [(recV ns vs)
        .... (find n ns vs) ....]
       [(else error 'interp "not a record")])])
   #:suffix new-find-suffix)
  (rinterp
   (code
    [(setE r n v)
     (type-case Value (interp r env)
       [(recV ns vs)
        (set-box! (find n ns vs) (interp v env))]
       [(else error 'interp "not a record")])])
   #:suffix new-find-suffix)))

#;
(slide
 #:title "Imperative Update Result"
 (para #:width WIDE "Let's specify the result of" (code set!) "to be the field's new value")
 (blank)
 (para
  (code {let {[r1 {record {a {+ 1 1}}
                          {b {+ 2 2}}}]}
          {let {[v {set! r1 a 5}]}
            {+ v
               {get r1 a}}}}))
 'next
 (para sym:implies (code 10))
 (blank)
 (colorize (para #:align 'right "This is an arbitrary choice, but a common one")
           "blue"))

(slide
 #:title rimpl-mut-title
 'alts
 (list
  (rinterp
   (code
    [(setE r n v)
     (type-case Value (interp r env)
       [(recV ns vs)
        (let ([f (interp v env)])
          (begin
            (set-box! (find n ns vs) f)
            f))]
       [(else error 'interp "not a record")])])
   #:suffix (code code:blank))

  (rinterp
   (code
    [(recordE ns vs)
     (recV ns
           (map (lambda (v) (box (interp v env)))
                vs))]))
  (rinterp
   (let ([mapx (let ([mapx (code map)])
                 (refocus
                  (pin-balloon (wrap-balloon (t "Won't work with a store!")
                                             'sw 0 gap-size)
                               mapx
                               mapx ct-find)
                  mapx))])
     (code
      [(recordE ns vs)
       (recV ns
             (#,mapx (lambda (v) (box (interp v env)))
              #||#   vs))])))))


;; ----------------------------------------

(part 5)

(define terminology-title "API Terminology")

(define =eqls (inset (t "=") gap-size 0))

(let ([> (inset (tt ">") 0 gap-size 0 0)])
  (slide
   #:title terminology-title
   'alts
   (list
    (list
     (para (bit "Imperative update") =eqls (bit "Mutable datatype"))
     'next
     (blank)
     (code
      #,> (define ht
            (make-hash (list (values 'a 1)
                             (values 'b 2))))
      #,> (hash-ref ht 'a)
      (some 1)
      #,> (hash-set! ht 'a 3)
      #,> (hash-ref ht 'a)
      (some 3)))
    (list
     (para (bit "Functional update") =eqls (bit "Persistent datatype"))
     'next
     (blank)
     (code
      #,> (define ht
            (hash (list (values 'a 1)
                        (values 'b 2))))
      #,> (hash-ref ht 'a)
      (some 1)
      #,> (define ht2 (hash-set ht 'a 3))
      #,> (hash-ref ht2 'a)
      (some 3)
      #,> (hash-ref ht 'a)
      (some 1))))))
