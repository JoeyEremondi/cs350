#lang slideshow
(require slideshow/code
         slideshow/step
         scheme/class
         scheme/gui/base
         (for-syntax syntax/stx)
         texpict/symbol)

(provide pattern-slides
         simple-pattern-slides
         pattern-...-slides
         lexical-slides
         lexical-how-slides
         define-syntax-slides
         syntax-case-slides
         id-macro-slides
         macro-generating-macro-slides
         cbr-slides)

;; Unusual indentation needed:
;;   let^1
;;   let-one
;; Watch out for (#,...) in two cases

(define GreenColor "green")
(define DarkGreenColor "forest green")
(define BlueColor "blue")
(define RedColor "red")
(define BeigeColor (make-object color% 255 255 200))

(define dt bit)

(define expands (colorize sym:implies BlueColor))
(define longish (- client-w (* 2 gap-size)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc-w (* 3/4 client-w))

(define (color-box p color)
  (cc-superimpose
   (colorize
    (dc (lambda (dc x y)
          (send dc draw-rectangle x y (pict-width p) (pict-height p)))
        (pict-width p) (pict-height p) (pict-ascent p) (pict-descent p))
    color)
   p))
(define light-shade (make-object color% 195 195 255))
(define (darker c)
  (scale-color 0.95 c))

(define (itt s)
  (text s `(italic . ,(current-code-font)) (current-font-size)))

(define pattern (color-box (itt "pattern") light-shade))
(define template (color-box (itt "template") (darker light-shade)))

(define cloud-shade "lightgray") ; (make-object color% 195 255 195))
(define (encloud p shade)
  (let ([n (cc-superimpose
            (cloud (* 9/8 (pict-width p))
                   (* 3/2 (pict-height p))
                   shade)
            p)])
    (lift-above-baseline (drop-below-ascent n (- (pict-ascent p))) (- (pict-descent p)))))

(define-for-syntax (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls)
  (map (lambda (pt)
         (if (stx-pair? pt)
             (let ([tmpl (loop (stx-car (stx-cdr pt)) 
                               #`(darker (darker #,light-shade))
                               cloud-shade)]
                   [pat (stx-car pt)])
               (with-syntax ([us #'unsyntax])
                 (datum->syntax
                  pt
                  (list
                   (quasisyntax/loc pat
                     (us (color-box (code #,pat) #,light-shade)))
                   (quasisyntax/loc (stx-car (stx-cdr pt)) 
                     (us (color-box (code #,tmpl) (darker #,light-shade)))))
                  pt
                  pt)))
             (loop pt light-shade cloud-shade)))
       pat--tmpls))

;; The scode macro is like code, except that it
;;   - Shades behind patterns and templates (by recognizing keywords)
;;   - Puts clouds around expr for (code:encloud expr)
;; Much of the complexity has to do with preserving source-location
;;  info, which is crucial to proper typesetting of the code
;; >>> Some cut and paste here should be cleaned up! <<<
(define-syntax (scode stx)
  (syntax-case stx ()
    [(_ stx ...)
     #`(code
        #,@(map
            (lambda (stx)
              (let loop ([stx stx][light-shade #'light-shade][cloud-shade #'cloud-shade])
                (syntax-case stx (..... code:encloud)
                  [(ds n b)
                   (and (identifier? #'ds)
                        (free-identifier=? #'ds #'define-syntax))
                   (datum->syntax
                    stx
                    (list
                     #'ds
                     (loop #'n light-shade cloud-shade)
                     (loop #'b light-shade cloud-shade))
                    stx)]
                  [(dsr p t)
                   (and (identifier? #'dsr)
                        (free-identifier=? #'dsr #'define-syntax-rule))
                   (datum->syntax
                    stx
                    (list
                     #'dsr
                     (with-syntax ([us #'unsyntax])
                       (quasisyntax/loc #'p
                         (us (color-box (code #,(loop #'p light-shade cloud-shade)) #,light-shade))))
                     (with-syntax ([us #'unsyntax])
                       (quasisyntax/loc #'t
                         (us (color-box (code #,(loop #'t light-shade cloud-shade)) (darker #,light-shade))))))
                    stx)]
                  [(sr pat--tmpl ...)
                   (and (identifier? #'sr)
                        (or (free-identifier=? #'sr #'identifier-syntax)))
                   (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sr
                       (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls))
                      stx))]
                  [(sr kws pat--tmpl ...)
                   (and (identifier? #'sr)
                        (or (free-identifier=? #'sr #'syntax-rules)
                            (free-identifier=? #'sr #'syntax-id-rules)))
                   (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sr
                       #'kws
                       (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls))
                      stx))]
                  [(sc expr kws pat--expr ...)
                   (and (identifier? #'sc)
                        (free-identifier=? #'sc #'syntax-case))
                   (let ([pat--exprs (syntax->list #'(pat--expr ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sc
                       (loop #'expr light-shade cloud-shade)
                       #'kws
                       (map (lambda (pe)
                              (if (stx-pair? pe)
                                  (let ([expr (stx-car (stx-cdr pe))]
                                        [pat (stx-car pe)])
                                    (with-syntax ([us #'unsyntax])
                                      (datum->syntax
                                       pe
                                       (list
                                        #`(us (datum->syntax
                                               #f
                                               (color-box (code #,pat) #,light-shade)
                                               (quote-syntax #,pat)))
                                        (loop expr light-shade cloud-shade))
                                       pe
                                       pe)))
                                  (loop pe light-shade cloud-shade)))
                            pat--exprs))
                      stx))]
                  [(ws pat--expr body)
                   (and (identifier? #'ws)
                        (free-identifier=? #'ws #'with-syntax))
                   (let ([pat--exprs (syntax->list #'pat--expr)])
                     (datum->syntax
                      stx
                      (list
                       #'ws
                       (datum->syntax
                        #'pat--expr
                        (map (lambda (pe)
                               (if (stx-pair? pe)
                                   (let ([expr (stx-car (stx-cdr pe))]
                                         [pat (stx-car pe)])
                                     (with-syntax ([us #'unsyntax])
                                       (datum->syntax
                                        pe
                                        (list
                                         #`(us (datum->syntax-object
                                                #f
                                                (color-box (code #,pat) #,light-shade)
                                                (quote-syntax #,pat)))
                                         (loop expr light-shade cloud-shade))
                                        pe)))
                                   (loop pe light-shade cloud-shade)))
                             pat--exprs)
                        #'pat--expr)
                       (loop #'body light-shade cloud-shade))
                      stx))]
                  [(sx tmplt)
                   (and (identifier? #'sx)
                        (free-identifier=? #'sx #'syntax))
                   (let ([tmpl (loop #'tmplt
                                     #`(darker (darker #,light-shade))
                                     cloud-shade)])
                     (datum->syntax
                      stx
                      (list
                       #'sx
                       (with-syntax ([us #'unsyntax])
                         #`(us (datum->syntax
                                #f
                                (color-box (code #,tmpl) (darker #,light-shade))
                                (quote-syntax tmplt)))))
                      stx))]
                  [(code:encloud x)
                   (with-syntax ([us #'unsyntax])
                     (quasisyntax/loc stx
                       (us (encloud (code #,(loop #'x light-shade
                                                  #`(darker #,cloud-shade)))
                                    #,cloud-shade))))]
                  [(a . b)
                   (datum->syntax
                    stx
                    (cons (loop #'a light-shade cloud-shade) (loop #'b light-shade cloud-shade))
                    stx
                    stx)]
                  [..... 
                   (with-syntax ([us #'unsyntax])
                     (quasisyntax/loc stx
                       (us .....-val)))]
                  [dsr
                   (and (identifier? #'dsr)
                        (free-identifier=? #'dsr #'DEFINE-SYNTAX-RULE))
                   (datum->syntax stx 'define-syntax-rule stx)]
                  [x #'x])))
            (syntax->list #'(stx ...))))]))

(define (introduced id)
  (colorize (parameterize ([code-colorize-enabled #f])
              (typeset-code id))
            RedColor))

(define (expands-table . l)
  (table 3
         l
         ltl-superimpose ltl-superimpose
         gap-size (current-line-sep)))

(define swap-defn
  (scode
   (define-syntax-rule (swap a b)
     (let ([tmp b])
       (set! b a)
       (set! a tmp)))))

(define .....-val (let ([p (code .....)])
                    (refocus (cc-superimpose 
                              (ghost p)
                              (scale (cloud (pict-width p) (pict-height p)) 0.95))
                             p))) 

;; ----------------------------------------

(define (simple-pattern-slides)
  (void
   (with-steps
    (dsr dsr-expl pat-tmpl pattern-show pattern pattern-fill template-show template)
    (slide
     #:title "Simple Pattern-Based Macros"
     (lt-superimpose
      ((vbetween dsr dsr-expl)
       (scode
        (DEFINE-SYNTAX-RULE .....
          .....)))
      ((vbetween pat-tmpl pat-tmpl)
       (scode
        (define-syntax-rule #,pattern
          #,template)))
      ((vbetween pattern-show pattern-fill)
       (scode
        (define-syntax-rule (swap a b)
          .....)))
      ((vafter template-show)
       swap-defn))
     (blank)
     (ct-superimpose
      ((vbetween dsr-expl dsr-expl)
       (item (code define-syntax-rule) "indicates a simple-pattern macro definition"))
      ((vbetween pat-tmpl pat-tmpl)
       (vc-append
        gap-size
        (item "A" pattern "to match")
        (item "Produce result from" template)))
      ((vbetween pattern pattern-fill)
       (vc-append
        gap-size
        (item "Pattern for this macro:" (code (swap a b)))        
        ((vbetween pattern-fill pattern-fill)
         (vc-append
          gap-size
          (item "Each pattern identifier matches anything")
          (expands-table
           (code (swap x y))
           expands
           (para #:fill? #f (code a) (t "is") (code x))
           (blank)(blank)
           (para #:fill? #f (code b) (t "is") (code y))
           (tt " ")(blank)(blank)
           (code (swap 9 (+ 1 7)))
           expands
           (para #:fill? #f (code a) (t "is") (code 9))
           (blank)(blank)
           (para #:fill? #f (code b) (t "is") (code (+ 1 7))))))))
      ((vbetween template template)
       (vc-append
        gap-size
        (para #:width longish
              "Matches substituted into template to generate the result")
        (expands-table
         (code (swap x y))
         expands
         (code (let ([tmp y])
                 (set! y x)
                 (set! x tmp)))
         (tt " ")(blank)(blank)
         (code (swap 9 (+ 1 7)))
         expands
         (code (let ([tmp (+ 1 7)])
                 (set! (+ 1 7) 9)
                 (set! 9 tmp)))))))))))

;; ----------------------------------------

(define (pattern-slides)
  (void
   (with-steps
    (shift ds rs pat-tmpl pattern template)
    (slide
     #:title "General Pattern-Based Macros"
     (lt-superimpose
      ((vbetween shift ds)
       (scode
        (define-syntax shift
          .....)))
      ((vbetween rs rs)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            .....))))
      ((vbetween pat-tmpl pat-tmpl)
       (code
        (define-syntax shift
          (syntax-rules (back)
            [#,pattern #,template]
            ...
            [#,pattern #,template]))))
      ((vbetween pattern pattern)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            [(shift a b c) .....]
            [(shift back a b c) .....]))))
      ((vafter template)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            [(shift a b c) (begin
                             (swap a b)
                             (swap b c))]
            [(shift back a b c) (begin
                                  (swap c b)
                                  (swap b a))])))))
     (blank)
     (ct-superimpose
      ((vbetween shift shift)
       (htl-append
        (* 3 gap-size)
        (code
         (let ([x 0]
               [y 1]
               [z 2])
           (shift x y z)))
        (code
         (let ([x 0]
               [y 1]
               [z 2])
           (shift back x y z)))))
      ((vbetween ds ds)
       (item (code define-syntax) "indicates a macro definition"))
      ((vbetween rs rs)
       (vc-append
        gap-size
        (item (code syntax-rules) "means a pattern-matching macro")
        (item (code (back)) "means that" (code back) "is literal in patterns")))
      ((vbetween pat-tmpl pat-tmpl)
       (vc-append
        gap-size
        (item "Any number of" (hbl-append pattern (t "s")) "to match")
        (item "Produce result from" template "of first match")))
      ((vbetween pattern pattern)
       (vc-append
        gap-size
        (para "Two patterns for this macro")
        (item (code (shift x y z)) "matches first pattern")
        (item (code (shift back x y z)) "matches second pattern")
        (item (code (shift rev x y z)) "does not match")))
      ((vbetween template template)
        (expands-table
         (code (shift x y z))
         expands
         (code (begin
                 (swap x y)
                 (swap y z)))
         (tt " ")(blank)(blank)
         (code (shift back x y z))
         expands
         (code (begin
                 (swap z y)
                 (swap y x))))))))))
  
;; ----------------------------------------

(define (lexical-slides)
  (void
   (with-steps
    (setup setupx lexical)
    (slide
     #:title "Macro Scope"
     swap-defn
     (blank)
     (vc-append
      gap-size
      (para #:width longish "What if we" (code swap) "a variable named" (code tmp) "?")
      (expands-table
       (code (let ([tmp 5]
                   [other 6])
               (swap tmp other)))
       (cc-superimpose
        ((vbetween setup setupx)
         (vc-append (- (current-line-sep)) (colorize (t "?") RedColor) expands))
        ((vafter lexical)
         expands))
       (lc-superimpose
        ((if (between? setup setupx) values ghost)
         (code (let ([tmp 5]
                     [other 6])
                 (let ([tmp other])
                   (set! other tmp)
                   (set! tmp tmp)))))
        ((if (between? setup setupx) ghost values)
         (code (let ([tmp 5]
                     [other 6])
                 (let ([tmp_1 other])
                   (set! other tmp)
                   (set! tmp tmp_1))))))))
     (lt-superimpose
      ((vbetween setupx setupx)
       (colorize (para #:fill? #t (it "This expansion would break scope")) RedColor))
      ((vafter lexical)
       (colorize (para #:fill? #t (bit "Hygienic macros") "rename the introduced binding") BlueColor))))))
  
  (slide
   #:title
   "Hygienic Macros: Local Bindings"
   (para #:width longish "Hygiene means that local macros work, too:")
   (scode (define (f x)
            (define-syntax swap-with-arg
              (syntax-rules ()
                [(swap-with-arg y) (swap x y)]))
            (code:encloud
             (code:line
              code:blank
              (let ([z 12]
                    [x 10])
                (code:comment "Swaps z with original x:")
                (swap-with-arg z))
              code:blank))))))

;; ----------------------------------------

(define (pattern-...-slides)

  (define seq-title "Matching Sequences")

  (slide
   #:title seq-title
   (para "Some macros need to match sequences")
   (vl-append
    gap-size
    (code (rotate x y))
    (code (rotate red green blue))
    (code (rotate front-left
                  rear-right
                  front-right
                  rear-left))))
  
  (slide
   #:title seq-title
   (scode (define-syntax rotate
            (syntax-rules ()
              [(rotate a) (void)]
              [(rotate a b c ...) (begin
                                    (swap a b)
                                    (rotate b c ...))])))
   (blank)
   (item (code ...) "in a pattern: 0 or more of previous sub-pattern")
   (table 3
	  (list (code (rotate x y z w))
		expands
		(para #:fill? #f (code c) "is" (code z w)))
	  ltl-superimpose ltl-superimpose
	  gap-size (current-line-sep))
   'next
   (item (code ...) "in a template: 0 or more of previous sub-template")
   (table 3
	  (list (code (rotate x y z w))
		expands
		(code (begin
			(swap x y)
			(rotate y z w))))
	  ltl-superimpose ltl-superimpose
	  gap-size (current-line-sep)))

  (slide
   #:title seq-title
   (scode (define-syntax rotate
            (syntax-rules ()
              [(rotate a c ...) 
               (shift-to (c ... a) (a c ...))]))
          code:blank
          (define-syntax shift-to
            (syntax-rules ()
              [(shift-to (to0 to ...) (from0 from ...)) 
               (let ([tmp from0])
                 (set! to from) ...
                 (set! to0 tmp))])))
   (blank)
   (item (code ...) "maps over same-sized sequences")
   (item (code ...) "duplicates constants paired with sequences")))

;; ----------------------------------------

(define (define-syntax-slides)
  (slide
   #:title
   "Transformer Definitions"
   (para #:width longish
         "In general," (code define-syntax) "binds a transformer procedure:")
   (blank)
   (vl-append
    gap-size
    (scode (define-syntax swap
             (syntax-rules .....)))
    expands
    (scode (define-syntax swap
             (lambda (stx)
               (code:encloud #,(vc-append
                                (para #:fill? #f 
                                      "use syntax-object primitives to")
                                (para #:fill? #f
                                      (t "match") (code stx)
                                      (t "and generate result"))))))))))

;; ----------------------------------------

(define (syntax-case-slides)
  
  (define syntax-case-title "Matching Syntax and Having It, Too")
  
  (slide
   #:title syntax-case-title
   (para #:width longish
         (code syntax-case) "and" (colorize (tt "#'") keyword-color)
         "combine patterns and computation")
   (vl-append
    gap-size
    (scode (syntax-case _stx-expr ()
             [_pattern _result-expr]
             ...
             [_pattern _result-expr]))
    (blank)
    (scode #'template)))
  
  (slide
   #:title syntax-case-title
   (vl-append
    gap-size
    swap-defn
    expands
    (scode (define-syntax swap
             (lambda (stx)
               (syntax-case stx ()
                 [(swap_1 a b) #'(let ([tmp b])
                                   (set! b a)
                                   (set! a tmp))]))))))
  
  (slide
   #:title syntax-case-title
   (para #:width longish "Check for identifiers before expanding:")
   (scode (define-syntax swap
            (lambda (stx)
              (syntax-case stx ()
                [(swap a b) 
                 (if (and (identifier? #'a) 
                          (identifier? #'b))
                     #'(let ([tmp b])
                         (set! b a)
                         (set! a tmp))
                     (raise-syntax-error
                      'swap "needs identifiers" 
                      stx))]))))))

;; ----------------------------------------

(define how-it-works-title "How Hygiene Works")
  
(define (lexical-how-slides)
  
  (define roughly-expands expands)
  
  (with-steps
   (obvious one can cannot explain hygiene reftrans)
   (slide
    #:title how-it-works-title
    (lt-superimpose
     ((vbefore one) swap-defn)
     ((vbetween one explain)
      (scode
       (define-syntax-rule (swap a b)
         (let-one [tmp b]
           (set! b a)
           (set! a tmp)))))
     ((vafter hygiene) swap-defn))
    (blank)
    (ct-superimpose
     ((vbefore hygiene)
      (vc-append
       gap-size
       (cc-superimpose
        ((vonly obvious)
         (para #:width longish 
               "Seems obvious that" (code tmp) "can be renamed..."))
        ((vonly can)
         (para #:width longish "Can rename" (code tmp) ":"))
        ((vafter cannot)
         (para #:width longish (colorize (it "Cannot") RedColor) "rename" (code tmp) ":")))
       (lt-superimpose
        ((vonly can)
         (code (define-syntax-rule (let-one (x v) body)
                 (let ([x v]) body))))
        ((vafter cannot)
         (code (define-syntax-rule (let-one (x v) body)
                 (list 'x v body)))))
       (blank)
       ((vafter explain)
        (colorize (para #:align 'center
                        "Track identifier introductions,"
                        "then rename only as binding forms are discovered")
                  BlueColor))))
     ((vafter hygiene)
      (ct-superimpose
       ((vbetween hygiene hygiene)
        (vc-append
         gap-size
         (para #:width longish
               "Tracking avoids capture by introduced variables")
         (expands-table
          (code (let ([tmp 5]
                      [other 6])
                  (swap tmp other)))
          roughly-expands
          (code (let ([tmp 5]
                      [other 6])
                  (let^1 ([tmp^1 other])
                    (set!^1 other tmp)
                    (set!^1 tmp tmp^1)))))
         (para #:fill? #f
               (typeset-code (datum->syntax #f (string->symbol "\240^1")))
               "means introduced by expansion")
         (para #:fill? #f (code tmp^1) "does not capture" (code tmp))))
       ((vbetween reftrans reftrans)
        (vc-append
         gap-size
         (para #:width longish
               "Tracking also avoids capture" (it "of") "introduced variables")
         (expands-table
          (code (let ([set! 5]
                      [let 6])
                  (swap set! let)))
          roughly-expands
          (code (let ([set! 5]
                      [let 6])
                  (let^1 ([tmp^1 let])
                    (set!^1 let set!)
                    (set!^1 set! tmp^1)))))
         (para #:fill? #f (code set!) "does not capture" (code set!^1))
         (para #:fill? #f (code let) "does not capture" (code let^1)))))))))

  (void))

;; ----------------------------------------

(define (lexical-detail-slides)  

  (define-syntax pscale (syntax-rules () [(_ e) (scale/improve-new-text e 0.9 0.9)]))
  
  (define precise-how-it-works-title
    (string-append how-it-works-title " (More Precisely)"))
  
  (define (*bright-subsup show id sub mode -superimpose)
    (hbl-append id (let ([p (text (number->string sub)
                                  `(,mode . ,(current-code-font))
                                  (current-font-size))])
                     (refocus (-superimpose
                               (show 
                                (colorize (filled-rectangle (pict-width p) (* (pict-height p) 2/3))
                                          "pink"))
                               p)
                              p))))
  (define-syntax-rule (bright-sub show id sub)
    (*bright-subsup show (code id) sub 'subscript cb-superimpose))
  (define-syntax-rule (bright-sup show id sup)
    (*bright-subsup show (code id) sup 'superscript ct-superimpose))
  
  (with-steps
   (let added added-more continue last-name summary)
   (slide
    #:title precise-how-it-works-title
    #:layout 'top
    (para
     #:width longish
     (htl-append
      gap-size
      (pscale
       (code (let ([tmp 5]
                   [other 6])
               (swap tmp other))))
      ((vafter added) expands)
      ((vafter added)
       (pscale
        (code (let ([#,(bright-sub (vbetween added added-more) tmp 0) 5]
                    [#,(bright-sub (vbetween added added-more) other 0) 6])
                (swap #,(bright-sub (vbetween added added-more) tmp 0) #,(bright-sub (vbetween added added-more) other 0))))))))
    (blank)
    ((vafter continue) 
     (para
      #:width longish
      (htl-append 
       gap-size
       expands
       (pscale
        (code (let ([tmp_0 5]
                    [other_0 6])
                (#,(bright-sup (vonly continue) let 1) ([#,(bright-sup (vbetween continue last-name) tmp 1) other_0])
                 (#,(bright-sup (vonly continue) set! 1) other_0 tmp_0)
                 (#,(bright-sup (vonly continue) set! 1) tmp_0 #,(bright-sup (vbetween continue last-name) tmp 1))))))
       ((vafter last-name) expands)
       ((vafter last-name)
        (pscale
         (code (let ([tmp_0 5]
                     [other_0 6])
                 (let^1 ([#,(bright-sub (vonly last-name) tmp 2) other_0])
                   (set!^1 other_0 tmp_0)
                   (set!^1 tmp_0 #,(bright-sub (vonly last-name) tmp 2))))))))))
    (blank)
    (blank)
    (lt-superimpose
     ((vbetween added added-more)
      (vc-append
       gap-size
       (para #:width longish
             "To parse" (code let)
             ", rename bindings by adding a subscript")
       ((vbetween added-more added-more)
        (vc-append
         gap-size
         (para #:width longish
               "To parse" (code quote) ", drop the subscript")
         (htl-append gap-size
                     (code (let ([x 1])
                             'x))
                     expands
                     (code (let ([x_1 1])
                             'x_1))
                     expands
                     (code (let ([x_1 1])
                             'x)))))))
     ((vbetween continue continue)
      (para #:width longish
            "Mark superscripts on introduced identifiers"))
     ((vbetween last-name last-name)
      (para #:width longish
            "Rename for" (code let)
            "--- but only where superscript marks match"))
     ((vbetween summary summary)
      (vc-append
       gap-size
       (item "Introductions" (dt "marked") "with fresh superscript")
       (item "Matching marks" (dt "renamed") "with fresh subscript")
       (para #:width longish " "))))))
  
  ;; >>>>>>>> CUT and PASTE from previous <<<<<<
  ;;  The problem is that it's really difficult to abstract
  ;;  when the source location matters....
  (with-steps
   (let added continue last-name)
   (slide
    #:title precise-how-it-works-title
    #:layout 'top
    (para
     #:width longish
     (htl-append
      gap-size
      (pscale
       (code (let ([set! 5]
                   [let 6])
               (swap set! let))))
      ((vafter added) expands)
      ((vafter added)
       (pscale
        (code (let ([#,(bright-sub (vbetween added added) let 0) 5]
                    [#,(bright-sub (vbetween added added) set! 0) 6])
                (swap #,(bright-sub (vbetween added added) let 0) #,(bright-sub (vbetween added added) set! 0))))))))
    (blank)
    ((vafter continue) 
     (para
      #:width longish
      (htl-append 
       gap-size
       expands
       (pscale
        (code (let ([let_0 5]
                    [set_0 6])
                (#,(bright-sup (vonly continue) let 1) ([#,(bright-sup (vbetween continue last-name) tmp 1) set!_0])
                 (#,(bright-sup (vonly continue) set! 1) let_0 set!_0)
                 (#,(bright-sup (vonly continue) set! 1) let_0 #,(bright-sup (vbetween continue last-name) tmp 1))))))
       ((vafter last-name) expands)
       ((vafter last-name)
        (pscale
         (code (let ([set!_0 5]
                     [let_0 6])
                 (let^1 ([#,(bright-sub (vonly last-name) tmp 2) let_0])
                   (set!^1 let_0 set!_0)
                   (set!^1 set!_0 #,(bright-sub (vonly last-name) tmp 2))))))))))
    (blank)
    (blank)
    (lt-superimpose
     ((vbetween last-name last-name)
      (para #:width longish "Superscript mark is" (bt "not") "a rename:"
            (code let^1) "refers to" (code let))))))
  
  (void))

;; --------------------------------------------------

(define (id-macro-slides)
  (slide
   #:title "Identifier Macros"
   (para #:width longish
         "The" (code swap) "and" (code rotate) "names work only"
         "in an ``application'' position")
   (expands-table
    (scode (swap x y)) expands (scode (let ([tmp y]) .....))
    (scode (+ swap 2)) expands (colorize (it "syntax error") RedColor))
   'next
   (blank)
   (blank)
   (para #:width longish
         "An" (dt "identifier macro") "works in any expression position")
   (expands-table
    (code clock) expands (code (get-clock))
    (code (+ clock 10)) expands (code (+ (get-clock) 10))
    (code (clock 5)) expands (code ((get-clock) 5)))
   'next
   (para #:width longish
         "...or as a" (code set!) "target")
   (expands-table
    (code (set! clock 10)) expands (code (set-clock! 10))))

  (slide
   #:title "Identifier Macros"
   (para #:width longish
         "Using" (code syntax-id-rules) ":")
   (scode (define-syntax clock
            (syntax-id-rules (set!)
              [(set! clock e) (put-clock! e)]
              [(clock a ...) ((get-clock) a ...)]
              [clock (get-clock)])))
   'next
   (blank)   
   (item #:width longish
         (code set!) "is designated as a literal")
   (item #:width longish
         (code syntax-rules) "is a special case of" (code syntax-id-rules)
         "with errors in the first and third cases")))

;; ----------------------------------------

(define define-get/put-id-defn
  (scode
   (define-syntax define-get/put-id
     (syntax-rules ()
       [(define-get/put-id id get put!)
        (define-syntax id
          (syntax-id-rules (set!)
           [(set! id e) (put! e)]
           [(id a (... ...)) ((get) a (... ...))]
           [id (get)]))]))))

(define (macro-generating-macro-slides)
  (slide
   #:title "Macro-Generating Macros"
   (para #:width longish
         "If we have many identifiers like" (code clock) "...")
   (blank)
   'next
   (scale
    (vl-append
     define-get/put-id-defn
     (scode code:blank
            (define-get/put-id clock get-clock put-clock!)))
    0.9)
   (blank)
   (para #:width longish
         "where" (color-box (code (... ...)) light-shade)
         "in a template gets replaced by" (color-box (code ...) light-shade))))

;; ----------------------------------------

(define (cbr-slides)
  (define cbr-code
    (code (define-cbr (f a b)
            (swap a b))
          code:blank
          (let ([x 1] [y 2])
            (f x y)
            x)))
   
  (slide
   #:title "Extended Example"
   (para #:width longish
         "Let's add call-by-reference definitions to Racket")
   (blank)
   (blank)
   (vl-append
    (current-line-sep)
    cbr-code
    (code (code:comment "should produce 2"))))
  
  (define cbr-swap-expansion
    (code (define (do-f get-a get-b put-a! put-b!)
            (define-get/put-id a get-a put-a!)
            (define-get/put-id b get-b put-b!)
            (swap a b))))
  
  (slide
   #:title "Extended Example"
   (para #:width longish
         "Expansion of first half:")
   (blank)
   (vl-append
    gap-size
    (code (define-cbr (f a b)
            (swap a b)))
    expands
    cbr-swap-expansion))
  
  (slide
   #:title "Extended Example"
   (para #:width longish "Expansion of second half:")
   (blank)
   (vl-append
    gap-size
    (code (let ([x 1] [y 2])
            (f x y)
            x))
    expands
    (code (let ([x 1] [y 2])
            (do-f (lambda () x) 
                  (lambda () y)
                  (lambda (v) (set! x v)) 
                  (lambda (v) (set! y v)))
            x))))
           
  (define define-cbr-defn
    (scode 
     (define-syntax define-cbr
       (syntax-rules ()
	 [(_ (id arg ...) body)
	  (begin
	    (define-for-cbr do-f (arg ...)
	      () body)
	    (define-syntax id
	      (syntax-rules ()
		[(id actual (... ...))
		 (do-f (lambda () actual) 
		       (... ...)
		       (lambda (v) 
			 (set! actual v))
		       (... ...))])))]))))
  
  (slide
   #:title "Call-by-Reference Setup"
   (para #:width longish
         "How the first half triggers the second half:")
   define-cbr-defn)

  (slide
   #:title "Call-by-Reference Body"
   (para #:width longish
         "Remaining expansion to define:")
   (blank)
   (vl-append
    gap-size
    (code (define-for-cbr do-f (a b)
            () (swap a b)))
    expands
    cbr-swap-expansion)
   'next
   (blank)
   (colorize (para #:fill? #f
                   #:width longish
                   "How can" (code define-for-cbr) "make" (code get-) "and" (code put-!) "names?")
             BlueColor))
         
  (define define-for-cbr-defn
   (scode (define-syntax define-for-cbr
            (syntax-rules ()
              [(define-for-cbr do-f (id0 id ...)
                 (gens ...) body)
               (define-for-cbr do-f (id ...) 
                 (gens ... (id0 get put)) body)]
              [(define-for-cbr do-f ()
                 ((id get put) ...) body)
               (define (do-f get ... put ...)
                 (define-get/put-id id get put) ...
                 body)]))))
  
  (slide
   #:title "Call-by-Reference Body"
   (para #:width longish
         "A name-generation trick:")
   (blank)
   define-for-cbr-defn)

  (slide
   #:title "Call-by-Reference Body"
   (para #:width longish
         "More accurate description of the expansion:")
   (blank)
   (vl-append
    gap-size
    (code (define-for-cbr do-f (a b)
            () (swap a b)))
    expands
    (code (define (do-f get^1 get^2 put^1 put^2)
            (define-get/put-id a get^1 put^1)
            (define-get/put-id b get^2 put^2)
            (swap a b)))))
  
  (slide
   #:title "Complete Code to Add Call-By-Reference"
   (ht-append
    gap-size
    (vl-append
     gap-size
     (scale define-cbr-defn 0.5 0.5)
     (scale define-for-cbr-defn 0.5 0.5))
    (scale define-get/put-id-defn 0.5 0.5))))
