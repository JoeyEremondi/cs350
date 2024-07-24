#lang slideshow
(require slideshow/code
         slideshow/balloon
         scheme/class
         scheme/gui/base
         scheme/math
         "utils/utils.rkt"
         "utils/htdp.rkt")

(fullscreen-para-width!)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 (titlet "How to Design Programs")
 (t "using Plait")
 (blank)
 (scale (bitmap "htdp-2e-cover.gif") 0.75)
 (blank)
 (para #:align 'center (tt "http://www.htdp.org")))

(define (keyword s) (colorize (bt s) "brown"))

(define (here which if-which p)
  (if (eq? which if-which)
      (refocus (htl-append
                gap-size
                (colorize (arrow gap-size 0) "forestgreen")
                p)
               p)
      p))

(define (design-slide which)
  (slide
   #:title "How to Design Programs"
   (here which 'representation
         (item "Determine the" (keyword "representation")))
   (subitem (code define-type) ", if needed")
   (here which 'examples
         (item "Write" (keyword "examples")))
   (subitem (code test))
   (here which 'template
         (item "Create a" (keyword "template") "for the implementation"))
   (vl-append
    (current-line-sep)
    (subitem (code type-case) ", if variants")
    (subitem "extract field values, if any")
    (subitem "cross- and self-calls, if data references"))
   (here which 'body
         (item "Finish" (keyword "body") "implementation case-by-case"))
   (here which 'test
         (item "Run" (keyword "tests")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(design-slide #f)

(define cookies-rep
  (para (code Number
              code:blank
              eat-cookie : (Number -> Number))))

(define posn-rep
  (para (code (define-type Posn
                (posn [x : Number] 
                      [y : Number]))
              code:blank
              (code:line flip : (Posn -> Posn)))))

(define ant-rep
  (para (code (define-type Ant
                (ant [location : Posn]
                     [weight : Number]))
              code:blank
              (code:line ant-at-home? : (Ant -> Boolean)))))

(define animal-rep
  (para (code (define-type Animal
                (tiger [color : Symbol]
                       [stripe-count : Number])
                (snake [color : Symbol]
                       [weight : Number]
                       [food : String]))
              code:blank
              (code:line heavy-animal? : (Animal -> Boolean)))))

(define fish-rep0
  (para (code (define-type Listof-Number
                (emptyL)
                (biggerL [n : Number]
                         [rst : Listof-Number]))
              code:blank
              (code:line feed-fish : (Listof-Number -> Listof-Number)))))

(define list-def
  (code (define-type (Listof Number)
          empty
          (cons [n : Number]
                [rst : (Listof Number)]))))
  
(define fish-rep
  (inset
   (para (code #,list-def
               code:blank
               (code:line feed-fish : ((Listof Number) -> (Listof Number)))))
   0 0 0 (* -1/2 gap-size)))

(define (wrap-rep p)
  (inset (scale p 0.9)
         0 gap-size 0 0))

(define (make-cookie-examples #:all? [all? #f]
                              #:output? [output? all?]
                              #:input? [input? output?]
                              #:eat-cookies? [eat-cookies? input?])
  (para
   (code (test #,((if eat-cookies? values ghost)
                  (code (eat-cookie #,((if input? values ghost) (code 10)))))
               #,((if output? values ghost) (code 9)))
         #,((if all? values ghost)
            (code
             (test (eat-cookie 1)
                   0)
             (test (eat-cookie 0)
                   0))))))

(define cookie-examples (make-cookie-examples #:all? #t))

(define (make-posn-example #:all? [all? #f]
                           #:output? [output? all?]
                           #:input? [input? output?])
  (para
   (code (test (flip #,((if input? values ghost) (code (posn 1 17))))
               #,((if output? values ghost) (code (posn 17 1))))
         #,((if all? values ghost)
            (code
             (test (flip (posn -3 4))
                   (posn 4 -3)))))))

(define posn-examples (make-posn-example #:all? #t))

(define-syntax-rule (rslide e ...) (slide #:layout 'top e ...))

(rslide
 #:title "Representation"
 (problem "Keep track of the number of cookies in a cookie jar")
 'next
 (wrap-rep cookies-rep))
(rslide
 #:title "Examples"
 cookies-rep
 'next
 (blank)
 'alts
 (list
  (list
   (make-cookie-examples))
  (list
   (make-cookie-examples #:eat-cookies? #t))
  (list
   (make-cookie-examples #:input? #t))
  (list
   (make-cookie-examples #:output? #t))
  (list
   cookie-examples)))
(rslide
 #:title "Template"
 cookies-rep
 'next
 (blank)
 (para
  (code (define (eat-cookie [n : Number])
          ... n ...))))
(define eat-cookie-done
  (list
   (para
    (code (define (eat-cookie [n : Number])
            (if (> n 0) 
                (- n 1) 
                0))))
   (blank)
   cookie-examples))

(rslide
 #:title "Body"
 cookies-rep
 (blank)
 'alts
 (list
  (list
   (para
    (code (define (eat-cookie [n : Number])
            ... n ...)
          code:blank
          code:blank))
   'next
   (blank)
   cookie-examples)
  eat-cookie-done))

(rslide
 #:title "Test"
 cookies-rep
 (blank)
 'alts
 (list
  eat-cookie-done))

(rslide
 #:title "Representation"
 (problem "Track a position on the screen")
 'next
 (wrap-rep posn-rep))
(rslide
 #:title "Examples"
 posn-rep
 'next
 (blank)
 'alts
 (list
  (list
   (make-posn-example))
  (list
   (make-posn-example #:input? #t))
  (list
   (make-posn-example #:output? #t))
  (list
   posn-examples)))
(rslide
 #:title "Template"
 posn-rep
 'next
 (blank)
 (para (code (define (flip [p : Posn])
               ... (posn-x p)
               ... (posn-y p) ...)))
 'next
 (t "or")
 (para (code (define (flip [p : Posn])
               (type-case Posn p
                 [(posn x y) ... x ... y ...])))))
(rslide
 #:title "Body"
 posn-rep
 (blank)
 'alts
 (list
  (list
   (para (code (define (flip [p : Posn])
                 (type-case Posn p
                   [(posn x y) ... x ... y ...]))))
   'next
   (blank)
   posn-examples)
  (list
   (para (code (define (flip [p : Posn])
                 (type-case Posn p
                   [(posn x y) (posn y x)]))))
   (blank)
   posn-examples)))
 

(rslide
 #:title "Representation"
 (problem "Track an ant, which has a location and a weight")
 'next
 (wrap-rep ant-rep))
(rslide
 #:title "Examples"
 ant-rep
 'next
 (blank)
 (para
  (code (test (ant-at-home? (ant (posn 0 0) 0.0001)) 
              #t)
        (test (ant-at-home? (ant (posn 5 10) 0.0001)) 
              #f))) )
(rslide
 #:title "Template"
 ant-rep
 'next
 (blank)
 'alts
 (list
  (list
   (para
    (code (define (ant-at-home? [a : Ant])
            (type-case Ant a
              [(ant loc wgt)
               ... loc ...
               ... wgt ...])))))
  (list
   (para
    (code (define (ant-at-home? [a : Ant])
            (type-case Ant a
              [(ant loc wgt)
               ... (is-home? loc) ...
               ... wgt ...]))))
   'next
   (para
    (code (define (is-home? [p : Posn])
            (type-case Posn p
              [(posn x y) ... x ... y ...])))))))
(rslide
 #:title "Body"
 ant-rep
 (blank)
 'alts
 (list
  (list
   (para
    (code (define (ant-at-home? [a : Ant])
            (type-case Ant a
              [(ant loc wgt)
               ... (is-home? loc) ...
               ... wgt ...]))))
   (para
    (code (define (is-home? [p : Posn])
            (type-case Posn p
              [(posn x y) ... x ... y ...])))))
  (list
   (para
    (code (define (ant-at-home? [a : Ant])
            (type-case Ant a
              [(ant loc wgt) (is-home? loc)]))))
   (para
    (code (define (is-home? [p : Posn])
            (type-case Posn p
              [(posn x y) ... x ... y ...])))))
  (list
   (para
    (code (define (ant-at-home? [a : Ant])
            (type-case Ant a
              [(ant loc wgt) (is-home? loc)]))))
   (para
    (code (define (is-home? [p : Posn])
            (type-case Posn p
              [(posn x y) (and (zero? x)
                               (zero? y))])))))))

(rslide
 #:title "Representation"
 (problem "Track an animal, which is a tiger or a snake")
 'next
 (wrap-rep animal-rep))

(define (make-animal-examples #:all? [all? #f]
                              #:output2? [output2? all?]
                              #:input2? [input2? output2?]
                              #:output? [output? input2?]
                              #:input? [input? output?])
  (para
   (code (test (heavy-animal? #,((if input? values ghost) (code (tiger 'orange 14))))
               #,((if output? values ghost) (code #t)))
         #,((if input2? values ghost)
            (code (test (heavy-animal? (snake 'green 10 "rats"))
                        #,((if output2? values ghost) (code #t)))))
         #,((if all? values ghost)
            (code (test (heavy-animal? (snake 'yellow 8 "cake")) 
                        #f))))))

(rslide
 #:title "Examples"
 animal-rep
 'next
 (blank)
 'alts
 (list
  (list
   (make-animal-examples))
  (list
   (make-animal-examples #:input? #t))
  (list
   (make-animal-examples #:output? #t))
  (list
   (make-animal-examples #:input2? #t))
  (list
   (make-animal-examples #:output2? #t))
  (list
   (make-animal-examples #:all? #t))))

(rslide
 #:title "Template"
 animal-rep
 'next
 (blank)
 (para
  (code (define (heavy-animal? [a : Animal])
          (type-case Animal a
            [(tiger c sc) 
             ... c ... sc ...]
            [(snake c w f)
             ... c ... w ...
             ... f ...])))))
(rslide
 #:title "Body"
 animal-rep
 (blank)
 'alts
 (list
  (list
   (para
    (code (define (heavy-animal? [a : Animal])
            (type-case Animal a
              [(tiger c sc) 
               ... c ... sc ...]
              [(snake n w f)
               ... c ... w ...
               ... f ...])))))
  (list
   (para
    (code (define (heavy-animal? [a : Animal])
            (type-case Animal a
              [(tiger c sc) #t]
              [(snake c w f)
               ... c ... w ...
               ... f ...])))))
  (list
   (para
    (code (define (heavy-animal? [a : Animal])
            (type-case Animal a
              [(tiger c sc) #t]
              [(snake c w f) (>= w 10)])))))))

 
(rslide
 #:title "Representation"
 (problem #:width ((get-current-para-width #:aspect (and widescreen? 'widescreen)))
          "Track an aquarium, which has any number of fish,"
          "each with a weight")
 'next
 'alts
 (list (list (wrap-rep fish-rep0))
       (list (wrap-rep fish-rep))))

(define feed-example-rst-lon
  (code (cons 2 (cons 3 empty))))
(define feed-example-feed-rst-lon
  (code (cons 3 (cons 4 empty))))

(define feed-example
  (code (test (feed-fish empty)
              empty)
        (test (feed-fish (cons 1 #,feed-example-rst-lon))
              (cons 2 #,feed-example-feed-rst-lon))))

(rslide
 #:title "Examples"
 fish-rep
 'next
 (blank)
 'alts
 (list
  (list
   (para feed-example))
  (list
   (para
    (code (test (feed-fish (list))
                (list))
          (test (feed-fish (list 1 2 3))
                (list 2 3 4)))))))
(rslide
 #:title "Template"
 fish-rep
 'next
 (blank)
 'alts
 (list
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty ...]
             [(cons n rst-lon) ...])))))
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty ...]
             [(cons n rst-lon) 
              ... n ...
              ... rst-lon ...])))))
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty ...]
             [(cons n rst-lon) 
              ... n ...
              ... (feed-fish rst-lon) ...])))))))

(define show-feed-example (inset (scale feed-example 0.8) 0 (* -1/2 gap-size) 0 0))

(define (hilite-behind p p2
                       content spike dx dy)
  (define-values (lx ty) (lt-find p p2))
  (define-values (rx by) (rb-find p p2))
  (pin-balloon (wrap-balloon (scale content 0.9)
                             spike dx dy
                             balloon-color
                             16)
               (pin-under p
                          p2 lt-find
                          (colorize (filled-rectangle (- rx lx)
                                                      (- by ty))
                                    "lightblue"))
               p2 rt-find))

(define (show-feed-rst p)
  (hilite-behind p feed-example-rst-lon
                 (code rst-lon) 'sw 0 gap-size))

(define (show-feed-feed-rst p)
  (hilite-behind p feed-example-feed-rst-lon
                 (code (feed-fish rst-lon)) 'nw (- gap-size) 0))

(rslide
 #:title "Body"
 fish-rep
 (blank)
 'alts
 (list
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty ...]
             [(cons n rst-lon) 
              ... n ...
              ... (feed-fish rst-lon) ...])))))
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty ...]
             [(cons n rst-lon) 
              ... n ...
              ... (feed-fish rst-lon) ...]))))
   show-feed-example)
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty empty]
             [(cons n rst-lon)
              ... n ...
              ... (feed-fish rst-lon) ...]))))
   show-feed-example)
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty empty]
             [(cons n rst-lon)
              ... (+ 1 n) ...
              ... (feed-fish rst-lon) ...]))))
   show-feed-example)
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty empty]
             [(cons n rst-lon)
              ... (+ 1 n) ...
              ... (feed-fish rst-lon) ...]))))
   (show-feed-rst show-feed-example))
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty empty]
             [(cons n rst-lon)
              ... (+ 1 n) ...
              ... (feed-fish rst-lon) ...]))))
   (show-feed-feed-rst (show-feed-rst show-feed-example)))
  (list
   (para
    (code (define (feed-fish [lon : (Listof Number)])
            (type-case (Listof Number) lon
             [empty empty]
             [(cons n rst-lon)
              (cons (+ 1 n)
                    (feed-fish rst-lon))]))))
   (show-feed-feed-rst (show-feed-rst show-feed-example)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists

(define lists-title "Implementation Matches Data")

(define list-tmpl
  (code
   code:blank
   (define (feed-fish [lon : (Listof Number)])
     (type-case (Listof Number) lon
      [empty ...]
      [(cons n rst-lon) ... n
       ... (feed-fish rst-lon) ...]))))

(rslide
 #:title lists-title
 'alts~
 (list
  (list list-def)
  (list
   (add-gp-arrow list-def
                 2/3 25/30 1/2 8/30)))
 'next
 (blank)
 'alts~
 (list
  (list list-tmpl)
  (list
   (add-gp-arrow list-tmpl
                 1/2 55/60 1/3 18/60))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 (vc-append
  (titlet "How to Design Programs")
  (t "More Examples")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define show-panels (make-parameter #f))

(define (gui-button s enabled?)
  (let* ([txt (bt s)]
         [t (inset txt 8)]
         [t (if enabled?
                t
                (colorize t "gray"))])
    (define p
      (round-frame (cc-superimpose (colorize (filled-rounded-rectangle
                                              (pict-width t)
                                              (pict-height t)
                                              12)
                                             "white")
                                   t)
                   12))
    (define-values (pl pt) (lt-find p txt))
    (define-values (pr pb) (rb-find p txt))
    (define r (inset (refocus p txt) pl pt (- (pict-width p) pr) (- (pict-height p) pb)))
    (use-last r r)))
(define (gui-label s)
  (let ([t (bt s)])
    (inset t 5)))
(define (gui-list l)
  (let ([p (frame (let ([p (inset (apply vl-append 5 (map bt l))
                                  5)])
                    (cc-superimpose (colorize (filled-rectangle (pict-width p)
                                                                (pict-height p))
                                              "white")
                                    p)))])
    (ht-append p (frame (cb-superimpose
                         (ct-superimpose
                          (blank 20 (pict-height p))
                          (frame (arrowhead 20 (/ pi 2))))
                         (frame (arrowhead 20 (/ pi -2))))))))
(define (gui-vertical a b)
  (gui-panel (vc-append 10 a b)))
(define (gui-horizontal a b)
  (gui-panel (hc-append 10 a b)))
(define (gui-panel p)
  (let* ([p (inset p 5)]
         [p (if (show-panels)
                (frame p #:segment 5 #:color "red" #:line-width 2)
                p)])
    (inset p 5)))

(define (gui-frame p)
  (frame (cc-superimpose
          (colorize (filled-rectangle (pict-width p)
                                      (pict-height p))
                    "beige")
          p)))

(define (example-gui)
  (scale
   (gui-frame
    (gui-vertical
     (gui-horizontal
      (gui-label "Pick a fruit:")
      (gui-list `("Apple" "Banana" "Coconut")))
     (gui-horizontal (gui-button "Ok" #f)
                     (gui-button "Cancel" #t))))
   0.8))

(slide
 #:title "GUIs"
 #:layout 'top
 (example-gui)
 (blank)
 (vl-append
  gap-size
  (para #:fill? #f "Possible programs:")
  (item #:fill? #f "Can click?")
  (item #:fill? #f "Find a label")
  (item #:fill? #f "Read screen")))

(define (maybe-shift-right p)
  ;; Use a little more space to the right, if available
  (define delta (/ (- client-w
                      (get-client-w #:aspect 'fullscreen))
                   2))
  (inset p delta 0 (- delta) 0))

(define (gui-slide show-i show-l show-b show-c show-s)
  (slide
   #:title "Representing GUIs"
   #:layout 'top
   (example-gui)
   (blank)
   (parameterize ([current-para-width (get-client-w #:aspect 'fullscreen)])
     (rt-superimpose
      (show-i
       (vl-append
        (/ gap-size 2)
        (vc-append
         (current-line-sep)
         (item "labels")
         (show-l (subitem "a label string")))
        (vc-append
         (current-line-sep)
         (item "buttons")
         (show-b (subitem "a label string"))
         (show-b (subitem "enabled state")))
        (vc-append
         (current-line-sep)
         (item "lists")
         (show-c (subitem "a list of choice strings"))
         (show-c (subitem "selected item")))))
      (show-s
       (maybe-shift-right
        (code (define-type GUI
                (label [text : String])
                (button [text : String]
                        [enabled? : Boolean])
                (choice [items : (Listof String)]
                        [selected : Number])))))))))
(no-print (gui-slide values ghost ghost ghost ghost))
(no-print (gui-slide values values ghost ghost ghost))
(no-print (gui-slide values values values ghost ghost))
(no-print (gui-slide values values values values ghost))
(gui-slide values values values values values)

(define read-screen-problem
  (problem "Implement" (code read-screen) ", which takes"
           "a GUI and returns a list of strings for all the GUI"
           "element labels"))

(slide
 #:title "Read Screen"
 read-screen-problem)

(slide
 #:title "Read Screen"
 (code
  (define (read-screen [g : GUI]) : (Listof String)
    (type-case GUI g
      [(label t) (list t)]
      [(button t e?) (list t)]
      [(choice i s) i]))
  code:blank
  (test (read-screen (label "Hi"))
        (list "Hi"))
  (test (read-screen (button "Ok" #t))
        (list "Ok"))
  (test (read-screen (choice (list "Apple" "Banana")
                             0))
        (list "Apple" "Banana"))))

(define define-type:gui
  (scale/improve-new-text
   (code (define-type GUI
           (label [text : String])
           (button [text : String]
                   [enabled? : Boolean])
           (choice [items : (Listof String)]
                   [selected : Number])
           (vertical [top : GUI]
                     [bottom : GUI])
           (horizontal [left : GUI]
                       [right : GUI])))
   0.8))

(define (gui-panel-slide show-hv show-p show-s show-ex)
  (slide
   #:title "Assembling GUIs"
   (example-gui)
   (blank)
   (parameterize ([current-para-width (get-client-w #:aspect 'fullscreen)])
     (rt-superimpose
      (vl-append
       gap-size
       (item "label")
       (item "buttons")
       (item "lists")
       (show-hv
        (vl-append
         gap-size
         (vc-append
          (current-line-sep)
          (item "vertical stacking")
          (show-p (subitem "two sub-GUIs")))
         (vc-append
          (current-line-sep)
          (item "horizontal stacking")
          (show-p (subitem "two sub-GUIs"))))))
      (show-s define-type:gui)
      (show-ex
       (maybe-shift-right
        (scale/improve-new-text
         (code (define gui1
                 (vertical
                  (horizontal 
                   (label "Pick a fruit:")
                   (choice
                    (list "Apple" "Banana" "Coconut") 
                    0))
                  (horizontal 
                   (button "Ok" #f)
                   (button "Cancel" #t)))))
         0.9)))))))

(no-print (gui-panel-slide ghost ghost ghost ghost))
(parameterize ([show-panels #t])
  (no-print (gui-panel-slide values ghost ghost ghost))
  (no-print (gui-panel-slide values values ghost ghost))
  (gui-panel-slide values values values ghost)
  (gui-panel-slide values values ghost values))

(define read-screen-code
  (scale/improve-new-text
   (code
    (define (read-screen [g : GUI]) : (Listof String)
      (type-case GUI g
        [(label t) (list t)]
        [(button t e?) (list t)]
        [(choice i s) i]
        [(vertical t b) (append (read-screen t)
                                (read-screen b))]
        [(horizontal l r) (append (read-screen l)
                                  (read-screen r))])))
   0.8))

(slide
 #:title "Read Screen"
 read-screen-problem)

(slide
 #:title "Read Screen"
 (vl-append
  (current-line-sep)
  read-screen-code
  (scale/improve-new-text
   (code
    code:blank
    ...
    (test (read-screen gui1)
          (list "Pick a fruit:" 
                "Apple" "Banana" "Coconut"
                "Ok" "Cancel")))
   0.8)))

(define (shape-slide add-gp-arrow)
  (slide
   #:title
   "Function and Data Shapes Match"
   (vc-append
    gap-size
    (add-gp-arrow 
     (add-gp-arrow 
      (add-gp-arrow 
       (add-gp-arrow 
        define-type:gui
        0.55 65/100 1/2 1/10)
       0.65 75/100 1/2 1/10)
      0.65 85/100 1/2 1/10)
     0.65 95/100 1/2 1/10)
    (add-gp-arrow 
     (add-gp-arrow 
      (add-gp-arrow 
       (add-gp-arrow 
        read-screen-code
        0.65 55/90 38/100 8/90)
       0.65 65/90 38/100 8/90)
      0.70 75/90 38/100 8/90)
     0.70 85/90 38/100 8/90))))
(no-print (shape-slide (lambda (p x1 y1 x2 y2) p)))
(shape-slide add-gp-arrow)

(slide
 #:title "Design Steps"
 (item "Determine the representation")
 (subitem (code define-type) ", maybe")
 (item "Write examples")
 (subitem (code test))
 (item "Create a template for the implementation")
 (subitem (code type-case) "plus natural recursion,"
               (colorize (bt "check shape!") "red"))
 (item "Finish body implementation case-by-case")
 (subitem (it "usually the interesting part"))
 (item "Run tests"))

(define enable-button-code
  (scale/improve-new-text
   (code
    (define (enable-button [g : GUI] [name : String]) : GUI
      (type-case GUI g
        [(label t) g]
        [(button t e?) (cond
                        [(equal? t name) (button t #t)]
                        [else g])]
        [(choice i s) g]
        [(vertical t b) (vertical (enable-button t name)
                                  (enable-button b name))]
        [(horizontal l r) (horizontal (enable-button l name)
                                      (enable-button r name))])))
   0.75))

(slide
 #:title "Enable Button"
 (problem "Implement" (code enable-button)
          ", which takes a GUI and a string"
          "and enables the button whose name matches the string"))

(slide
 #:title "Enable Button"
 (para #:width client-w "The" (code name) "argument is \u201Calong for the ride\u201D:")
 (vl-append
  (current-line-sep)
  enable-button-code
  (scale/improve-new-text
   (code
    ...
    (test (enable-button gui1 "Ok")
          (vertical
           (horizontal (label "Pick a fruit:")
                       (choice (list "Apple" "Banana" "Coconut") 0))
           (horizontal (button "Ok" #t)
                       (button "Cancel" #t)))))
   0.75)))


(define last-example-title
  "Show Depth")

(parameterize ([show-panels #t])
  (slide
   #:title last-example-title
   (code
    (test (show-depth #,(gui-frame
                         (gui-vertical
                          (gui-label "Hello")
                          (gui-horizontal (gui-button "Ok" #f)
                                          (gui-button "Cancel" #t)))))
          #,(gui-frame
             (gui-vertical
              (gui-label "1 Hello")
              (gui-horizontal (gui-button "2 Ok" #f)
                              (gui-button "2 Cancel" #t))))))))

(slide
 #:title last-example-title
 (para #:width client-w "Template:")
 (code
  (define (show-depth [g : GUI]) : GUI
    (type-case GUI g
      [(label t) ... t ... ]
      [(button t e?) ... t ... e? ...]
      [(choice i s) ... i ... s ...]
      [(vertical t b) ... (show-depth t)
                #||#  ... (show-depth b) ...]
      [(horizontal l r) ... (show-depth l)
                  #||#  ... (show-depth r) ...])))
 'next
 (blank)
 'alts
 (list
  (list
   (hc-append gap-size
              (code (show-depth #,(gui-button "Ok" #t)))
              (t "\u2192")
              (gui-button "0 Ok" #t)))
  (list
   (parameterize ([show-panels #t])
     (let ([dots (t "...")])
       (hc-append gap-size
                  (code (show-depth #,(gui-horizontal (gui-button "Ok" #f)
                                                      (gui-button "Cancel" #t))))
                  (t "\u2192")
                  dots (gui-button "0 Ok" #f) dots (gui-button "0 Cancel" #t) dots))))
  (list
   (para #:align 'right "recursion results don't have the right labels..."))))

(slide
 #:title last-example-title
 (para #:width client-w "The" (code n) "argument is an" (it "accumulator") ":")
 (scale/improve-new-text
  (code
   (define (show-depth-at [g : GUI] [n : Number]) : GUI
     (type-case GUI g
       [(label t) (label (prefix n t))]
       [(button t e?) (button (prefix n t) e?)]
       [(choice i s) g]
       [(vertical t b) (vertical (show-depth-at t (+ n 1))
                                 (show-depth-at b (+ n 1)))]
       [(horizontal l r) (horizontal (show-depth-at l (+ n 1))
                                     (show-depth-at r (+ n 1)))]))
   code:blank
   (define (show-depth [g : GUI]) : GUI
     (show-depth-at g 0)))
  (if widescreen?
      1.0
      0.8)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "How to Design Programs"
 (item "Follow the design steps")
 (item "Use accumulators when necessary")
 (item "Reuse functions and/or ``wish'' for helpers"))

