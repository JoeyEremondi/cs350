#lang slideshow
(require scheme/class
         scheme/gui/base
         scheme/math
         slideshow/flash
         slideshow/code
         slideshow/balloon
         texpict/symbol
         "colors.ss")
  
(provide (all-from-out texpict/symbol)

         widescreen?
         no-print
         pi dt
         prog prog* prog$*
         code/patterns scode sscode ssscode sshcode xscode
         un-s-scale un-ss-scale un-sss-scale un-xs-scale
         strike background
         parsed parsed+sub cparsed
         balloon-code

         infer
         ante-append
         ante-append/close
         
         nonterm new-label

         round-frame
         strike-out
         hilite
         part

         fullscreen-para-width!)

(current-keyword-list (append
                       '("define-type" "type-case" "local" "try" "test" "test/exn" "let-macro" "define-type-alias"
                                       "get" "set" "record" "with"
                                       "object" "send" "ssend" "new" "super")
                       (remove*
                        '("delay")
                        (current-keyword-list))))

(define widescreen?
  (= (get-client-w #:aspect #f)
     (get-client-w #:aspect 'widescreen)))

(define-syntax no-print
  (syntax-rules ()
    [(_ e) (if printing? 
               (skip-slides 1)
               e)]))
  
;; defterm
(define (dt i) (bit i))

;; tt typeset  with optional hiliting
(define prog 
  (case-lambda
   [(s) (prog #f s)]
   [(p s) (prog p s "purple")]
   [(p s color)
    (let ([lines (let loop ([s (regexp-replace* (string #\tab) s "        ")])
                   (let ([m (regexp-match (format "([^~a~a]*)[~a~a]+(.*)"
                                                  #\newline #\return #\newline #\return)
                                          s)])
                     (if m
                         (if (string=? "" (cadr m))
                             (loop (caddr m))
                             (cons (cadr m)
                                   (loop (caddr m))))
                         (list s))))])
      (apply
       vl-append
       (map
        (lambda (line)
          (let loop ([line line])
            (let ([m (and p (regexp-match-positions p line))])
              (if m
                  (hc-append
                   (tt (substring line 0 (caar m)))
                   (colorize (tt (substring line (caar m) (cdar m))) color)
                   (loop (substring line (cdar m) (string-length line))))
                  (tt line)))))
        lines)))]))

(define (cat l)
  (let loop ([l l])
    (if (null? (cdr l))
        (car l)
        (string-append (car l) "\n" (loop (cdr l))))))

(define prog* (lambda l (prog (cat l))))
(define prog$* (lambda (p . l) (prog p (cat l))))

(define (code/patterns patterns s)
  (let ([l (string-length s)])
    (let loop ([p 0])
      (if (= p l)
          (blank)
          (let ploop ([patterns patterns])
            (cond
             [(null? patterns) (error 'code "can't match: ~a" (substring s p l))]
             [(regexp-match-positions (car patterns) s p)
              => (lambda (m)
                   (hbl-append
                    ((cadr patterns) (substring s (caar m) (cdar m)))
                    (loop (cdar m))))]
             [else (ploop (cddr patterns))]))))))

(define (strike color p)
  (cbl-superimpose
   p
   (linewidth
    3
    (colorize
     (dc (lambda (dc x y)
           (send dc draw-line 
                 x (+ y (/ (pict-height p) 2))
                 (+ x (pict-width p)) (+ y (/ (pict-height p) 2))))
         (pict-width p) (pict-height p)
         (pict-ascent p) (pict-descent p))
     color))))

(define (background color p)
  (cbl-superimpose
   (colorize
    (dc (lambda (dc x y)
          (send dc draw-rectangle
                x y 
                (pict-width p) (pict-height p)))
        (pict-width p) (pict-height p)
        (pict-ascent p) (pict-descent p))
    color)
   p))

(define (infer r . l)
  (vc-append
   (* 2 (current-line-sep))
   (apply vc-append
          (current-line-sep)
          l)
   (linewidth (ceiling (/ 2 (apply min (current-expected-text-scale))))
              (hline (+ (current-font-size) (apply max (map pict-width (cons r l)))) 1))
   r))

(define (ante-append . l)
  (apply hbl-append (* 3 (current-font-size)) l))
(define (ante-append/close . l)
  (apply hbl-append (* 1.5 (current-font-size)) l))


(define (nonterm s)
  (colorize (tt (format "<~a>" s)) MetaColor))

(define new-label
  (let* ([new (text "NEW" '(bold . swiss) 12 (/ pi 8))]
         [bg (filled-flash (* 3/2 (pict-width new)) (* 3/2 (pict-height new))
                           10 0.25 (/ pi 8))])
    (cc-superimpose
     (colorize bg "yellow")
     new)))


(define-syntax define-scode
  (syntax-rules ()
    [(_ scode un-s-scale s)
     (begin
       (define-syntax scode
         (syntax-rules ()
           [(_ x (... ...)) (let ([bl #f])
                              (let ([p (scale/improve-new-text
                                        (let ([p (code x (... ...))])
                                          (set! bl (code-pict-bottom-line-pict p))
                                          p)
                                        s)])
                                (pict->code-pict p bl)))]))
       (define (un-s-scale p) 
         (pict->code-pict (scale p (/ s))
                          (code-pict-bottom-line-pict p))))]))

(define-scode scode un-s-scale 0.9)
(define-scode sscode un-ss-scale 0.75)
(define-scode sshcode un-ssh-scale 0.65)
(define-scode ssscode un-sss-scale 0.5)
(define-scode xscode un-xs-scale (if widescreen? 0.9 0.75))

(define-syntax parsed
  (syntax-rules ()
    [(_ v) (let ([p (code v)])
             (let ([p2 (frame (inset p (* 2 (current-line-sep))))])
               (use-last p2 p2)))]))

(define-syntax cparsed
  (syntax-rules ()
    [(_ v) (let ([p (code v)])
             (frame (inset p (* 2 (current-line-sep))) #:color OrangeColor))]))

(define-syntax balloon-code
  (syntax-rules ()
    [(_ s ...)
     (wrap-balloon 
      (scale/improve-new-text
       (code s ...)
       0.7)
      'sw 0 10
      balloon-color (/ gap-size 2))]))

(define-syntax parsed+sub
  (syntax-rules ()
    [(_ v s) (let ([p (parsed v)])
               (refocus
                (pin-balloon (balloon-code s)
                             p
                             (* 0.8 (pict-width p))
                             (* 0.1 (pict-height p)))
                p))]))

(define (round-frame p r)
  (cc-superimpose
   p
   (rounded-rectangle (pict-width p) (pict-height p) r)))

(define (hilite p
                #:color [color "lightblue"]
                #:on? [on? #t])
  (if on?
      (refocus (cc-superimpose
                (colorize
                 (filled-rounded-rectangle
                  (+ (pict-width p) 5)
                  (+ (pict-height p) 2)
                  5)
                 color)
                p)
               p)
      p))

(define (strike-out p)
  (refocus
   (cc-superimpose
    p
    (colorize (linewidth 3 (hline (pict-width p) 0))
              RedColor))
   p))

(define (part n #:chapter [chapter #f])
  (define lbl (~a (if chapter
                      (~a chapter " — ")
                      "")
                  "Part " n))
  (slide #:name lbl
         (titlet lbl)))

(define (fullscreen-para-width!)
  ((get-current-para-width #:aspect #f)
   ((get-current-para-width #:aspect 'fullscreen))))
