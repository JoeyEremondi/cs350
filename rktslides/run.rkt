#lang slideshow
(require scheme/gui
         scheme/class
         "colors.ss"
         (for-syntax scheme/list))

(provide add-copy)

(define-syntax add-copy
  (syntax-rules ()
    [(_ code b ...)
     (let ([p (code b ...)]
           [s (to-string b ...)])
       (vr-append
        gap-size
        p
        ((if printing? ghost values)
         (scale/improve-new-text (mk-copy* s) 0.5))))]))

(define prev-custodian (make-custodian))
(define (run-code stx)
  (custodian-shutdown-all prev-custodian)
  (set! prev-custodian (make-custodian))
  (parameterize ([current-custodian prev-custodian])
    (let ([n (make-gui-namespace)])
      (parameterize ([current-namespace n])
        (parameterize ([current-eventspace (make-eventspace)])
          (queue-callback
           (lambda ()
             (namespace-require "new-gui.ss")
             (namespace-require '(lib "etc.ss"))
             (eval (datum->syntax #f (syntax->datum stx))))))))))

(define-syntax (to-string stx)
  (syntax-case stx ()
    [(_ . c)
     (let* ([c #'c]
            [s (open-output-string)]
            [l (syntax->list c)]
            [init-col (or (syntax-column (first l)) 0)]
            [col init-col]
            [line (or (syntax-line (first l)) 0)])
       (define (advance c init-line!)
         (let ([c (syntax-column c)]
               [l (syntax-line c)])
           (when (and l (l . > . line))
             (newline)
             (set! line l)
             (init-line!))
           (when c
             (display (make-string (max 0 (- c col)) #\space))
             (set! col c))))
       (parameterize ([current-output-port s]
                      [read-case-sensitive #t])
         (define (loop init-line!)
           (lambda (c)
             (cond
              [(eq? 'code:blank (syntax-e c))
               (advance c init-line!)]
              [(eq? '_ (syntax-e c)) (void)]
              [(eq? '... (syntax-e c))
               (void)]
              [(and (pair? (syntax-e c))
                    (eq? (syntax-e (car (syntax-e c))) 'code:comment))
               (advance c init-line!)
               (printf "; ")
               (display (syntax-e (cadr (syntax->list c))))]
              [(and (pair? (syntax-e c))
                    (eq? (syntax-e (car (syntax-e c))) 'code:contract))
               (advance c init-line!)
               (printf "; ")
               (let* ([l (cdr (syntax->list c))]
                      [s-col (or (syntax-column (first l)) col)])
                 (set! col s-col)
                 (for-each (loop (lambda ()
                                   (set! col s-col)
                                   (printf "; ")))
                           l))]
              [(and (pair? (syntax-e c))
                    (memq (syntax-e (car (syntax-e c))) 
                          '(quote quasiquote unquote unquote-splicing)))
               (advance c init-line!)
               (printf (case (syntax-e (car (syntax-e c)))
                         [(quote) "'"]
                         [(unquote) ","]
                         [(unquote-splicing) ",@"]
                         [(quasiquote) "`"]))
               (let ([i (cadr (syntax->list c))])
                 (set! col (or (syntax-column i) col))
                 ((loop init-line!) i))]
              [(pair? (syntax-e c))
               (advance c init-line!)
               (printf "(")
               (set! col (+ col 1))
               (map (loop init-line!) (syntax->list c))
               (printf ")")
               (set! col (+ col 1))]
              [else
               (advance c init-line!)
               (let ([s (format "~s" (syntax-e c))])
                 (set! col (+ col (string-length s)))
                 (display s))])))
         (for-each (loop (lambda () (set! col init-col))) l))
       (datum->syntax #'here (get-output-string s) #'c))]))

(define-syntax (define-code/string/scale stx)
  (define (drop-to-run l)
    (map (lambda (x)
           (cond
            [(and (pair? (syntax-e x))
                  (eq? 'local (syntax-e (car (syntax-e x)))))
             (let ([l (syntax->list x)])
               (list* 'local
                      (drop-to-run (syntax->list (cadr l)))
                      (cddr l)))]
            [(and (pair? (syntax-e x))
                  (eq? 'define (syntax-e (car (syntax-e x)))))
             (let ([l (syntax->list x)])
               (list* 'define
                      (cadr l)
                      (drop-to-run (cddr l))))]
            [else x]))
         (filter (lambda (x)
                   (cond
                    [(eq? '_ (syntax-e x))
                     #f]
                    [(eq? '... (syntax-e x))
                     #f]
                    [(eq? 'code:blank (syntax-e x))
                     #f]
                    [(and (pair? (syntax-e x))
                          (eq? 'code:comment (syntax-e (car (syntax-e x)))))
                     #f]
                    [(and (pair? (syntax-e x))
                          (eq? 'code:contract (syntax-e (car (syntax-e x)))))
                     #f]
                    [(and (pair? (syntax-e x))
                          (eq? 'unsyntax (syntax-e (car (syntax-e x)))))
                     #f]
                    [else #t]))
                 l)))
  (define (drop-to-show l)
    (foldr (lambda (x r)
             (cond
              [(and (identifier? x) (eq? '_ (syntax-e x)))
               (cdr r)]
              [(and (pair? (syntax-e x))
                    (eq? 'local (syntax-e (car (syntax-e x)))))
               (cons
                (let ([l (syntax->list x)])
                  (datum->syntax
                   x
                   (list* (car l)
                          (datum->syntax
                           (cadr l)
                           (drop-to-show (syntax->list (cadr l)))
                           (cadr l))
                          (cddr l))
                   x))
                r)]
              [(and (pair? (syntax-e x))
                    (eq? 'cond (syntax-e (car (syntax-e x)))))
               (cons
                (let ([l (syntax->list x)])
                  (datum->syntax 
                   x
                   (list* (car l)
                          (drop-to-show (cdr l)))
                   x))
                r)]
              [(and (pair? (syntax-e x))
                    (eq? 'define (syntax-e (car (syntax-e x)))))
               (cons (let ([l (syntax->list x)])
                       (datum->syntax 
                        x
                        (list* (car l)
                               (cadr l)
                               (drop-to-show (cddr l)))
                        x))
                     r)]
              [else (cons x r)]))
           empty
           l))
  
  (syntax-case stx ()
    [(_ s (runnable-name showable-name string-name) . c)
     #`(begin
         (define runnable-name
           (quote-syntax
            (begin
              #,@(drop-to-run (syntax->list #'c)))))
         (define showable-name
           (scale/improve-new-text
            (code
             #,@(drop-to-show (syntax->list #'c)))
            s))
         (define string-name
           (to-string c)))]))

(define-syntax define-code
  (syntax-rules ()
    [(_ (runnable-name showable-name) . code)
     (define-code/string/scale 0.7 (runnable-name showable-name string-name) . code)]))

(define (gray-bitmap fn)
  (let* ([bm (make-object bitmap% fn)]
         [bm2 (make-object bitmap% (send bm get-width) (send bm get-height))]
         [dc (make-object bitmap-dc% bm2)])
    (send dc set-brush (send the-brush-list find-or-create-brush (make-object color% 128 128 128) 'solid))
    (send dc draw-rectangle -10 -10 1000 1000)
    (send dc set-bitmap #f)
    (send bm set-loaded-mask bm2)
    (bitmap bm)))

(define (link-text s)
  (colorize (let ([r (t s)])
              (vl-append
               r
               (linewidth 2 (hline (pict-width r) 1))))
            BlueColor))

(define (mk-run run)
  (para
   #:width client-w
   #:align 'right
   (clickback (link-text "Run")
              (lambda () (run-code run)))))

(define (mk-copy s)
  (para
   #:width client-w
   #:align 'right
   (mk-copy* s)))

(define (mk-copy* s)
  (clickback (link-text "Copy")
             (lambda ()
               (send the-clipboard set-clipboard-string s 0))))