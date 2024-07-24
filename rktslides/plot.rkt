#lang slideshow
(require racket/math)

(provide plot
         plot-xy)

(define (plot grades)
  (define hi (apply max grades))
  (define lo (apply min grades))
  (define buckets (make-vector (- hi lo -1)))
  (for-each (lambda (i) 
              (let ([i (- 100 i)])
                (vector-set! buckets i (add1 (vector-ref buckets i)))))
            grades)
  (let ([plot (let ([w (* client-w 3/4)]
                    [h (* client-h 1/4)])
                (dc (lambda (dc x y)
                      (let ([b (send dc get-brush)])
                        (send dc set-brush "blue" 'solid)
                        (let ([full (apply max (vector->list buckets))]
                              [len (vector-length buckets)])
                          (let loop ([i 0])
                            (unless (= i len)
                              (let ([bh (* (/ h full) (- full (vector-ref buckets i)))]
                                    [bw (/ w len)])
                                (send dc draw-rectangle 
                                      (+ x (* (/ w len) (- len i 1)))
                                      (+ y bh)
                                      bw (- h bh)))
                              (loop (add1 i)))))
                        (send dc set-brush b)))
                    w h 0 0))])
    (vc-append
     plot
     (hline (pict-width plot) 0)
     (hc-append
      (- (pict-width plot) (* 2 (current-font-size)))
      (t (format "~a" lo))
      (t (format "~a" hi))))))

(define (plot-xy pairs x-axis y-axis)
  (define hi-x (apply max (map car pairs)))
  (define lo-x (apply min (map car pairs)))
  (define hi-y (apply max (map cdr pairs)))
  (define lo-y (apply min (map cdr pairs)))
  (define dot (colorize (filled-ellipse 4 6) "blue"))
  (let ([p (scale
            (inset
             (apply
              lt-superimpose
              (for/list ([p (in-list pairs)])
                (inset dot 
                       (* 100
                          (/ (- (car p) lo-x)
                             (- hi-x lo-x)))
                       (* 100
                          (- 1.0
                             (/ (- (cdr p) lo-y)
                                (- hi-y lo-y))))
                       0
                       0)))
             10)
            6 4)])
    (hc-append
     (/ gap-size 2)
     (text y-axis (current-main-font) (current-font-size) (/ pi 2))
     (vc-append
      (/ gap-size 2)
      (vc-append (hc-append (vline 0 (pict-height p))
                            p)
                 (hline (pict-width p) 0))
      (t x-axis)))))
