
(module posn3d-obj (lib "slideshow.ss" "slideshow")
  (require (lib "code.ss" "slideshow")
	   "colors.ss")

  (provide posn-obj
	   posn-obj+label
	   posn3d-obj
	   posn3d-obj+label
	   obj+label)
  
  (define (posn-obj)
    (let-values ([(o l) (posn-obj+label)])
      o))

  (define (posn-obj+label)
    (obj+label (code posn)
	       (list (code 5) (code 7))))

  (define (posn3d-obj)
    (let-values ([(o l) (posn3d-obj+label)])
      o))

  (define (posn3d-obj+label)
    (obj+label (code Posn3D)
	       (list (code 1) (code 2) (code 3))))

  (define (obj+label label fields)
    (values
     (color-round-frame (let* ([h-inset gap-size]
			       [v-inset (* 2 line-sep)]
			       [i (lambda (p)
				    (inset p h-inset v-inset))])
			  (let ([cls (i label)])
			    (apply
			     vl-append
			     v-inset
			     cls
			     (hline (pict-width cls) 0)
			     (map i fields))))
			(/ (current-font-size) 2)
			BlueColor)
     label)))
