#lang flit

(has-type '() : (Listof (Listof (Listof (Listof String)))))

(define myPair (pair "hello" '(2 3 4)))

(define-type Shape
  (Rectangle [length : Number]
             [width : Number])
  (Circle [radius : Number]))

(define (square width) : Shape
  (Circle width))

(define (area (shp : Shape))
  : Number
  (type-case Shape shp
    [(Rectangle happy w)
       TODO]
    ;[else 0]
    ;[(Circle rad)
    ;  (error 'x "Circle will never happen")]
    ))

(area (Circle 3))
