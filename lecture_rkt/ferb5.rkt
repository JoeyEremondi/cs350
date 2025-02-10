#lang flit
(define-type (Tree 'data)
  ;; zero-children constructor
  ;; has a field for the name of the alien at the root of the tree
  (Leaf)
  ;; two-chidlren constructor.
  ;; Has a field for the name of the alien at the root,
  ;; and the family trees of that alien's two children.
  (TwoChildren [name : 'data]
               [childTree1 : (Tree 'data)]
               [childTree2 : (Tree 'data)]))

(define (filterTree[p : ('data -> Boolean)]
                   [t : (Tree 'data)])
  : (Tree 'data)
  (type-case (Tree 'data) t
    [(Leaf)
       (Leaf)]
    [(TwoChildren name c1 c2)
       (let* ([rec1 (filterTree p c1)]
              [rec2 (filterTree p c2)])
         (if (p name)
             (TwoChildren name rec1 rec2)
             (Leaf)))]))