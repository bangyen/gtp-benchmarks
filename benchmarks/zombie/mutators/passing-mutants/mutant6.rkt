(module math typed/racket/base
  (#%module-begin
   (provide min abs max sqrt sqr msqrt)
   (: min (-> Real Real Real))
   (define (min x y) (if (<= x y) x y))
   (: max (-> Real Real Real))
   (define (max x y) (if (>= x y) x y))
   (: abs (-> Real Real))
   (define (abs x) (if (>= x 0) x (- 0 x)))
   (: sqr (-> Real Real))
   (define (sqr x) (* x x))
   (: msqrt (-> Real Real))
   (define (msqrt x) (assert (sqrt x) real?))))
