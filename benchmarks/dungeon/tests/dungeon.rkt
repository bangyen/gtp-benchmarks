#lang racket

(require "../untyped/grid.rkt")
(require "../untyped/cell.rkt")
(require "../untyped/main.rkt")
(require rackunit)

(require/expose "../untyped/main.rkt"
                (smooth-walls room commit-room))
(require/expose "../untyped/grid.rkt"
                (parse-grid))

; test parameters
(define max 5)
(define dub (* max 2))

; grid setup for tests
(define (build n m)
  (build-array (vector n m)
               (lambda (p) (+ (vector-ref p 0)
                              (vector-ref p 1)))))

; grid build/set tests
(test-begin
 (define vec
   (make-vector max))
 (define res
   (make-vector max))
 (define alt
   (build max dub))

 (define (ref x y [f #f])
   (define pos
     (vector x y))
   (define alt
     (if f (f pos) pos))

   (grid-ref vec alt))

 (for ([n (grid-height vec)])
   (vector-set! vec n
                (make-vector dub))
   (vector-set! res n
                (list->vector
                 (range n (+ n dub))))

   (for ([k (grid-width vec)])
     (array-set! vec
                 (vector n k)
                 (+ n k))
     (check-eq? (ref n k)
                (+ n k))))

 (check-false (ref -1 0))
 (check-false (ref 0 -1))
 (check-false (ref (grid-height vec) 0))
 (check-false (ref 0  (grid-width vec)))

 (check-eq? (ref 0 0 up)   (ref 0 0))
 (check-eq? (ref 0 0 left) (ref 0 0))
 (check-false
   (ref (sub1 (grid-height vec))
        (sub1 (grid-width  vec))
        right))
 (check-false
   (ref (sub1 (grid-height vec))
        (sub1 (grid-width  vec))
        down))

 (check-equal? vec res)
 (check-equal? vec alt)
 (check-equal? res alt))

; direction tests
(test-begin
 (define vec (build max dub))

 (for ([n (grid-height vec)])
   (for ([k (grid-width vec)])
     (define pos (vector n k))
     (define sum (+ n k))

     (check-eq? (grid-ref vec (up    pos))
                (if (= n 0) sum (- sum 1)))
     (check-eq? (grid-ref vec (down  pos))
                (if (= (+ n 1) (grid-height vec))
                    #f (+ sum 1)))
     (check-eq? (grid-ref vec (left  pos))
                (if (= k 0) sum (- sum 1)))
     (check-eq? (grid-ref vec (right pos))
                (if (= (+ k 1) (grid-width vec))
                    #f (+ sum 1))))))

; cell tests
(test-begin
 (define (pred type char)
   (check-pred (λ (c) is-a? c type)
               (char->cell% char)))

 (pred empty-cell%       #\space)
 (pred void-cell%        #\.)
 (pred wall%             #\X)
 (pred pillar%           #\#)
 (pred vertical-wall%    #\u2551)
 (pred horizontal-wall%  #\u2550)
 (pred four-corner-wall% #\u256c)
 (pred north-east-wall%  #\u2557)
 (pred north-west-wall%  #\u2554)
 (pred south-east-wall%  #\u255d)
 (pred south-west-wall%  #\u255a)
 (pred north-tee-wall%   #\u2566)
 (pred south-tee-wall%   #\u2569)
 (pred east-tee-wall%    #\u2563)
 (pred west-tee-wall%    #\u2560)
 (pred vertical-door%    #\|)
 (pred horizontal-door%  #\-)
 (check-exn exn:fail?
            (thunk (char->cell% 'err))))

; main tests
(test-begin
 (define p '(1 2))
 (define lst (list p p))
 (define res (list '(1 3) p))
 (check-equal? (dict-set lst 1 '(3))
               res))

(test-begin
 (define (pred type . grid)
   (define res (parse-grid grid))
   (check-pred (λ (c) (is-a? c type))
             (grid-ref (smooth-walls res)
                       (vector 1 1))))

 (define cells (list (cons (vector 1 1)
                           wall%)))
 (define add (room 1 1 cells '() '()))
 (define vec (list "   " "   " "   "))
 (define res (parse-grid vec))

 (commit-room res add)
 (check-pred (λ (c) (is-a? c wall%))
             (grid-ref res
                       (vector 1 1)))
 (pred pillar%          "   " " X " "   ")
 (pred horizontal-wall% "   " " XX" "   ")
 (pred horizontal-wall% "   " "XX " "   ")
 (pred horizontal-wall% "   " "XXX" "   ")
 (pred vertical-wall%   "   " " X " " X ")
 (pred north-west-wall% "   " " XX" " X ")
 (pred north-east-wall% "   " "XX " " X ")
 (pred vertical-wall%   " X " " X " "   ")
 (pred south-west-wall% " X " " XX" "   ")
 (pred south-east-wall% " X " "XX " "   ")
 (pred vertical-wall%   " X " " X " " X "))
