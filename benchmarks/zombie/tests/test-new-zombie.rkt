#lang racket

(require rackunit
         rackunit/text-ui
         (submod "../untyped/zombie.rkt" test)
         ;(submod "zombie.rkt" test)
         "../untyped/image.rkt"
         ;"image.rkt"
         "test-image.rkt"
         "test-new-posn.rkt"
         )

(provide test-posn
         posn-equal?
         algo-move-toward
         )

;; UTILITIES
;; zombie-equal?: Zombie Zombie -> Boolean
(define (zombie-equal? zombie-1 zombie-2)
  (posn-equal? ((zombie-posn zombie-1))
               ((zombie-posn zombie-2))))
;; tests for zombie-equal? 
; (check-true (zombie-equal? (new-zombie (new-posn 0 0))
;                            (new-zombie (new-posn 0 0))))
; (check-false (zombie-equal? (new-zombie (new-posn 0 1))
;                            (new-zombie (new-posn 0 0))))

; algo-move-toward-for-zombie - takes a posn and a zombie, and returns a new zombie
; Zombie Posn -> Zombie
; from-zombie: the original zombie
; to-posn: the posn the original zombie is moving towards
(define (algo-move-toward-for-zombie from-zombie to-posn)
  ;; get from-posn
  (define from-posn ((zombie-posn from-zombie)))
  ;; define speed
  (define speed ZOMBIE-SPEED)
  ;; create the new zombie
  (new-zombie (algo-move-toward from-posn to-posn speed)))
;; test zombie-algo-move-toward
; (check-true (zombie-equal? (algo-move-toward-for-zombie (new-zombie (new-posn 0 0))
;                                                   (new-posn 50 60))
;                            (new-zombie (new-posn 0 ZOMBIE-SPEED))))
; (check-false (zombie-equal? (algo-move-toward-for-zombie (new-zombie (new-posn 0 0))
;                                                         (new-posn 40 30))
;                            (new-zombie (new-posn 0 ZOMBIE-SPEED))))

;; is-zombie-really-touching?: Zombie Posn -> Boolean
(define (is-zombie-really-touching? z p)
  ;; zombie's posn
  (define z-posn ((zombie-posn z)))
  ;; dist
  (<= ((posn-dist p) z-posn) ZOMBIE-RADIUS))
;; tests for is-zombie-really-touching?
; (check-true (is-zombie-really-touching? (new-zombie (new-posn 0 0)) 
;                                         (new-posn ZOMBIE-RADIUS 0)))
; (check-false (is-zombie-really-touching? (new-zombie (new-posn 0 0))
;                                          (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS)))

;; place-zombie-image-check Zombie string? scene -> void
;; checks whether a given zombie z draws properly with
;; a given color c and scene s 
(define (place-zombie-image-check z c s)
  (define z-posn ((zombie-posn z)))
  (place-image-check (lambda () ((zombie-draw-on/color z) c s))
                      (circle ZOMBIE-RADIUS "solid" c)
                      ((posn-x z-posn))
                      ((posn-y z-posn))
                      s
                      ""))

;; test-zombie: (-> Zombie) Real Real String -> Void
;; generates and runs a test suite with message msg, 
;; testing the result of z-fxn against the Zombie 
;; created with position (x, y)
(define (test-zombie z-fxn x y msg)
  (define result (z-fxn))
  (define posn-touching-z ((posn-move ((zombie-posn result))) ZOMBIE-RADIUS 0))
  (define posn-not-touching-z ((posn-move ((zombie-posn result))) ZOMBIE-RADIUS ZOMBIE-RADIUS))
  (test-suite
   msg
   ;; tests
   ;; posn
   (posn-equal? ((zombie-posn result))
                (new-posn x y))
   ;; draw-on/color
  ;  (place-image-check (lambda () ((zombie-draw-on/color result) "red" MT-SCENE))
  ;                     (circle ZOMBIE-RADIUS "solid" "red")
  ;                     x
  ;                     y
  ;                     MT-SCENE
  ;                     "")
  (place-zombie-image-check result "red" MT-SCENE)

   ;; touching?
   (check-eq? (is-zombie-really-touching? result posn-touching-z)
              ((zombie-touching? result) posn-touching-z))
   (check-eq? (is-zombie-really-touching? result posn-not-touching-z)
              ((zombie-touching? result) posn-not-touching-z))           
   ;; move-toward
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn 0 0))
                              ((zombie-move-toward result) (new-posn 0 0))))
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn -300000 -40000))
                              ((zombie-move-toward result) (new-posn -300000 -40000))))
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn 30000 30000))
                              ((zombie-move-toward result) (new-posn 30000 30000))))
   ))


(define testing-test-zombie (test-zombie (lambda () (new-zombie (new-posn 0 0)))
                                         0 0 ""))
; (run-tests testing-test-zombie)

;; place-images-recursively: List (Any -> Image) string? Image -> Image
;; helper for place-zombies-image-check
(define (place-images-recursively l i-fxn x-fxn y-fxn s)
  (if (empty? l)
      s
      (local [(define obj (first l))]
        (place-image (i-fxn obj)
                    (x-fxn obj)
                    (y-fxn obj)
                    (place-images-recursively (rest l)
                                              i-fxn
                                              x-fxn
                                              y-fxn
                                              s)))))
;; place-zombies-image-check: Zombies (listof zombie) string image -> void
;; checks whether a list of zombies zs (should incl. mt-zombie) 
;; draws properly with color c and scene s
(define (place-zombies-image-check actual-zs zs c s)
  (define img (place-images-recursively zs (lambda (z)
                                              (circle ZOMBIE-RADIUS "solid" c))
                                           (lambda (z)
                                              ((posn-x ((zombie-posn z)))))
                                           (lambda (z)
                                              ((posn-y ((zombie-posn z)))))
                                           s))
  (test-image-equal ((zombies-draw-on/color actual-zs) c s)
                    img))

;; zombies-really-touching?: (listof zombie) posn -> Boolean 
;; discerns whether a list of zombie zs is touching a
;; posn p
(define (zombies-really-touching? zs p)
  (cond [(empty? zs)
         #f]
        [(is-zombie-really-touching? (first zs) p)
         #t]
        [else
         (zombies-really-touching? (rest zs) p)]))


;; convert-list-to-cons-zombie: (listof zombie) -> Zombies
(define (convert-list-to-cons-zombie list-zs)
  (if (empty? list-zs)
      (new-mt-zombies)
      (begin
        ; (printf "~s" (first list-zs))
        (new-cons-zombies (first list-zs)
                (convert-list-to-cons-zombie (rest list-zs))))))


;; equal-images: Image Image -> Boolean
(define (equal-images? img1 img2)
  (unless (image? img1)
    (error "img1 not an image"))
  (unless (image? img1)
    (error "img2 not an image"))
  (cond
    ; both lists
    [(and (list? (image-impl img1))
          (list? (image-impl img2)))
     (cond
       ; equal length
       [(= (length (image-impl img1))
           (length (image-impl img2)))
        (define l (length (image-impl img1)))
        (define bool #t)
        (for ([a (in-list (image-impl img1))]
              [b (in-list (image-impl img2))])
          (cond [(and (image? a) (image? b))
                (unless (equal-images? a b)
                  (set! bool #f))]
                [(and (not (image? a))
                      (not (image? b)))
                (unless (equal? a b)
                  (set! bool #f))]
                [else (set! bool #f)]))
        bool]
       ; unequal length
       [else (fail "lists are not equal length")]
       )]
    ; both cons and not lists
    [(and (cons? (image-impl img1))
          (cons? (image-impl img2))
          (not (list? (image-impl img1)))
          (not (list? (image-impl img2))))   
     (and 
      (equal? (car (image-impl img1))
                    (car (image-impl img2)))
      (equal? (cdr (image-impl img1))
                    (cdr (image-impl img2))))]
    ; not both cons or both lists
    [else (fail "image-impl not both cons or not both lists")]
  ))

(check-false (equal-images? (place-image (circle ZOMBIE-RADIUS "solid" "red")
                                        100
                                        100
                                        MT-SCENE)
                           (place-image (circle ZOMBIE-RADIUS "solid" "yellow")
                                        100
                                        100
                                        MT-SCENE)))
(check-true (image? (circle ZOMBIE-RADIUS "solid" "red")))
(check-true (list? (image-impl (circle ZOMBIE-RADIUS "solid" "red"))))

;; equal-zombies?: Zombies Zombies -> Boolean
;; if the images produced by zs1 and zs2 on 
;; an empty scene are the same, then
;; they are equal
(define (equal-zombies? zs1 zs2)
  (equal-images? ((zombies-draw-on/color zs1) "red" MT-SCENE)
                 ((zombies-draw-on/color zs2) "red" MT-SCENE)))
;; test for equality
; (check-true (equal-zombies? (new-mt-zombies) (new-mt-zombies)))
; (check-true (equal-zombies? (new-cons-zombies (new-zombie (new-posn 0 0))
;                                               (new-mt-zombies))
;                             (new-cons-zombies (new-zombie (new-posn 0 0))
;                                               (new-mt-zombies))))
;; test convert-list-to-cons-zombie
; (define real-zombie-list 
;           (new-cons-zombies (new-zombie (new-posn 0 0))
;                             (new-cons-zombies (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
;                                               (new-cons-zombies (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))
;                                                                 (new-mt-zombies)))))
; (define fake-zombie-list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
;                              (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))))
; (check-true (equal-zombies? real-zombie-list
;                             (convert-list-to-cons-zombie fake-zombie-list)))

;; kill-all-zombies: (listof zombie) (listof zombie) -> (listof (listof zombie))
;; kills all the zombies in undead-zombies if they
;; come in contact with dead-zombies without using 
;; higher-order recursive functions to test zombie 
;; functions properly
(define (kill-all-zombies undead-zombies dead-zombies)
  (cond [(empty? undead-zombies)
         (list undead-zombies dead-zombies)]
        [(or (zombies-really-touching? (rest undead-zombies) 
                                       ((zombie-posn (first undead-zombies))))
             (zombies-really-touching? dead-zombies
                                       ((zombie-posn (first undead-zombies)))))
         (kill-all-zombies (rest undead-zombies) (cons (first undead-zombies) dead-zombies))]
        [else
         (define res (kill-all-zombies (rest undead-zombies) dead-zombies))
         (list (cons (first undead-zombies)
                     (first res))
               (second res))]))

;; equal-hordes? Horde Horde -> Boolean
;; approximates equality by checking if
;; the images produced by the Hordes
;; on an empty scene are equal
(define (equal-hordes? h1 h2)
  (equal-images? ((horde-draw-on h1) MT-SCENE)
          ((horde-draw-on h2) MT-SCENE)))
;; test for equality
; (check-true (equal-hordes? (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
;                                                         (new-mt-zombies))
;                                       (new-mt-zombies))
;                            (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
;                                                         (new-mt-zombies))
;                                       (new-mt-zombies))))

;; test-new-mt-zombies
(define (test-new-mt-zombies z-fxn msg)
  (define result (z-fxn))
  (define real-zombie-list 
          (new-cons-zombies (new-zombie (new-posn 0 0))
                            (new-cons-zombies (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                                              (new-cons-zombies (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))
                                                                (new-mt-zombies)))))
  (define fake-zombie-list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                             (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))))
  (define h-list (kill-all-zombies '() fake-zombie-list))
  (define h (new-horde (convert-list-to-cons-zombie (first h-list))
                      (convert-list-to-cons-zombie (second h-list))))
  (define h2-list (kill-all-zombies fake-zombie-list '()))
  (define h2 (new-horde (convert-list-to-cons-zombie (first h2-list))
                        (convert-list-to-cons-zombie (second h2-list))))
  (test-suite
   msg
   ;; move-toward check
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn 0 0)) (new-mt-zombies)))
   ;; draw-on/color check
   (place-zombies-image-check result '() "red" MT-SCENE)
   ;; touching?
   (check-equal? (zombies-really-touching? '() (new-posn 0 0))
                   ((zombies-touching? result) (new-posn 0 0)))
   ;; kill-all
   (check-true (equal-hordes? h ((zombies-kill-all result) real-zombie-list)))
   (check-false (equal-hordes? h ((zombies-kill-all real-zombie-list) result)))
   (check-true (equal-hordes? h2 ((zombies-kill-all real-zombie-list) result)))
   ))

;; algo-move-toward-for-zombies: (listof zombie) -> (listof zombie)
(define (algo-move-toward-for-zombies zs p)
  (if (empty? zs)
      empty
      (cons (algo-move-toward-for-zombie (first zs) p)
            (algo-move-toward-for-zombies (rest zs) p))))

;; test-new-cons-zombies: z-fxn (listof zombie) -> test-suite
(define (test-new-cons-zombies z-fxn zs msg)
  ;; define result
  (define result (z-fxn))
  ;; define important local variables
  ;; test suite
  (define real-zombie-list 
          (new-cons-zombies (new-zombie (new-posn 0 0))
                            (new-cons-zombies (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                                              (new-cons-zombies (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))
                                                                (new-mt-zombies)))))
  (define fake-zombie-list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                             (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))))
  (define h-list (kill-all-zombies zs fake-zombie-list))
  (define h (new-horde (convert-list-to-cons-zombie (first h-list))
                      (convert-list-to-cons-zombie (second h-list))))
  (define h2-list (kill-all-zombies fake-zombie-list zs))
  (define h2 (new-horde (convert-list-to-cons-zombie (first h2-list))
                        (convert-list-to-cons-zombie (second h2-list))))
  (test-suite
   msg
   ;; test move-toward
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn 0 0))
                               (convert-list-to-cons-zombie 
                                (algo-move-toward-for-zombies zs (new-posn 0 0)))))
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn -500 -500))
                               (convert-list-to-cons-zombie 
                                (algo-move-toward-for-zombies zs (new-posn -500 -500)))))
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn 100 1400))
                               (convert-list-to-cons-zombie 
                                (algo-move-toward-for-zombies zs (new-posn 100 1400)))))
   ;; test draw-on-color
   (place-zombies-image-check result zs "red" MT-SCENE)
   ;; test touching?
   (check-equal? (zombies-really-touching? zs (new-posn 0 0))
                 ((zombies-touching? result) (new-posn 0 0)))
   (check-equal? (zombies-really-touching? zs (new-posn -500 -500))
                 ((zombies-touching? result) (new-posn -500 -500)))
   (check-equal? (zombies-really-touching? zs (new-posn 300 300))
                 ((zombies-touching? result) (new-posn 300 300)))
   ;; test kill-all
   (check-true (equal-hordes? h ((zombies-kill-all result) real-zombie-list)))
   (check-false (equal-hordes? h ((zombies-kill-all real-zombie-list) result)))
   (check-true (equal-hordes? h2 ((zombies-kill-all real-zombie-list) result)))
   ))

;; test-new-cons-zombies
(run-tests (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-cons-zombies (new-zombie (new-posn 300 300))
                                                              (new-mt-zombies))))
                       (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 300 300)))
                       ""))


;; algo-horde-move-toward: (listof (listof zombie)) Posn -> (listof (listof zombie))
;; h: '(undead dead)
;; pos: posn
;; undead zombies in the horde h are moving towards posn pos
(define (algo-horde-move-toward h pos)
  (list (algo-move-toward-for-zombies (first h) pos) 
        (second h)))

;; test-new-horde: lambda (listof (listof zombie)) string -> test-suite
;; h-fxn: function that creates a new horde
;; h-list: '(dead undead)
;; msg: test-suite message
(define (test-new-horde h-fxn h-list msg)
  (define result (h-fxn))
  (define expected-dead-scene (place-images-recursively (second h-list)
                                                        (lambda (z)
                                                          (circle ZOMBIE-RADIUS "solid" "black"))
                                                        (lambda (z)
                                                          ((posn-x ((zombie-posn z)))))
                                                        (lambda (z)
                                                          ((posn-y ((zombie-posn z)))))
                                                        MT-SCENE))

  (define move-toward-1 (algo-horde-move-toward h-list (new-posn 0 0)))
  (define move-toward-2 (algo-horde-move-toward h-list (new-posn 450 -100000)))
  (define kill-all-1 (kill-all-zombies (first h-list) (second h-list)))
  (test-suite
   msg
   ;; tests
   ;; dead
   (check-true (equal-zombies? ((horde-dead result))
                               (convert-list-to-cons-zombie (second h-list))))
   ;; undead
   (check-true (equal-zombies? ((horde-undead result))
                               (convert-list-to-cons-zombie (first h-list))))
   ;; draw-on
   (check-true (equal-images? 
          ;; result image
          ((horde-draw-on result) MT-SCENE)
          ;; h-list image
          (place-images-recursively (first h-list)
                                    (lambda (z)
                                      (circle ZOMBIE-RADIUS "solid" "yellow"))
                                    (lambda (z)
                                      ((posn-x ((zombie-posn z)))))
                                    (lambda (z)
                                      ((posn-y ((zombie-posn z)))))
                                    expected-dead-scene)
          ))
   ;; move-toward
   (check-true (equal-hordes? ((horde-move-toward result) (new-posn 0 0))
                              (new-horde (convert-list-to-cons-zombie (first move-toward-1))
                                          (convert-list-to-cons-zombie (second move-toward-1)))))
                  
   (check-true (equal-hordes? ((horde-move-toward result) (new-posn 450 -100000))
                 (new-horde (convert-list-to-cons-zombie (first move-toward-2))
                            (convert-list-to-cons-zombie (second move-toward-2)))))
   ;; kill-all
   (check-true (equal-hordes? ((horde-eat-brains result))
                              (new-horde (convert-list-to-cons-zombie (first kill-all-1))
                                         (convert-list-to-cons-zombie (second kill-all-1)))))
                  ))


;; TESTS
;; tests for new-zombie
(define new-zombie-test-suite
  (test-suite
    "test for new-zombie"
    ;; tests using test-zombie
    (test-zombie (lambda () (new-zombie (new-posn 0 0))) 0 0 "")
    (test-zombie (lambda () (new-zombie (new-posn 500 300))) 500 300 "")
    (test-zombie (lambda () (new-zombie (new-posn -100 100))) -100 100 "")
    (test-zombie (lambda () (new-zombie (new-posn -30000 -500))) -30000 -500 "")
    (test-zombie (lambda () (new-zombie (new-posn 500 -500))) 500 -500 "")
    ;; test for wrong inputs
    (check-exn exn:fail? (lambda () ((new-zombie ""))))
    ;; (check-exn exn:fail? (lambda () ((zombie-posn (new-posn 0 0)))))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color "red") "red" 5)))
    (check-exn exn:fail? (lambda () ((zombie-touching? (new-posn 5 5)))))
    (check-exn exn:fail? (lambda () ((zombie-move-toward (new-zombie (new-posn 0 0))))))
    ))

(define new-cons-zombies-test-suite
  (test-suite "test for new-cons-zombies"
   ;; tests using test-new-cons-zombies
   (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-cons-zombies (new-zombie (new-posn 300 300))
                                                              (new-mt-zombies))))
                       (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 300 300)))
                       "")
                       
    (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn -1000 -100))
                                            (new-cons-zombies (new-zombie (new-posn 300 300))
                                                              (new-mt-zombies))))
                       (list (new-zombie (new-posn -1000 -100)) (new-zombie (new-posn 300 300)))
                       "")
    (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn -1000 -100))
                                            (new-mt-zombies)))
                       (list (new-zombie (new-posn -1000 -100)))
                       "")
    (test-new-cons-zombies (lambda ()
                              (new-cons-zombies (new-zombie (new-posn -1000 -100))
                                            (new-mt-zombies)))
                             (list (new-zombie (new-posn -1000 -100)))
                       "")
    (test-new-cons-zombies (lambda ()
                              (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-mt-zombies)))
                             (list (new-zombie (new-posn 0 0)))
                       "")
    (test-new-cons-zombies (lambda ()
                              (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-cons-zombies (new-zombie (new-posn 3000 -300))
                                                              (new-cons-zombies (new-zombie (new-posn 3000 -300))
                                                                                (new-mt-zombies)))))
                             (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 3000 -300)) (new-zombie (new-posn 3000 -300)))
                       "")
    ;; test for wrong inputs
    (check-exn exn:fail? (lambda () ((new-cons-zombies ""))))
    ;; (check-exn exn:fail? (lambda () ((zombie-posn (new-posn 0 0)))))
    (check-exn exn:fail? (lambda () ((zombies-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombies-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombies-draw-on/color "red") "red" 5)))
    (check-exn exn:fail? (lambda () ((zombies-touching? (new-posn 5 5)))))
    (check-exn exn:fail? (lambda () ((zombies-move-toward (new-zombie (new-posn 0 0))))))
    ))

(define new-horde-test-suite
  (test-suite
   "tests for new-horde"
   ;; tests
   ;; horde with empty undead, filled dead
   (test-new-horde (lambda ()
                 (new-horde (new-mt-zombies)
                            (new-cons-zombies (new-zombie (new-posn 0 0))
                                              (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                (new-mt-zombies)))))
               (list (list)
                     (list (new-zombie (new-posn 0 0))
                           (new-zombie (new-posn 100 5000))))
               "")
   
   ;; horde with filled undead, empty dead
   (test-new-horde (lambda ()
                    (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
                                                  (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                    (new-mt-zombies)))
                                (new-mt-zombies)))
                  (list (list (new-zombie (new-posn 0 0))
                              (new-zombie (new-posn 100 5000)))
                        (list))
               "")
   ;; horde with empty, empty
   (test-new-horde (lambda ()
                    (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
                                                  (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                    (new-mt-zombies)))
                                (new-mt-zombies)))
                  (list (list (new-zombie (new-posn 0 0))
                              (new-zombie (new-posn 100 5000)))
                        (list))
               "")
   ;; horde with dead overlapping
   (test-new-horde (lambda ()
                      (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
                                                    (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                      (new-mt-zombies)))
                                 (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                   (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                                     (new-mt-zombies)))))
                   (list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 100 5000)))
                         (list (new-zombie (new-posn -1000 -1000)) (new-zombie (new-posn -1000 -1000))))
                   "")
  ;  ;; horde with undead overlapping
  (test-new-horde (lambda ()
                      (new-horde (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                   (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                                     (new-mt-zombies)))
                                 (new-cons-zombies (new-zombie (new-posn 0 0))
                                                    (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                      (new-mt-zombies)))))
                   (list (list (new-zombie (new-posn -1000 -1000)) (new-zombie (new-posn -1000 -1000)))
                         (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 100 5000))))
                   "")
   ;; wrong input: None of these raise errors.
   ; (check-exn exn:fail? (lambda () (new-horde "" "")))
   ; (check-exn exn:fail? (lambda () ((horde-draw-on (new-horde (new-mt-zombies) (new-mt-zombies))) "")))
   ; (check-exn exn:fail? (lambda () ((horde-touching? (new-horde (new-mt-zombies) (new-mt-zombies))) "")))
   ; (check-exn exn:fail? (lambda () ((horde-move-toward (new-horde (new-mt-zombies) (new-mt-zombies))) "")))
   ))

;; run test suites
(run-tests new-zombie-test-suite)
(run-tests (test-new-mt-zombies (lambda () (new-mt-zombies)) ""))
(run-tests new-cons-zombies-test-suite)
(run-tests new-horde-test-suite)