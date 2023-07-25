#lang racket

(provide func1
         func2
         func3)

(define (func1 x) (if #t 1 x))

(define (func2 x) (if #t x 10))

(define (func3 x y) (if (zero? x) y 10))
