#lang racket

(require racket/runtime-path
         (submod "logger-utility.rkt" parse-info))

(define-runtime-path mutant-data "./logging-test-data/mutant-info-data-file")

(define (compute-mutation-score)
  (define in (open-input-file mutant-data))
  (define mutants-killed 0)
  (define total-mutants 0)
  (let loop ([val (read in)])
    (unless (equal? val eof)
      (set! total-mutants (+ total-mutants 1))
      (when (mutant-info-killed? val)
          (set! mutants-killed (+ mutants-killed 1)))
      (set! val (read in))
      (loop val)))
  (exact->inexact (/ mutants-killed total-mutants))
  )
(compute-mutation-score)
