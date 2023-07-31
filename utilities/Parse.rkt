#lang racket

(struct module-record
  (module mutants-pass mutants-fail mutant-num score stime ftime))

(struct mutant-record
  (module mutant result identifer stime ftime))

(define input-data
  (string-split (port->string (open-input-file "logging-test-data/test-data.txt")) "\n"))

(define cur-module "init")
(define cur-mutant "init")
(define mutants-p '())
(define mutants-f '())
(define result #t)
(define start-time 0)
(define finish-time 0)
(define first #t)
(define pass 0)
(define fail 0)

(define module-list '())

(define (new-module module)
  (if (not (eq? cur-module "init"))
      (begin (if result
                 (begin (set! mutants-p (cons cur-mutant mutants-p))
                        (set! pass (+ pass 1)))
                 (begin (set! mutants-f (cons cur-mutant mutants-f))
                        (set! fail (+ fail 1))))
             (set! result #t)
             (cond [(not (equal? cur-module module))
                    (begin (set! module-list (cons (module-record cur-module mutants-p mutants-f (+ pass fail) (/ pass (+ pass fail)) start-time finish-time) module-list))
                           (set! first #t)
                           (set! mutants-p '())
                           (set! mutants-f '())
                           (set! pass 0)
                           (set! fail 0))]))
      (displayln "Parsing Begins")))

(define (line-op text)
  (cond
    [(string-prefix? text "MODULE: ") (begin (new-module (substring text 8 (string-length text)))
                                             (set! cur-module
                                                   (substring text 8 (string-length text))))]
    [(string-prefix? text "MUTANT: ") (set! cur-mutant
                                            (substring text 8 (string-length text)))]
    [(string-prefix? text "passed: ") (cond [(eq? 10 (string-length text)) (set! result #f)])]
    [(string-prefix? text "identifer: ") (set! result result)]
    [(string-prefix? text "start-time: ") (cond [first (begin (set! first #f)
                                                              (set! start-time (string->number (substring text 12 (string-length text)))))])]
    [(string-prefix? text "finish-time: ") (set! finish-time (string->number (substring text 13 (string-length text))))]))

(for ([i input-data])
  (line-op i))

(new-module "Finish")

(for ([i module-list])
  (displayln (format "Module: ~a" (module-record-module i)))
  (displayln (format "Mutants that passed: ~a" (module-record-mutants-pass i)))
  (displayln (format "Mutants that failed: ~a" (module-record-mutants-fail i)))
  (displayln (format "Number of Mutants: ~a" (module-record-mutant-num i)))
  (displayln (format "Mutation Score: ~a" (module-record-score i)))
  (displayln (format "Time Module Started: ~a" (module-record-stime i)))
  (displayln (format "Time Module Finished: ~a" (module-record-ftime i)))
  (newline))



