#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/log
         rackunit/private/check-info
         "test-macro-for-logging.rkt")

(provide format-tests)

;; TBD: 
;;; - Test what we have so far for code coverage of the three cases
;;; - Change test-case-around
;;; - Test this thoroughly for test-cases, test-suites, and all rackunit functionality

; FORMAT: 
;; module: RESPONSIBILITY OF TEST RUNNER
;; mutant: RESPONSIBILITY OF TEST RUNNER
;; time: number RESPONSIBILITY OF TEST RUNNER
;; passed?: "yes" or "no"
;; identifier:
;; expression:
;; location:
;; fail-reason: ["N/A" "test" "type-checker"]
;; newline

(define-struct (exn:test:pass exn:test) ())

;; continuation-mark-set-parameter-value : Continuation-Mark-Set (Parameterof X) -> X
(module continuation-mark-set-parameter-value racket/base
  (require (only-in '#%paramz parameterization-key))
  (provide continuation-mark-set-parameter-value)
  (define (continuation-mark-set-parameter-value marks param)
    (call-with-parameterization
     (continuation-mark-set-first marks parameterization-key)
     param)))
(require 'continuation-mark-set-parameter-value)

;; exn-check-info : Exn -> (Listof Check-Info)
(define (exn-check-info e)
  (continuation-mark-set-parameter-value (exn-continuation-marks e)
                                         current-check-info))

(struct test-info (expr location identifier))

;; display-delimiter (-> void)
(define (display-delimiter)
  (displayln "------------------"))

;; pass-handler (-> Exn void)
(define (pass-handler e)
  (displayln "passed?: yes")
  (display-delimiter))

;; process-check-infos: (-> check-info test-info)
(define (process-check-infos check-infos)
  (define test-expr "")
  (define test-location "")
  (define test-identifier "")
  (for-each (lambda (chk-info)
              (let ([name (check-info-name chk-info)]
                    [val (check-info-value chk-info)])
                (cond [(equal? name 'expression)
                      (set! test-expr val)]
                      [(equal? name 'location)
                      (set! test-location val)])))
              check-infos)
  (test-info test-expr test-location test-identifier))

;; print-error: (-> check-info string -> void)
(define (print-error chk-info fail-reason)
  (define tst-info (process-check-infos chk-info))
  (printf "passed?: no~n")
  ;; test-message
  (printf "identifier: ")
  (printf "~s" (test-info-identifier tst-info))
  (newline)
  ;; test-expression
  (printf "expression: ")
  (print-info-value (test-info-expr tst-info))
  (newline)
  ;; test-location
  (printf "location: ")
  (print-info-value (test-info-location tst-info))
  (newline)
  ;; fail-reason
  (printf "reason: ~a ~n" fail-reason)
  (display-delimiter))

;; error-handler: (-> Exn void)
(define (error-handler e)
  (cond [(exn:test:check? e)
         (print-error (exn:test:check-stack e) "test")]
        [(exn? e)
         (print-error (exn-check-info e) "type-checker")]
        [else
         (print-error (current-check-info) "not sure")]))

;; format-tests (-> (-> void) void)
(define (format-tests test-thunk)
  (parameterize*
      ([current-check-handler error-handler]
       [current-check-around
        (lambda (chk-thk)
          (define err-handler (current-check-handler))
          (define (log-and-handle-error! e) (test-log! #f) (err-handler e))
          (define (log-and-handle-pass! e) (test-log! #t) (pass-handler e))
          (define (plain-check-around chk-thk) (chk-thk))
          ;; Nested checks should be evaluated as normal functions, to avoid double
          ;; counting test results.
          (parameterize ([current-check-around plain-check-around])
            (with-handlers ([(lambda (e) (exn:test:pass? e)) log-and-handle-pass!]
                            [(λ (_) #t) log-and-handle-error!])
              (print (test-id))
              (chk-thk)
              (raise (exn:test:pass "test passed" (current-continuation-marks))))))]
      ;[current-test-case-around ...]
      )
      (test-thunk)))