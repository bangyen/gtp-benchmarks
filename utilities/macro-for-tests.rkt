#lang racket

(require (prefix-in rackunit: rackunit)
         rackunit/private/check-info
         rackunit/log
         racket/provide
         syntax/parse/define
         syntax/parse
         syntax/location
         (for-syntax racket/syntax)
         (submod "run-tests-on-mutations-of-benchmark-lib.rkt" for-macro))

;; HOW TO USE
;; Merely replace your requirement of rackunit with "macro-for-tests.rkt" in a file, 
;; and bang, you now have a bunch of tests that log raw data. 
;; time: number RESPONSIBILITY OF TEST RUNNER
;; passed?: "yes" or "no"
;; identifier:
;; expression:
;; location:
;; fail-reason: ["N/A" "test" "type-checker"]

;; Creates a parameter that initializes to ""
;; This parameter changes for when a test is run in this program
;; test-id will contain a string in the following format, 
;; "filename:linenumber; ..." that will serve as its identifier
(define test-id (make-parameter ""))

;; RACKUNIT TESTS
#|
(check-equal? ...)
-> (parameterize* ([test-id (string-append (test-id)
quote-source-file
)]
[current-check-handler ...]
[current-check-around ...(referencing test-id)])
(rackunit:check-equal? ...))
|#

(define-struct (exn:test:pass rackunit:exn:test) ())
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

;; display-delimiter (-> void)
(define (display-delimiter)
  (displayln "------------------"))

;; process-check-infos: (-> check-info test-info)
(define (process-check-infos check-infos fail-reason)
  (define mtnt (parameter-for-current-mutant))
  (define mod (parameter-for-current-module))
  (define test-expr "")
  (define test-location "")
  (define test-identifier (test-id))
  (for-each (lambda (chk-info)
              (let ([name (check-info-name chk-info)]
                    [val (check-info-value chk-info)])
                (cond [(equal? name 'expression)
                       (set! test-expr val)]
                      [(equal? name 'location)
                       (set! test-location val)])))
            check-infos)
  (test-info mtnt mod test-expr test-location test-identifier fail-reason))

;; print-error: (-> check-info string -> void)
(define (log-error chk-info fail-reason)
  (define tst-info (process-check-infos chk-info fail-reason))
  (log-message test-data-logger
               'debug
               'test-data
               "FAILURE OR ERROR"
               tst-info))

(define (error-handler e)
  (cond [(rackunit:exn:test:check? e)
         (log-error (rackunit:exn:test:check-stack e) "test")]
        [(exn:fail:syntax? e)
         (log-error (exn-check-info e) "syntax error")]
        [(exn:fail? e)
         (log-error (exn-check-info e) "type error")]
        [else
         (log-error (current-check-info) "not sure")]))

;; pass-handler (-> Exn void)
(define (pass-handler e)
  (log-message test-data-logger
               'debug
               'test-data
               "PASS"
               (test-info (parameter-for-current-mutant)
                          (parameter-for-current-module)
                          ""
                          ""
                          (test-id)
                          "")))

(define-syntax-parse-rule (define-wrapped-rackunit-checks rackunit-check-name:id ...)
  #:with [prefixed-check-name ...] (map (lambda (unprefixed-name)
                                          (format-id this-syntax
                                                     "rackunit:~a"
                                                     unprefixed-name))
                                        (attribute rackunit-check-name))
  (begin
    (define-syntax-parse-rule (rackunit-check-name args (... ...))
      #:with dummy-that-gets-the-right-line (datum->syntax this-syntax '(quote-line-number) this-syntax this-syntax)
      #:with dummy-that-gets-the-right-loc (datum->syntax this-syntax '(quote-source-file) this-syntax this-syntax)
      (let-values ([(_0 filename _1) (split-path dummy-that-gets-the-right-loc)]
                   [(linenumber) dummy-that-gets-the-right-line])
        (parameterize* ([test-id (string-append (test-id) (path->string filename) ":" (number->string linenumber) ";")]
                        [rackunit:current-check-handler error-handler]
                        [rackunit:current-check-around
                         (lambda (chk-thk)
                           (define err-handler (rackunit:current-check-handler))
                           (define (log-and-handle-error! e) (test-log! #f) (err-handler e))
                           (define (log-and-handle-pass! e) (test-log! #t) (pass-handler e))
                           (define (plain-check-around chk-thk) (chk-thk))
                           ;; Nested checks should be evaluated as normal functions, to avoid double
                           ;; counting test results.
                           (parameterize ([rackunit:current-check-around plain-check-around])
                             (with-handlers ([(lambda (e) (exn:test:pass? e)) log-and-handle-pass!]
                                             [(λ (_) #t) log-and-handle-error!])
                               (chk-thk)
                               (raise (exn:test:pass "test passed" (current-continuation-marks))))))])
          (prefixed-check-name args (... ...)))))                      
    ...))

(define-wrapped-rackunit-checks
  check-exn
  check-true
  check-false
  check-equal?
  check-eq?)

;; OTHERS
#|
(test-posn ...)
-> (parameterize ([test-id ...])
(test-posn ...))
|#

(define-syntax (define-parameterizing-test-id stx)
  (syntax-parse stx
    [(_ (tester-name args ...) body ...)
     #'(begin
         (define (impl args ...)
           body ...)
         (define-syntax-parse-rule (tester-name args ...)
           #:with dummy-that-gets-the-right-loc (datum->syntax this-syntax '(quote-source-file) this-syntax this-syntax)
           #:with dummy-that-gets-the-right-line (datum->syntax this-syntax '(quote-line-number) this-syntax this-syntax)
           (let-values ([(_0 filename _1) (split-path dummy-that-gets-the-right-loc)]
                        [(linenumber) dummy-that-gets-the-right-line])
             (parameterize ([test-id (string-append (test-id) (path->string filename) ":" (number->string linenumber) ";")])
               (impl args ...)))))]
    [(_ id expr)
     #'(define id expr)]))

(provide (all-defined-out)
         (rename-out [define-parameterizing-test-id define])
         (filtered-out
          (lambda (name)
            (substring name 9))
          (except-out (all-from-out rackunit)
                      rackunit:check-exn
                      rackunit:check-true
                      rackunit:check-false
                      rackunit:check-equal?
                      rackunit:check-eq?))
         (all-from-out syntax/location)
         (all-from-out rackunit/private/check-info)
         (all-from-out rackunit/log))
