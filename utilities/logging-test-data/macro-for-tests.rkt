#lang racket

(require (prefix-in rackunit: rackunit)
         rackunit/private/check-info
         syntax/parse/define
         syntax/parse
         syntax/location
         rackunit/log
         (for-syntax racket/syntax)
         )

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

(struct test-info (expr location identifier))

;; display-delimiter (-> void)
(define (display-delimiter)
  (displayln "------------------"))

;; process-check-infos: (-> check-info test-info)
(define (process-check-infos check-infos)
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
  (test-info test-expr test-location test-identifier))


;; print-error: (-> check-info string -> void)
(define (print-error chk-info fail-reason)
  (define tst-info (process-check-infos chk-info))
  (printf "passed?: no~n")
  ;; test-message
  (printf "identifier: ")
  (printf "~a" (test-info-identifier tst-info))
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

(define (error-handler e)
  (cond [(rackunit:exn:test:check? e)
         (print-error (rackunit:exn:test:check-stack e) "test")]
        [(exn? e)
         (print-error (exn-check-info e) "type-checker")]
        [else
         (print-error (current-check-info) "not sure")]))

;; pass-handler (-> Exn void)
(define (pass-handler e)
  (displayln "passed?: yes")
  (printf "identifier: ~a~n" (test-id))
  (display-delimiter))

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
 check-equal?
)

;; OTHERS
#|
(test-posn ...)
-> (parameterize ([test-id ...])
       (test-posn ...))
|#

; (define-syntax-parse-rule (define-parameterizing-test-id (tester-name args ...) body ...)
;        (begin
;         (define (impl args ...)
;               body ...)
;         (define-syntax-parse-rule (tester-name args ...)
;               (parameterize ([test-id (string-append (test-id) (path->string filename) ":" (number->string linenumber) ";")])
;                      (impl args ...)))))

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
         (all-from-out rackunit)
         (all-from-out syntax/location)
         (all-from-out rackunit/private/check-info)
         (all-from-out rackunit/log))