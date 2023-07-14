#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/log
         rackunit/private/check-info)

;; TBD: 
;;; - Reformat pass messages
;;; - Reformat error messages
;;; - Change test-case-around
;;; - Test this thoroughly for test-cases, test-suites, and all rackunit functionality

; FORMAT: 
;; module: RESPONSIBILITY OF TEST RUNNER
;; mutant: RESPONSIBILITY OF TEST RUNNER
;; test-expression:  
;; test-location:  
;; result: ["pass" or "fail"]
;; fail-reason: ["N/A" "test" "type-checker"]
;; time: number (responsibility of test runner?)
;; newline

; check info types
; (define-check-type name any/c)
; (define-check-type params any/c #:wrapper pretty-info)
; (define-check-type location location/c #:wrapper location-info)
; (define-check-type expression any/c #:wrapper verbose-info)
; (define-check-type message any/c)
; (define-check-type actual any/c #:wrapper pretty-info)
; (define-check-type expected any/c #:wrapper pretty-info)

;; dummy struct
(define-struct (exn:test:check:passed exn:test:check) ())

(define (pass-handler e)
  (define (process-check-info chk-info)
    (define name (check-info-name chk-info))
    (define value (check-info-value chk-info))
    ;; print test expression and location
    (cond [(equal? name 'expression)
           (displayln (string-append "test-expression: " (info-value->string value)))]
        ;   [(equal? name 'name)
        ;    (displayln (string-append "name: " (symbol->string value)))]
          [(equal? name 'location)
           (displayln (string-append "test-location: " (info-value->string value)))]
           ))
  (for-each process-check-info (current-check-info))
  ;; print test result
  (displayln "result: pass")
  ;; print fail-reason
  (displayln "fail-reason: N/A")
  ; (newline)
)

(parameterize*
    (
     ;[current-check-handler ...] // Error messages also have to be reformatted
     [current-check-around
      (lambda (chk-thk)
        (define error-handler (current-check-handler))
        (define (log-and-handle-error! e) (test-log! #f) (println (exn:test:check-stack e)) (error-handler e))
        (define (log-and-handle-pass! e) (test-log! #t) (println (exn:test:check-stack e)) (pass-handler e))
        (define (plain-check-around chk-thk) (chk-thk))
        ;; Nested checks should be evaluated as normal functions, to avoid double
        ;; counting test results.
        (parameterize ([current-check-around plain-check-around])
          (with-handlers ([exn:test:check:passed? log-and-handle-pass!]
                          [(λ (_) #t) log-and-handle-error!])
            (chk-thk)
            (raise (make-exn:test:check:passed "test passed" 
                                               (current-continuation-marks) 
                                               (current-check-info))))))]
     ;[current-test-case-around ...]
     )
    (define x "message in variable")
    (check-equal? (min 5.0 6.0) 5.1 x)
    (check-equal? (min 5.0 6.0) 5.0 x)
   )