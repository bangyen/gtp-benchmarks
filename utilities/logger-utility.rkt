#lang racket


(require racket/runtime-path)

(provide test-data-logger
         test-data-receiver
         parameter-for-current-module
         parameter-for-current-mutant
         log-test-data-debug
         (struct-out test-info))

(struct test-info (mtnt mod test-expr test-loc test-id fail-reason))

;;; DEFINE LOGGER / PARAMETERS FOR GATHERING DATA
(define-logger test-data)
(define parameter-for-current-module (make-parameter ""))
(define parameter-for-current-mutant (make-parameter ""))
(define test-data-receiver (make-log-receiver test-data-logger 'debug 'test-data))
(define-runtime-path logging-test-data-file "./logging-test-data/test-data.txt")

;; print-error: (-> check-info string -> void)
(define (print-error tst-inf out)
  (fprintf out "mutant: ")
  (fprintf out "~a~n" (test-info-mtnt tst-inf))

  (fprintf out "module: ")
  (fprintf out "~a~n" (test-info-mod tst-inf))
  
  (fprintf out "passed: no~n")
  ;; test-message
  (fprintf out "identifier: ")
  (fprintf out "~a~n" (test-info-test-id tst-inf))
  ;; test-expression
  (fprintf out "expression: ")
  (fprintf out "~a~n" (test-info-test-expr tst-inf))
  ;; test-location
  (fprintf out "location: ")
  (fprintf out "~a~n" (test-info-test-loc tst-inf))
  ;; fail-reason
  (fprintf out "reason: ~a~n" (test-info-fail-reason tst-inf))
  (fprintf out "~n"))

;; pass-handler (-> Exn void)
(define (print-pass tst-inf out)
  (fprintf out "mutant: ")
  (fprintf out "~a~n" (test-info-mtnt tst-inf))

  (fprintf out "module: ")
  (fprintf out "~a~n" (test-info-mod tst-inf))
  
  (fprintf out "passed: yes~n")
  (fprintf out "identifier: ~a~n" (test-info-test-id tst-inf))
  (fprintf out "~n")
  )

;; create a thread
(void
 (thread
  (lambda ()
    (let loop ()
      (define v (sync test-data-receiver))
      (define test-out (open-output-file logging-test-data-file #:exists 'append))
      ;; (display (format "~a~n" (test-info-test-id (vector-ref v 2))) test-out)
      (define tst-inf (vector-ref v 2))
      (if (equal? (test-info-fail-reason tst-inf) "")
          (print-pass tst-inf test-out)
          (print-error tst-inf test-out))
      (close-output-port test-out)
      (loop)))))
