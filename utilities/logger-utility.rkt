#lang racket


(require racket/runtime-path)

(provide test-data-logger
         test-data-receiver
         mutant-data-logger
         mutant-data-receiver
         ;; parameter-for-current-module
         ;; parameter-for-current-mutant
         log-test-data-debug
         (struct-out test-info)
         (struct-out mutant-info)
         logging-test-data-file
         print-error
         print-pass)

(module+ parse-info
  (provide (struct-out test-info)
           (struct-out mutant-info)))

(struct
  test-info
  (mtnt mod test-expr test-loc test-id fail-reason start-time finish-time)
  #:prefab)

(struct
  mutant-info
  (mtnt mod killed?)
  #:prefab)

;;; DEFINE LOGGER / PARAMETERS FOR GATHERING DATA
(define-logger test-data)
(define test-data-receiver (make-log-receiver test-data-logger 'debug 'test-data))
(define-logger mutant-data)
(define mutant-data-receiver (make-log-receiver mutant-data-logger 'debug 'mutant-data))

(define-runtime-path logging-test-data-file "./logging-test-data/test-data.txt")
(define-runtime-path logging-test-info-data-file "./logging-test-data/test-data-info")
(define-runtime-path logging-mutant-info-data-file "./logging-test-data/mutant-info-data-file")

;; print-error: (-> check-info string -> void)
(define (print-error tst-inf out)
  (fprintf out "passed: no~n")
  ;; test-message
  (fprintf out "module: ")
  (fprintf out "~a~n" (test-info-mod tst-inf))
  (fprintf out "mutant: ")
  (fprintf out "~a~n" (test-info-mtnt tst-inf))
  ;; test-id
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
  ;; start-time
  (fprintf out "start-time: ~a~n" (test-info-start-time tst-inf))
  (fprintf out "finish-time: ~a~n" (test-info-finish-time tst-inf))
  (fprintf out "~n")
  )

;; pass-handler (-> Exn void)
(define (print-pass tst-inf out)  
  (fprintf out "passed: yes~n")
  (fprintf out "module: ")
  (fprintf out "~a~n" (test-info-mod tst-inf))
  (fprintf out "mutant: ")
  (fprintf out "~a~n" (test-info-mtnt tst-inf))
  (fprintf out "identifier: ~a~n" (test-info-test-id tst-inf))
  (fprintf out "start-time: ~a~n" (test-info-start-time tst-inf))
  (fprintf out "finish-time: ~a~n" (test-info-finish-time tst-inf))
  (fprintf out "~n")
  )

;; create a thread
(void
 (thread
  (lambda ()
    (let loop ()
      (define v (sync test-data-receiver))
      (define test-out (open-output-file logging-test-data-file #:exists 'append))
      (define tst-inf (vector-ref v 2))
      (if (equal? (test-info-fail-reason tst-inf) "")
          (print-pass tst-inf test-out)
          (print-error tst-inf test-out))
      (close-output-port test-out)

      (define test-info-out (open-output-file logging-test-info-data-file #:exists 'append))
      (write tst-inf test-info-out)
      (close-output-port test-info-out)
      (loop)))))

(void
 (thread
  (lambda ()
    (let loop ()
      (define v (sync mutant-data-receiver))
      (define mtnt-out (open-output-file logging-mutant-info-data-file #:exists 'append))
      (write (vector-ref v 2) mtnt-out)
      (close-output-port mtnt-out)
      (loop)))))
