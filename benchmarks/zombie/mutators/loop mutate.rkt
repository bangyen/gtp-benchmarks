#lang racket

(require syntax/parse
         mutate ; provides both mutate/define and mutate/quick
         "simple.rkt"
         "code/math.rkt"
         "read-module.rkt"
         racket/stream
         racket/runtime-path
         mutate/mutators/code
         whereis)

(define (selector stx)
  (syntax-parse stx
    [({~datum :} _ ...) #f]
    [other (list (list #'other) 'tmp first)]))

(define program-mutations
  (build-mutation-engine
   #:mutators
   arithmetic-op-swap
   boolean-op-swap
   comparison-op-swap
   negate-conditionals
   force-conditionals
   replace-constants/type-level
   replace-constants/similar
   swap-arguments
   delete-begin-result-expr
   begin-drop
   data-accessor-swap
   nested-list-construction-swap
   class-method-publicity-swap
   delete-super-new
   add-extra-class-method
   replace-class-parent
   swap-class-initializers
   #:top-level-selector
   selector
   #:syntax-only
   #:streaming
   #:module-mutator))

(define-runtime-path actual-working-dir "math.rkt/../")
(current-directory actual-working-dir)

(define program-to-mutate
  (read-module "code/math.rkt"))

; create mutants
(define mutants (map syntax->datum (stream->list (program-mutations program-to-mutate))))
(define x (length mutants))
(display (string-append "We've made " (number->string x) " mutants\n"))
(define second-mutant (second mutants))

;loop through mutants
;variable storing current mutant working on
(define cur-mutant #f)
;the file that stores mutant
(define cur-out #f)
;variable storing total number of mutants successfully identifed
(define test-fail 0)
(for ([i x])
  (display (string-append "Running Mutant" (number->string (+ i 1)) "\n"))
  ;get current mutant to work on
  (set! cur-mutant (list-ref mutants i))
  (set! cur-out (open-output-file "curmutant.rkt"))
  (pretty-write cur-mutant cur-out)
  (close-output-port cur-out)
  ;run test on mutant, increment test-fail if test gives error or fail.
  (if (parameterize ([current-output-port (open-output-nowhere)]
                     [current-error-port (open-output-nowhere)]) (system* (whereis-system 'exec-file) (whereis-raco "test") "tests/test-math.rkt"))
      (display (string-append "Mutant " (number->string (+ i 1)) " not identified, " (number->string test-fail) " in total\n"))
      (begin (set! test-fail (+ test-fail 1))
             (display (string-append "Mutant " (number->string (+ i 1)) " identified, " (number->string test-fail) " in total\n"))))
  ;delete and clean for next round
  (delete-file "curmutant.rkt"))

(define res (exact->inexact (/ test-fail x)))

(display (string-append (number->string test-fail) " mutants identified out of " (number->string x) ", mutation score of " (number->string res)))