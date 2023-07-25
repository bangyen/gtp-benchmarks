#lang racket

(require syntax/parse
         mutate ; provides both mutate/define and mutate/quick
         "simple.rkt"
         "read-module.rkt"
         racket/stream
         racket/runtime-path
         mutate/mutators/code
         whereis)
 
(define program-mutations
  (build-mutation-engine
   #:mutators
   (define-simple-mutator (if-swap stx)
     #:pattern ({~datum if} cond t e)
     #'(if cond e t))
   (define-constant-mutator (constant-swap v)
     [(? number?) #:-> (- v)])
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
   #:syntax-only
   #:streaming
   #:module-mutator))

(define-runtime-path actual-working-dir "simple mutate.rkt/../")
(current-directory actual-working-dir)

(define program-to-mutate
  (read-module "simple.rkt"))

; create mutants
(define mutants (map syntax->datum (stream->list (program-mutations program-to-mutate))))
(define second-mutant (second mutants))

; output the mutant
(define out (open-output-file "secondmutant.rkt"))
(pretty-write second-mutant out)
(close-output-port out)

; generate the file that calls the mutant
(set! out (open-output-file "runmutant.rkt"))
(pretty-display "(module runmutant racket (require \"secondmutant.rkt\") (func1 50))" out)
(close-output-port out)

; call the mutant with runmutant.rkt
(unless (system* (whereis-system 'exec-file) "runmutant.rkt")
  (error "calling mutant threw an error"))

; clean up the directory
(delete-file "secondmutant.rkt")
(delete-file "runmutant.rkt")