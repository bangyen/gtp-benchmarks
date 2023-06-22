#lang racket

(require syntax/parse
         mutate ; provides both mutate/define and mutate/quick
         "simple.rkt"
         "read-module.rkt"
         racket/stream)
 
(define program-mutations
  (build-mutation-engine
   #:mutators
   (define-simple-mutator (if-swap stx)
     #:pattern ({~datum if} cond t e)
     #'(if cond e t))
   (define-constant-mutator (constant-swap v)
     [(? number?) #:-> (- v)])
   #:syntax-only
   #:streaming
   #:module-mutator))
 
(define program-to-mutate
  (read-module "simple.rkt"))

(map syntax->datum (stream->list (program-mutations program-to-mutate)))