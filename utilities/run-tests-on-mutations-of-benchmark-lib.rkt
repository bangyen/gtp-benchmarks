#lang at-exp racket

;; INSTRUCTIONS:
;; In command line: "racket run-tests-on-mutations-of-benchark <benchmark-name> <bench-config>"
;; <benchmark-name>: what it sounds like ("zombie", "dungeon", etc.)
;; <bench-config>: a binary integer that represents the configuration of typed and untyped modules.
;; - Imagine the modules in a benchmark are sorted lexicographically. Take Zombie for instance.
;; - The first module is "image.rkt." The second is "main.rkt." The third is "math.rkt". The fourth,
;; "zombie.rkt."
;; - <bench-config> is a representation of whether you want to leave each module typed, or untyped.
;; In binary, 0 is untyped. 1 is typed. If you wanted to leave every module untyped except for zombie,
;; the number 0001 would generate that configuration.
;;
;; PREREQUISITES:
;; 1. Clone Lukas's "blame-evaluation-gt" repo
;; 2. Install...
;; - "bex" - it's a directory inside blame-evaluation-gt
;; - "whereis" - a package that tells you where things are

(require syntax/parse
         bex/configurations/config
         bex/configurations/configure-benchmark
         bex/configurables/configurables
         racket/runtime-path
         racket/file
         whereis
         mutate
         mutate/mutators/code
         racket/stream
         "read-module.rkt"
         "logger-utility.rkt")

(module+ for-macro
  (provide (struct-out test-info)
           test-data-logger
           print-error
           print-pass))

(module+ main
  (provide run-tests-on-mutations))


;;; SETUP RUNTIME PATHS
;; benchmarks-path: path of the benchmarks directory in gtp-benchmarks
(define-runtime-path benchmarks-path "../benchmarks")

;;; GENERATE BENCHMARK-CONFIGURATION
;; necessary to install the proper configuration
;; relies on requiring the package bex/configurables/configurables
(define-runtime-path config-path "tests-configuration.rkt")
(install-configuration! config-path)
;; create-benchmark-configuration: path-string? string -> benchmark-configuration
(define (create-benchmark-configuration bench config-string)
  ;; generate a config
  (define config (deserialize-config (string->number config-string) #:benchmark bench))
  ;; generate a benchmark config
  (configure-benchmark bench config))

;;; GENERATE MUTATIONS
(define (selector stx)
  (syntax-parse stx
    [({~datum :} _ ...) #f]
    [other (list (list #'other) 'tmp first)]))


;; program-mutations: mutation-engine
;; add mutators to create more or different mutators per module
(define program-mutations
  (build-mutation-engine
   #:mutators
   ; Lukas's ICFP 2021 Paper: Type-Level Mutators
   ; constant
   replace-constants/type-level
   ; deletion
   ; delete-begin-result-expr
   ; position
   ; swap-arguments
   ; list
   ; nested-list-construction-swap
   ; top-level-id 
   ; make-top-level-id-swap-mutator (not a mutator/c, a function that produces a mutator/c)
   ; imported-id
   ;; not in Racket Mutate docs (but present in Lukas's blame-evaluation-gt repo)
   ; method-id
   ; make-method-id-swap-mutator (not a mutator/c, a function that produces a mutator/c)
   ; field-id
   ; make-field-id-swap-mutator (not a mutator/c, a function that produces a mutator/c)
   ; class:init
   ; swap-class-initializers
   ; class:parent
   ; replace-class-parent
   ; class:public
   ; class-method-publicity-swap
   ; class:super
   ; delete-super-new
   ; arithmetic
   ; arithmetic-op-swap
   ; boolean
   ; boolean-op-swap
   ; negate-cond
   ; negate-conditionals
   ; force-cond
   ; force-conditionals
   #:top-level-selector
   selector
   #:syntax-only
   #:streaming
   #:module-mutator))

;; get-mutant-hash: (listof string?) -> hash (string -> (listof syntax-object))
(define (get-mutant-hash program-path-strings)
  (define programs-to-mutate
    (map read-module program-path-strings))
  ;; create mutants
  (define mutants (make-hash))
  (for ([p programs-to-mutate]
        [program-name program-path-strings])
    (hash-set! mutants
               program-name
               (map syntax->datum (stream->list (program-mutations p)))))
  mutants)


;;; WRITE MUTATIONS TO DISK
;; write-mutant-to-disk: hash string number path-string? -> void
(define (write-mutant-to-disk mutant-hash mod mutation-index dest)
  (define out (open-output-file dest))
  (pretty-write (list-ref (hash-ref mutant-hash mod) mutation-index) out)
  (close-output-port out))

(define (writeln-to-test-out str)
  (define test-out (open-output-file logging-test-data-file
                                     #:exists 'append))
  (fprintf test-out (string-append str "~n"))
  (close-output-port test-out))


;;; config -> dir
(define (copy-modules-into-a-directory bench-config target-dir)
  ;; copy main
  (copy-file (benchmark-configuration-main bench-config) (build-path target-dir "main.rkt"))
  ;; copy others
  (for ([src-file (benchmark-configuration-others bench-config)])
    (copy-file src-file
               (build-path target-dir (file-name-from-path src-file)))))

;; log-mutant-data
(define (log-mutant-data mod i killed?)
  (log-message mutant-data-logger
               'debug
               ""
               (mutant-info i mod killed?)))

;;; RUN TESTS
;; run-tests-on-mutations: string string -> void
(define (run-tests-on-mutations bench-string config-string)
  ;;; SETUP
  ;; get the benchmark path
  (define bench-path (build-path benchmarks-path bench-string))
  ;; get a benchmark struct
  (define bench (read-benchmark bench-path))
  ;; create benchmark-configuration
  (define bench-config (create-benchmark-configuration bench config-string))
  ;; make a new path called test-env where tests are run
  (define test-env (build-path bench-path "test-env"))
  (make-directory test-env)
  (current-directory test-env)
  ;; get strings of mutatable-modules
  (define mutatable-modules (benchmark->mutatable-modules bench))
  ;; copy all the modules into a new directory
  (copy-modules-into-a-directory bench-config test-env)
  ; Generate program mutations: use get-mutant-hash
  (define mutants (get-mutant-hash mutatable-modules))
  ;; put hash on disk
  (define hash-out (open-output-file (build-path bench-path
                                                 "../../utilities/logging-test-data/mutant-hash")))
  (write mutants hash-out)
  (close-output-port hash-out)
  ;; get the number of mutants to compute mutation score later
  (define number-of-mutants 0)
  ;; delete all the files
  (for ([file (directory-list test-env #:build? #t)])
    (delete-file file))
  ;; copy all files in "tests" and add their names to a list
  (define test-dir (build-path bench-path "tests"))
  (define test-file-names '())
  (for ([test-file (directory-list test-dir)]
        #:when (file-exists? (build-path test-dir test-file)))
    (copy-file (build-path test-dir test-file)
               (build-path test-env test-file))
    (set! test-file-names (cons test-file test-file-names)))
  ;; copy the module files again
  (copy-modules-into-a-directory bench-config test-env)
  ;; the loop loads a mutant in test-env, runs the tests, deletes the mutants, and repeats.
  (for ([mod mutatable-modules])
    (when (equal? mod "math.rkt")
    (putenv "MODULE" mod)
    ; backup the module
    (copy-file (build-path test-env mod) (build-path test-env (string-join (list "--" mod))))
    ; current module
    ; generate the mutants of mod, and run the tests on them
    (for ([i (in-range (length (hash-ref mutants mod)))])
      (putenv "MUTANT" (number->string i))
      ; mutate the module
      (define module-path (build-path test-env mod))
      (delete-file module-path)
      (write-mutant-to-disk mutants mod i module-path)
      ; run tests
      (define identified? #f)
      (for ([test-env-file (directory-list test-env)]
            #:when (and (file-exists? (build-path test-env test-env-file))
                        (path-has-extension? (build-path test-env test-env-file) ".rkt")
                        (member test-env-file test-file-names)))
        (current-directory test-env)
        (when (parameterize ([current-output-port (open-output-nowhere)])
                (system* (whereis-system 'exec-file)
                         (whereis-raco "test")
                         (build-path test-env test-env-file)))
          (set! identified? #t)))
      (log-mutant-data mod i identified?))
    ; get the original module back
    (delete-file (build-path test-env mod))
    (copy-file (build-path test-env (string-join (list "--" mod))) (build-path test-env mod))
    (delete-file (build-path test-env (string-join (list "--" mod))))))
  ;; clean up directory
  (delete-directory/files test-env))
