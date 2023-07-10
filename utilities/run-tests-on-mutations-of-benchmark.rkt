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
;; 
;; POTENTIAL CHANGES:
;; 1. Refine our set of mutators (include mutators from 2020, ICFP 2021, ICFP 2023)
;; 2. Calculate a mutation score for the test suite for a benchmark, or compute other data


(require syntax/parse
         bex/configurations/config
         bex/configurations/configure-benchmark
         bex/configurables/configurables
         racket/runtime-path
         racket/file
         racket/cmdline
         whereis
         mutate
         mutate/mutators/code
         racket/stream
         "read-module.rkt")

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
   arithmetic-op-swap
   boolean-op-swap
;    comparison-op-swap
;    negate-conditionals
;    force-conditionals
;    replace-constants/type-level
;    replace-constants/similar
;    swap-arguments
;    delete-begin-result-expr
;    begin-drop
;    data-accessor-swap
;    nested-list-construction-swap
;    class-method-publicity-swap
;    delete-super-new
;    add-extra-class-method
;    replace-class-parent
;    swap-class-initializers
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
    (copy-file (benchmark-configuration-main bench-config) (build-path test-env "main.rkt"))
    ;; copy others
    (for ([src-file (benchmark-configuration-others bench-config)])
        (copy-file src-file
                    (build-path test-env (file-name-from-path src-file))))
    ; Generate program mutations: use get-mutant-hash
    (define mutants (get-mutant-hash mutatable-modules))
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
    (copy-file (benchmark-configuration-main bench-config) (build-path test-env "main.rkt"))
    ;; copy others
    (for ([src-file (benchmark-configuration-others bench-config)])
        (copy-file src-file
                    (build-path test-env (file-name-from-path src-file))))
    ;; number of mutants the test suite successfully kills
    (define mutants-killed 0)
    ;; the loop loads a mutant in test-env, runs the tests, deletes the mutants, and repeats.
    (for ([mod mutatable-modules])
        ; backup the module
        (copy-file (build-path test-env mod) (build-path test-env (string-join (list "--" mod))))
        ; generate the mutants of mod, and run the tests on them
        (for ([i (in-range (length (hash-ref mutants mod)))])
            ; mutate the module
            (define module-path (build-path test-env mod))
            (delete-file module-path)
            (write-mutant-to-disk mutants mod i module-path)
            ; run tests
            (for ([test-env-file (directory-list test-env)]
                    #:when (and (file-exists? (build-path test-env test-env-file))
                                (path-has-extension? (build-path test-env test-env-file) ".rkt")
                                (member test-env-file test-file-names)))
                    (current-directory test-env)
                    (set! number-of-mutants (+ number-of-mutants 1))
                    (if (parameterize ([current-output-port (open-output-nowhere)]
                                       [current-error-port (open-output-nowhere)])
                            (system* (whereis-system 'exec-file) (whereis-raco "test") (build-path test-env test-env-file)))
                        ;; then, mutant not identified
                        (displayln "Mutant not identified")
                        ;; else, mutant identified
                        (begin (set! mutants-killed (+ mutants-killed 1))
                               (displayln "Mutant identified")))))
        ; get the original module back
        (delete-file (build-path test-env mod))
        (copy-file (build-path test-env (string-join (list "--" mod))) (build-path test-env mod))
        (delete-file (build-path test-env (string-join (list "--" mod)))))
    ;; clean up directory
    (delete-directory/files test-env)
    ;; display mutation score
    (define res (exact->inexact (/ mutants-killed number-of-mutants)))
    (displayln (string-append (number->string mutants-killed) " mutants identified out of " (number->string number-of-mutants) ", mutation score of " (number->string res))))

;; COMMAND LINE PARSING & RUNNING THE SCRIPT
;; parser: parses the command line and returns a list of format: string string
;; the first string is the benchmark, like "zombie"
;; the second string in the nonnegative integer that represents the config, like "1100"
(define parser
    (command-line
     #:program "run-benchmark-tests"
     #:args(benchmark-string benchmark-config)
     (list benchmark-string benchmark-config)))
(run-tests-on-mutations (first parser)
                        (second parser))