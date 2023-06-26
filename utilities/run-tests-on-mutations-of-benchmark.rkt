#lang at-exp racket

(require syntax/parse
         bex/configurations/config
         bex/configurations/configure-benchmark
         bex/configurables/configurables
         racket/runtime-path
         racket/file
         racket/cmdline
         whereis
         mutate
         racket/stream
         "read-module.rkt")

;; 1. generate the mutations
;; 2. write first mutation to disk
;; 3. run the tests
;; 4. repeat steps 2-4 

;;; SETUP RUNTIME PATHS
;; benchmarks-path: path of the benchmarks directory in gtp-benchmarks
(define-runtime-path benchmarks-path "../benchmarks")

;; GENERATE BENCHMARK-CONFIGURATION

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
;; program-mutations: mutation-engine
;; add mutators to create more or different mutators per module
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
   #:module-mutator
   ))


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

    ;; Generate program mutations: use get-mutant-hash
    (define mutants (get-mutant-hash mutatable-modules))
    ;; delete all the files
    (for ([file (directory-list test-env #:build? #t)])
        (delete-file file))
    
    ;;; COPY TEST FILES
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

    ; ;; loop: put a mutant on disk, run tests, delete the mutants, repeat
    ; ;; do this for ONE MUTANT FIRST
    ; ;; write-mutant-to-disk: hash string number path-string? -> void
    ; ;; (define (write-mutant-to-disk mutant-hash mod mutation-index dest)
    ;; each mutatable module
    (for ([mod mutatable-modules])
        ; backup the module
        ; ...
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
                    (system* (whereis-system 'exec-file) (build-path test-env test-env-file)))
            )
        ; get the original module back
        ; ...
        )
    
    ;; single file test
    ; (write-mutant-to-disk mutants "zombie.rkt" 5 (build-path test-env "zombie.rkt"))
    ; (length (hash-ref mutants "zombie.rkt"))

    ; (define out (open-output-file (build-path bench-path "mutation3-of-zombie.rkt")))
    ; (pretty-write (list-ref (hash-ref mutants "zombie.rkt") 3) out)
    ; (close-output-port out)
    ; (set! out (open-output-file (build-path bench-path "mutation15-of-zombie.rkt")))
    ; (pretty-write (list-ref (hash-ref mutants "zombie.rkt") 15) out)
    ; (close-output-port out)

    ;; clean up directory
    (delete-directory/files test-env)
    )

;; COMMAND LINE PARSING & RUN SCRIPT
;; command line parsing
;; it parses the command line and returns a list of format:
;; string config
(define parser
    (command-line
     #:program "run-benchmark-tests"
     #:args(benchmark-string benchmark-config)
     (list benchmark-string benchmark-config)))

(run-tests-on-mutations (first parser)
                        (second parser))
