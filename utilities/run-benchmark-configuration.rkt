#lang at-exp racket

;; INSTRUCTIONS:
;; To run benchmark tests, you must do the following:
;; 1. Clone Lukas's "blame-evaluation-gt" repo
;; 2. Install...
;; - "bex" - it's a directory inside blame-evaluation-gt
;; - "whereis" - a package that tells you where things are
;; Then, type in "racket run-benchmark-configuration.rkt <benchmark> <config-number>". 
;; <benchmark>: names of any of the gtp-benchmarks
;; <config-number>: a binary integer that represents the configuration of typed and untyped modules.
;; - Imagine the modules in a benchmark are sorted lexicographically. Take Zombie for instance.
;; - The first module is "image.rkt." The second is "main.rkt." The third is "math.rkt". The fourth,
;; "zombie.rkt."
;; - <config-number> is a representation of whether you want to leave each module typed, or untyped. 
;; In binary, 0 is untyped. 1 is typed. If you wanted to leave every module untyped except for zombie,
;; the number 0001 would generate that configuration.

;; TODO: I have yet to test this thoroughly

(require bex/configurations/config
         bex/configurations/configure-benchmark
         racket/runtime-path
         racket/file
         racket/cmdline
         whereis)

;; command line parsing
;; it parses the command line and returns a list of format:
;; string config
(define parser
    (command-line
     #:program "run-benchmark-tests"
     #:args(benchmark-string benchmark-config)
     (list benchmark-string benchmark-config)))

;; benchmarks-path: path of the benchmarks directory in gtp-benchmarks
(define-runtime-path benchmarks-path "../benchmarks")

;; necessary to install the proper configuration
(define-runtime-path config-path "tests-configuration.rkt")
(require bex/configurables/configurables)
(install-configuration! config-path)

;; run-benchmark-tests: string integer -> void
;; copy pastes all the necessary benchmark modules and tests according to 
;; the config and runs the tests
(define (run-benchmark-tests bench-string config-string)
    ;; generate a benchmark struct
    (define bench-path (build-path benchmarks-path bench-string))
    (define bench (read-benchmark bench-path))
    
    ;; generate a config
    (define config (deserialize-config (string->number config-string) #:benchmark bench))
    ;; generate a benchmark config
    (define bench-config (configure-benchmark bench config))
    
    ;; make a new path called test-env where tests are run
    (define test-env (build-path bench-path "test-env"))
    (make-directory test-env)
    
    ;; copy main
    (copy-file (benchmark-configuration-main bench-config) (build-path test-env "main.rkt"))
    ; (create-test-module (build-path test-env "main.rkt"))
    
    ;; copy others
    (for ([src-file (benchmark-configuration-others bench-config)])
        (copy-file src-file
                    (build-path test-env (file-name-from-path src-file)))
        ; (create-test-module (build-path test-env (file-name-from-path src-file)))
        )
    
    ;; copy all files in "tests" and add their names to a list
    (define test-dir (build-path bench-path "tests"))
    (define relative-paths-of-test-files '())
    (for ([test-file (directory-list test-dir)]
            #:when (file-exists? (build-path test-dir test-file)))
        (copy-file (build-path test-dir test-file) 
                    (build-path test-env test-file))
        (set! relative-paths-of-test-files (cons test-file relative-paths-of-test-files)))
    
    ;; run all the tests
    (for ([test-env-file (directory-list test-env)]
            #:when (and (file-exists? (build-path test-env test-env-file))
                        (path-has-extension? (build-path test-env test-env-file) ".rkt")
                        (member test-env-file relative-paths-of-test-files)))
        (system* (whereis-system 'exec-file) (build-path test-env test-env-file)))
    ;; cleanup directory
    (delete-directory/files test-env)
    )

(define (copy-files-in-a-directory src-dir dest-dir)
    (for ([src-file (directory-list src-dir)]
            #:when (file-exists? (build-path src-dir src-file)))
        (copy-file (build-path src-dir src-file) 
                    (build-path dest-dir src-file))))

; (define (create-test-module filepath)
;     (define out (open-output-file filepath
;                           #:exists 'append))
;     (displayln "(module+ test (provide (all-defined-out)))" out)
;     (close-output-port out))

(run-benchmark-tests (first parser)
                     (second parser))