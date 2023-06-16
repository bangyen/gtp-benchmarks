#lang at-exp racket

(require bex/configurations/config
         bex/configurations/configure-benchmark
         ; bex/configurables/configurations/module-types
         racket/runtime-path
         racket/file)

;; TODO: 
;; generate a benchmark-configuration
;; files in untyped & typed are copied according to the config

;; benchmarks-path: path of the benchmarks directory in gtp-benchmarks
(define-runtime-path benchmarks-path "../benchmarks")
(define-runtime-path config-path "tests-configuration.rkt")
(require bex/configurables/configurables)
(install-configuration! config-path)

;; run-benchmark-tests: string config
;; copy pastes all the necessary benchmark modules and associated tests according to 
;; the config and runs the tests
(define (run-benchmark-tests bench-string config)
    ;; generate a benchmark struct
    (define bench-path (build-path benchmarks-path bench-string))
    (define bench (read-benchmark bench-path))
    
    ;; generate a benchmark config
    (define bench-config (configure-benchmark bench config))

    ;; make a new path called test-env where tests are run
    (define test-env (build-path bench-path "test-env"))
    (make-directory test-env)
    
    ;; copy all files in "tests"
    (copy-files-in-a-directory (build-path bench-path "tests")
                                test-env)

    ;; copy all files in "both"
    ;; don't need this because it's in the bench-config
    ;; (copy-files-in-a-directory (build-path bench-path "both")
    ;;                             test-env)

    ;; benchmark->mutable-modules
    ;; properly create the hash map
    ;; copy all files in the config

    ;; (delete-directory/files test-env)
    )

(define (copy-files-in-a-directory src-dir dest-dir)
    (for ([src-file (directory-list src-dir)]
            #:when (file-exists? (build-path src-dir src-file)))
        (copy-file (build-path src-dir src-file) 
                    (build-path dest-dir src-file))))

(run-benchmark-tests "zombie" (hash "image.rkt" 'none
                                    "main.rkt" 'none
                                    "math.rkt" 'none
                                    "zombie.rkt" 'none))
