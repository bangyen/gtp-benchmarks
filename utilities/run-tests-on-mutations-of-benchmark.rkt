#lang racket



(require (submod "run-tests-on-mutations-of-benchmark-lib.rkt" main)
         racket/cmdline
         )

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
