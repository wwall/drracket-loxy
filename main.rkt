#lang racket

(require "utils/filefunction.rkt"
         "lang/lexer/lexer.scm"
         "rules/lexer-interface.rkt"
         "rules/loc-rules.rkt"
         "rules/loc-eval.rkt"
         
         )
(provide main)

(define *version* "0.0.1")

(define *work-dir* #f)
(define *output-dir* #f)

(define *show-rules* #f)

(define (main args)
  (process-args args)
  (if *show-rules*
      (list-rules)
      (if (and *work-dir* *output-dir*)
          (if (directory-exists? *work-dir*)
              (if (directory-exists? *output-dir*)
                  (lexer-and-token-dir *work-dir* *output-dir*)
                  (printf ("Output directory ~a not exists" *output-dir*)))
              (printf ("input directory ~a not exists" *work-dir*)))
          (display-help))))

(define (process-args args)
  (if (member "--version" args)
      (display-version)
      (if (null? args)
          (begin
            (display-version)
            (newline)
            (display-help))
          (parse-args args))))


(define (parse-args args)
  (command-line
   #:program "loxy"
   #:argv args
   
   #:help-labels "loxy options:"
   
   #:once-each
   ("-i"  dir
          "input dir"
          (set! *work-dir* (if (equal? (string-ref dir (- (string-length dir) 1)) #\/)
                               dir
                               (string-append dir "/"))))
   ("-o" dir
         "output dir"
         (set! *output-dir* (if (equal? (string-ref dir (- (string-length dir) 1)) #\/)
                                dir
                                (string-append dir "/"))))
   ("--list-rules"  
    "show list of exists rules"
    (set! *show-rules* #t))

    
   #:help-labels "Misc options:"

   #:once-each
   ("--debug" "Activates debug information"
    (void))
   ("--version" "Prints version number and copyright notice"
    (void))


   ))

(define (display-version)
  (printf "loxy v ~a~n" *version*))


(define (display-help)
  (parse-args '("--help")))


