#lang racket

(require xml
         "../lang/lexer/lexer.scm"
         "../rules/lexer-interface.rkt"
         "../rules/loc-rules.rkt"
         "../rules/loc-eval.rkt"
         "xml.rkt")
(require racket/file)


(define (rewrite-path base rest)
  (if  (empty? rest)
       base
       (begin
         (if (not (directory-exists? (build-path base (car rest))))
             (make-directory (build-path base (car rest)))
             null)
         (rewrite-path (build-path base (car rest)) (cdr rest) ))))


(define (mk-filename from to name)
  (let-values ([(f s t) (split-path from)])
    (build-path (rewrite-path to (remove "txt" (string-split (path->string s) "."))) (string-append name "." "xml") )))


(define (has-extension where x)
  (string-suffix? (path->string where) (string-append "." x)))


(define (get-file-list path)
  (map (lambda (x) (build-path wrk-dir x)) (filter (lambda (x) (has-extension x "txt")) (directory-list path))))


(define (token-to-xml x)
  (list 'token
        (list 'line (number->string  (token-line x)))
        (list 'column (number->string  (token-column x)))
        (list 'type (symbol->string (token-type x)))
        (list 'id (number->string (token-id x)))
        (list 'value (if (symbol? (token-value x)) (symbol->string (token-value x)) (token-value x)))))




(define (write-tokens tf out-dir token-list)
  (let ([fname (mk-filename tf out-dir "tokens")])
    (write-to-xml fname 'Tokens (map token-to-xml token-list)
                  )))




(define wrk-dir (string->path "e:/tmp/cf-test/"))

(define out-dir (string->path "e:/tmp/result/"))


(define tf (car (get-file-list wrk-dir)))


(define (lexer-and-token file out-dir)
  (let ([token-list (file->token-list file)])
    (printf "Tokens count = ~a~n" (length token-list))
      (write-tokens file out-dir token-list)
      (apply-rules token-list)
      (hash-map (get-rules)
                (lambda (k v)
                  (let ([nf (mk-filename file out-dir (symbol->string k))]
                        [result (get-result k)])
                    (when (list? result)
                      (write-to-xml nf k result)))))))

(define (lexer-and-token-dir in-dir out-dir)
  (let ([files (get-file-list in-dir)])
    (for-each (lambda (x)
                  (printf "Process ~a. " x)
                (lexer-and-token x out-dir))
              files)
    (printf "End work. Processed ~a files" (length files) )))


(provide lexer-and-token-dir)