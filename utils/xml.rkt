#lang racket

(require xml
         "../lang/lexer/lexer.scm"
         "../rules/lexer-interface.rkt"
         "../rules/loc-rules.rkt")
(require racket/file)

(provide write-to-xml)


(define (make-file-name dir name suffix ext)
  (build-path (if (null? dir) (current-directory) dir)  (string-append name "." suffix "." ext))
  )

(define (write-to-xml file-name rule-name ilist)
  (call-with-output-file file-name
    (lambda (out)
      (write-xml/content (xexpr->xml `(,rule-name '() ,@ilist)) out))
    #:mode 'text
    #:exists  'replace))



(define (has-extension where x)
  (string-suffix? (path->string where) (string-append "." x)))


(define wrk-dir (string->path "d:/tmp/cf-test/"))

(define (get-file-list path)
  (map (lambda (x) (build-path wrk-dir x)) (filter (lambda (x) (has-extension x "txt")) (directory-list path))))

(file->token-list
 (car (get-file-list wrk-dir)))