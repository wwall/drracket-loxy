#lang racket

(require xml
         "../lang/lexer/lexer.scm"
         "../rules/lexer-interface.rkt"
         "../rules/loc-rules.rkt")
(require racket/file)

(provide write-to-xml)

(define (write-to-xml file-name rule-name ilist)
  (when (< 0 (length ilist))
    (call-with-output-file file-name
      (lambda (out)
        (fprintf out "<?xml version=\"1.0\"?>~n")
        (write-xml/content (xexpr->xml `(,rule-name '() ,@ilist)) out))
      #:mode 'text
      #:exists  'replace)))

