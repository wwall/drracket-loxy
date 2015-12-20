#lang racket 
(require 
  rackunit
  "../lang/lexer/keywords.scm"
  "../lang/lexer/lexer.scm")


(define lex->list
  (lambda (s)
    (maybe-only-id! #f)
    (let* ([buf (open-input-string (string-append s " "))]
           [lexer (construct-lexer buf )])
      (let loop ((results '()))
        (let ((next-token   (lexer)))
          (if (null? (token-type next-token))
              (reverse results)
              (loop (cons  (list (token-type next-token) (token-value next-token)) results))))))))


(test-case
 "Проверка ключевых слов"
 [check-equal? (get-keyword-symbol "to" 'tknID) 'lxmTO]
 [check-equal? (begin (maybe-only-id! #t) (get-keyword-symbol "to" 'tknID)) 'tknID])

(test-case
 "Проверка лексера"
 [check-equal? (lex->list "_enLexeme12") '((tknID "_enLexeme12"))]
 [check-equal? (lex->list "русская_Лексема") '((tknID "русская_Лексема"))]
 [check-equal? (lex->list "1") '((tknNUMBER "1"))]
 [check-equal? (lex->list "1.2") '((tknNUMBER "1.2"))]
 [check-equal? (lex->list "\"as\"") '((tknSTRING "as"))]
 [check-equal? (lex->list "to") '((lxmTO "to"))]
 [check-equal? (lex->list "x.data") '((tknID "x") (lxmDOT ".") (tknID "data"))]
 [check-equal? (lex->list "\"строка с экранированной кавычкой \"\"\"") '((tknSTRING "строка с экранированной кавычкой \"\""))]
 [check-equal? (map (lambda (x) (car x)) (lex->list "( ) = >= <= < > <> . , ; + - * /"))
               '(lxmOPEN lxmCLOSE lxmEQU lxmGT-EQU lxmLT-EQU lxmLT lxmGT lxmNOT-EQU lxmDOT lxmCOMMA lxmSEMI lxmPLUS lxmMINUS lxmMUL lxmDIV)]
 [check-equal? (lex->list "// commant 1
                           data
                           //comment 2
                           for ") '((tknID "data") (lxmFOR "for"))]
 [check-equal? (lex->list "// commant 1
                           data
                           //comment at end") '((tknID "data"))]
 
 [check-equal? (lex->list "перем var") '((lxmVAR "перем") (lxmVAR "var"))])
