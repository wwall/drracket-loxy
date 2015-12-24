#lang racket 
(require 
  rackunit
  "../lang/lexer/lexer.scm"
  "../rules/lexer-interface.rkt"
  "../rules/loc-rules.rkt")

(test-case
 "Проверка создания правила"
 [check-equal? (hash-count (get-rules)) 1])


(define bad-text-one "function x()
function y()
")

(define goot-text-one "function x()
1
2
3
EndFunction
")

(define goot-text-two "function x()
1
2
3
EndFunction
function y()
1
2
3
EndFunction

")


(define (do-rule text)
  (apply-rules (string->token-list text)) (get-result 'rule-LOC))

(test-case
 "Проверка работы правила"
 [check-equal? (do-rule goot-text-one) '((:id "x" :len 4))]
 [check-equal? (do-rule goot-text-two) '((:id "x" :len 4) (:id "y" :len 4))]
 [check-equal? (do-rule  bad-text-one) '()])

