#lang racket
(provide get-rules
          lexer-rule
          add-rule
          list-rules
          apply-rules
          get-result)
;;; список правил анализатора токенов
(define rule-hash (make-hash))

(define (get-rules)
  rule-hash)

;;;; интерфейс который должен реализовать клас правила лексера
(define lexer-rule (interface () init-rule get-description put-token get-result end-work))

(define (add-rule name rule)
  (hash-set! rule-hash name rule))

(define (list-rules)
  (hash-for-each rule-hash (lambda (k v) (printf "~nИд правила : {~a} ~nОписание : {~a}" k (send v get-description)))))

(define (apply-rules token-list)
  (hash-map rule-hash
     (lambda (k v) (list k (send v init-rule ))))
  
  (for-each (lambda (y)
              (hash-map rule-hash
                        (lambda (k v) (list k (send v put-token y))))) token-list))
(define (get-result rule-id)	
	(send (hash-ref rule-hash rule-id) get-result))
