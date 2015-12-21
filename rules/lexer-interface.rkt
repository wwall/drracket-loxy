#lang racket


(provide (all-defined-out))

;;; список правил анализатора токенов
(define rule-hash (make-hash))


(define (get-rule-list)
  rule-hash)

(define-struct rule-struct
  (name func time description))

(define (full-list)
  'strategy-LIST)


(define (by-token)
  'by-token)

(define (add-rule name func time description)
  (hash-set! rule-hash name (make-rule-struct name func time description)))


(define (list-rules)
  (hash-for-each rule-hash (lambda (k v) (printf "~nИд правила : {~a} ~nОписание : {~a}" k (rule-struct-description v)))))

  
(define (apply-rules token-list)
  (hash-map rule-hash (lambda (k v) (list k ((rule-struct-func v) token-list null)))))
