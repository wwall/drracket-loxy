#lang racket
; Лексер для языка запросов



(provide
 throw-wtf-exception
 wtf-exception?)

(require rnrs)

(define-condition-type wtf-exception
  &violation
  make-wtf-exception
  wtf-exception?)

(define throw-wtf-exception
  (lambda (message)
    (raise-continuable
     (condition
      (make-wtf-exception)
      (make-message-condition message)))))