#lang racket
(require "../lang/lexer/lexer.scm"
         "lexer-interface.rkt"
         srfi/1)



(define warn-rule%
  (class* object% (lexer-rule)
    (init warnlistinit)
    (define warnlist warnlistinit)
    (define result (list))
    (super-new)
    (define/public (init-rule)
      (set! result (list)))
    (define/public (put-token token)
      (when (member (token-type token) warnlist)
        (set! result (append result (list (token-id token))))))
    
    (define/public (get-result)
      (when (< 0 (length result))
        (map (lambda (x) (list 'rule-warn '()  (list 'id '() (number->string x)))) result)))
    (define/public (get-description)
      (with-output-to-string
       (lambda ()
       (printf "Указывает индексы токенов с типами токенов ~a" warnlist))))
    (define/public (end-work)
      (set! result null))))



(add-rule 'rule-WARN-pos (new warn-rule% (warnlistinit (list 'lxmEVAl 'lxmEXECUTE) )))
          
          