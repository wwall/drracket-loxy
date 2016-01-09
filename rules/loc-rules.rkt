#lang racket
(require "../lang/lexer/lexer.scm"
         "lexer-interface.rkt"
         srfi/1)


(struct rule-struct
  (id value))
(define loc-rule%
  (class* object% (lexer-rule)
    (init start end)
    (define start-token start)
    (define end-token end)
    (define result (list))
    (define token-list (list))
    
    (super-new)
    (define/private (calc-data last-token)
      (let* [(start-token (find (lambda (x) (eq? start-token (token-type x))) token-list))
            (after (drop-while (lambda (x) (eq? start-token (token-type x))) token-list))
            (id (find (lambda (x) (eq? 'tknID (token-type x))) after))
            (name (token-value id))
            (len (- (token-line last-token) (token-line start-token)))]
        (set! token-list (list))
      (rule-struct  name   len )))

      
    (define/public (init-rule)
      (set! result (list))
      (set! token-list (list)))
    (define/public (put-token token)
      (if (eq? end-token (token-type token))
          (set! result (append result (list (calc-data token))))
          (set! token-list (append token-list (list token)))))

    (define/public (get-result)
      (map (lambda (x) (list 'rule-loc '()  (list 'id '() (rule-struct-id x)) (list 'len '() (number->string (rule-struct-value x))))) result) )
    (define/public (get-description)
      (with-output-to-string
       (lambda ()
       (printf "Рассчитывает количество строк между токенами  ~a и ~a. В качестве идентификатора возвращает значени токена после ~a" start-token end-token start-token))))
    
    (define/public (end-work)
      (set! result null)
      (set! token-list null))))



(add-rule 'rule-LOC-func (new loc-rule% [start 'lxmFUNC] [end 'lxmENDFUNC]) )
(add-rule 'rule-LOC-proc (new loc-rule% [start 'lxmPROC] [end 'lxmENDPROC]) )

