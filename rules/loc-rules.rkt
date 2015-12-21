#lang racket
(require "../lang/lexer/lexer.scm"
         "lexer-interface.rkt"
         srfi/1)

(provide (all-defined-out))

(define-struct structure-rule-loc
  (token size) #:transparent)


(define (rule-LOC input-list output-list)
  (define (calc-fun-len data)
    (make-structure-rule-loc
     (token-value (find (lambda (x) (eq? 'tknID (token-type x))) data))
     (- (token-line (last data)) (token-line (first data)))))

  (define (split-list stop x)
    (define (non-stop? x) (not (eq? stop (token-type x))))
    (let [(start (take-while non-stop? x))
          (stop (drop-while non-stop? x))]
      (list  (append (if (list? start) start (list start))
                     (list (first stop)))
             (rest stop))))
  
  (define (loc-func stop x)
    (let* [(data (split-list stop x))
           (calc (list (calc-fun-len (first data))))]
      (list calc (second data))))
  ;;; main
  (if (empty? input-list)
      output-list
      (case (token-type (first input-list))
        [(lxmFUNC)  (let [(result (loc-func 'lxmENDFUNC input-list))]
                      (rule-LOC (second result) (append output-list (first result)) ))]
        [(lxmPROC) (let [(result (loc-func 'lxmENDPROC input-list))]
                      (rule-LOC (second result) (append output-list (first result)) ))]
        [else (rule-LOC (cdr input-list) output-list)])))




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
      (list ':id name ':len len)))

      
    (define/public (init-work)
      (set! result (list))
      (set! token-list (list)))
    (define/public (put-token token)
      (if (eq? end-token (token-type token))
          (set! result (append result (list (calc-data token))))
          (set! token-list (append token-list (list token)))))

    (define/public (get-result)
      result)
    (define/public (get-description)
       "Рассчитывает количество строк в функции или процедуре. На выходе - структура с идентификатором и количеством строк")
    (define/public (end-work)
      (set! result null)
      (set! token-list null))))


;(define tst-data (file->token-list "../test-data/loc-file-test.txt"))
;(define funcs (new loc-rule% [start 'lxmFUNC] [end 'lxmENDFUNC]))
;(for-each (lambda (x) (send funcs put-token x)) tst-data)
;(send funcs get-result)


(add-rule 'rule-LOC (new loc-rule% [start 'lxmFUNC] [end 'lxmENDFUNC]) )

