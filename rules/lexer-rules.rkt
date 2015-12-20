#lang racket
(require "../lang/lexer/lexer.scm"
         srfi/1)

 (provide (all-defined-out))

(define-struct structure-rule-loc
  (token size) #:transparent)


(define (rule-LOC input-list output-list)
  (define (calc-fun-len data)
    (make-structure-rule-loc
     (second data)
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


