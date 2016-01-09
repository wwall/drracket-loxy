#lang racket
; Лексер для языка запросов

(require 
  (prefix-in util: "../../utils/utils.rkt")
  "exception-api.scm"
  "keywords.scm")

(struct token
  (line ;; номер строки
   column ;; номер колонки
   type ;; тип лексемы
   value ;; значение лексемы (null если значение лексемы востанавливается из типа, например для "+" будет тип lxmPLUS)
   id) ;;; Счетчик токенов. Дальше все ссылаются именно на него)
  #:transparent)




(provide
 token-line
 token-column
 token-type
 token-value
 token-id
 construct-lexer
 string->token-list
 file->token-list)





(define construct-lexer
  (lambda (input-port )
    (letrec (;;; Lexer IO support functions:
             (line-position 1)
             (column-position 1)
             (my-read-char
              (lambda ()
                (let ((c (read-char-or-special input-port)))
                  (cond
                    ((eof-object? c)
                     c)
                    ((char=? c #\newline)
                     (set! line-position (+ line-position 1))
                     (set! column-position 1)
                     c)
                    (else
                     (set! column-position (+ column-position 1))
                     c)))))
             (my-peek-char
              (lambda () (let [(t (peek-char input-port))]
                           (if (eof-object? t)
                               null
                               t))))
             (lexer-error
              (lambda (message character)
                (throw-wtf-exception  ;;;Abort lexing with error message
                 (string-append "Lexer Error (" (number->string line-position) "," (number->string column-position) ";[" (string character) "]): " message))))
             (new-token
              (lambda (type value)
                (begin
                  (maybe-only-id! (or (eq? type 'lxmDOT) (eq? type 'lxmAS)))
                  (token line-position (- column-position
                                               (if (string? value) (string-length value) 1))
                              type value (util:gID)))))
             ;;; Complicated token processing functions:
             (is-whitespace
              (lambda (c)
                (or (char=? c #\space)
                    (char=? c #\newline)
                    (char=? c (integer->char #x000D)) ; Carriage return
                    (char=? c #\tab)
                    (char=? c (integer->char #x0008)) ; Backspace
                    (char=? c (integer->char #x000C))))) ; Formfeed
             (read-integer ; Read sequence of digits
              (lambda (n)
                (let ((c (my-peek-char)))
                  (if (char-numeric? c)
                      (read-integer (cons (my-read-char) n))
                      (apply string (reverse n))))))
             (read-number ; Read integer or real
              (lambda (n)
                (let ((integer-part (read-integer n)))
                  (if (char=? (my-peek-char) #\.)
                      (begin
                        (my-read-char) ; Consume the "."
                        (let ((c (my-read-char)))
                          (if (not (char-numeric? c))
                              (lexer-error "Invalid number. Real numbers must have a decimal place." c)
                              (string-append integer-part (string #\.) (read-integer (list c))))))
                      integer-part))))
             (read-identifier ; Read sequence of letters and digits
              (lambda (id)
                (let ((c (my-peek-char)))
                  (if (and c (char? c) (or (char-alphabetic? c) (char-numeric? c) (eq? c #\_)))
                      (read-identifier (cons (my-read-char) id))
                      (apply string (reverse id))))))

             (read-comment ; Read a string's content
              (lambda (str)
                (let ((c (my-read-char)))
                  (if (not (or (eof-object? c) (char=? c #\newline)))
                      (read-comment (cons c str))
                      (apply string (reverse str))))))
             (read-string-data ; Read a string's content
              (lambda (str)
                (let ((c (my-read-char)))
                  (if (not (char=? c #\'))
                      (read-string-data (cons c str))
                      (apply string (reverse str))))))
             (read-string ; Read a string's content
              (lambda (str)
                (let ((c (my-peek-char)))
                  (if (not (char=? c #\"))
                      (read-string (cons (my-read-char) str))
                      (let ([was (my-read-char)])
                        (if (char=? (my-peek-char) #\")
                            (read-string (cons was (cons (my-read-char) str)))
                            (apply string (reverse str))))))))
             (convert-char
              (lambda (c)

                (cond
                  ((char=? c #\space) 'Space)
                  ((char=? c #\newline) 'NewLine)
                  ((char=? c (integer->char #x000D)) 'CR) ; Carriage return
                  ((char=? c #\tab) 'Tab)
                  ((char=? c (integer->char #x0008)) 'BS); Backspace
                  ((char=? c (integer->char #x000C)) '0C)
                  (else 'UF))
                ))
             )
      
      ;;; Return lexer function:
      (lambda ()
        (let recognize-token ((c (my-read-char)))
          (cond
            ((eof-object? c)
             null) ;(new-token 'lxmEOF ""))
            ((char=? c #\uFEFF)
             (recognize-token (my-read-char)))
            ((is-whitespace c)
             (new-token 'lxmWS (convert-char c)))
            
            ((or (eq? c #\#) (eq? c #\&) (eq? c #\_) (char-alphabetic? c))
             (let ((id (read-identifier (list c))))
               (new-token (get-keyword-symbol id 'tknID) id)))
            ((char-numeric? c)
             (new-token 'tknNUMBER (read-number (list c))))
            ((char=? c #\')
               (new-token 'tknDATE (read-string-data (list))))
            ((char=? c #\")
             (let ((content (read-string (list))))
               (new-token 'tknSTRING content)))
            ((char=? c #\.)
             (new-token 'lxmDOT null))
            ((char=? c #\,)
             (new-token 'lxmCOMMA null))
            ((char=? c #\;)
             (new-token 'lxmSEMI null))
            ((char=? c #\()
             (new-token 'lxmOPEN null))
            ((char=? c #\))
             (new-token 'lxmCLOSE null))
            
            ((char=? c #\[)
             (new-token 'lxmBOPEN null))
            ((char=? c #\])
             (new-token 'lxmBCLOSE null))
            
            ((char=? c #\?)
             (new-token 'lxmQUEST null))
            ((char=? c #\+)
             (new-token 'lxmPLUS null))
            ((char=? c #\-)
             (new-token 'lxmMINUS null))
            ((char=? c #\%)
             (new-token 'lxmMOD null))
            ((char=? c #\*)
             (new-token 'lxmMUL null))
            ((char=? c #\/)
             (if (char=? #\/ (my-peek-char))
                 (new-token 'lxmCOMMENT (read-comment (list)))
                 (new-token 'lxmDIV null)))
            ((char=? c #\=)
             (new-token 'lxmEQU null))
            ((char=? c #\<)
             (cond 
               ((char=? (my-peek-char) #\=)
                (begin
                  (my-read-char) ; Consume the "="
                  (new-token 'lxmLT-EQU null)))
               ((char=? (my-peek-char) #\>)
                (begin
                  (my-read-char) ; Consume the ">"
                  (new-token 'lxmNOT-EQU null)))
               (#t (new-token 'lxmLT null))))
            ((char=? c #\>)
             (if (char=? (my-peek-char) #\=)
                 (begin
                   (my-read-char) ; Consume the "="
                   (new-token 'lxmGT-EQU null))
                 (new-token 'lxmGT null)))
            (else
             (lexer-error  "Illegal character." c))))))))


(define string->token-list
  (lambda (s)
    (maybe-only-id! #f)
    (let* ([buf (open-input-string (string-append s " "))]
           [lexer (construct-lexer buf )])
      (let loop ((results '()))
        (let ((next-token   (lexer)))
          (if (null?  next-token)
              (reverse results)
              (loop (cons   next-token results))))))))

(define file->token-list
  (lambda (s)
    (maybe-only-id! #f)
    (let* ([buf (open-input-file s )]
           [lexer (construct-lexer buf )])
      (let loop ((results '()))
        (let ([next-token   (lexer)])
          (if  (null?  next-token)
              (reverse results)
              (loop (cons   next-token results))))))))