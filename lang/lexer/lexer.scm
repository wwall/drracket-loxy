#lang racket
; Лексер для языка запросов

(require 
  "exception-api.scm"
  "keywords.scm")


(provide
 token-line
 token-column
 token-type
 token-value
 construct-lexer
 string->token-list
 file->token-list)


(define-struct token
  ( line column type value) #:transparent)

(define construct-lexer
  (lambda (input-port )
    (letrec (;;; Lexer IO support functions:
             (line-position 1)
             (column-position 1)
             (my-read-char
              (lambda ()
                (let ((c (read-char input-port)))
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
              (lambda () (peek-char input-port)))
             (lexer-error
              (lambda (message character)
                (throw-wtf-exception ; Abort lexing with error message
                 (string-append "Lexer Error (" (number->string line-position) "," (number->string column-position) ";[" (string character) "]): " message))))
             (new-token
              (lambda (type value)
                (begin
                  (maybe-only-id! (or (eq? type 'lxmDOT) (eq? type 'lxmAS)))
                  (make-token line-position (- column-position
                                               (if (string? value) (string-length value) 1)) type value))))
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
                  (if (or (char-alphabetic? c) (char-numeric? c) (eq? c #\_))
                      (read-identifier (cons (my-read-char) id))
                      (apply string (reverse id))))))
             (read-string ; Read a string's content
              (lambda (str)
                (let ((c (my-peek-char)))
                  (if (not (char=? c #\"))
                      (read-string (cons (my-read-char) str))
                      (let ([was (my-read-char)])
                        (if (char=? (my-peek-char) #\")
                            (read-string (cons was (cons (my-read-char) str)))
                            (apply string (reverse str))))))))
             )
      
      ;;; Return lexer function:
      (lambda ()
        (let recognize-token ((c (my-read-char)))
          
          (cond
            ((eof-object? c)
             (new-token null ""))
            ((is-whitespace c)
             (new-token 'lxmWS c))
            
            ((or (eq? c #\_) (char-alphabetic? c))
             (let ((id (read-identifier (list c))))
               (new-token (get-keyword-symbol id 'tknID) id)))
            ((char-numeric? c)
             (new-token 'tknNUMBER (read-number (list c))))
            ((char=? c #\")
             (let ((content (read-string (list))))
               (new-token 'tknSTRING content)))
            ((char=? c #\.)
             (new-token 'lxmDOT "."))
            ((char=? c #\,)
             (new-token 'lxmCOMMA ","))
            ((char=? c #\;)
             (new-token 'lxmSEMI ";"))
            ((char=? c #\()
             (new-token 'lxmOPEN "("))
            ((char=? c #\))
             (new-token 'lxmCLOSE ")"))

            ((char=? c #\[)
             (new-token 'lxmBOPEN "["))
            ((char=? c #\])
             (new-token 'lxmBCLOSE "]"))

            ((char=? c #\?)
             (new-token 'lxmQUEST "?"))
            ((char=? c #\#)
             (new-token 'lxmPREPROCESSORSTART "#"))

            ((char=? c #\+)
             (new-token 'lxmPLUS "+"))
            ((char=? c #\-)
             (new-token 'lxmMINUS "-"))
            ((char=? c #\*)
             (new-token 'lxmMUL "*"))
            ((char=? c #\/)
             (if (char=? #\/ (my-peek-char))
                 (let consume-one-line-comment ()
                   (if (not (char=? (my-peek-char) #\newline))
                       (begin
                         (my-read-char)
                         (if (not (eof-object? (my-peek-char)))
                             (consume-one-line-comment)
                             (recognize-token (my-read-char))))
                       (recognize-token (my-read-char))))
                 (new-token 'lxmDIV "/")))
            ((char=? c #\=)
             (new-token 'lxmEQU "="))
            ((char=? c #\<)
             (cond 
               ((char=? (my-peek-char) #\=)
                (begin
                  (my-read-char) ; Consume the "="
                  (new-token 'lxmLT-EQU "<=")))
               ((char=? (my-peek-char) #\>)
                (begin
                  (my-read-char) ; Consume the ">"
                  (new-token 'lxmNOT-EQU "<>")))
               (#t (new-token 'lxmLT "<"))))
            ((char=? c #\>)
             (if (char=? (my-peek-char) #\=)
                 (begin
                   (my-read-char) ; Consume the "="
                   (new-token 'lxmGT-EQU ">="))
                 (new-token 'lxmGT ">")))
            (else
             (lexer-error "Illegal character." c))))))))


(define string->token-list
  (lambda (s)
    (maybe-only-id! #f)
    (let* ([buf (open-input-string (string-append s " "))]
           [lexer (construct-lexer buf )])
      (let loop ((results '()))
        (let ((next-token   (lexer)))
          (if (null? (token-type next-token))
              (reverse results)
              (loop (cons   next-token results))))))))

(define file->token-list
  (lambda (s)
    (maybe-only-id! #f)
    (let* ([buf (open-input-file s )]
           [lexer (construct-lexer buf )])
      (let loop ((results '()))
        (let ((next-token   (lexer)))
          (if (null? (token-type next-token))
              (reverse results)
              (loop (cons   next-token results))))))))
