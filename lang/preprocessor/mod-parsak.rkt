;;;;; Модифицированый под список parsak
#lang racket/base
(require  racket/match  racket/format racket/string)
(require (prefix-in lexer: "../lexer/lexer.scm"))
(require (for-syntax racket/base syntax/parse racket/syntax))
(provide (struct-out Consumed)
         (struct-out Empty)
         (struct-out Ok)
         (struct-out Error)
         err $err (struct-out exn:fail:parsack) parsack-error parse-source
         return
         satisfy
         token
         
         >>= >> <or> <any>
         option optionMaybe optional
         try lookAhead <!> notFollowedBy
         many many1 skipMany skipMany1 manyTill many1Till manyUntil many1Until
         sepBy sepBy1 endBy between
         <?>
         $eof 
         parse parse-result
         parser-compose parser-seq parser-cons parser-one
         choice
         getState setState withState)
         

;;;;;;;;;;;;;;;;;;;;;;;;; Parsec (with error messages) ;;;;;;;;;;;;;;;;;;;;;;;;;
;; implements monadic combinators for LL parsing

;; p is an InputPort when (input-port? p)

;; A [Parser X] is a function: InputPort -> [Result X]

;; Naming conventions:
;; Parsers use a $-prefixed name.
;; Parser combinators have camelCase names, with 1st char lowercase

;; Two different factors may be used to combine parsers:
;; - whether a parser consumed input, ie returns Consumed and not empty
;; - whether a parser succeeds, ie returns an Ok and not an Error
;; Some combinators use only one of the factors when determining how to combine
;; parsers; others use both.

;; A [Result X] is one of:
;; - (Consumed [Reply X])
;; - (Empty [Reply X])
;; NOTE: Cannot use #f as parse result because some parsers want to produce #f, eg json
(struct Consumed (reply) #:transparent)
(struct Empty (reply) #:transparent)

;; A [Reply X] is one of:
;; - (Ok X)
;; - (Error)
(struct Ok (parsed) #:transparent)
(struct Error ())

; racket/function
(define-syntax-rule (thunk e) (λ () e))
(define (negate f) (λ (x) (not (f x))))
(define (curry f x) (λ args (apply f x args)))

;; global parameters ----------------------------------------------------------
;; current-unexpected : [Thunk String]
;; The current unexpected char.
(define current-unexpected "")
(define (reset!-unexpected) (set! current-unexpected ""))
(define (get-unexpected) current-unexpected)
(define (set!-unexpected str) (set! current-unexpected str))
; current-expected : [List [Thunk String]]
;; The current expected chars.
(define current-expected null)
(define (reset!-expected) (set! current-expected null))
(define (get-expected) current-expected)
(define (set!-expected exps) (set! current-expected exps))
(define (merge!-expected exps1 exps2) (set!-expected (append exps1 exps2)))
(define (cons!-expected exp) (set!-expected (cons exp current-expected)))
(define (append!-expected exps) (set!-expected (append exps current-expected)))
;; user-state [MutHashEq Sym => X]
;; state customizable by the user
(define user-state (make-hasheq))
(define (user-state-reset!) (set! user-state (make-hasheq)))
(define (user-state-get key) (hash-ref user-state key #f))
(define (user-state-set! key val) (hash-set! user-state key val))

;; errors ---------------------------------------------------------------------
(define (err expected)
  (λ (in)
    (set!-unexpected (thunk (car in)))
    (set!-expected (list expected))
    (Empty (Error))))
(define $err (err ""))

(struct exn:fail:parsack exn:fail ())

(define-syntax-rule (parsack-error msg)
  (raise (exn:fail:parsack (string-append "parse ERROR: " msg)
                           (current-continuation-marks))))

;; A Pos is a (Pos line col ofs), numbers are 1-based
(struct Pos (ofs) #:transparent)
(define parse-source (make-parameter #f)) ;; not in Pos for efficiency
(define (format-pos p)
  (match* (p (parse-source))
    [((Pos ofs) (? path-string? src))
     (format "~a:"  ofs)]
    [((Pos  ofs) _)
     (format "~a:"  ofs)]))

;; creates a Parser that consumes no input and returns x
(define (return x)
  (λ (in)
    (reset!-unexpected)
    (reset!-expected)
    (Empty (Ok x))))

;; creates a parser that consumes 1 char or byte (ie one unit of input)
;; if it satisfies predicate p?
(define (satisfy p?)
  (λ (in)
    (define c (car in))
    (cond
      [(null? c)
       (set!-unexpected "end of input")
       (reset!-expected)
       (Empty (Error))]
      [(p? (lexer:token-type c))
       (reset!-unexpected)
       (reset!-expected)
       (Consumed (Ok c))] ; commit peeked
      [else
       (set!-unexpected (thunk  (~v c)))
       (reset!-expected)
       (Empty (Error))])))

;; >= : [Parser X] ([Result X] -> [Parser Y]) -> [Result Y]
;; Creates a Parser from a Parser p and a continuation f for p's result.
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return Consumed with reply from f
;; - if p fails, do not continue with f
(define (>>= p f)
  (λ (in)
    (match (p in)
      [(Empty (Ok x))
       (define saved-expected (get-expected))
       (match ((f x) in)
         [(Empty y)
          (append!-expected saved-expected)
          (Empty y)]
         [consumed consumed])]
      [(Consumed (Ok x))
       (define saved-expected (get-expected))
       (Consumed
        (match ((f x) in)
          [(Empty (Error))
          (append!-expected saved-expected)
          (Error)]
         [(or (Consumed reply) (Empty reply)) reply]))]
      [err err])))
(define (>> p q) (>>= p (λ _ q)))

;; <|> choice combinator
;; first tries to parse with p, only tries q if p does not consume input
;; - if p does not consume input and errors, return result of q
;; - if p does not consume input but returns result, then use that result unless q consumes
;; thus, <or> implements "longest match"
(define (<or>2 p q)
  (λ (in)
    (match (p in)
      [(Empty (Error))
       (define saved-expected (get-expected))
       (match (q in)
         [(Empty x)
          (append!-expected saved-expected)
          (Empty x)]
         [consumed consumed])]
      [(Empty (Ok x))
       (define saved-expected (get-expected))
       (match (q in)
         [(Empty _)
          (append!-expected saved-expected)
          (Empty (Ok x))]
         [consumed consumed])]
      [consumed consumed])))

;; assumes (length args) >= 1
(define (<or> . args)
  (foldl (λ (p acc) (<or>2 acc p)) (car args) (cdr args)))

;; short-circuiting choice combinator
;; only tries 2nd parser q if p errors and consumes no input
;; differs from <or> in the case where p returns (Empty (Ok ...))
;; - <or>: parse with q
;; - <any>: stops
(define (<any>2 p q)
  (λ (in)
    (match (p in)
      [(Empty (Error))
       (define saved-expected (get-expected))
       (match (q in)
         [(Empty x)
          (append!-expected saved-expected)
          (Empty x)]
         [consumed consumed])]
      [res res])))
                      
;; assumes (length args) >= 2
(define (<any> . args)
  (foldl (λ (p acc) (<any>2 acc p)) (car args) (cdr args)))

(define (option x p) (<or> p (return x)))
(define (optionMaybe p) (option #f p))
(define (optional p) 
  (<or> (>> p (return null))
        (return null)))

;; tries to parse with p but backtracks and does not consume input if error
(define (try p)
  (λ (in)
    (define-values (r c pos) (port-next-location in))
    (define byte-pos (file-position in))
    (match (p in)
      [(Consumed (Error))
       (file-position in byte-pos) ; backtrack
       (set-port-next-location! in r c pos)
       (Empty (Error))]
      ; dont need to back track if Empty-Error
      [other other])))

;; Parse p and return the result, but don't consume input.
(define (lookAhead p)
  (λ (in)
    (define-values (r c pos) (port-next-location in)) ; save current loc
    (define byte-pos (file-position in))
    (match (p in)
      [(Consumed (Ok result))
       (file-position in byte-pos) ; backtrack
       (set-port-next-location! in r c pos)
       (set!-unexpected (thunk (car in)))
       (Empty (Ok result))]
      [res res])))

;; fails (and does not consume input) if p succeeds and consumed input,
;; otherwise parse with q
(define (<!> p [q token])
  (λ (in)
    (define-values (r c pos) (port-next-location in)) ; saved location
    (define byte-pos (file-position in))
    (define res (p in))
    (file-position in byte-pos) ; always backtrack
    (set-port-next-location! in r c pos)
    (match res
      [(Consumed (Ok res))
       (set!-unexpected res)
       (set!-expected (list (thunk (format "not: ~a"  res))))
       (Empty (Error))]
      [_ (q in)])))

;; succeeds when p fails; does not consume input
;; differs from <!> in that there is no second parser to try
(define (notFollowedBy p)
  (λ (in)
    (define-values (r c pos) (port-next-location in)) ; saved locaition
    (define byte-pos (file-position in))
    (define res (p in))
    (file-position in byte-pos) ; always backtrack; never consume
    (set-port-next-location! in r c pos)
    (match res
      [(Consumed (Ok res))
       (set!-unexpected res)
       (set!-expected (list (thunk (format "not: ~a"  res))))
       (Empty (Error))]
      [_ (reset!-unexpected)
         (reset!-expected)
         (Empty (Ok null))])))

;; parse with p 0 or more times
;; some notes:
;; - default #:till can be (return <anything>), just needs to not consume input
;; - using many with #:or <any> and the default #:till will immediately return
;;   empty result without consuming input
(define (many p #:till [end (return 0)] #:or [<or> <or>]) 
  (<or> (>> end (return null))
        (parser-cons p (many p #:till end #:or <or>))))

;; parse with p 1 or more times
(define (many1 p #:till [end (return null)] #:or [<or> <or>])
  (parser-cons p (many p #:till end #:or <or>)))

(define (skipMany p) 
  (<or> (parser-compose p (skipMany p))
        (return null)))
(define (skipMany1 p) (parser-compose p (skipMany p)))

;; applies parser p zero or more times until parser end succeeds
(define (manyTill p end #:or [<or> <or>])
  (many p #:till end #:or <or>))

;; applies parser p one or more times until parser end succeeds
(define (many1Till p end #:or [<or> <or>])
  (parser-cons p (manyTill p end #:or <or>)))

;; manyUntil = manyTill #:or <any>
(define (manyUntil p end) (manyTill p end #:or <any>))
(define (many1Until p end) (many1Till p end #:or <any>))

(define (sepBy1 p sep) (parser-cons p (many (>> sep p))))
(define (sepBy p sep) (<or> (sepBy1 p sep) 
                            (return null)))

(define (endBy p end) 
  (many (parser-one (~> p) end)))

(define (between open close p)
  (parser-one open (~> p) close))
   
;; Creates a Parser that parses with p, using exp as the expected input.
;; TODO: why is exp not merged?
(define (<?> p exp)
  (λ (in)
    (match (p in)
      [(Empty x)
       (set!-expected (list exp))
       (Empty x)]
      [other other])))

(define (token c)
  (<?> (satisfy (curry eq?  c))
       c))



;; parser that only succeeds on empty input
(define $eof
  (<?>
   (λ (in)
     (define c (car in))
     (cond
       [(null? c)
        (reset!-unexpected)
        (reset!-expected)
        (Empty (Ok null))]
       [else
        (set!-unexpected "non-empty input")
        (reset!-expected)
        (Empty (Error))]))
   "end-of-file"))

(define (frc e) (if (procedure? e) (e) e))

(define (mformat-exp exp) 
  (string-join (map frc (map symbol->string exp)) ", " #:before-last " or "))

;; An Input is one of:
;; - String,
;; - Path p, where (path? p) = #t
;; - InputPort in, where (input-port? in) = #t

;; parse : [Parser X] Input -> [Reply X] or exception
;; Raises exception if p does not succeed, ie returns Ok
;; errors have to be printed ~s, otherwise newlines get messed up
(define (parse p inp)
  (define res (p inp))
  (match res
    [(or (Empty (Error)) (Consumed (Error)))
     (define-values (r c pos) (values 1 2 3))
     (parsack-error 
      (format "at ~a\nunexpected: ~s\n  expected: ~s"
              (format-pos (Pos pos))
              (frc (get-unexpected))
              (mformat-exp (get-expected))))]
    [ok ok]))

(define (parse-result p s)
  (match (parse p s)
    [(Consumed (Ok parsed)) parsed]
    [(Empty     (Ok parsed)) parsed]
    [x (parsack-error (~v x))]))

;; parser compose
(define-syntax (parser-compose stx)
  (syntax-parse stx  #:datum-literals (<-)
    [(_ p) #'p]
    [(_ (x <- p) e ...)
     #'(>>= p (λ (x) (parser-compose e ...)))]
    [(_ q e ...) #'(>>= q (λ (x) (parser-compose e ...)))]))

(define-syntax (parser-seq stx)
  (define (add-bind stx)
    (syntax-parse stx #:datum-literals (~)
      [(~ p) #'p]
      [q #`(#,(generate-temporary) <- q)]))
  (syntax-parse stx #:datum-literals (~)
    [(_ p:expr ...
        (~optional (~seq #:combine-with combine:expr) #:defaults ([combine #'list])))
     (with-syntax ([(new-p ...) (map add-bind (syntax->list #'(p ...)))])
       (syntax-parse #'(new-p ...) #:datum-literals (<-)
         [(~and ((~or (x <- q1) q2) ...)
                (q ...))
          ;(printf "~a\n" (syntax->datum #'(q2 ...))) ; uncomment for debugging
          #'(parser-compose q ... (return (combine x ...)))]))]))

(define-syntax-rule (parser-cons x y) (parser-seq x y #:combine-with cons))

(define-syntax (parser-one stx)
  (define (add-bind stx)
    (syntax-parse stx #:datum-literals (~>)
      [(~> p) #'p]
      [q #`(~ q)]))
  (syntax-parse stx #:datum-literals (~>)
    [(_ (~and (~seq (~or (~once (~> q1:expr) 
                                #:name "return parse (wrapped with ~>)"
                                #:too-many "too many parses to return (wrapped with ~>)"
                                #:too-few "missing return parse (wrapped with ~>)") 
                         (~not (~> q2:expr))) ...)
              (~seq p:expr ...)))
     (with-syntax ([(new-p ...) (map add-bind (syntax->list #'(p ...)))])
       #'(parser-seq new-p ... #:combine-with (λ (x) x)))]))

(define (choice ps) (apply <or> ps))

(define (getState key)
  (λ (in)
    (reset!-expected)
    (reset!-unexpected)
    (Empty (Ok (user-state-get key)))))

(define (setState key val)
  (λ (in)
    (define current-val (user-state-get key))
    (reset!-expected)
    (reset!-unexpected)
    (user-state-set! key val)
    (Empty (Ok current-val))))

;; Roughly like `parameterize`, but for user state
(define-syntax (withState stx)
  (syntax-case stx ()
    [(_ ([k v] ...) p)
     (with-syntax ([(orig ...) (generate-temporaries #'(k ...))])
       (syntax/loc stx
         (parser-compose (orig <- (setState k v)) ...
                         (result <- p)
                         (setState k orig) ...
                         (return result))))]))
