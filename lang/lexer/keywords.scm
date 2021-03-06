#lang racket
(require (lib "srfi/13")
         racket/syntax)

(define maybe-only-id #f)

(define hash-keyword (make-hash)) 

(define KeywordList '(("and" "и" and) ("break" "прервать" break) ("continue" "продолжить" continue) ("do" "цикл" do)   
                                      ("each" "каждого" each)  ("elsif" "иначеесли" elsif) ("else" "иначе" else)   ("enddo" "конеццикла" enddo)  
                                      ("endfunction" "конецфункции" endFunc) ("endif" "конецесли" endif) ("endprocedure" "конецпроцедуры" endProc) ("endtry" "конецпопытки" endtry)  
                                      ("except" "исключение" except)   ("export" "экспорт" export) ("for" "для" for)   ("function" "функция" func)   
                                      ("goto" "перейти" goto)  ("if" "если" if) ("in" "из" in)  ("new" "новый" new)   
                                      ("not" "не" not)   ("or" "или" or)  
                                      ("procedure" "процедура" proc) ("raise" "вызватьисключение" raise)  
                                      ("return" "возврат" ret)   ("then" "тогда" then)  ("try" "попытка" try)  
                                      ("to" "по" to)   ("val" "знач" val)   
                                      ("var" "перем" var)  ("while" "пока" while)   
                                      ("true" "истина" true)   ("false" "ложь" false)  
                                      ("undefined" "неопределено" undefined)   ("client" "клиент" client)
                                      ("#if" "#если" pif)
                                      ("#endif" "#конецесли" pendif)
                                      ("#elsif" "#иначеесли" pelsif)

                                      ("eval" "вычислить" eval)
                                      ("execute" "выполнить" execute)
                                      ("&onserverwithoutcontexrt" "&насерверебезконтекста" sonservercontext)
                                      ("&onclient" "&наклиенте" sonclient)
                                      ("&onserver" "&насервере" sonserver)
                                      
                                      ("server" "сервер" server)  ("externalconection" "внешнеесоединение" externalconnection)))

(for-each  (lambda (x)
             (hash-set! hash-keyword (string-upcase (first x )) (string->symbol (string-concatenate (list "lxm" (string-upcase (symbol->string (third x)))))))
             (hash-set! hash-keyword (string-upcase (second x)) (string->symbol (string-concatenate (list "lxm" (string-upcase (symbol->string (third x)))))))
             ) KeywordList)



(define (get-keyword-symbol lexeme what)
  (if maybe-only-id
      what
      (let ([result (hash-ref hash-keyword (string-upcase lexeme) null)])
        (if (null? result)
            what
            result))))

(define (maybe-only-id! x)
  (set! maybe-only-id x)
  )

(provide get-keyword-symbol maybe-only-id!)