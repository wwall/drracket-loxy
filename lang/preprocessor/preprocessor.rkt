#lang racket
(require "../lexer/lexer.scm")
(require (prefix-in p: "mod-parsak.rkt"))

(define tstString "#if data then data1 #else data2 #endif")
(define tst (string->token-list tstString))




(define $if (p:token 'lxmPIF))