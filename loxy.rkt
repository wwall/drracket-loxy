#lang racket

(require "main.rkt")
(main (vector->list (current-command-line-arguments)))
