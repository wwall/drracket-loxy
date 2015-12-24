#lang racket

(provide gID)

;;;; уникальный ид для всех используемых сущностей 
(define id 0)
;;;; Функция для получения номера
(define (gID)
  (set! id (+ id 1))
  id)
