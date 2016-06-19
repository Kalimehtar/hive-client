#lang racket/base
(require racket/contract racket/tcp)
(provide (contract-out
          [language (parameter/c (or/c 'en 'ru))]))

(define language (make-parameter 'en))
