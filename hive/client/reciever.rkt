#lang racket
(provide (all-defined-out))
(struct receiver (id thread data) #:transparent)
