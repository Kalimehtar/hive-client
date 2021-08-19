#lang racket
(require "receiver.rkt")
(provide (all-defined-out))

(define (init default-thread)
  (box (list (receiver #f default-thread #f))))

(define (resend receivers thr)
  (for ([r (in-list (unbox receivers))] #:when (receiver-data r))
    (thread-send thr r)))

(define (dispatch! receivers id data)
  (let loop ([r (unbox receivers)] [seen null])
    (match r
      [(cons head rest)
       (cond
         [(eq? (receiver-id head) #f)
          (thread-send (receiver-thread head) data)]
         [(eqv? (receiver-id head) id)
          (thread-send (receiver-thread head) data)
          (set-box! receivers (append (reverse seen) rest))]
         [else (loop rest id data (cons head seen))])])))

(define (add! receivers new)
  (set-box! receivers (cons new (unbox receivers))))