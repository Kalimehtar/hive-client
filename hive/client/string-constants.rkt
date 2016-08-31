#lang racket/base
(require "defaults.rkt"
         (only-in hive/client/string-constants-en)
         (only-in hive/client/string-constants-ru)
         (for-syntax racket/base))

(define (module) (string->symbol (format "hive/client/string-constants-~a" (language))))

;; Get all provided names from hive/client/string-constants-en
(define-for-syntax (names)
  (map car
       (cdr (assv 0 (call-with-values (λ () (module->exports 'hive/client/string-constants-en))
                                      (λ (x _) x))))))

;; Try to get `name` from (module). If fail, then from hive/client/string-constants-en
(define-syntax (reexport stx)
  (syntax-case stx ()
    [(reexport)
     (with-syntax ([(name ...) (datum->syntax stx (names))])
       #'(begin
           (begin
             (provide name)
             (define (name)
               (with-handlers ([exn:fail:contract?
                                (λ (t)
                                  (dynamic-require 'hive/client/string-constants-en 'name))])
                 (dynamic-require (module) 'name)))) ...))]))

(reexport)