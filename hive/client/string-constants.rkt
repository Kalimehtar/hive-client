#lang racket/base
(require "defaults.rkt"
         (only-in hive/client/string-constants-en)
         (only-in hive/client/string-constants-ru)
         (for-syntax racket/base))
(provide (all-defined-out))

(define (module) (string->symbol (format "hive/client/string-constants-~a" (language))))

(define-for-syntax (check-name name-stx stx)
  (define datum (syntax->datum name-stx))
  (unless (symbol? datum)
    (raise-syntax-error #f (format "expected name, got: ~s" datum) stx))
  (with-handlers ([exn:fail:contract?
                   (λ (t)
                     (raise-syntax-error #f
                                         (format "~a is not a known string constant" datum)
                                         stx))])
    (dynamic-require 'hive/client/string-constants-en datum)))

(define-syntax (reexport stx)
  (syntax-case stx ()
    [(reexport name ...)
     (for ([name (in-list (syntax->list #'(name ...)))])
       (check-name name stx))
     #'(begin    
         (define (name)
           (with-handlers ([exn:fail:contract?
                            (λ (t)
                              (dynamic-require 'hive/client/string-constants-en 'name))])
             (dynamic-require (module) 'name))) ...)]))

(reexport bad-password settings users server username password save messages)
