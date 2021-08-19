#lang racket/base
(require racket/gui/base 
         racket/class
         hive/common/users
         (only-in hive/common/serialize object-id)
         "commands.rkt" 
         (prefix-in txt: "string-constants.rkt"))
(provide users-list users-form)

(define (users-list connection [limit 0] [skip 0])
  (map (λ (l) (apply user l))
       (do-std-command connection '(users list) limit skip)))

(define (users-form connection parent)
  (when connection
    (define users (users-list connection))
    (define users-form (new horizontal-pane% [parent parent]))
    (define list-box (new list-box% 
                          [label "Users"]
                          [style '(vertical-label single)]
                          [parent users-form]
                          [choices (map user-name users)]
                          [callback (λ (l e)
                                      (define select (send l get-selections))
                                      (unless (null? select)
                                        (set! current (send l get-data (car select)))
                                        (send role
                                              set-value
                                              (symbol->string (user-role current)))))]))
    (for ([n (in-range (length users))]
          [u (in-list users)])
      (send list-box set-data n u))
    (define current (car users))
    
    (define detail-form (new vertical-panel% [parent users-form]))
    (define password (new text-field% 
                          [parent detail-form]
                          [label (txt:password)]
                          [init-value (user-password current)]))
    (define role (new text-field% 
                      [parent detail-form]
                      [label "Role"]
                      [init-value (symbol->string (user-role current))]))
    #f))

(module test racket/base) ; gui