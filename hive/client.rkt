#lang racket
(provide application% log-event)
(require racket/gui
         (prefix-in txt: "client/string-constants.rkt")
         (rename-in "client/commands.rkt" [disconnect real-disconnect])
         "client/users.rkt")

(define ((log-event text) data)
  (send text set-position (send text get-end-position))
  (send text insert (format "~a\n" data)))

(define application%
  (class frame%
    (init [program-name ""]
          [width 800]
          [height 600] 
          [default-host "locahost"]
          [init-status "Started"])
    (init-field
     [port 1313]
     [on-event (λ (data) #t)]
     [on-connect (λ (the-connection) #t)])
    (super-new [label program-name] [width width] [height height])

    (inherit create-status-line set-status-text show)

    (define user/pass #f)

    (define/public (get-menu) menu-bar)
    (define/public (get-settings-menu) settings-menu)
    (define/public (set-user-pass! username password)
      (set! user/pass (cons username password)))
    
    (define the-connection #f)
    
    (define/public (connection)
      (unless (connection-alive? the-connection)
        (define (on-fail e)
          (cond
            [(exn:fail:network? e) 
             (message-box (txt:error)
                          (format "~a\n~a" (txt:cannot-connect) (exn-message e))
                          this
                          '(ok caution))
             (send frame-settings show #t)]
            [(exn:fail:user? e)
             (message-box (txt:error) (exn-message e) this '(ok caution))
             (send frame-settings show #t)]))
        (set! the-connection
              (if user/pass
                  (connect port on-event on-connect on-fail (car user/pass) (cdr user/pass))
                  (connect port on-event on-connect on-fail))))
      the-connection)
    (define/public (disconnect)
      (when the-connection
        (real-disconnect the-connection)
        (set! the-connection #f)))
    (define/augment (on-close)
      (disconnect)
      (inner (void) on-close))
    (define menu-bar (new menu-bar% [parent this]))
    (define settings-menu (new menu% [parent menu-bar] [label (txt:settings)]))
    (define settings-menu-item (new menu-item% 
                                    [parent settings-menu] 
                                    [label (txt:settings)] 
                                    [callback (λ (m e)
                                                (send frame-settings show #t))]))
    (define users-menu-item (new menu-item% 
                                 [parent settings-menu] 
                                 [label (txt:users)] 
                                 [callback (λ (m e)
                                             (when (connection-alive? (connection))
                                               (define f (new frame%
                                                              [label "Users"]
                                                              [width width]
                                                              [height height]))
                                               (users-form (connection) f)
                                               (send f show #t)))]))
    
    (create-status-line)
    (set-status-text init-status)

    (define frame-settings 
      (let* ([dialog (new dialog% [label (txt:settings)] [parent this])]
             [server (new text-field%
                          [label (txt:server)]
                          [parent dialog]
                          [init-value (or (get-preference 'server) default-host)])]
             [username (new text-field%
                            [label (txt:username)]
                            [parent dialog]
                            [init-value (or (get-preference 'username) "")])]
             [password (new text-field%
                            [label (txt:password)]
                            [parent dialog]
                            [init-value (or (get-preference 'password) "")])])
        (new button% 
             [label (txt:save)]
             [parent dialog]
             [callback (λ (b e)
                         (put-preferences '(server username password)
                                          (map (λ (x)(send x get-value)) (list server username password)))
                         (send dialog show #f))])
        dialog))))
