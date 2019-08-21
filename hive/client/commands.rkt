#lang racket/base
(provide connect do-command do-std-command disconnect connection-alive?)
(require racket/match
         racket/file
         racket/tcp
         (prefix-in txt: "string-constants.rkt")
         hive/common/read-write)

(struct connection (send-thread close) #:mutable)

(struct exn:bad-password exn:fail:user ())

(struct receiver (id thread data) #:transparent)

(define (connect tcp-port
                 on-event
                 on-connect
                 on-fail
                 [username (get-preference 'username)]
                 [password (get-preference 'password)]
                 [result-connection (connection #f #f)])
  (define reconnect
    (thread (λ ()
              (define e (thread-receive))
              (custodian-shutdown-all main-custodian)
              (sleep 1)
              (cond
                [(eq? e 'kill) #f]
                [(exn:bad-password? e) (on-fail e)]
                [else
                 (on-fail e)
                 (connect tcp-port on-event on-connect on-fail username password result-connection)
                 (for ([r (in-list receivers)])
                   (when (receiver-data r)
                     (thread-send (connection-send-thread result-connection) r)))]))))
  (define main-custodian (make-custodian))
  (define-syntax-rule (thread-loop BODY ...)
    (thread
     (λ ()
       (with-handlers ([exn:fail?  (λ (e) (thread-send reconnect e))])
         (let loop ()
           BODY ...
           (loop))))))
  (define receivers (list (receiver #f (thread-loop (on-event (thread-receive))) #f)))
  (parameterize ([current-custodian main-custodian])
    (with-handlers ([exn:fail? (λ (e)
                                 (thread-send reconnect e)
                                 result-connection)])
      (define-values (in out) (tcp-connect (get-preference 'server) tcp-port))
      (write/flush (list username password) out)
      (define auth-result (read/timeout in))
      (case auth-result
        [(ok) #t]
        [(bad-password)
         (raise (exn:bad-password (txt:bad-password) (current-continuation-marks)))]
        [else
         (raise-user-error 'connect auth-result)])
      (define (receive! %receivers id data [seen null])
        (match %receivers
          [(cons head rest)
           (cond
             [(eq? (receiver-id head) #f)
              (thread-send (receiver-thread head) data)]
             [(eqv? (receiver-id head) id)
              (thread-send (receiver-thread head) data)
              (set! receivers (append (reverse seen) rest))]
             [else (receive! rest id data (cons head seen))])]))
      (define dispatch (thread-loop
                        (match (thread-receive)
                          [(list-rest 'data id data) (receive! receivers id data)]
                          [(cons 'data _) #f]
                          [new-receiver (set! receivers (cons new-receiver receivers))])))
      (define next!
        (let ([n 0])
          (λ ()
            (set! n (if (n . > . 10000)  0 (add1 n)))
            n)))
      (define sender (thread-loop
                      (sync/timeout 10 (thread-receive-evt))
                      (match (or (thread-try-receive) 'keepalive)
                        [(receiver _id return data)
                         (define id (or _id (next!)))
                         (thread-send dispatch (receiver id return data) #f)
                         (write/flush (cons id data) out)]
                        [data (write/flush data out)])))
      (thread-loop
       (define data (read/timeout in))
       (cond
         [(eof-object? data)
          (thread-send reconnect #t)
          (custodian-shutdown-all main-custodian)]
         [else (thread-send dispatch (cons 'data data) #f)]))
      (set-connection-send-thread! result-connection sender)
      (set-connection-close! result-connection (λ () (custodian-shutdown-all main-custodian)))
      (on-connect result-connection)
      result-connection)))

(define (talk connection data)
  (when connection
    (call-in-nested-thread
     (λ ()
       (with-handlers ([exn:fail? displayln])
       (thread-send (connection-send-thread connection) (receiver #f (current-thread) data))
       (thread-receive))))))

(define (do-command connection id . args)
  (talk connection (list* 'command id args)))

(define (do-std-command connection id . args)
  (talk connection (list* 'std-command id args)))

(define (connection-alive? connection)
  (and connection
       (connection-send-thread connection)
       (thread-running? (connection-send-thread connection))))

(define (disconnect connection)
  (and (connection-close connection)
       (connection-close connection)))
