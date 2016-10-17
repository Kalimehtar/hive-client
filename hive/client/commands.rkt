#lang racket/base
(provide connect do-command do-std-command disconnect connection-alive?)
(require racket/match
         racket/file
         racket/tcp
         (prefix-in txt: "string-constants.rkt")
         hive/common/read-write)

(struct connection (send-thread close) #:mutable)

(struct exn:bad-password exn:fail:user ())

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
              (cond
                [(eq? e 'kill) #f]
                [(exn:bad-password? e) (on-fail e)]
                [else
                 (on-fail e)
                 (connect tcp-port on-event on-connect on-fail username password result-connection)]))))
  (define main-custodian (make-custodian))
  (define-syntax-rule (thread-loop BODY ...)
    (thread
     (λ ()
       (with-handlers ([exn:fail?  (λ (e)
                                     (thread-send reconnect e))])
         (let loop ()
           BODY ...
           (loop))))))
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
         (raise (exn:bad-password (txt:bad-password)))]
        [else
         (raise-user-error 'connect
                           auth-result)])
      (define receivers (list (cons #f (thread-loop (on-event (thread-receive))))))
      (define (receive! %receivers id data [seen null])
        (define r (car %receivers))
        (cond
          [(eqv? (car r) id)
           (thread-send (cdr r) data)
           (set! receivers (append (reverse seen) %receivers))]
          [(eq? (car r) #f)
         (thread-send (cdr r) data)]
          [else (receive! (cdr %receivers) id data (cons r seen))]))
      (define dispatch (thread-loop
                        (match (thread-receive)
                          [(list-rest 'data id data) (receive! receivers id data)]
                          [new-receiver (set! receivers (cons new-receiver receivers))])))
      (define next!
        (let ([n 0])
          (λ ()
            (set! n (if (n . > . 10000)  0 (add1 n)))
            n)))
      (define sender (thread-loop
                      (define (write-or-die data out)
                        (with-handlers ([(λ (e) #t) (λ (e)
                                                      (thread-send reconnect #t)
                                                      (custodian-shutdown-all main-custodian))])
                          (write/flush data out)))
                      (match (thread-receive)
                        [(cons return data)
                         (define id (next!))
                         (thread-send dispatch (cons id return) #f)
                         (write-or-die (cons id data) out)]
                        [data (write-or-die data out)])))
      (thread-loop
       (define data (read/timeout in))
       (cond
         [(eof-object? data)
          (thread-send reconnect #t)
          (custodian-shutdown-all main-custodian)]
         [else (thread-send dispatch (cons 'data data) #f)]))
      (thread-loop
       (sleep 10)
       (thread-send sender 'keepalive #f))
      (set-connection-send-thread! result-connection sender)
      (set-connection-close! result-connection (λ ()
                                                 (custodian-shutdown-all main-custodian)))
      (on-connect result-connection)
      result-connection)))

(define (talk connection data)
  (when connection
    (call-in-nested-thread
     (λ ()
       (thread-send (connection-send-thread connection) (cons (current-thread) data))
       (thread-receive)))))

(define (do-command connection id . args)
  (talk connection (list* 'command id args)))

(define (do-std-command connection id . args)
  (talk connection (list* 'std-command id args)))

(define (connection-alive? connection)
  (and connection (thread-running? (connection-send-thread connection))))

(define (disconnect connection)
  ((connection-close connection)))
