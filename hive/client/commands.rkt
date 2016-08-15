#lang racket/base
(provide connect do-command do-std-command disconnect connection-alive?)
(require racket/match
         racket/file
         racket/tcp
         (prefix-in txt: "string-constants.rkt")
         hive/common/read-write)

(struct connection (send-thread close) #:mutable)

(define-syntax-rule (thread-loop BODY ...)
  (thread
   (λ ()
     (let loop ()
       BODY ...
       (loop)))))

(define (connect tcp-port
                 on-event
                 on-reconnect
                 [username (get-preference 'username)]
                 [password (get-preference 'password)]
                 [result-connection (connection #f #f)])
  (define main-custodian (make-custodian))
  (define reconnect
    (thread (λ ()
              (thread-receive)              
              (on-reconnect (connect tcp-port on-event username password result-connection)))))
  (parameterize ([current-custodian main-custodian])
    (define-values (in out) (tcp-connect (get-preference 'server) tcp-port))
    (write/flush (list username password) out)
    (define auth-result (read/timeout in))
    (case auth-result
      [(ok) #t]
      [else
       (unless (eq? auth-result 'bad-password)
         (thread-send reconnect #t))
       (custodian-shutdown-all main-custodian)
       (when auth-result
         (raise-user-error 'connect
                           (if (eq? auth-result 'bad-password)
                               (txt:bad-password)
                               auth-result)))])
    (define receivers (list (cons #f (thread-loop (on-event (thread-receive))))))
    (define (receive! receivers id data [seen null])
      (define r (car receivers))
      (cond
        [(eqv? (car r) id)
         (thread-send (cdr r) data)
         (set! receivers (append (reverse seen) receivers))]
        [(eq? (car r) #f)
         (thread-send (cdr r) data)]
        [else (receive! (cdr receivers) id data (cons r seen))]))
    (define dispatch (thread (λ ()
                               (let loop ()
                                 (match (thread-receive)
                                   [(list-rest 'data id data) (receive! receivers id data)]
                                   [new-receiver (set! receivers (cons new-receiver receivers))])
                                 (loop)))))
    (define next!
      (let ([n 0])
        (λ ()
          (set! n (if (n . > . 10000)  0 (add1 n)))
          n)))
    (define sender (thread-loop
                    (match (thread-receive)
                      [(cons return data)
                       (define id (next!))
                       (thread-send dispatch (cons id return) #f)
                       (write/flush (cons id data) out)]
                      [data (write/flush data out)])))
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
    result-connection))
      
(define (talk connection data)
  (call-in-nested-thread
   (λ ()
     (thread-send (connection-send-thread connection) (cons (current-thread) data))
     (thread-receive))))

(define (do-command connection id . args)
  (talk connection (list* 'command id args)))

(define (do-std-command connection id . args)
  (talk connection (list* 'std-command id args)))

(define (connection-alive? connection)
  (and connection (thread-running? (connection-send-thread connection))))

(define (disconnect connection)
  ((connection-close connection)))
