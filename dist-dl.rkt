#lang racket
(require racket/tcp)
(require net/url net/head)
(require json)

(require "./allocate.rkt")

(define (resolve-redirects link)
  (let redirect-loop ([link link]
                        [redirections 3])
      (define url (string->url link))
      [define ip (head-impure-port url)]
      [define resp (port->string ip)]
      (close-input-port ip)
      [define redirect?
        (regexp-match #rx#"^HTTP/[0-9]+[.][0-9]+ 3[0-9][0-9]" resp)]
      (if redirect?
          (if (> redirections 0)
              (redirect-loop (extract-field "Location" resp)
                             (sub1 redirections))
              #f)
          resp)))

(define (extract-content-length response)
  ;; (define resp (resolve-redirects link))
  (define len (extract-field "Content-Length" response))
  (string->number (or len "-1")))

(define (shareable-download? response)
  (define mem-unit (extract-field "Accept-Ranges" response))
  (and mem-unit (not (string=? mem-unit "none"))))

(define (start-dl link)
  #|
  (define resp (resolve-redirects link))
  (unless resp
    (error "failed to get valid response from link"))
  (unless (shareable-download? resp)
    (error "The link doesnt support shared download"))
  |#
  (define content-len 4020 #;(extract-content-length resp))


  ;; ["ip:port" "name" (chunk-start chunk-end)]
  (define downloaders '())
  (define downloader-address first)
  (define downloader-name   second)
  (define downloader-chunk   third)
  
  (define (register! addr name)
    (define (schedule limit)
      ;; stub doesnt allocate properly !!!!!
      (define start (random 0 content-len))
      (define end (min (- content-len start) limit))
      (list start end))
    
    (define chunk (schedule content-len))
    (set! downloaders
          (list* (list addr name chunk) downloaders))
    (hash 'status "OK"
          'chunk chunk))

  ;; progress - % of downloaded file
  (define (progress addr name)
    (define dlr (findf (λ (dlr) (string=? (downloader-address dlr) addr)) downloaders))
    (define prg% (if dlr (random 1 100) 0))
    (hash 'status (if dlr "OK" "FAIL")
          'progress prg%))

  (define (deregister! addr _name)
    (define other-dlrs
      (filter (λ (dlr) (not (string=? (downloader-address dlr) addr)))
              downloaders))
    (set! downloaders other-dlrs)
    (hash 'status "OK"))
  
  (define (active-dlers _addr _name)
    (hash 'status "OK"
          'downloaders downloaders))
  
  (define handle-request
    (let ([dispatch-table (hash "JOIN"   register!
                                "EXIT" deregister!
                                "PROG"    progress
                                "STAT" active-dlers
                                )])
      (λ (request addr)
        ;; action name
        (define-values [action name] (apply values (string-split request "\n")))
        (define fn (hash-ref dispatch-table action #f))
        (if fn
            (fn addr name)
            (hash 'status "FAIL"))
        )))
 
  (define (start-server!)
    (define *PORT* 8428)
    (define listener (tcp-listen  *PORT*))
    (let-values ([(_0 port _1 _2) (tcp-addresses listener #t)])
      (displayln (format "CIA ears on ~a" port)))
    (let serve ()
      (define-values (in out) (tcp-accept listener))
      (thread (lambda ()
                (define-values [si sp ip port] (tcp-addresses listener #t))
                (define addr (format "~a:~a" ip port))
                (define reply (handle-request (read in) addr))
                ;; (write-json reply) ;; copy to self
                (write-json reply out)
                (close-output-port out)))
      (serve)))

  (when (positive? content-len)
    (start-server!)))

(define link-samples
  '("http://0.0.0.0:8000/Music/2006.Miles%20Davis%20-%20Kind%20Of%20Blue/04%20-%20All%20Blues.flac"
    "http://hcmaslov.d-real.sci-nnov.ru/public/mp3/Nirvana/Nirvana%20'Come%20As%20You%20Are'.mp3"
    ))

(start-dl (list-ref link-samples (random 0 (length link-samples))))

