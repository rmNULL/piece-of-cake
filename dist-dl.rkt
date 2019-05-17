#lang racket
(require racket/tcp)
(require net/url net/head)
(require json)

(require "./allocate.rkt")
(define *PORT* 8428)

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

(define LINKS (make-hash))
(define DLERS (make-hash))

(define (generate-token link)
  "P0@252")

(define (link-register! peer req)
  ;; TODO LINK VERIFICATION
  (define link (first req))
  
  (define (link-download-size)
    (define resp (resolve-redirects link))
    (unless resp
      (error "failed to get valid response from link"))
    (unless (shareable-download? resp)
      (error "The link doesnt support shared download"))
    (define content-len (extract-content-length resp))
    content-len)
  
  (define loh
    (with-handlers ([exn:fail? (λ (exn) (hash 'status "FAIL"
                                              'message (exn-message exn)))])
    (link-download-size)))
  
  (if (hash? loh)
      loh
      (let ([clen loh]
            [token (generate-token link)])
        (hash-set! LINKS token link)
        (hash-set! DLERS token '())
        (set-filesize! clen)
        (hash 'status "OK" 'token token))))

(define (verify-token! token)
  (unless (hash-ref LINKS token #f)
    (displayln token (current-error-port))
    (raise (hash 'status "FAIL" 'message "Invalid token"))))

(define (peer-allot peer token rest-req)
  (verify-token! token)
  (define link (hash-ref LINKS token #f))
  (define chunk (allot peer))
  (when (void? chunk) (raise (hash 'status "FAIL")))
  (hash 'status "OK"
        'link link ;; this is redundant!!
        'chunk chunk))

(define (peer-cancel peer token rest-req)
  #f)

(define (peer-mark peer token rest-req)
  (verify-token! token)
  (define dlers (hash-ref DLERS token))
  (define chunk (mark-allotment peer))
  (when chunk
    (hash-set! DLERS token (cons (list chunk peer) dlers)))
  (define complete? (all-allotments-complete?))
  (hash 'status (if chunk "OK" "FAIL")
        'completed complete?))

(define handle-request
    (let ([dispatch-table (hash "JOIN" link-register!
                                "ASK"  peer-allot
                                "QUIT" peer-cancel
                                "DONE" peer-mark
                                )])
      (λ (request)
        ;; action token
        ;; PEER-NAME
        ;; 
        (define req (string-split request "\n"))
        (define acn-tkn (string-split (first req) " "))
        (define action (first acn-tkn))
        (define fn (hash-ref dispatch-table action #f))
        (when fn
          (define peer (second req))
          (with-handlers ([hash? (λ (rsp) rsp)])
           (if (string=? action "JOIN")
              (fn peer (cddr req))
              (fn peer (second acn-tkn) (cddr req))))))))

(define (start-server! port)
    (define listener (tcp-listen  *PORT*))
    (let-values ([(_0 port _1 _2) (tcp-addresses listener #t)])
      (displayln (format "CIA ears on ~a" port)))
    (let serve ()
      (define-values (in out) (tcp-accept listener))
      (displayln "connected")
      (thread (λ ()
                (define request (read in)) 
                (define reply (handle-request request))
                (when (void? reply) (displayln request (current-error-port)))
                (unless (void? reply)
                  (displayln reply)
                  (write-json reply out)
                  (close-output-port out))))
      (serve)))

(start-server! *PORT*)