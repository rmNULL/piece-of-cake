#lang racket
(require racket/random)
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

(define (20x-ok? resp)
  [define rmatch
    (regexp-match #rx#"^HTTP/[0-9]+[.][0-9]+ (20[06])" resp)]
  (define http-status (and rmatch (list-ref rmatch 1)))
  (not (false? rmatch)))

(define (extract-content-length response)
  ;; (define resp (resolve-redirects link))
  (define len (extract-field "Content-Length" response))
  (string->number (or len "-1")))

(define (shareable-download? response)
  (define mem-unit (extract-field "Accept-Ranges" response))
  (and mem-unit (not (string=? mem-unit "none"))))

(define LINKS (make-hash))
(define DOWNLOADED (make-hash))

(define (generate-token link)
  (define symbols
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ123456789!@#$%^&*()-+_={}[]:;,<>.?`~")
  (let loop ()
    (define token
      (list->string (build-list (random 8 16) (λ (_idx) (random-ref symbols)))))
    (if (hash-ref LINKS token #f)
      (loop)
      token)))

(define (link-register! peer req)
  ;; TODO LINK VERIFICATION
  (define link (first req))
  
  (define (link-download-size)
    (define resp (resolve-redirects link))
    (unless resp
      (error "failed to get valid response from link"))
    (unless (20x-ok? resp)
      (error "The link doesnt resolve to a valid resource"))
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
        (hash-set! DOWNLOADED token '())
        (set-filesize! token clen)
        (hash 'status "OK" 'token token))))

(define (verify-token! token)
  (unless (hash-ref LINKS token #f)
    (displayln token (current-error-port))
    (raise (hash 'status "FAIL" 'message "Invalid token"))))

(define (peer-allot peer token rest-req)
  (verify-token! token)
  (define link (hash-ref LINKS token #f))
  (define chunk (allot token peer))
  (when (void? chunk)
    (raise (hash 'status "FAIL" 'message "Failed to allocate chunk")))
  (hash 'status "OK"
        'link link ;; this is redundant!!
        'chunk chunk))

(define (peer-cancel peer token rest-req)
  #f)

(define (completed-% token)
  (let ([dled (length (hash-ref DOWNLOADED token))]
          [tot (total-chunks token)])
      (* (/ dled tot) 100.0)))

(define (peer-mark peer token rest-req)
  (verify-token! token)
  (define dlers (hash-ref DOWNLOADED token))
  (define chunk (mark-allotment token peer))
  (when chunk
    (hash-set! DOWNLOADED token (cons (list chunk peer) dlers)))
  ;;(define complete? (all-allotments-complete? token))
  (define completed (completed-% token))
  (hash 'status (if chunk "OK" "FAIL")
        'completed completed))

(define (dl-stats peer token rest-req)
  (verify-token! token)
  (hash 'status "OK"
        'downloaders (hash-ref DOWNLOADED token '())
        'completed (completed-% token)))

(define handle-request
    (let ([dispatch-table (hash "JOIN" link-register!
				"ASK"  peer-allot
                                "CNCL" peer-cancel
                                "DONE" peer-mark
                                "STAT" dl-stats
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
    (define listener (tcp-listen port 6 true))
    (let-values ([(_0 port _1 _2) (tcp-addresses listener #t)])
      (displayln (format "CIA ears on ~a" port)))
    (let serve ()
      (define-values (in out) (tcp-accept listener))
      (displayln "connected")
      (thread (λ ()
                (define request
                  (let loop ([read ""]
                             [l (read-line in)])
                    (if (equal? l "")
                        read
                        (loop (string-append read "\n" l)
                              (read-line in)))))
                (displayln request)
                (define reply (handle-request request))
                (when (void? reply) (displayln request (current-error-port)))
                (unless (void? reply)
                  (displayln reply)
                  (write-json reply out)
                  (close-output-port out))))
      (serve)))

(module+ main
  (define *PORT* 8428)
  (start-server! *PORT*))
