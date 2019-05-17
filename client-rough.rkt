#lang racket
(require racket/tcp)
(require json)

(define *PORT* 8428)
(define PEERNAME "λ")

(define (join peer link)
  (define req (format "JOIN\n~a\n~a" peer link))
  (define-values [i o] (tcp-connect "localhost" *PORT*))
  (write req o)
  (close-output-port o)
  (define c (read-json i))
  (close-input-port i)
  (if (eq? (hash-ref c 'status) 'FAIL)
      #f ;; no token
      c))

(define (download link chunk)
  (displayln chunk (current-error-port))
  (define range (string-join (map ~a chunk) "-"))
  (define fname (string-append range ".part"))
  (define cmd
    (format "curl -r ~a -o ~a ~a 2>/dev/null 1>&2" range fname link))
  (define status (system cmd))
  status)

(define (ask-req-cycle peer token #:callback [cb void])
  (define (handle-done c)
    (not (hash-ref c 'completed)))

  (define (handle-ask c)
    (define link (hash-ref c 'link))
    (define chunk (hash-ref c 'chunk))
    (let try-downloading ([attempts 3])
      (define downloaded? (download link chunk))
      (when downloaded? (cb chunk))
      (if downloaded?
          #t
          (if (positive? attempts)
              (try-downloading (sub1 attempts))
              #f))))
  
  (define cc (list`("ASK" . ,handle-ask)
                  `("DONE" . ,handle-done)))
  (define (switch-action!)
    (set! cc (reverse cc))
    #t)

  (let loop ([acn-hdl (first cc)])
    [define action-handler (cdr acn-hdl)]
    [define action (car acn-hdl)]
    [define req (format "~a ~a\n~a" action token "peer")]
    (define-values (i o) (tcp-connect "localhost" *PORT*))
    (write req o)
    (close-output-port o)
    (define c (read-json i))
    (close-input-port i)
    (println c)
    (if (equal? (hash-ref c 'status) "OK")
        (and (action-handler c)
             (switch-action!)
             (loop (first cc)))
        (error (hash-ref c 'message "flop")))
    ))




#;(define resp
  (join "JALLU"
        "https://www.nasa.gov/sites/default/files/thumbnails/image/stsci-h-p1930a-f-3213x2836.jpg"))

#;(when (equal? (hash-ref resp 'status) "OK")
  (define token (hash-ref resp 'token))
  (ask-req-cycle PEERNAME token))


(require racket/gui)
(define main-window
  (new frame% [label "Client sketch"]
       [width 800] [height 600]
       [stretchable-width #t] [stretchable-height #t]))

(define (clear-main-window)
  (send main-window change-children (λ (c) '())))

(define (verify-link-and-dl link)
  (define resp (join PEERNAME link))
  (define fail? (equal? (hash-ref resp 'status) "FAIL"))
  (if fail?
      (hash-ref resp 'message)
      (string-append "TOKEN " (hash-ref resp 'token))))

  #;(when (equal? (hash-ref resp 'status) "OK")
      (define token (hash-ref resp 'token))
    (ask-req-cycle PEERNAME token))

(define (draw-join)
  (clear-main-window)
  (define row (new horizontal-panel%
                   [stretchable-height #f]
                   [parent main-window]))
  (define link-field
    (new text-field% (label "Link") (parent row) (init-value "")
         (style '(single vertical-label))))

  (define log-field (new message% (label "") (parent main-window)))

  (define go-btn (new button% (parent row) (label "Get Token")
                      (callback (λ (_m _b)
                                  (define link (send link-field get-value))
                                  (thread (λ ()
                                               (define msg (with-handlers ([hash? (λ (h) (hash-ref h 'message))])
                                                             (verify-link-and-dl link)))
                                               (send log-field set-label msg)))))))
  (send link-field get-value))




(define (draw-dl)
  (clear-main-window)
  (define row (new horizontal-panel%
                   [stretchable-height #f]
                   [parent main-window])) 
  (define token-field
    (new text-field% (label "token") (parent row) (init-value "")
         (style '(single vertical-label))))
  (define log-field (new message% (label "") (parent main-window)))
  (define go-btn (new button% (parent row) (label "Start Download")
                      (callback (λ (_m _b)
                                  (thread (λ ()
                                            (define (update-progress brange)
                                              (define range (string-join (map ~a brange) "-"))
                                              (define msg (string-append "completed " range))
                                              (send log-field set-label msg))
                                            (define token (send token-field get-value))
                                            (define msg
                                              (ask-req-cycle PEERNAME token #:callback update-progress))
                                            (send log-field set-label msg)))))))
  (void))

(define menu-bar (new menu-bar% [parent main-window]))
(define m (new menu% [label "Options"] [parent menu-bar]))
(new menu-item% [parent m] [label "Join"] [callback (λ (m c) (draw-join))])
(new separator-menu-item% [parent m])
(new menu-item% [parent m] [label "Download"]
     [callback (λ (m c) (draw-dl))])
(new separator-menu-item% [parent m])
(new menu-item% [parent m] [label "Exit"]
     [callback (λ (m c) (send main-window show #f))])


(send main-window show true)


#|
(require racket/tcp)
(require json)

(define *PORT* 8428)
(define token "P0@252")
(define req (format "DONE ~a\n~a" token "peer"))
(define-values [i o] (tcp-connect "localhost" *PORT*))
(write req o)
(close-output-port o)
(define c (read-json i))
(close-input-port i)
c
|#

