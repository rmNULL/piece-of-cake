#lang racket
(require racket/tcp)
(require json)

(define *PORT* 8428)

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
  (define status
    (system "curl -r ~a -o ~a ~a" range fname link))
  status)

(define (ask-req-cycle peer token)
  (define (handle-done c)
    (not (hash-ref c 'completed)))

  (define (handle-ask c)
    (define link (hash-ref c 'link))
    (define chunk (hash-ref c 'chunk))
    (let try-downloading ([attempts 3])
      (define downloaded? (download link chunk))
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

(define token
  (join "JALLU"
        "https://www.nasa.gov/sites/default/files/thumbnails/image/stsci-h-p1930a-f-3213x2836.jpg"))


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

