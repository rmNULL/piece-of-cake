#lang racket
(require racket/tcp)
(require json)

(define *PORT* 8428)

(define (join peer link)
  (define req (format "JOIN\n~a\n~a\n\n" peer link))
  (displayln req)
  (define-values [i o] (tcp-connect "localhost" *PORT*))
  (display req o)
  (close-output-port o)
  (define c (read-json i))
  (close-input-port i)
  (if (eq? (hash-ref c 'status) 'FAIL)
      #f ;; no token
      c))

(define token
  (join "JALLU" "https://apod.nasa.gov/apod/image/1906/LDN1773-Jupiter_1024.jpg"))


#|
(require racket/tcp)
(require json)

(define *PORT* 8428)
(define token "P0@252")
(define req (format "DONE ~a\n~a" token "peer" ))
(define-values [i o] (tcp-connect "localhost" *PORT*))
(write req o)
(close-output-port o)
(define c (read-json i))
(close-input-port i)
c
|#

