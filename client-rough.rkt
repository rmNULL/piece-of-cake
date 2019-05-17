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



(define token
  (join "JALLU" "http://hcmaslov.d-real.sci-nnov.ru/public/mp3/Nirvana/Nirvana%20'Come%20As%20You%20Are'.mp3"))


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

