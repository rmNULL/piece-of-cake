#lang racket
#|
(require racket/tcp)
(define-values [i o] (tcp-connect "localhost" 8428))
(write "PROG\nJALLU" o)
(close-output-port o)
(read i)
(close-input-port i)
|#

(provide allot reset-allotments!)

(define KB 1024)
(define MB (* KB 1024))

(define segsize (*   4 MB))
(define    clen (* 100 MB))



(define (unalloted allotments)
  (for/list ([blk-alloted? (in-list allotments)]
             #:unless (cdr blk-alloted?))
    blk-alloted?))
(define (fresh-allotments)
  (for/list ([chunk (in-range (quotient clen segsize))])
    (cons chunk #f)))

(define allotments #f)
(define (reset-allotments!)
  (set! allotments (fresh-allotments)))


(define (allot peer limit)
  (when (limit . < . segsize)
      (error (format "Requested limit must be >= ~a bytes" segsize)))
  (define-values [free-chunks alloted-chunks]
    (partition (λ (chunk) (false? (cdr chunk))) allotments))
  (define available (length free-chunks))
  (unless (zero? available)
    (define requested (quotient limit segsize))
    (define granted (min available requested))
    (define-values [give n-given] (split-at free-chunks granted))
    (set! allotments
          (append (map (λ (c) (cons (car c) peer)) give)
                  n-given
                  alloted-chunks))
    (map car give)))

(reset-allotments!)