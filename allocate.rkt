#lang racket

(provide set-filesize! allot reset-allotments! mark-allotment all-allotments-complete?)

(define KB 1024)
(define MB (* KB 1024))

(define SEG 2)

(define segsize (* SEG MB))
(define    clen #f)
(define (set-filesize! len)
  (set! clen (+ (quotient  len MB)
                (if (zero? (remainder len MB)) 0 1)))
  (reset-allotments!))

(define (unalloted allotments)
  (for/list ([blk-alloted? (in-list allotments)]
             #:unless (cdr blk-alloted?))
    blk-alloted?))

(define UNALLOTED 0)
(define COMPLETED 1)
(define chunk-offset car)
(define chunk-state cdr) 
(define (chunk-unalloted? v) (equal? UNALLOTED (chunk-state v)))
(define (chunk-completed? v) (pair? (chunk-state v)))

(define (fresh-allotments)
  (define q (quotient clen SEG))
  (define r (remainder clen SEG))
  (define segs
    (for/list ([start (in-range q)])
      (cons start UNALLOTED)))
  (if (zero? r)
      segs
      (cons (cons q UNALLOTED) segs)))

;; ( (chunk-begin . state) * )
;; state is one of ( UNALLOTED (COMPLETED . "PEER") "PEER NAME" )
(define allotments #f)
(define (reset-allotments!)
  (set! allotments (fresh-allotments)))

(define (find-chunk peer)
  (for/first ([chunk allotments]
              #:when (and (equal? (chunk-state chunk) peer)))
    chunk))

(define (mark-allotment peer)
  (define chunk (find-chunk peer))
  (define cbytes
    (when (and chunk (not (chunk-completed? chunk)))
      (define others (filter (λ (c) (not (eq? c chunk))) allotments))
      (set! chunk (cons (chunk-offset chunk)
                        (cons COMPLETED peer)))
      (set! allotments (cons chunk others))
      (offset->byterange (chunk-offset chunk))))
  (and (list? cbytes) cbytes))

(define (offset->byterange offset)
  ;; exclusive end
  (list (*  offset segsize)
        (sub1 (+ (*  offset segsize) segsize))))

(define (allot peer)
  (define alloted? (find-chunk peer))
  (unless alloted?
    (define-values [u t] (partition chunk-unalloted? allotments))
    (unless (empty? u)
      (define chunk (first u))
      (define offset (chunk-offset chunk))
      (set! allotments
            (append (rest u) (cons (cons offset peer) t)))
      (offset->byterange offset))))

(define (all-allotments-complete?)
  (for/and ([chunk allotments])
    (chunk-completed? chunk)))