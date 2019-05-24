#lang racket

(provide set-filesize! allot mark-allotment all-allotments-complete? total-chunks)

(define KB 1024)
(define MB (* KB 1024))

(define SEG 2)

(define segsize (* SEG MB))
(define    clen (make-hash))

(define (total-chunks token)
  (length (hash-ref allotments-by-token token)))

(define (set-filesize! token len)
  (define lenm  (+ (quotient  len MB)
                   (if (zero? (remainder len MB)) 0 1)))
  (define (fresh-allotments)
    (define q (quotient lenm SEG))
    (define r (remainder lenm SEG))
    (define segs
      (for/list ([start (in-range q)])
        (cons start UNALLOTED)))
    (if (zero? r)
        segs
        (cons (cons q UNALLOTED) segs)))
  
  (define (reset-allotments!)
    (hash-set! allotments-by-token token (fresh-allotments)))
  (hash-set! clen token lenm)
  (reset-allotments!))

(define (unalloted allotments)
  (for/list ([blk-alloted? (in-list allotments)]
             #:unless (cdr blk-alloted?))
    blk-alloted?))

(define UNALLOTED 'unalloted)
(define COMPLETED 'completed)
(define chunk-offset car)
(define chunk-state cdr) 
(define (chunk-unalloted? v) (equal? UNALLOTED (chunk-state v)))
(define (chunk-completed? v) (pair? (chunk-state v)))



;; token => ( (chunk-begin . state) * )
;; state is one of ( UNALLOTED (COMPLETED . "PEER") "PEER NAME" )
(define allotments-by-token (make-hash))


(define (find-chunk peer allotments)
  (for/first ([chunk allotments]
              #:when (and (equal? (chunk-state chunk) peer)))
    chunk))

(define (mark-allotment token peer)
  (define allotments (hash-ref allotments-by-token token))
  (define chunk (find-chunk peer allotments))
  (define cbytes
    (when (and chunk (not (chunk-completed? chunk)))
      (define others (filter (λ (c) (not (eq? c chunk))) allotments))
      (set! chunk (cons (chunk-offset chunk)
                        (cons COMPLETED peer)))
      (hash-set! allotments-by-token token (cons chunk others))
      (offset->byterange (chunk-offset chunk))))
  (and (list? cbytes) cbytes))

(define (offset->byterange offset)
  ;; exclusive end
  (list (*  offset segsize)
        (sub1 (+ (*  offset segsize) segsize))))

(define (allot token peer)
  (define allotments (hash-ref allotments-by-token token))
  (define chunk (find-chunk peer allotments))
  (if chunk
    (offset->byterange (chunk-offset chunk))
    (let ()
      (define-values [u t] (partition chunk-unalloted? allotments))
      (unless (empty? u)
        (define chunk (first u))
        (define offset (chunk-offset chunk))
        (hash-set! allotments-by-token token
                   (append (rest u) (cons (cons offset peer) t)))
        (offset->byterange offset)))))

(define (all-allotments-complete? token)
  (define allotments (hash-ref allotments-by-token token))
  (for/and ([chunk allotments])
    (chunk-completed? chunk)))
