#lang racket
; ayyyy
(define (zipper? a b mess)
  (let helper ((aa (string->list a)) (bb (string->list b)) (m (string->list mess)))
    (cond ([null? m] #t)
          ((and (not (null? aa)) [equal? (car aa) (car m)]) (helper (cdr aa) bb (cdr m)))
          ((and (not (null? bb)) [equal? (car bb) (car m)]) (helper aa (cdr bb) (cdr m)))
          (else #f)
          )))
