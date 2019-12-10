#lang racket
; ayyyy
(define (zipper? a b mess)
  (let helper ((aa (string->list a)) (bb (string->list b)) (m (string->list mess)))
    (cond ([null? m] #t)
          ((and (not (null? aa)) [equal? (car aa) (car m)]) (helper (cdr aa) bb (cdr m)))
          ((and (not (null? bb)) [equal? (car bb) (car m)]) (helper aa (cdr bb) (cdr m)))
          (else #f)
          )))

(define (zipper a b c)
  (let ((a-lst (string->list a)) (b-lst (string->list b)) (c-lst (string->list c)))
    (define (zipper/list a-lst b-lst c-lst)
      (cond
        [(null? c-lst) #t]
        [(and (not (null? a-lst)) (not (null? b-lst)) (equal? (car a-lst) (car c-lst)) (equal? (car b-lst) (car c-lst)))
         (or (zipper/list (cdr a-lst) b-lst (cdr c-lst)) (zipper/list a-lst (cdr b-lst) (cdr c-lst)))]
        [(and (not (null? a-lst)) (equal? (car a-lst) (car c-lst))) (zipper/list (cdr a-lst) b-lst (cdr c-lst))]
        [(and (not (null? b-lst)) (equal? (car b-lst) (car c-lst))) (zipper/list a-lst (cdr b-lst) (cdr c-lst))]
        [else #f]))
    (zipper/list a-lst b-lst c-lst)))

(zipper "cat" "tree" "tcraete")
(zipper "cat" "tree" "catrtee")
(zipper "cat" "tree" "cttaree")
(zipper "catcatcatcatcat" "treetreetreetreetree" "catrteecatrteecatrteecatrteecatrtee")
