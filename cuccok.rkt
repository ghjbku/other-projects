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

#lang racket
(require racket/hash)

(define (make-graph lst)
  (foldl (lambda (a result)
           (hash-union result
                       (hash (car a)  (cdr a))
           #:combine/key (lambda (k v1 v2) (append v1 v2))
           ))
         (hash)
         lst))
  (make-graph '([2 3] [3 1] [3 6] [6 1] [1 5] [1 4] [9 5] [4 7] [4 8] [8 7] [6 9]))
  
  
  (define (monoton lst)
  (let loop ((lst lst) (utolso (last lst)) (vege null))
    (cond
      [(null? lst) vege]
      [(< (last lst) utolso) vege]
      [else (loop (drop-right lst 1) (last lst) (append (list (last lst)) vege))])))

 

(define (minlst lst mitol)
  (if (null? lst) null
      (let ((lst (filter (lambda (i) (> i mitol)) lst)))
        (foldl min (first lst) (rest lst)))))

 

(define (nextlex lst)
  (let ((vege (monoton lst)))
    (if (equal? lst vege) "Nincs lexikografikusan rakovetkezo."
        (let* ((cserepos (sub1 (- (length lst) (length vege)))) (csere (list-ref lst cserepos)))
          (let* ((eddigimin (minlst vege csere)) (minpos (index-of vege eddigimin)) (vegeuj (list-set vege minpos csere)))
            (append (drop-right (list-set lst cserepos eddigimin) (length vege)) (reverse vegeuj)))))))

(nextlex '(1 2 3 4))
(nextlex '(1 2 4 3))
(nextlex '(1 3 2 4))
