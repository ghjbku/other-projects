(define (lcsht a b)
  (let ((a-lst (string->list a)) (b-lst (string->list b)) (ht (make-hash)))
    (define (lcs/list a-lst b-lst)
      (let ((cache-result (hash-ref ht (list a-lst b-lst) "not-found")))
        (cond
          [(not (equal? cache-result "not-found")) cache-result]
          [else
             (cond
               [(or (null? a-lst) (null? b-lst)) 0]
               [(equal? (car a-lst) (car b-lst))
                (let ((result (+ 1 (lcs/list (cdr a-lst) (cdr b-lst)))))
                  (hash-set! ht (list a-lst b-lst) result)
                  result)
                ]
               [else
                (let ((result (max (lcs/list a-lst (cdr b-lst)) (lcs/list (cdr a-lst) b-lst))))
                  (hash-set! ht (list a-lst b-lst) result)
                  result)
                ])])))
    (lcs/list a-lst b-lst)))
