(module CountingCalories racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define read-syntax
    (λ (path port)
      (let* ((src-lines     (port->lines port))
             (data          (map (λ (line) (string->number line)) (cdr src-lines)))
             (elve-cals     (endby data #f))
             (elve-totals   (map (λ (cs) (apply + cs)) elve-cals))
             (part1-result  (apply max elve-totals))
             (part2-result  (apply + (take (sort elve-totals >) 3)))
             (syntax-datum  `(module solution racket
                                (display "Result part 1: ")  ,part1-result
                                (display "Result part 2: ")  ,part2-result)))
        (datum->syntax #f syntax-datum))))

  ; splits a list by a delimiter
  (define endby
    (λ (ls del)
      (foldr (λ (elem col)
               (if (equal? elem del)
                   (cons empty col)
                   (cons (cons elem (car col))
                         (cdr col))))
             (list empty)
             ls)))

) ; module