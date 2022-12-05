#lang racket
(provide #%module-begin  ; reexport from racket
         #%app           ; reexport from racket
         #%top           ; reexport from racket
         #%datum         ; reexport from racket
         part1
         part2
         elve-calories)
(require (for-syntax syntax/parse))

; Calculate the total calories of a given elve
(define elve-calories (λ x (apply + x)))

; Result part 1
(define part1
  (λ x
    (display "\nSolution part 1: ")
    (apply max x)))

; Result part 2
(define part2
  (λ x
    (display "\nSolution part 2: ")
    (apply + (take (sort x >) 3))))

; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define endby
    (λ (ls del)
      (foldr (λ (elem col)
               (if (equal? elem del)
                   (cons empty col)
                   (cons (cons elem (car col))
                         (cdr col))))
             (list empty)
             ls)))

  (define my-read-line
    (λ (port)
      (let* ((next    (read-line port))
            (number   (unless (eof-object? next) (string->number next))))
        (if (number?  number)
            number
            next))))

    (define read-syntax
      (λ (src in)
        (let* ((src          (port->list my-read-line in))
               (elves         (map (λ (ls) (cons 'elve-calories ls)) (endby (cdr src) "")))
               (syntax-datum  `(module f countingcaloriesdsl
                                 (part1 ,@elves)
                                 (part2 ,@elves))))
          (datum->syntax #f syntax-datum)))))
