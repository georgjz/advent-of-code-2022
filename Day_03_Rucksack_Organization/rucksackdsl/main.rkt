#lang racket
(provide #%module-begin
         #%app
         #%top
         #%datum
         part1
         part2)
(require (for-syntax syntax/parse))

(define part1
  (λ rucksacks
    (display "\nSolution part 1: ")
    (let* ((items          (map string->list rucksacks))
           (priorities     (map (λ (ls) (map priority ls)) items))
           (compartments   (map (λ (ls)
                                  (let-values (((a b) (split-at ls (/ (length ls) 2))))
                                    (list a b)))
                                priorities))
           (top-priorities  (map (λ (rs)
                                   (car
                                    (set->list
                                     (set-intersect (list->set (car rs))
                                                    (list->set (cadr rs))))))
                            compartments)))
      (apply + top-priorities))))

(define part2
  (λ rucksacks
    (display "\nSolution part 2: ")
    (let* ((items          (map string->list rucksacks))
           (priorities     (map (λ (ls) (map priority ls)) items))
           (groups          (apply split-threes priorities))
           (top-priorities  (map (λ (rs)
                                   (car
                                    (set->list
                                     (set-intersect (list->set (car rs))
                                                    (list->set (cadr rs))
                                                    (list->set (caddr rs))))))
                                 groups)))
      (apply + top-priorities))))

(define priority
  (λ (c)
    (- (char->integer c) (if (char-lower-case? c) 96 38))))

(define split-threes
  (λ (a b c . rest)
    (if (null? rest)
        (list (list a b c))
        (cons (list a b c)
              (apply split-threes rest)))))

; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define read-rucksacks
    (λ (port)
      (let* ((next   (read-line port))
             (items  (unless (eof-object? next) (string->list next))))
        (if (eof-object? next)
            next
            (let-values (((a b) (split-at items (/ (length items) 2))))
              (list a b))))))

  (define read-syntax
    (λ (src in)
      (let* ((rucksacks     (port->lines in))
             (syntax-datum  `(module f rucksackdsl
                               (part1 ,@(cdr rucksacks))
                               (part2 ,@(cdr rucksacks)))))
        (datum->syntax #f syntax-datum)))))