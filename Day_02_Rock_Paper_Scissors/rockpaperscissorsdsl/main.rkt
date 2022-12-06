#lang racket
(provide (rename-out (rps-module-begin #%module-begin))
         #%datum)        ; reexport from racket
(require (for-syntax syntax/parse))

; expander
(define-syntax rps-module-begin
  (λ (stx)
    (syntax-parse stx
      ((_ hand ...)
       #'(#%module-begin
           (let ((plays (split-twos hand ...)))
             ; FIXME: why?
             (print (part1 plays))
             (part2 plays)))))))

; solutions
(define part1
  (λ (plays)
    (display "\nSolution part 1: ")
    (apply + (map (λ (x) (apply score x)) plays))))

(define part2
  (λ (plays)
    (display "\nSolution part 2: ")
    (apply + (map (λ (x) (apply match-strategy x)) plays))))

; split
(define split-twos
  (λ (opp me . rest)
    (if (null? rest)
        (list (list opp me))
        (cons (list opp me)
              (apply split-twos rest)))))

; scoring functions
(define score
  (λ (opp me)
    (+ (hand-score me) (result-score opp me))))

(define hand-score
  (λ (hand)
    (match hand
      (#\A  1)  (#\X  1)
      (#\B  2)  (#\Y  2)
      (#\C  3)  (#\Z  3))))

(define result-score
  (λ (op me)
    (match (list op me)
      ((list #\A #\Y)  6)
      ((list #\C #\X)  6)
      ((list #\B #\Z)  6)
      ((list #\B #\Y)  3)
      ((list #\C #\Z)  3)
      ((list #\A #\X)  3)
      (else            0))))

(define match-strategy
  (λ (opp strategy)
    (let* ((winner (λ (hand)
                     (match hand
                       (#\A  #\Y)
                       (#\B  #\Z)
                       (#\C  #\X))))
           (loser (λ (hand)
                    (match hand
                      (#\A  #\Z)
                      (#\B  #\X)
                      (#\C  #\Y)))))
      (match strategy
        (#\Z (+ 6 (hand-score (winner opp))))  ; win
        (#\Y (+ 3 (hand-score opp)))  ; draw
        (#\X (+ 0 (hand-score (loser opp))))))))  ; lose

; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define read-hands
    (λ (port)
      (let* ((next  (read-char port)))
        (when (or (eof-object? next) (char-alphabetic? next))
          next))))

  (define read-syntax
    (λ (src in)
      (let* ((hands         (filter-not void? (port->list read-hands in)))
             (syntax-datum  `(module f rockpaperscissorsdsl
                               ,@hands)))
        (datum->syntax #f syntax-datum)))))