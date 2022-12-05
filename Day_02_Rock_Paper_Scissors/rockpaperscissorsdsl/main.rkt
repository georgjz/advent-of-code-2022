#lang racket
(provide (rename-out (rps-module-begin #%module-begin))
         ; #%module-begin
        ;  #%app           ; reexport from racket
        ;  #%top           ; reexport from racket
         #%datum         ; reexport from racket
         )
(require (for-syntax syntax/parse))

; expander
(define-syntax rps-module-begin
  (λ (stx)
    (syntax-parse stx
      ((_ hand ...)
       #'(#%module-begin
           (let ((plays (apply split-twos (map translate (list hand ...)))))
             (display "\nSolution part 1: ")
              (part1 plays)
             ))))))

; solutions
(define part1
  (λ (plays)
    (apply + (map (λ (x) (apply score x)) plays))))

; translation
(define translate
  (λ (hand)
    (match hand
      (#\A  'rock)
      (#\B  'paper)
      (#\C  'scissors)
      (#\X  'rock)
      (#\Y  'paper)
      (#\Z  'scissors))))

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
      ('rock      1)
      ('paper     2)
      ('scissors  3))))

(define result-score
  (λ (op me)
    (match (list op me)
      ((list 'rock     'paper)     6)
      ((list 'scissors  'rock)     6)
      ((list 'paper 'scissors)     6)
      ((list 'paper       'paper)  3)
      ((list 'scissors 'scissors)  3)
      ((list 'rock         'rock)  3)
      (else                        0))))

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
              ;  (strategy      (map translate hands))
               (syntax-datum  `(module f rockpaperscissorsdsl
                                 ,@hands)))
          ; (display strategy)
          (datum->syntax #f syntax-datum)))))