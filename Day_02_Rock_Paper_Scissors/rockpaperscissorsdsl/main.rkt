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
           (let ((plays (split-twos hand ...)))))
             (display "\nSolution part 1: ")
              (part1 plays)
             ))))))

; solutions
(define part1
  (λ (plays)
    (apply + (map (λ (x) (apply score x)) plays))))

; translation
; (define translate
;   (λ (hand)
;     (match hand
;       (#\A  #\X
;       (#\B  #\Y
;       (#\C  #\Z
;       (#\X  #\X
;       (#\Y  #\Y
;       (#\Z  #\Z)))

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
      (#\C  3)  (#\Z  3)
      )))

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