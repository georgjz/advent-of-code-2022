#lang racket
 (provide (rename-out (campcleanupdsl-begin-module #%module-begin))
          #%app           ; reexport from racket
          #%top           ; reexport from racket
          #%datum         ; reexport from racket
          list)           ; reexport from racket
(require (for-syntax syntax/parse)
         srfi/1)  ; for iota

; The expander
(define-syntax campcleanupdsl-begin-module
  (λ (stx)
    (syntax-parse stx
      ((_ a ...)
       #'(#%module-begin
           (display "\nSolution part 1: ")
           (part1 a ...)
           (display "\nSolution part 2: ")
           (part2 a ...))))))

(define sub-range?
  (λ (a b c d)
    (let ((set1 (iota (add1 (- b a)) a 1))
          (set2 (iota (add1 (- d c)) c 1)))
      (or (subset? set1 set2) (subset? set2 set1)))))

(define intersect?
  (λ (a b c d)
    (let ((set1 (iota (add1 (- b a)) a 1))
          (set2 (iota (add1 (- d c)) c 1)))
      (not (empty? (set-intersect set1 set2))))))

(define solution
  (λ (pred?)
    (λ bs
      (foldl (λ (a b) (if (apply pred? a) (+ 1 b) b))
            0
            bs))))

(define part1 (solution sub-range?))

(define part2 (solution intersect?))

; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define ranges-read
    (λ (port)
      (let ((next (read-line port)))
        (if (eof-object? next)
            next
            (map string->number (regexp-split #rx"[-,]" next))))))

  (define read-syntax
    (λ (src in)
      (let* ((ranges        (port->list ranges-read in))
             (src           (map (λ (range) (cons 'list range)) (cdr ranges)))
             (syntax-datum  `(module f campcleanupdsl
                               ,@src)))
        (datum->syntax #f syntax-datum)))))
