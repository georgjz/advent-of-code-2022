#lang racket
(provide (rename-out (supplystacksdsl-module-begin #%module-begin))
         #%app           ; reexport from racket
         #%top           ; reexport from racket
         #%datum         ; reexport from racket
         list)           ; reexport from racket
(require (for-syntax syntax/parse))

; The expander
(define-syntax supplystacksdsl-module-begin
  (λ (stx)
    (syntax-parse stx
      ((_ commands ...)
       #'(#%module-begin
           (display "\nSolution part 1: ")
           (map car (part1 initial-stacks commands ...))
           (display "\nSolution part 2: ")
           (map car (part2 initial-stacks commands ...)))))))

; (define initial-stacks
;   (list (list 'N 'Z)
;         (list 'D 'C 'M)
;         (list 'P)))
(define initial-stacks
  (list (list 'G 'P 'N 'R)
        (list 'H 'V 'S 'C 'L 'B 'J 'T)
        (list 'L 'N 'M 'B 'D 'T)
        (list 'B 'S 'P 'V 'R)
        (list 'H 'V 'M 'W 'S 'Q 'C 'G)
        (list 'J 'B 'D 'C 'S 'Q 'W)
        (list 'L 'Q 'F)
        (list 'V 'F 'L 'D 'T 'H 'M 'W)
        (list 'F 'J 'M 'V 'B 'P 'L)))

(define crate-move
  (λ (f)
    (λ (stacks cnt src dst)
      (let* ((src-stack  (list-ref stacks (sub1 src)))
             (dst-stack  (list-ref stacks (sub1 dst)))
             (crates     (f (take src-stack cnt)))
             (new-src    (drop src-stack cnt))
             (new-dst    (append crates dst-stack)))
        (list-set (list-set stacks (sub1 src) new-src)
                  (sub1 dst)
                  new-dst)))))

(define crate-move-9000 (crate-move reverse))

(define crate-move-9001 (crate-move identity))

(define solution
  (λ (move)
    (λ (supplies cmd . cmds)
      (let* ((cnt           (car cmd))
             (src           (cadr cmd))
             (dst           (caddr cmd))
             (new-supplies  (move supplies cnt src dst)))
        (cond
          ((null? cmds)  new-supplies)
          (else          (apply (solution move) (cons new-supplies cmds))))))))

(define part1 (solution crate-move-9000))

(define part2 (solution crate-move-9001))

; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define ranges-read
    (λ (port)
      (let ((next (read-line port)))
        (if (eof-object? next)
            next
            (map string->number (cdr (regexp-split #rx"(move | to | from )" next)))))))

  (define read-syntax
    (λ (src in)
      (let* ((src           (port->list ranges-read in))
             (cmds          (map (λ (cmd) (cons 'list cmd)) src))
             (syntax-datum  `(module f supplystacksdsl
                               ,@(cdr cmds))))
        (datum->syntax #f syntax-datum)))))