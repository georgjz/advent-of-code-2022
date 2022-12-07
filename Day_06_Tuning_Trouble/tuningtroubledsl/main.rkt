#lang racket
(provide (rename-out (tuningtroubledsl-module-begin #%module-begin))
         #%app           ; reexport from racket
         #%top           ; reexport from racket
         #%datum)        ; reexport from racket
(require (for-syntax syntax/parse))

; The expander
(define-syntax tuningtroubledsl-module-begin
  (λ (stx)
    (syntax-parse stx
      ((_ a ...)
       #'(#%module-begin
           (display "\nSolution part 1: ")
           ((solution 4) a ...)
           (display "\nSolution part 2: ")
           ((solution 14) a ...)
           )))))

(define solution
  (λ (pl)
    (λ signals
      (letrec ((R (λ (acc ss)
                    (let ((token (list->set (take ss pl))))
                      (cond
                        ((= pl (set-count token))  (+ acc pl))
                        (else                      (R (add1 acc) (cdr ss))))))))
        (R 0 signals)))))


; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define read-syntax
    (λ (src in)
      (let* ((src           (string->list (cadr (port->lines in))))
             (syntax-datum  `(module f tuningtroubledsl
                               ,@src)))
        (datum->syntax #f syntax-datum)))))