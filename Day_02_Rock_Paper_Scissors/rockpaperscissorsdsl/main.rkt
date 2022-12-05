#lang racket
(provide #%module-begin  ; reexport from racket
         #%app           ; reexport from racket
         #%top           ; reexport from racket
         #%datum         ; reexport from racket
         )
(require (for-syntax syntax/parse))

; The reader
(module reader racket
  (provide read-syntax)
  (require (for-syntax syntax/parse))

  (define my-read-line
    (λ (port)
      (let* ((next    (read-line port))
             (symbol   (unless (eof-object? next) (string->symbol next))))
        (if (symbol? symbol)
            symbol
            next))))

    (define read-syntax
      (λ (src in)
        (let* ((strategy  (port->list my-read-line in))
               (syntax-datum  `(module f countingcaloriesdsl
                                 ,@strategy)))
          (datum->syntax #f syntax-datum)))))