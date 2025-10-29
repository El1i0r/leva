#lang typed/racket

(: add-* (-> Symbol Symbol))
(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  "*")))

(: freshen (-> (List Symbol) Symbol Symbol))
(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

(provide freshen)
