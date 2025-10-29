#lang typed/racket

(: add-* (-> String String))
(define (add-* x)
   (string-append x) "*")

(: freshen (-> (List String) String String))
(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

(provide freshen)
