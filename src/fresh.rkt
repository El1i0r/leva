#lang typed/racket

(: add-* (-> String String))
(define (add-* x)
   (string-append x "*"))

(: freshen (-> (Listof String) String String))
(define (freshen used x)
  (if (member x used)
      (freshen used (add-* x))
      x))

(provide freshen)