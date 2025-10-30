#lang typed/racket

(require "types.rkt")

(: with-numerals (-> Any (Listof Any)))
(define (with-numerals e)
  `((define church-zero
      (λ (f)
        (λ (x)
          x)))
    (define church-add1
      (λ (n-1)
        (λ (f)
          (λ (x)
            (f ((n-1 f) x))))))
    ,e))

(define church-add
  `(λ (j)
    (λ (k)
      (λ (f)
        (λ (x)
          ((j f) ((k f) x)))))))

(: to-church (-> Integer Any))
(define (to-church n)
  (cond [(zero? n) 'church-zero]
        [(positive? n)
         (let ([church-of-n-1 (to-church (sub1 n))])
           `(church-add1 ,church-of-n-1))]))

(provide with-numerals)
