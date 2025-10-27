#lang racket

#| Closures:
  A closure packages an expression that has not yet been evaluated
  with the run-time environment in which the expression was created
|#
(struct CLOS (env var body) #:transparent)

(define (extend ρ x v)
  (cons (cons x v) ρ))

(define (val ρ e)
  (match e
    [`(λ (,x) ,b)
     (CLOS ρ x b)]
    [x #:when (symbol? x)
     (let ((xv (assv x ρ)))
       (if xv
           (cdr xv)
           (error 'val "Unknown variable ~a" x)))]
    [`(,rator ,rand)
     (do-ap (val ρ rator) (val ρ rand))]))

(define (do-ap clos arg)
  (match clos
    [(CLOS ρ x b)
     (val (extend ρ x arg) b)]))

(define (run-program ρ exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,x ,e) rest)
     (let ([v (val ρ e)])
       (run-program (extend ρ x v) rest))]
    [(cons e rest)
     (displayln (val ρ e))
     (run-program ρ rest)]))

(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  "*")))

(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))
