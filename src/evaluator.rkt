#lang typed/racket

#| Essential Types: |#

;; Environment
(define-type Env (List (Pair String CLOS)))

;; Expression
(define-type Expr
  (U
    (Var)
    (Abs)
    (App)))

; Struct for variable
(struct Var 
  ([name : String]))

; Struct for lambda abstraction (function)
(struct Abs 
  ([param : String]
   [body : Expr]))

; Struct for function application
(struct App 
  ([func : Expr]
   [arg : Expr]))

#| Closures:
  A closure packages an expression that has not yet been evaluated
  with the run-time environment in which the expression was created
|#
(struct CLOS ([env : Env] [var : Symbol] [body : Expr]) #:transparent)

;; Extends an environment
(define (extend [ρ : Env] [x : String] [v : CLOS])
  (cons (cons x v) ρ))

#| Evaluation:
  Evaluates a expression given a environment ρ, and expression e.
|#
(define (evaluate ρ e)
  (match e
    [`(λ (,x) ,b)
     (CLOS ρ x b)]
    [x #:when (symbol? x)
     (let ((xv (assv x ρ)))
       (if xv
           (cdr xv)
           (error 'val "Unknown variable ~a" x)))]
    [`(,rator ,rand)
     (do-ap (evaluate ρ rator) (evaluate ρ rand))]))

(define (do-ap clos arg)
  (match clos
    [(CLOS ρ x b)
     (evaluate (extend ρ x arg) b)]))

(define (run-program ρ exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,x ,e) rest)
     (let ([v (evaluate ρ e)])
       (run-program (extend ρ x v) rest))]
    [(cons e rest)
     (displayln (evaluate ρ e))
     (run-program ρ rest)]))

(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  "*")))

(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))
