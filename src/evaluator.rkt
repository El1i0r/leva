#lang typed/racket

#| Essential Types: |#

;; Environment
(define-type Env (Listof (Pair String CLOS)))

;; Expression
(define-type Expr
  (U
    Var
    Abs
    App))

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
(struct CLOS ([env : Env] [var : String] [body : Expr]) #:transparent)

;; Extends an environment
(: extend (-> Env String CLOS Env))
(define (extend ρ x v)
  (cons (cons x v) ρ))

;; Helper function for parsing a said lambda expression
(: parse (-> Any Expr))
(define (parse e)
  (match e
    [(? symbol?) (Var (symbol->string e))]
    [(list 'λ (list (? symbol? x)) b) (Abs (symbol->string x) (parse b))]
    [`(,rator ,rand) (App (parse rator) (parse rand))]
    [other (error 'parse "Invalid expression: ~s:" other)]))

#| Evaluation:
  Evaluates a expression given a environment ρ, and expression e.
|#

(: evaluate (-> Env Expr CLOS))
(define (evaluate ρ e)
  (match e
    [(Var x)
     ;; Lookup x in the environment
     (let ([xv (assoc x ρ)])
       (if xv
           (cdr xv)
           (error 'evaluate "Unknown variable ~a" x)))]
    [(Abs x b)
     (CLOS ρ x b)]
    [(App rator rand)
     (application (evaluate ρ rator) (evaluate ρ rand))]))

#| Function Application:
  Apply a function value to an argument. 
|#
(: application (-> CLOS CLOS CLOS))
(define (application clos arg)
  (match clos
    [(CLOS ρ x b)
     (evaluate (extend ρ x arg) b)]))

;; Definitions
(: run-program (-> Env (Listof Expr) Void))
(define (run-program ρ exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,x ,e) rest)
     (let ([v (evaluate ρ (parse e))])
       (run-program (extend ρ (symbol->string x) v) rest))]
    [(cons e rest)
     (displayln (evaluate ρ (parse e)))
     (run-program ρ rest)]))
