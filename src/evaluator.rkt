#lang typed/racket

(require "fresh.rkt")

#| Essential Types: |#

;; Environment
(define-type Env (Listof (Pair String Value)))

;; Expression
(define-type Expr
  (U
    Var
    Abs
    App))

;; Value
(define-type Value
  (U
    CLOS
    N-var
    N-ap))

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

; Neutral variable
(struct N-var
  ([name : String]))

; Neutral application
(struct N-ap
  ([rator : Any]
   [rand : Value]))

#| Closures:
  A closure packages an expression that has not yet been evaluated
  with the run-time environment in which the expression was created
|#
(struct CLOS ([env : Env] [var : String] [body : Expr]) #:transparent)

;; Extends an environment
(: extend (-> Env String Value Env))
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
(: evaluate (-> Env Expr Value))
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

#| Reading back:
  Convert the values back into their representations as syntax.
|#
(: read-back (-> (List String) Value Expr))
(define (read-back used-names v)
  (match v
    [(CLOS ρ x body)
     (let* ((y (freshen used-names x))
            (neutral-y (N-var y)))
       (parse `(λ (,y)
          ,(read-back (cons y used-names)
                      (application (extend ρ x neutral-y) body)))))]
    [(N-var x) x]
    [(N-ap rator rand)
     `(,(read-back used-names rator) ,(read-back used-names rand))]))

#| Function Application:
  Apply a function value to an argument. 
|#
(: application (-> Value Value Value))
(define (application fun arg)
  (match fun
    [(CLOS ρ x b)
     (evaluate (extend ρ x arg) b)]
    ; If the argument is neutral construct an even bigger neutral expr.
    [neutral-fun
     (N-ap fun arg)]))

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
