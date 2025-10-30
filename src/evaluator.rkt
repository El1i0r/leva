#lang typed/racket

(require "fresh.rkt")
(require "types.rkt")

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

#| Reading back:
  Convert the values back into their representations as syntax.
|#
(: read-back (-> (Listof String) Value Expr))
(define (read-back used-names v)
  (match v
    [(CLOS ρ x body)
     (let* ((y (freshen used-names x))
            (neutral-y (N-var y)))
       (Abs y
          (read-back (cons y used-names)
                      (evaluate (extend ρ x neutral-y) body))))]
    [(N-var x) (Var x)]
    [(N-ap rator rand)
     (App (read-back used-names rator) (read-back used-names rand))]))

#| Normalize:
  Normalize an expression.
|#
(: norm (-> Env Expr Expr))
(define (norm ρ e)
  (read-back '() (evaluate ρ e)))


;; Definitions
(: run-program (-> Env (Listof Any) Void))
(define (run-program ρ exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,(? symbol? x) ,e) rest)
     (let ([v (evaluate ρ (parse e))])
       (run-program (extend ρ (symbol->string x) v) rest))]
    [(cons e rest)
     (displayln (norm ρ (parse e)))
     (run-program ρ rest)]))


(provide (all-defined-out))
