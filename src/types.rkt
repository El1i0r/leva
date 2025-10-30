#lang typed/racket

#| Essential Types: |#

;; Environment
(define-type Env (Listof (Pair String Value)))

;; Expression (Syntax)
(define-type Expr
  (U
    Var
    Abs
    App))

;; Value (Semantics / Results of Evaluation)
;; A value is a closure or a neutral term.
(define-type Value
  (U
    CLOS
    N-var
    N-ap))

;; Struct for variable
(struct Var
  ([name : String])
  #:transparent)

;; Struct for lambda abstraction (function)
(struct Abs
  ([param : String]
   [body : Expr])
  #:transparent)

;; Struct for function application
(struct App
  ([func : Expr]
   [arg : Expr])
  #:transparent)

#| Neutrals:
  Expressions which are not values and cannot *yet* be evaluated,
  are called neutral.
|#

; Neutral variable
(struct N-var
  ([name : String])
  #:transparent)

; Neutral application
(struct N-ap
  ([rator : Value]
   [rand : Value])
  #:transparent)

#| Closures:
  A closure packages an expression that has not yet been evaluated
  with the run-time environment in which the expression was created
|#
(struct CLOS ([env : Env] [var : String] [body : Expr]) #:transparent)

(provide Env Expr Value Var Abs App N-var N-ap CLOS)
