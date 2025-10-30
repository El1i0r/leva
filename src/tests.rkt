#lang racket

(require test-engine/racket-tests)
(require "evaluator.rkt")
(require "types.rkt")

;;; Top-level tests

(check-expect (norm '() (parse '(λ (x) x)))
              (Abs "x" (Var "x")))

(check-expect (norm '() (parse '((λ (x) x) (λ (y) y))))
              (Abs "y" (Var "y")))

(check-expect (norm '() (parse '(λ (x) (λ (x) x))))
              (Abs "x" (Abs "x*" (Var "x*"))))


;;; Tests requiring a local environment

(define (test-free-var-in-lambda)
  (let ([env (extend '() "y" (N-var "y"))])
    (norm env (parse '(λ (x) y)))))

(check-expect (test-free-var-in-lambda)
              (Abs "x" (Var "y")))


(define (test-app-with-free-var)
  (let ([env (extend '() "f" (N-var "f"))])
    (norm env (parse '(f (λ (x) x))))))

(check-expect (test-app-with-free-var)
              (App (Var "f") (Abs "x" (Var "x"))))


;;; Run all tests

(test)