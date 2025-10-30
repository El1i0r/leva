#lang typed/racket

(require "evaluator.rkt")
(require "types.rkt")

(: main (-> Void))
(define (main)
  (displayln "Leva REPL (enter :q to quit)")
  (let loop ([ρ '()])
    (printf "> ")
    (flush-output)
    (let ([input (read)])
      (unless (equal? input '(:q))
        (displayln input)
        (loop '()))
        )))
