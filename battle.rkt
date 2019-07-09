#lang racket
(require data/collection)
(require "rng.rkt")
(require "combs.rkt")
(require "state.rkt")

(define (get-action comb)
  (let ([actions (comb-actions comb)])
  (if (player? comb)
    (return (hash-ref actions
                      (read-player-input!)))
    (make-chance-choice actions))))

(define (make-chance-choice choices)
  (match-define (cons chance choice)
                (car choices))
  (do bind-state
    (rng <- get)
    (put (drop 1 rng))
    (if (< (first rng) chance)
      (return choice)
      (make-chance-choice (cdr choices)))))

(define last-one-standing?
  (compose null? cdr))

(define (battle combs)
  (match-define (cons active passive) combs)
  (cond
    [(last-one-standing? combs) (return active)]
    [else
      (do bind-state
        (let ([action (get-action active)])
          action))]))

(define erdrick (player "Erdrick" 10 8 (hash 1 'atk) '(asleep) 0))
(define slib (new-slime))

((battle `(,erdrick . ,slib)) (make-rng 1))
