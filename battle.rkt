#lang racket
(require data/collection)
(require "rng.rkt")
(require "combs.rkt")
(require "state.rkt")

(define (player-turn player enemy)
  (let ([action (hash-ref (comb-actions player)
                          (read-player-input!))])
    (return action)))

(define (enemy-turn enemy player)
  (let ([actions (comb-actions enemy)])
  (do bind-state
    (rng <- get)
    (put (drop (length actions) rng))
    (let ([action (make-chance-choice actions
                                      (take (length actions) rng))])
      (return (action enemy player))))))

(define (get-action comb)
  (let ([actions (comb-actions comb)])
    (if (player? comb)
      (hash-ref actions
                (read-player-input!))
      (make-chance-choice actions))))

(define (make-chance-choice choices rands)
  (match-define (cons chance choice)
    (car choices))
  (if (< (first rands) chance)
    choice
    (make-chance-choice (rest choices) (rest rands))))

(define last-one-standing?
  (compose null? cdr))

(define (battle combs)
  (match-define (cons active passive) combs)
  (cond
    [(last-one-standing? combs) (return active)]
    [(player? active) (player-turn active passive)]
    [(enemy? active) (enemy-turn active passive)]))


(define erdrick (new-player))
(define slib (new-enemy))

((battle `(,slib . ,erdrick)) (make-rng 1))
