#lang racket
(require data/collection)
(require "state.rkt")
(require lens)
(require fancy-app)

(provide (all-defined-out))

(define hurt "hurt")

(define (attack attacker target)
  (let ([strength (comb-strength attacker)])
    (lens-transform comb-health-lens
                    target
                    (- _ strength))))

(define (asleep? comb)
  (member 'asleep (comb-status comb)))

;must be alist to ensure these are sequential
(define enemy-behaviour-1
  `((4/4 . ,attack)))
(define enemy-behaviour-2
  `((4/4 . ,attack)
    (4/4 . ,attack)))

(struct/lens comb (name health strength agility actions status) #:transparent)
(struct player comb () #:transparent)
(struct enemy comb () #:transparent)
(define (new-player)
  (player "player" 25 28 22 (hash 1 attack) '()))
(define (new-enemy)
  (enemy "drakeema" 16 22 26 enemy-behaviour-2 '()))
(define-struct-lenses player)

(define (read-player-input!)
  (displayln "command?")
  (let ([input (read)])
    (if (member input '(1 2 3 4))
      input
      (read-player-input!))))
