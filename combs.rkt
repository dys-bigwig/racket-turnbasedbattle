#lang rackjure
(require data/collection)
(require "state.rkt")
(require lens)

(define attack "attack")
(define hurt "hurt")
(define heal "heal")
(define run "run")

(provide (all-defined-out))

(define (asleep? comb)
  (member 'asleep (comb-status comb)))

;current-curly-dict must be alist to ensure these are sequential
(define enemy-behaviour-1
  '((4/4 . attack)))
(define enemy-behaviour-2
  '((2/4 . hurt)
    (4/4 . attack)))
(define enemy-behaviour-3
  {3/4 hurt})

(struct/lens comb (name health strength actions status) #:transparent)
(struct player comb (exp) #:transparent)
(struct enemy comb () #:transparent)
(define (new-player)
  (player "Player" 10 7 (hash 1 attack) '() 0))
(define (new-slime)
  (enemy "Slib" 3 1 enemy-behaviour-2 '(asleep)))
(define-struct-lenses player)

(define (read-player-input!)
  (displayln "command?")
  (let ([input (read)])
    (if (member input '(1 2 3 4))
      input
      (read-player-input!))))
