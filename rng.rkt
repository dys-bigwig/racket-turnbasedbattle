#lang rackjure
(require data/collection)
(provide (all-defined-out))

(define (make-rng seed)
  (random-seed seed)
  (randoms (current-pseudo-random-generator)))
