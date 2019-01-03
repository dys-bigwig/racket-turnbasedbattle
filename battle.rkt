#lang racket

(define (make-comb name health strength)
  (list name health strength))
(define (comb-name comb)
  (car comb))
(define (comb-health comb)
  (cadr comb))
(define (comb-strength comb)
  (caddr comb))
(define (reduce-health comb damage)
  (let ([new-health (- (comb-health comb)
                     damage)])
    (if (> new-health 0)
      (list (comb-name comb)
            new-health
            (comb-strength comb))
      '())))

(define (get-input msg)
  (print msg)
  (read))

(define battle
  (lambda (comb1 comb2)
    (let loop ([combs (list comb1 comb2)])
      (display combs)
      (newline)
      (cond
        [(= (length combs) 1) combs]
        [else (loop (turn combs))]))))

(define turn
  (lambda (combs)
    (let loop ([choice (get-input (comb-name
                                    (car combs)))])
      (case choice
        [(1) (filter (lambda (c) (not (null? c)))
                     (list (reduce-health (cadr combs) 1)
                           (car combs)))]
        [else (loop (get-input (comb-name (car combs))))]))))

(battle (make-comb "erdrick" 10 2)
        (make-comb "slime" 2 1))
