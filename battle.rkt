#lang rackjure

; COMBATANT CONSTRUCTORS ;
(define (make-comb name health strength input-proc type [status '()])
  {  'name name
     'health health
     'strength strength
     'input-proc input-proc
     'type type
     'status status })

(define (new-player name)
  (make-comb name
             10
             3
             (λ ()
                (get-input! read
                            {1 normal-attack}
                            "1. ATTACK 2. NOTHING 3. UNIMPLEMENTED 4. FOO"))
             'PLAYER))

(define (random-slime)
  (make-comb 'slime
             5
             ( random 1 5 )
             { 1 'ATTACK }
             'SLIME))

; PREDICATES ;
(define (player? comb)
  (equal? (comb 'type) 'PLAYER))

(define (alive? comb)
  (> (comb 'health) 0))

(define (asleep? comb)
  (member 'ASLEEP (comb 'status)))

;; I/O ;;
(define (get-input! input-proc valid-inputs [msg ""])
  (let loop ()
    (display msg)
    (let ([choice (input-proc)])
      (or (assq choice valid-inputs)
          (loop)))))

;; BATTLE ;;
(define (adjust-health comb amount)
  (let ([new-health (+ (comb 'health)
                       amount)])
    (comb 'health new-health)))

(define (reduce-health comb amount)
  (adjust-health comb (- amount)))
(define (increase-health comb amount)
  (adjust-health comb amount))

(define normal-attack
  (λ (combs)
     (list (car combs)
           (reduce-health (cadr combs)
                          ((car combs) 'strength)))))

(define (last-one-standing? combs)
  (= (length combs) 1))

(define battle
  (lambda (player enemy)
    (let loop ([combs (list player enemy)])
      (if (last-one-standing? combs)
        (car combs)
        (loop (turn (car combs)
                    (cadr combs)))))))

(define (turn active passive)
  (if (asleep? active)
    (list passive active) 
    (begin
      (displayln (format "~a's turn"
                         (active 'name)))
      (let* ([input ((active 'input-proc))]
             [action (cdr input)])
        (~>> (list active passive)
             action
             (filter alive?)
             reverse)))))
