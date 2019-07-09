#lang racket

(provide (all-defined-out))

(define-syntax do
  (syntax-rules (<-)
    ((_ bind-state e) e)
    ((_ bind-state (v <- e0) e e* ...)
     (bind-state e0 (λ (v) (do bind-state e e* ...))))
    ((_ bind-state e0 e e* ...)
     (bind-state e0 (λ (_) (do bind-state e e* ...))))))

(define return
  (λ (a)
     (λ (s)
        `(,a . ,s))))

(define bind-state
  (λ (ma f)
     (λ (s)
        (let ([vs (ma s)])
          (let ([v (car vs)]
                [s^ (cdr vs)])
            ((f v) s^))))))

(define get
  (λ (s)
     `(,s . ,s)))

(define put
  (λ (new-s)
     (λ (s)
     `(_ . ,new-s))))
