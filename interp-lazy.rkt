#lang racket
(require racket/trace)

(struct closure (f ε))
(struct thunk (fv cached?)
  #:transparent
  #:mutable)

;; ∙ eager : Computation ⇒ Value
;; ∙ lazy  : Computation ⇒ Thunk ⇒ Value

;; Computation → Thunk
(define-syntax delay*
  (syntax-rules ()
    [(_ compu) (thunk (λ () compu) #f)]))

;; Thunk → Value
(define force*
  (λ (th)
    (match th
      [(thunk c #t) c]
      [(thunk c #f)
       (let ([v (c)])
         (cond
           [(thunk? v) (force* v)]
           [else
            (begin (set-thunk-fv! th v)
                   (set-thunk-cached?! th #t)
                   v)]))]
      [else (error "not a thunk.")])))

;; Exp × Env → Thunk
(define val-of
  (λ (expr ε)
    (match expr
      [(? symbol? x) (delay* (ap-ε ε x))]
      [(? number? n) (delay* n)]
      [(? boolean? b) (delay* b)]
      [`(quote ,x) (delay* x)]
      
      [`(,uop ,e)
       #:when (memq uop '(zero? add1 sub1 null? car cdr))
       (delay* (let ([v (force* (val-of e ε))])
                 ((eval uop) v)))]
      
      [`(,bop ,e₁ ,e₂)
       #:when (memq bop '(+ * cons))
       (delay* (let ([v₁ (force* (val-of e₁ ε))]
                     [v₂ (force* (val-of e₂ ε))])
                 ((eval bop) v₁ v₂)))]

      ;; cons^ should not evaluate its arguments
      [`(cons^ ,e₁ ,e₂)
       (delay* (let ([v₁ (delay* (val-of e₁ ε))]
                     [v₂ (delay* (val-of e₂ ε))])
                 (cons v₁ v₂)))]
      
      [`(if ,cnd ,thn ,els)
       (delay* (let ([cnd^ (force* (val-of cnd ε))])
                 (if cnd^
                     (val-of thn ε)
                     (val-of els ε))))]

      [`(let ([,x ,e]) ,body)
       (delay* (let* ([w (force* (val-of e ε))]
                      [v (delay* w)])
                 (val-of body (ext-ε x v ε))))]

      ;; let^ should not evaluate its rhs
      [`(let^ ([,x ,e]) ,body)
       (let ([v (delay* (val-of e ε))])
         (delay* (val-of body (ext-ε x v ε))))]
      
      [`(λ (,x) ,e)
       #:when (symbol? x) (delay* (closure expr ε))]
      
      [`(,e₁ ,e₂)
       (let ([v₁ (delay* (val-of e₁ ε))]
             [v₂ (delay* (val-of e₂ ε))])
         (delay* (match (force* v₁)
                   [(closure `(λ (,x) ,e) ε@)
                    (val-of e (ext-ε x v₂ ε@))])))])))

;; → Env
(define (mt-ε) '())
;; Key × Thunk × Env → Env
(define (ext-ε x v ε) (cons `(,x . ,v) ε))
;; Env × Key → Thunk
(define (ap-ε ε y)
  (match ε
    ['() (error "unbound variable" y)]
    [`((,x . ,v) . ,ε^)
     (cond
       [(eqv? y x) v]
       [else (ap-ε ε^ y)])]))
;; Exp → Value
(define (interp expr) (force* (val-of expr (mt-ε))))

(define ω '(λ (x) (x x)))
(define Ω `(,ω ,ω))
(define fix '(λ (f)
               ((λ (x) (f (x x)))
                (λ (x) (f (x x))))))
#;
(trace val-of)
(interp `((λ (x) 42) ,Ω))

(interp '(let ([x 1])
           (let ([y (+ x x)])
             (let ([f (λ (x) (+ x y))])
               (let ([x 4])
                 (f (+ x y)))))))

(interp '(let ([factf (λ (!)
                        ;; let^ .. ⇒ 120
                        ;; let ... ⇒ ⊥
                        (let^ ([!! (! !)])
                          (λ (n)
                            (if (zero? n)
                                1
                                (* n (!! (sub1 n)))))))])
           ((factf factf) 5)))
;; ⇒ 120

(interp `(let ([factf (λ (!)
                        (λ (n)
                          (if (zero? n)
                              1
                              (* n (! (sub1 n))))))])
           ((,fix factf) 5)))
;; ⇒ 120

#;
(interp '(let ([fix (λ (f)
                      (let^ ([x (f x)]) x))])
           (let ([factf (λ (!)
                          (λ (n)
                            (if (zero? n)
                                1
                                (* n (! (sub1 n))))))])
             ((fix factf) 5))))


(interp '(car (cons 42 24)))

(interp '(car (cons^ 42 24)))

(interp `(cdr (cons^ ,Ω 42)))
#;
(interp `(cdr (cons ,Ω 42)))
;; ⇒ ⊥

(interp `(cons^ (car (cons^ ,Ω 42))
                (cdr (cons^ 42 ,Ω))))

(interp `(cons^ (car (cons ,Ω 42))
                (cdr (cons 42 ,Ω))))

(interp `(let^ ([Ω ,Ω]) 42))
#;
(interp `(let ([Ω ,Ω]) 42))
;; ⇒ ⊥

(val-of `(,Ω ,Ω) (mt-ε))
;; ⇒ thunk of ⊥
(val-of 'x (mt-ε))
;; ⇒ thunk of error

#;
(interp '((λ (x) (x x))
          (λ (x) (x x))))
;; ⇒ ⊥


#;
(let ([factf (λ (!)
               (let ([!! (delay (! !))])
                 (λ (n)
                   (if (zero? n)
                       1
                       (* n ((force !!) (sub1 n)))))))])
  ((factf factf) 5))




;; stream tests

(define s-ℕ
  (λ (n)
    (cons n (delay (s-ℕ (add1 n))))))

(define s₀ (s-ℕ 0))


(define take$
  (λ (n s)
    (cond
      [(zero? n) '()]
      [else (cons (car s) (take$ (sub1 n) (force (cdr s))))])))

(define drop$
  (λ (n s)
    (cond
      [(zero? n) s]
      [else (drop$ (sub1 n) (force (cdr s)))])))

(take$ 10 (s-ℕ 1))
(drop$ 10 (s-ℕ 0))


(interp `(let ([Y ,fix])
           (let ([take$f (λ (f)
                           (λ (n)
                             (λ (s)
                               (if (zero? n)
                                   '()
                                   (let ([p (cons (car s) (cdr s))])
                                     (cons^ (car p) ((f (sub1 n)) (cdr s))))))))])
             (let ([take$ (Y take$f)])
               (let ([make-s-nat$ (λ (f)
                                    (λ (n)
                                      (cons^ n (f (add1 n)))))])
                 (let ([s ((Y make-s-nat$) 5)])
                   (let ([p ((take$ 10) s)])
                     (car (cdr (cdr p))))))))))
;; ⇒ 7
