#lang racket

(define val-of
  (λ (expr ε)
    (match expr
      [(? symbol? x) (ap-ε ε x)]
      [(? number? n) n]
      [(? boolean? b) b]
      [`(quote ,x) x]
      [`(+ ,e₁ ,e₂)
       (+ (val-of e₁ ε) (val-of e₂ ε))]
      [`(* ,e₁ ,e₂)
       (* (val-of e₁ ε) (val-of e₂ ε))]
      [`(if ,cnd ,thn ,els)
       (if (val-of cnd ε)
           (val-of thn ε)
           (val-of els ε))]
      [`(sub1 ,e) (sub1 (val-of e ε))]
      [`(zero? ,e) (zero? (val-of e ε))]
      [`(cons ,a ,d) (cons (val-of a ε)
                           (val-of d ε))]
      [`(car ,a) (car (val-of a ε))]
      [`(cdr ,a) (cdr (val-of a ε))]
      [`(null? ,a) (null? (val-of a ε))]
      [`(let ([,s ,e]) ,body)
       (let ([v (val-of e ε)])
         (val-of body (ext-ε s v ε)))]
      [`(letrec ,bindings ,body)
       (val-of body (rec-ε bindings ε))]
      [`(λ (,x) ,body)
       `(closure ,expr ,ε)]
      [`(,e₁ ,e₂)
       (let ([v₁ (val-of e₁ ε)]
             [v₂ (val-of e₂ ε)])
         (match v₁
           [`(closure (λ (,x) ,body) ,ε-saved)
            (val-of body (ext-ε x v₂ ε-saved))]))])))

(define mt-ε
  (λ ()
    '()))
(define ext-ε
  (λ (x v ε)
    `(ext ,x ,v ,ε)))
(define rec-ε
  (λ (bs ε)
    `(rec ,bs ,ε)))
(define ap-ε
  (λ (ε y)
    (match ε
      ['() (error "!")]
      [`(ext ,x ,v ,ε@)
       (cond
         [(eqv? y x) v]
         [else (ap-ε ε@ y)])]
      [`(rec ,bs ,ε@)
       (cond
         [(assv y bs)
          => (λ (pr)
               (match pr
                 [`(,x ,e) (val-of e ε)]))]
         [else (ap-ε ε@ y)])])))



(val-of '(letrec ([f (λ (x) (* (g x) 2))]
                  [g (λ (y) (* y 2))]
                  [v 20])
           (f v))
        (mt-ε))

(val-of '(letrec ([x 22]
                  [y 20]
                  [z (+ x y)])
           z)
        (mt-ε))

(val-of '(letrec ([even? (λ (n)
                           (if (zero? n)
                               #t
                               (odd? (sub1 n))))]
                  [odd? (λ (n)
                          (if (zero? n)
                              #f
                              (even? (sub1 n))))])
           (even? 11))
        (mt-ε))

(val-of '(letrec ([even? (λ (n)
                           (if (zero? n)
                               #t
                               (odd? (sub1 n))))]
                  [odd? (λ (n)
                          (if (zero? n)
                              #f
                              (even? (sub1 n))))])
           (even? 42))
        (mt-ε))




(val-of '(letrec ([¬rec (λ (n)
                          (if (zero? n)
                              0
                              n))])
           (¬rec 42))
        (mt-ε))

(val-of '(letrec ([fact (λ (n)
                          (if (zero? n)
                              1
                              (* n (fact (sub1 n)))))])
           (fact 5))
        (mt-ε))

(val-of '(letrec ([map (λ (f)
                         (λ (ls)
                           (if (null? ls)
                               '()
                               (cons (f (car ls)) ((map f) (cdr ls))))))])
           ((map (λ (x) (* x x)))
            '(1 2 3 4 5)))
        (mt-ε))

(val-of '(letrec ([map
                   (λ (f)
                     (λ (ls)
                       (if (null? ls)
                           '()
                           (cons (f (car ls)) ((map f) (cdr ls))))))])   
           ((λ (ls)
              ((map (λ (x) (cons x ls))) ls))
            '(1 2 3 4)))
        (mt-ε))

(val-of
 '(letrec ([f (λ (n)
                (if (zero? n)
                    0
                    (g (sub1 n))))]
           [g (λ (n)
                (if (zero? (sub1 n))
                    2
                    (h (sub1 n))))]
           [h (λ (n)
                (if (zero? (sub1 (sub1 n)))
                    1
                    (f (sub1 n))))])
    (f 10))
 (mt-ε))
