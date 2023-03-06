#lang racket

(define Y
  '(λ (g)
     ((λ (f)
        (g (λ (x) ((f f) x))))
      (λ (f)
        (g (λ (x) ((f f) x)))))))

(define val-of
  (λ (expr ε)
    (match expr
      [(? symbol? x) (ap-ε ε x)]
      [(? number? n) n]
      [`(quote ,x) x]
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
      [`(letrec ([,fn ,fdef]) ,body)
       (let* ([F  `(λ (,fn) ,fdef)]
              [rec (val-of `(,Y ,F) ε)]
              [ε^  (ext-ε fn rec ε)])
         (val-of body ε^))]
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
(define ap-ε
  (λ (ε y)
    (match ε
      ['() (error "!")]
      [`(ext ,x ,v ,ε@)
       (cond
         [(eqv? y x) v]
         [else (ap-ε ε@ y)])])))



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
