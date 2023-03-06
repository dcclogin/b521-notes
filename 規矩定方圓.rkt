#lang racket

(define-syntax 定義 (make-rename-transformer #'define))
(define-syntax 若 (make-rename-transformer #'if))
(define-syntax 若得 (make-rename-transformer #'cond))
(define-syntax 否則  (make-rename-transformer #'else))
(define-syntax 匹配 (make-rename-transformer #'match))
(define-syntax 零否 (make-rename-transformer #'zero?))
(define-syntax 空否 (make-rename-transformer #'null?))
(define-syntax 前 (make-rename-transformer #'add1))
(define-syntax 後 (make-rename-transformer #'sub1))
(define-syntax 加 (make-rename-transformer #'+))
(define-syntax 乘 (make-rename-transformer #'*))
(define-syntax 陣列 (make-rename-transformer #'list))
(define-syntax 構 (make-rename-transformer #'cons))
(define-syntax 左 (make-rename-transformer #'car))
(define-syntax 右 (make-rename-transformer #'cdr))
(define-syntax 示 (make-rename-transformer #'display))
(define-syntax 示行 (make-rename-transformer #'displayln))
(define-syntax 新行 (make-rename-transformer #'newline))
(define-syntax 真 (make-rename-transformer #'true))
(define-syntax 假 (make-rename-transformer #'false))
(define-syntax 和 (make-rename-transformer #'and))
(define-syntax 或 (make-rename-transformer #'or))
(define-syntax 否 (make-rename-transformer #'not))
(define-syntax 符號否 (make-rename-transformer #'symbol?))
(define-syntax 數否 (make-rename-transformer #'number?))
(define-syntax 函 (make-rename-transformer #'lambda))
(define-syntax 等值否 (make-rename-transformer #'eqv?))

(define 零 0)
(define 一 1)
(define 二 2)
(define 三 3)
(define 四 4)
(define 五 5)
(define 六 6)
(define 七 7)
(define 八 8)
(define 九 9)
(define 十 10)

(define 空 '())

(定義 (續乘 甲)
      (若得
       [(零否 甲) 一]
       [否則 (乘 甲 (續乘 (後 甲)))]))

(定義 (陣長 陣甲)
      (若得
       [(空否 陣甲) 零]
       [否則 (前 (陣長 (右 陣甲)))]))


(陣長 '(天 地 人))


(定義 (釋天地 相 寰)
      (匹配 相
            [(? 數否 原數) 原數]
            [`(加 ,數相甲 ,數相乙)
             (加 (釋天地 數相甲 寰) (釋天地 數相乙 寰))]
            [(? 符號否 原符) (寰 原符)]
            [`(函 (,符甲) ,函數體)
             (函 (值乙)
                 (釋天地 函數體
                         (函 (符丙)
                             (若得
                              [(等值否 符丙 符甲) 值乙]
                              [否則 (寰 符丙)]))))]
            [`(,符相 ,數相)
             ((釋天地 符相 寰) (釋天地 數相 寰))]))

(定義 空寰 (函 (符甲) 假))
(釋天地 `((函 (符甲) (加 符甲 符甲)) (加 ,一 ,一)) 空寰)
