;; λ-calculus to metta
(= ((λ $x $f) $y) (let $x $y $f))

;; Z
(= (Z) (λ $f (λ $x $x)))

;; S
(= (S) (λ $n (λ $f (λ $x ($f (($n $f) $x))))))

;; +
(= (plus) (λ $m (λ $n (λ $f (λ $x (($m $f) (($n $f) $x)))))))

;; Test
!(Z)
!((S) ((S) (Z)))
!(((plus) (Z)) (Z))
!(((plus) ((S) ((S) (Z)))) ((S) (Z)))
