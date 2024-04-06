; Some knowledge base entries
; `:=` is a custom symbol
; Combinatory logic rules
(:= (I $x) $x)
(:= ((K $x) $y) $x)
(:= (K $x $y) $x)
(:= (S $x $y $z) ($x $z ($y $z)))

!(match &self 
        (:= (S K K x) $r)
        $r)

; Peano Summation
(:= (Add $x Z) $x)
(:= (Add $x (S $y)) (Add (S $x) (S $x)))

!(match &self 
        (:= (Add (S Z) (S Z)) $r)
        $r)

!(match &self 
        (:= (Add (S Z) Z) $r)
        $r)    

;; equality
(= (I $x) $x)
(= ((K $x) $y) $x) ; Note that `(K $x)` is allowed here
(= (K $x $y) $x)
(= (S $x $y $z) ($x $z ($y $z)))

(S K K x)