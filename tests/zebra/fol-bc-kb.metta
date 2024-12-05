
;; last of list
(last $x (Cons $x Nil))
((last $x (Cons $_ $rest)) :- ( (last $x $rest) ) )

;; length list
(length Nil 0)
((length (Cons $x $rest) $out) :- (
    (length $rest $out2) 
    (is $out (+ $out2 1))
)
)

;; sum from 0 to n
(sum 0 0)
((sum $n $s) :- (
    (bool (> $n 0))
    (is $n1 (- $n 1))
    (sum $n1 $s1)
    (is $s (+ $n $s1))
))

;; list append
(append Nil $L $L)
((append (Cons $H $T) $L (Cons $H $R)) :- (
    (append $T $L $R)
))

;; negation as failure test
((bachelor $x) :- (
    (man $x)
    (naf (married $x))
))
(man John) (man Tim)
(married Tim)

((woman $x) :- (
    (naf (man $x))
))
