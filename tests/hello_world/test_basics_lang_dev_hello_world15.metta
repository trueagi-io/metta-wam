
(= (bin) 0)
(= (bin) 1)
; deterministic triple
(= (triple $x) ($x $x $x))
! (triple (bin)) ; (0 0 0) and (1 1 1)
; nondeterministic pair
(= (bin2) ((bin) (bin)))
! (bin2) ; (0 0), (0 1), (1 0), (1 1)
; deterministic summation
(= (sum ($x $y)) (+ $x $y))
(= (sum ($x $y $z)) (+ $x (+ $y $z)))
! (sum (triple (bin))) ; 0, 3
! (sum (bin2)) ; 0, 1, 1, 2
; nondeterministic increment
(= (inc-flip $x) (+ 0 $x))
(= (inc-flip $x) (+ 1 $x))
! (inc-flip 1) ; 1, 2
! (inc-flip (bin)) ; 0, 1, 1, 2

;[(1 1 1), (0 0 0)]
;[(0 0), (0 1), (1 0), (1 1)]
;[0, 3]
;[2, 1, 1, 0]
;[1, 2]
;[1, 2, 0, 1]

; mettalog 
;[(0 0 0), (1 1 1)]
;[(0 0), (1 0), (0 1), (1 1)]
;[0, 3] 
; [0, 1, 1, 2]
;[1, 2] 
; [0, 1, 1, 2]



% 102,655 inferences, 0.104 CPU in 0.104 seconds (100% CPU, 989197 Lips)