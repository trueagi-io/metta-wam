

(= (apply-twice $f $x)
   ($f ($f $x)))
(= (square $x) (* $x $x))
(= (duplicate $x) ($x $x))
! (apply-twice square 2) ; 16
! (apply-twice duplicate 2) ; ((2 2) (2 2))
! (apply-twice 1 2) ; (1 (1 2))

;[16]
;[((2 2) (2 2))]
;[(1 (1 2))]

;[16]
;[ ( (2 2) (2 2))]
;[ (1 (1 2))]
;% 36,664 inferences, 0.040 CPU in 0.042 seconds (97% CPU, 906757 Lips)