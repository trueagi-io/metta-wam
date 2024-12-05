
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; nondeterministic function
(= (bin) 0)
(= (bin) 1)

; deterministic triple
(= (triple $x) ($x $x $x))
; ready works 
!(assertEqualToResult ( triple (bin) )  ( (0 0 0) (1 1 1) )  ) 


; nondeterministic pair
(= (bin2) ((bin) (bin)))
; ready works
!(assertEqualToResult ( bin2 )  ( (0 0) (0 1) (1 0) (1 1) )  ) 


; deterministic summation
(= (sum ($x $y)) (+ $x $y))
(= (sum ($x $y $z)) (+ $x (+ $y $z)))
;! (sum (triple (bin))) 
; ready works 
!(assertEqualToResult ( sum (triple (bin)) )  ( (0) (3) )  ) 
; 0, 3
;! () 
; ready works 
!(assertEqualToResult ( sum (bin2) )  ( 0 1 1 2 )  ) 
; 


; nondeterministic increment
(= (inc-flip $x) (+ 0 $x))
(= (inc-flip $x) (+ 1 $x))
;! (inc-flip 1) 
; 1, 2
; ready works 
!(assertEqualToResult ( inc-flip 1 )  ( 1 2 )  ) 

;! (inc-flip (bin)) 
; 0, 1, 1, 2
; ready works 
!(assertEqualToResult ( inc-flip (bin) )  ( 0 1 1 2 )  ) 
