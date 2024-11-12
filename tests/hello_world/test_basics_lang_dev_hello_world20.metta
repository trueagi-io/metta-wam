

(= (factorial $x)
   (if (> $x 0)
       (* $x (factorial (- $x 1)))
       1))
! (factorial 5) 

; 120

; 120 