
; 20241014 r.v.v converted metta-lang.dev to metta 
; random bit

; random bit
(= (bin) 0)
(= (bin) 1)
; binary list with the same number of elements

(= (gen-bin-list ()) ())
(= (gen-bin-list (:: $x $xs))   (:: (bin) (gen-bin-list $xs))  )
; sum of products of elements of two lists

(= (scalar-product () ()) 0)
(= (scalar-product (:: $x $xs) (:: $y $ys))   (+ (* $x $y) (scalar-product $xs $ys))  )

; check the candidate solution
(= (test-solution $numbers $solution $target-sum)
   (if (== (scalar-product $numbers $solution)
           $target-sum)
       $solution
       (empty)
   )
)
; task

(= (task) (:: 8 (:: 3 (:: 10 (:: 17 ())))))

;! (test-solution (task) (gen-bin-list (task)) 20)
;[(:: 0 (:: 1 (:: 0 (:: 1 ()))))]


!(println "xx 35 ") 
; ready works
!(assertEqualToResult ( test-solution (task) (gen-bin-list (task)) 20 ) ( (:: 0 (:: 1 (:: 0 (:: 1 ())))) ) ) 