
; random bit
(= (bin) 0)
(= (bin) 1)
; binary list with the same number of elements
(= (gen-bin-list ()) ())
(= (gen-bin-list (:: $x $xs))
   (:: (bin) (gen-bin-list $xs))
)
; sum of products of elements of two lists
(= (scalar-product () ()) 0)
(= (scalar-product (:: $x $xs) (:: $y $ys))
   (+ (* $x $y) (scalar-product $xs $ys))
)
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

! (test-solution (task) (gen-bin-list (task)) 20)

; [(:: 0 (:: 1 (:: 0 (:: 1 ()))))]


;This solution is not scalable, but it illustrates
; the general idea of how nondeterminism and recursion can be
;  combined for problem solving. Note that passing a
;   variable instead of (gen-bin-list (task)) will not work here.
;    What is the difference with the frog example? The answer will be given 
;    in the next tutorial.


; [  (:: 0    (:: 1      (:: 0        (:: 1 ())))), ]
; % 3,378,285 inferences, 2.297 CPU in 2.388 seconds (96% CPU, 1470573 Lips)