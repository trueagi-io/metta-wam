
;; Test if we can implemnt Multidirectional append

; (= (append Nil $L $L) ())
(= (append Nil $L) $L)
; (= (append (Cons $H $T) $L (Cons $H $R)) (append $T $L $R))
(= (append (Cons $H $T) $L) 
   (== $R (append $T $L))
    (Cons $H $R))


; (append ++ ++ ) =  --
!(assertEqualToResult
 (let $_
   (== $Out (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil))))
     $Out)
    ((Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

; (append ++ -- ) =  ++
!(assertEqualToResult
  (let $_
   (== (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) append (Cons 1 (Cons 2 Nil)) $Out)
	 $Out)
  ((Cons 3 (Cons 4 Nil))))

; (append -- -- ) = ++
!(assertEqualToResult
 (let $_
  (== (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) (append $What $Out))
	 ($What $Out))
  (((Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) Nil)
   ((Cons 1 (Cons 2 (Cons 3 Nil)))                     (Cons 4 Nil)) 
   ((Cons 1 (Cons 2 Nil))                      (Cons 3 (Cons 4 Nil))) 
   ((Cons 1 Nil)                       (Cons 2 (Cons 3 (Cons 4 Nil)))) 
    (Nil                       (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))))

; (append ++ -- ) =  +
!(assertEqualToResult
  (let $_
	(== (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) (append (Cons 1 (Cons 2 Nil)) $Out))
	 $Out)
   ((Cons 3 (Cons 4 Nil))))


; (append ++ -- ) = + 
!(assertEqual
 (let $_
  (== (Cons 1 (Cons 2 $What)) (append (Cons 1 (Cons 2 Nil)) $Out))
	 (== $Out $What))
  True)




