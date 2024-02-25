
;; Test if we can implemnt Multidirectional append

; append([], L, L).
(= (append Nil $L $L) True)
; append([H|T], L, [H|R]) :- append(T, L, R).
(= (append (Cons $H $T) $L (Cons $H $R)) (append $T $L $R))


; (append ++ ++ -- )
!(assertEqualToResult
 (let $_
   (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)) $Out) $Out)
    ((Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

; (append ++ -- ++ )
!(assertEqualToResult
  (let $_
   (append (Cons 1 (Cons 2 Nil)) $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))) $Out)
  ((Cons 3 (Cons 4 Nil))))

; (append -- -- ++ )
!(assertEqualToResult
 (let $_
  (append $What $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
	 ($What $Out))
  (((Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) Nil)
   ((Cons 1 (Cons 2 (Cons 3 Nil)))                     (Cons 4 Nil)) 
   ((Cons 1 (Cons 2 Nil))                      (Cons 3 (Cons 4 Nil))) 
   ((Cons 1 Nil)                       (Cons 2 (Cons 3 (Cons 4 Nil)))) 
    (Nil                       (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))))

; (append ++ -- + )
!(assertEqualToResult
  (let $_
	(append (Cons 1 (Cons 2 Nil)) $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))) $Out)
   ((Cons 3 (Cons 4 Nil))))


; (append ++ -- + )
!(assertEqual
 (let $_
  (append (Cons 1 (Cons 2 Nil)) $Out (Cons 1 (Cons 2 $What)))
	 (== $Out $What))
  True)


;!(and (== (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)) $Out) True) (println! $Out))




