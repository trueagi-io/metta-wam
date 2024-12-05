
!(import! &self fol-bc)
; !(bind! &baseall (new-space))
!(import! &baseall fol-bc-kb)
(= (query $goal)
    (backward-chain $goal &baseall &baseall)
)

;;;;;;;;;;;;;;;;;
;; TEST query ;;;
;;;;;;;;;;;;;;;;;;


; last element of list
!(query (last $x (Cons 1 (Cons 2 (Cons 3 Nil)))))

;; length of list
!(query (length (Cons 1 (Cons 2 (Cons 3 Nil))) $out))

;; sum up to 3
!(query (sum 3 $x))

;; append (multi-directional)
!(query (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)) $x))
!(query  (append (Cons 1 (Cons 2 Nil)) $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))
!(query (append $What $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

;; Negation as failure test
!(query (bachelor $x))
!(query (woman Tim)) ;; should be no
!(query (woman Jane)) ;; should succeed
!(query (woman $x)) ;; should be no