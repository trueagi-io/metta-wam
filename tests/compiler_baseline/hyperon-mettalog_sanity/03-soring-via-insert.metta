
;;;;;;;;;;;;;;;;;;;;;;;;
; sorted lists
;;;;;;;;;;;;;;;;;;;;;;;;
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;(: if (-> bool Any Any) Any)
;(= (if true $then $else) $then)
;(= (if false $then $else) $else)

(: insert (-> $a (List $a) (List $a)))
!(get-type insert)

(= (insert $x Nil) (Cons $x Nil))
(= (insert $x (Cons $head $tail))
       (if (< $x $head)
	      (Cons $x (Cons $head $tail))
	      (Cons $head (insert $x $tail))))

!(assertEqual (insert 2 (insert 1 Nil)) (Cons 1 (Cons 2 Nil)))

!(assertEqual  (Cons 1 Nil)  (Cons 1 Nil))

!(assertEqual  (insert 1 Nil)  (Cons 1 Nil))



!(assertEqual (insert 1 (insert 2 Nil)) (Cons 1 (Cons 2 Nil)))

!(assertEqual (insert 3 (insert 2 (insert 1 Nil))) (Cons 1 (Cons 2 (Cons 3 Nil))))
