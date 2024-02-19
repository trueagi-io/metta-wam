
(= (Concat (Cons $head1 Nil) $list2)
	(Cons $head1 $list2))

(= (Concat (Cons $head1 (Cons $t1 $t11)) $list2)
	(Cons $head1 (Concat (Cons $t1 $t11) $list2)))


(= (lst1) (Cons a1 (Cons a2 Nil)))

(= (lst2) (Cons b1 (Cons b2 Nil)))

!(assertEqual (Concat (lst1) (lst2))
   (Cons a1 (Cons a2 (Cons b1 (Cons b2 Nil)))))

