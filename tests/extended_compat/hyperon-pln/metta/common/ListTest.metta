;; Import List
!(import! &self List.metta)

;; Test insert
!("============ Test insert ============")
!(insert 1 Nil)
!(insert 2 (insert 1 Nil))
!(insert 3 (insert 2 (insert 1 Nil)))
!(== (Cons 1 Nil) (Cons 1 Nil))

;; Test sort
!("============ Test sort ============")
!(sort (Cons 1 Nil))
!(sort (Cons 2 (Cons 1 Nil)))
!(sort (Cons 3 (Cons 1 (Cons 2 Nil))))

;; Test elem
!("============ Test elem ============")
!(elem 1 (Cons 3 (Cons 1 (Cons 2 Nil))))
!(elem 5 (Cons 3 (Cons 1 (Cons 2 Nil))))

;; Test uniq
!("============ Test uniq ============")
!(uniq (Cons 2 (Cons 3 (Cons 1 (Cons 2 Nil)))))
!(uniq (Cons 2 (Cons 3 (Cons 3 (Cons 2 Nil)))))

;; Test insert_uniq
!("============ Test insert_uniq ============")
!(insert_uniq 2 (insert_uniq 1 Nil))
!(insert_uniq 3 (insert_uniq 2 (insert_uniq 1 Nil)))
!(insert_uniq 2 (insert_uniq 3 (insert_uniq 2 (insert_uniq 1 Nil))))
