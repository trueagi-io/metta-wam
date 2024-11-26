
(import! &self sorting_tests)

!(pragma! coerce auto)

;; Test insert
!("============ Test insert ============")
!(assertEqual (insert (S Z) Nil) (Cons (S Z) Nil))  ;; insert 1 into empty list
!(assertEqual (insert (S (S Z)) (insert (S Z) Nil)) (Cons (S Z) (Cons (S (S Z) Nil))))  ;; insert 2, then 1
!(assertEqual (insert (S (S (S Z))) (insert (S (S Z)) (insert (S Z) Nil))) (Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) Nil))))  ;; insert 3, then 2, then 1
!(== (Cons (S Z) Nil) (Cons (S Z) Nil))  ;; check structural equality

;; Test sort
!("============ Test sort ============")
!(assertEqual (sort (Cons (S Z) Nil)) (Cons (S Z) Nil))  ;; sort a single-element list
!(assertEqual (sort (Cons (S (S Z)) (Cons (S Z) Nil))) (Cons (S Z) (Cons (S (S Z) Nil))))  ;; sort [2, 1]
!(assertEqual (sort (Cons (S (S (S Z))) (Cons (S Z) (Cons (S (S Z)) Nil)))) (Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) Nil))))  ;; sort [3, 1, 2]

;; Test elem
!("============ Test elem ============")
!(assertEqual (elem (S Z) (Cons (S (S (S Z))) (Cons (S Z) (Cons (S (S Z)) Nil)))) True)  ;; 1 is in [3, 1, 2]
!(assertEqual (elem (S (S (S (S (S Z))))) (Cons (S (S (S Z))) (Cons (S Z) (Cons (S (S Z)) Nil)))) False)  ;; 5 is not in [3, 1, 2]

;; Test uniq
!("============ Test uniq ============")
!(assertEqual (uniq (Cons (S (S Z)) (Cons (S (S (S Z))) (Cons (S Z) (Cons (S (S Z)) Nil))))) (Cons (S (S Z)) (Cons (S (S (S Z))) (Cons (S Z) Nil))))  ;; remove duplicate 2
!(assertEqual (uniq (Cons (S (S Z)) (Cons (S (S (S Z))) (Cons (S (S (S Z))) (Cons (S (S Z)) Nil))))) (Cons (S (S Z)) (Cons (S (S (S Z))) Nil)))  ;; remove duplicates of 2 and 3

;; Test insert_uniq
!("============ Test insert_uniq ============")
!(assertEqual (insert_uniq (S (S Z)) (insert_uniq (S Z) Nil)) (Cons (S Z) (Cons (S (S Z) Nil))))  ;; insert unique 1, then 2
!(assertEqual (insert_uniq (S (S (S Z))) (insert_uniq (S (S Z)) (insert_uniq (S Z) Nil))) (Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) Nil))))  ;; insert unique 1, 2, 3
!(assertEqual (insert_uniq (S (S Z)) (insert_uniq (S (S (S Z))) (insert_uniq (S (S Z)) (insert_uniq (S Z) Nil)))) (Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) Nil))))  ;; ensure duplicates are avoided
