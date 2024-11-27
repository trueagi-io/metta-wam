
!(import! &self sorting_tests.metta)

;; Define a generic less than operator, lessThan, for Nat.  < cannot be used
;; because it is a built-in, its type is hardwired and cannot be
;; overloaded.
(: lessThan (-> Nat Nat Bool))
(= (lessThan $_ Z) False)
(= (lessThan Z (S $_)) True)
(= (lessThan (S $x) (S $y)) (lessThan $x $y))

!("============ Test lessThan ============")

;; Test when the first number is zero
!(assertEqual (lessThan Z (S Z)) True)        ;; 0 < 1
!(assertEqual (lessThan Z (S (S Z))) True)    ;; 0 < 2
!(assertEqual (lessThan Z Z) False)          ;; 0 < 0

;; Test when the second number is zero
!(assertEqual (lessThan (S Z) Z) False)      ;; 1 < 0
!(assertEqual (lessThan (S (S Z)) Z) False)  ;; 2 < 0

;; Test when both numbers are the same
!(assertEqual (lessThan (S Z) (S Z)) False)  ;; 1 < 1
!(assertEqual (lessThan (S (S Z)) (S (S Z))) False) ;; 2 < 2
!(assertEqual (lessThan (S (S (S Z))) (S (S (S Z)))) False) ;; 3 < 3

;; Test when the first number is smaller
!(assertEqual (lessThan (S Z) (S (S Z))) True)    ;; 1 < 2
!(assertEqual (lessThan (S (S Z)) (S (S (S Z)))) True) ;; 2 < 3
!(assertEqual (lessThan (S Z) (S (S (S Z)))) True) ;; 1 < 3

;; Test when the first number is larger
!(assertEqual (lessThan (S (S Z)) (S Z)) False)   ;; 2 < 1
!(assertEqual (lessThan (S (S (S Z))) (S (S Z))) False) ;; 3 < 2
!(assertEqual (lessThan (S (S (S (S Z)))) (S (S Z))) False) ;; 4 < 2


;!(pragma! coerce auto)

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
