;; Import OrderedSet
!(import! &self OrderedSet.metta)

;; Test elem
!(assertEqual (elem 1 ∅) False)
!(assertEqual (elem 1 (insert 1 ∅)) True)
!(assertEqual (elem 1 (insert 2 ∅)) False)
!(assertEqual (elem 1 (insert 1 (insert 2 ∅))) True)
!(assertEqual (elem 1 (insert 2 (insert 3 ∅))) False)
!(assertEqual (elem 2 (insert 1 (insert 2 (insert 3 ∅)))) True)

;; Test insert
!(assertEqual
  (insert 1 ∅)
  (:: 1 ∅))
!(assertEqual
  (insert 2 (insert 1 ∅))
  (:: 1 (:: 2 ∅)))
!(assertEqual
  (insert 3 (insert 2 (insert 1 ∅)))
  (:: 1 (:: 2 (:: 3 ∅))))
!(assertEqual
  (insert 2 (insert 3 (insert 2 (insert 1 ∅))))
  (:: 1 (:: 2 (:: 3 ∅))))
!(assertEqual
  (insert (S Z) (insert (S (S Z)) (insert (S Z) (insert Z ∅))))
  (:: Z (:: (S Z) (:: (S (S Z)) ∅))))
