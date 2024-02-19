;; Import modules
!(import! &self Num.metta)

;; Definition of a set data structure.  Under the hood it is a sorted
;; list without duplicates, thus called OrderedSet.  For now its
;; constructor :: is unconstrained, meaning it does not guaranty that
;; the constructed OrderedSet are sorted and without duplicates.
;; Rather this is delegated to the insert function defined below.  In
;; order to guaranty that it is sorted without duplicates, the
;; constructor :: would need to take an additional argument encoding a
;; proof that the element to prepend is less than the head of the set
;; to be prepended to.
(: OrderedSet (-> $a Type))
(: ∅ (OrderedSet $a))
(: :: (-> $a (OrderedSet $a) (OrderedSet $a)))

;; Check if an element is in a set.  TODO: optimize assuming it is
;; ordered.
(: elem (-> $a (OrderedSet $a) Bool))
(= (elem $x ∅) False)
(= (elem $x (:: $h $t))
   (if (== $x $h) True (elem $x $t)))

;; Insert an element into a set.  Only insert if the element is not
;; already in the set as to produce an ordered set.
(: insert (-> $a (OrderedSet $a) (OrderedSet $a)))
(= (insert $x ∅) (:: $x ∅))          ; Base case
(= (insert $x (:: $h $t))            ; Recursive step
   (if (== $x $h)
       (:: $h $t)                    ; Present, no need to insert
       (if (⩻ $x $h)                 ; Use generic <
           (:: $x (:: $h $t))        ; Safely insert since $x < $h
           (:: $h (insert $x $t))))) ; Recursive call
