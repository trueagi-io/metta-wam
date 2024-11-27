;; Create new spaces for subtraction and union tests
!(bind! &space31 (new-space))
!(bind! &space32 (new-space))
!(bind! &space33 (new-space))
!(bind! &space34 (new-space))

;; Add atoms to &space31
!(add-atom &space31 (superpose ((foo $x $y) (foo 1 2) (foo 3 4) (foo $x 10) (foo $x $x))))

;; Add atoms to &space32
!(add-atom &space32 (superpose ((foo 1 2) (foo 5 6) (foo $x $y))))

;; Add atoms to &space33
!(add-atom &space33 (superpose ((foo 3 4) (foo 5 6))))

;; Subtraction Tests:

;; Test basic subtraction between spaces
;; This should return atoms in &space31 that are not in &space32.
!(assertEqual
  (subtraction (get-atoms &space31) (get-atoms &space32))
  (superpose ((foo 3 4) (foo $x 10) (foo $x $x)))
)

;; Test subtraction with variables and self-referential structures
;; This should return atoms in &space31 that are not logically equivalent to those in &space33.
!(assertEqual
  (subtraction (get-atoms &space31) (get-atoms &space33))
  (superpose ((foo $x $y) (foo 1 2) (foo $x 10) (foo $x $x)))
)

;; Test subtraction with empty space3
;; Subtracting from an empty space should return an empty set.
!(assertEqual
  (subtraction (get-atoms &space34) (get-atoms &space31))
  (superpose ())
)

;; Union Tests:

;; Test union of two spaces
;; The union of &space31 and &space32 should include all unique atoms from both spaces.
!(assertEqual
  (union (get-atoms &space31) (get-atoms &space32))
  (superpose ((foo $x $y) (foo 1 2) (foo 3 4) (foo 5 6) (foo $x 10) (foo $x $x)))
)

;; Test union with overlapping spaces
;; The union should not duplicate atoms that are common to both spaces.
!(assertEqual
  (union (get-atoms &space32) (get-atoms &space33))
  (superpose ((foo 1 2) (foo 5 6) (foo 3 4) (foo $x $y)))
)

;; Test union with a space containing a unique atom
;; The result should include the unique atom from the third space.
!(assertEqual
  (union (get-atoms &space31) (get-atoms &space33))
  (superpose ((foo $x $y) (foo 1 2) (foo 3 4) (foo $x 10) (foo $x $x) (foo 5 6)))
)

;; Test union with a non-space set
;; This checks that atoms not associated with a space are correctly unioned with a space.
!(assertEqual
  (union (get-atoms &space31) (superpose ((foo 7 8))))
  (superpose ((foo $x $y) (foo 1 2) (foo 3 4) (foo $x 10) (foo $x $x) (foo 7 8)))
)

;; Test union with an empty space3
;; The result should just be the atoms from the non-empty space.
!(assertEqual
  (union (get-atoms &space31) (get-atoms &space34))
  (superpose ((foo $x $y) (foo 1 2) (foo 3 4) (foo $x 10) (foo $x $x)))
)

