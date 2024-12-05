;; Spaces and Alpha Equivalency in Intersection:
;;
;; When performing intersections between atoms in different spaces, the system 
;; applies alpha equivalency to match atoms. This means that atoms with 
;; equivalent structures and logically equivalent variables (even if they 
;; have different names) are considered identical for the purposes of the 
;; intersection. For example, an atom (foo $x $y) in one space will intersect 
;; with (foo $a $b) in another space as long as the structure and the 
;; relationships between the variables are the same.
;;
;; However, alpha equivalency does not apply when the logical relationships 
;; between variables differ. For instance, the atoms (foo $a $b) and (foo $x $x)
;; will not intersect because the latter enforces that both variables must 
;; be the same, while the former does not impose this constraint.
;;
;; This ensures that intersections between spaces are flexible and recognize 
;; logically equivalent atoms, regardless of the specific variable names used 
;; in each space.
;;
;; However, when atoms from a space are compared to atoms not associated with 
;; any space, strict variable matching may be enforced, meaning only atoms 
;; with exactly matching variable names will be considered identical.


;; Test intersection between Space 1 and Space 2 using alpha equivalency
;; This should return the common atoms: (foo 1 2) and (foo (foo 1 2) 3)
!(bind! &space1 (new-space))
!(bind! &space2 (new-space))
!(add-atom &space1 (superpose ((= (foo $x $y) (+ $x $y)) (foo 1 2) (foo 3 4) (foo $x 10) (foo $x $x) (foo (foo 1 2) 3))))
!(add-atom &space2 (superpose ((= (foo $a $b) (+ $a $b)) (foo 1 2) (foo 5 6) (foo 5 10) (foo 9 9) (foo (foo 1 2) 3))))
!(assertEqual
  (intersection (get-atoms &space1) (get-atoms &space2))
  (superpose ((= (foo $x $y) (+ $x $y)) (foo 1 2) (foo (foo 1 2) 3)))
)

;; Test intersection with a space having a unique atom
;; This should return an empty set as there is no intersection.
!(bind! &space3 (new-space))
!(add-atom &space3 (superpose ((foo 7 8))))
!(assertEqual
  (intersection (get-atoms &space1) (get-atoms &space3))
  (superpose ())
)

;; Test intersection with variables using alpha equivalency
;; The intersection should consider variables as equivalent even if they are named differently.
!(bind! &space4 (new-space))
!(add-atom &space4 (superpose ((foo 1 2) (foo 3 4) (foo $m $n))))
!(assertEqual
  (intersection (get-atoms &space1) (get-atoms &space4))
  (superpose ((foo 1 2) (foo 3 4)))
)

;; Test intersection with overlapping variables using alpha equivalency
;; This test ensures that overlapping variables in atoms are correctly intersected, treating equivalent variables as equal.
!(bind! &space5 (new-space))
!(add-atom &space5 (superpose ((foo (foo 1 2) 3) (foo (foo 3 4) 7) (foo $p $q))))
!(assertEqual
  (intersection (get-atoms &space4) (get-atoms &space5))
  (superpose ((foo  $m $n)))
)

;; Test intersection with a variable and a fixed value using alpha equivalency
;; This test checks that an atom with a variable intersects correctly with a matching fixed value, using alpha equivalency.
!(bind! &space6 (new-space))
!(add-atom &space6 (superpose ((foo (foo 1 2) 3) (foo 7 7) (foo (foo $r 2) 3))))
!(assertEqual
  (pred-intersection unified (get-atoms &space4) (get-atoms &space6))
  (superpose ((foo 7 7)))
)

;; Test intersection with nested variables using alpha equivalency
;; This test checks the behavior when intersecting nested structures containing variables, treating equivalent variables as equal.
!(bind! &space7 (new-space))
!(add-atom &space7 (superpose ((foo $x $x) (foo 7 7) (foo (foo 1 2) 3))))
!(assertEqual
  (intersection (get-atoms &space6) (get-atoms &space7))
  (superpose ((foo (foo 1 2) 3)))
)

;; Test intersection with identical self-referential structures using alpha equivalency
;; This test ensures that intersecting identical self-referential structures returns the structure itself, treating variables as equal.
!(assertEqual
  (intersection (get-atoms &space7) (get-atoms &space6))
  (superpose ((foo (foo 1 2) 3))))
)

;; Test intersection with alpha equivalency (variable renaming)
;; This test ensures that atoms with equivalent variables but different names are considered equal.
!(bind! &space8 (new-space))
!(bind! &space9 (new-space))
!(add-atom &space8 (superpose ((foo $a $b) (foo 3 $c))))
!(add-atom &space9 (superpose ((foo $y $z) (foo 3 $d))))
!(assertEqual
  (intersection (get-atoms &space8) (get-atoms &space9))
  (superpose ((foo 3 $c)))
)

;; Test intersection with strict variable identity to ensure non-matching variables are excluded
;; This test ensures that only exact variable names match and others are excluded.
!(assertEqual
  (intersection (get-atoms &space1) (get-atoms &space8))
  (superpose ())
)

;; Test intersection with different variables using alpha equivalency
;; This checks that when variables are equivalent but differently named, they are considered equal.
!(assertEqual
  (intersection (get-atoms &space4) (get-atoms &space8))
  (superpose ())
)

;; Test intersection between a fresh space and another space using alpha equivalency
;; This should return the common atom: (foo (foo 1 2) 3)
!(bind! &space10 (new-space))
!(add-atom &space10 (superpose ((foo (foo 1 2) 3))))
!(assertEqual
  (intersection (get-atoms &space10) (get-atoms &space2))
  (superpose ((foo (foo 1 2) 3)))
)

;; Test intersection between fresh Space 11 and Space 12 with self-referential atoms
;; The intersection should correctly handle the self-referential atom using alpha equivalency.
!(bind! &space11 (new-space))
!(bind! &space12 (new-space))
!(add-atom &space11 (superpose ((foo $x $x))))
!(add-atom &space12 (superpose ((foo $y $y))))
!(assertEqual
  (intersection (get-atoms &space11) (get-atoms &space12))
  (superpose ((foo $x $x)))
)


;; Test intersection between Space 1 and a non-space2 atom list
;; This should return the common atoms between Space 1 and the provided atom list.
!(bind! &space21 (new-space))
!(add-atom &space21 (superpose ((foo 1 2) (foo 3 4) (foo 5 6))))

;; Intersect with a list of atoms not associated with any space2.
!(assertEqual
  (intersection (get-atoms &space21) (superpose ((foo 1 2) (foo 7 8))))
  (superpose ((foo 1 2)))
)

;; Test intersection between Space 2 and a non-space2 atom list with variables
;; This should correctly instantiate the variable and return the matching atom.
!(bind! &space22 (new-space))
!(add-atom &space22 (superpose ((foo $x 10) (foo 3 4) (foo $y $y))))

;; Intersect with a list of atoms, some of which contain variables.
!(assertEqual
  (intersection (get-atoms &space22) (superpose ((foo 5 10) (foo 3 $z))))
  (superpose ((foo 5 10) (foo 3 4)))
)

;; Test intersection between Space 3 and a non-space2 atom with strict matching
;; This should return an empty set because the variables in the non-space2 atom do not match the space2 atoms exactly.
!(bind! &space23 (new-space))
!(add-atom &space23 (superpose ((foo $a $b) (foo 3 4) (foo $c $c))))

;; Intersect with a non-space2 atom list with different variable names
!(assertEqual
  (intersection (get-atoms &space23) (superpose ((foo $x $y) (foo 3 4))))
  (superpose ((foo 3 4)))
)

;; Test intersection between Space 4 and a single non-space2 atom
;; This should return the single atom if it matches any in the space2.
!(bind! &space24 (new-space))
!(add-atom &space24 (superpose ((foo 7 7) (foo (foo 1 2) 3) (foo $z $z))))

;; Intersect with a single non-space2 atom
!(assertEqual
  (intersection (get-atoms &space24) (superpose ((foo 7 7))))
  (superpose ((foo 7 7)))
)

;; Test intersection between Space 5 and multiple non-space2 atoms
;; This should return only the atoms that match, considering variable instantiation.
!(bind! &space25 (new-space))
!(add-atom &space25 (superpose ((foo (foo 3 4) 7) (foo $p $p) (foo 5 5))))

;; Intersect with multiple non-space2 atoms
!(assertEqual
  (intersection (get-atoms &space25) (superpose ((foo 5 5) (foo 7 8))))
  (superpose ((foo 5 5)))
)

;; Test intersection with a nested non-space2 structure
;; The intersection should correctly handle the nested structure.
!(bind! &space26 (new-space))
!(add-atom &space26 (superpose ((foo (foo 1 2) 3) (foo $x $y) (foo (foo 3 4) 7))))

;; Intersect with a nested non-space2 structure
!(assertEqual
  (intersection (get-atoms &space26) (superpose ((foo (foo 1 2) 3))))
  (superpose ((foo (foo 1 2) 3)))
)

