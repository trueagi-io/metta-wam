; Test File: Unbounded/Free Variables in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/787

; Example 1: Subsumption Hierarchy and Free Variables
; ---------------------------------------------------

; Define a subsumption hierarchy
!(add-atom &self (subsumption Dog Corgi))
!(add-atom &self (subsumption Animal Dog))
!(add-atom &self (subsumption Being Animal))
!(add-atom &self (subsumption Mortal Being))

; Define functions to query the hierarchy
(= (type-of $x $y) (match &self (subsumption $x $y) $x))
(= (get-entity-subsumptions $x $z) (type-of $x $z))
(= (get-entity-subsumptions $x $z) (get-entity-subsumptions $x (type-of $y $z)))
(= (get-entity-subsumptions $x) (get-entity-subsumptions $a $x))

; Test: Querying subsumptions for Corgi
!(assertEqualToResult
   (get-entity-subsumptions Corgi)
   (Dog Being Mortal Animal))

; Example 2: Free Variables as Placeholders
; -----------------------------------------
(= (bar A) B)

; Test: Free variable $a receives value through unification
!(assertEqualToResult
   (let $r (bar $a) ($r $a))
   ((B A)))

; Example 3: Free Variables and Mismatches
; ----------------------------------------
(= (foo $x) (let $x A B))

; Test: Free variable $a receives value because it unifies with $x
!(assertEqualToResult
   (let $r (foo $a) ($r $a))
   ((B A)))

; Test: Function doesn't match when input doesn't unify with $x
!(assertEqualToResult
   (let $r (foo C) ($r $a))
   ())
