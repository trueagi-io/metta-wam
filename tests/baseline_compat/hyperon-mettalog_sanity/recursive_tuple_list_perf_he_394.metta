; Test File: Recursive Tuple/List Deconstruction Performance in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/394

; Issue Overview:
; Recursive deconstruction of tuples or lists in MeTTa has suboptimal performance, 
; particularly for deep recursion. This test measures performance for TupleCount, 
; ListCount, BuildTuple, and BuildList functions.

; Example 1: TupleCount Performance
; ---------------------------------
(= (TupleCount $tuple) 
   (if (== $tuple ()) 
       0 
       (+ 1 (TupleCount (cdr-atom $tuple)))))

!(assertEqualToResult
   (TupleCount (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))
   (30))
; Explanation:
; Counts the elements in a tuple. Tests for 30 elements.

; Example 2: ListCount Performance
; ---------------------------------
(= (ListCount Nil) 0)
(= (ListCount (Cons $head $tail)) 
   (+ 1 (ListCount $tail)))

!(assertEqualToResult
   (ListCount (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 
   (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 
   (Cons 21 (Cons 22 (Cons 23 (Cons 24 (Cons 25 (Cons 26 (Cons 27 (Cons 28 (Cons 29 (Cons 30 Nil)))))))))))))))))))))))))))))))))
   (30))
; Explanation:
; Counts the elements in a list represented as Cons cells. Tests for 30 elements.

; Example 3: BuildList Performance
; --------------------------------
(= (BuildList $w $c) 
   (if (> $c 0) 
       (BuildList (Cons 1 $w) (- $c 1)) 
       $w))

!(assertEqualToResult
   (BuildList Nil 30)
   ((Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 
   (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 
   (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 Nil)))))))))))))))))))))))))))))))))
; Explanation:
; Constructs a list of 30 elements using Cons.

; Example 4: BuildTuple Performance
; ---------------------------------
(= (BuildTuple $w $c) 
   (if (> $c 0) 
       (BuildTuple (1 $w) (- $c 1)) 
       $w))

!(assertEqualToResult
   (BuildTuple () 30)
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; Explanation:
; Constructs a tuple of 30 elements.

; Summary:
; - These tests validate the correctness of TupleCount, ListCount, BuildList, and BuildTuple functions.
; - They also serve as a baseline for measuring performance, particularly for recursion depth.

