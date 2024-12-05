; Test File: Recursive Tuple/List Deconstruction Benchmark in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/394

; Issue Overview:
; Recursive deconstruction of tuples and lists has suboptimal performance. This test benchmarks
; TupleCount, ListCount, BuildList, and BuildTuple functions, asserting that their execution time is less than 1 second.

; Define Python Module for Timing
!(py-module benchmark
"import time
from hyperon.atoms import OperationAtom
from hyperon.ext import register_atoms

@register_atoms
def my_atoms():
    return {
        'current_time': OperationAtom('current_time', lambda: time.time())
    }
")

; Extend with Timing Module
!(extend-py! benchmark)

; Example 1: TupleCount Benchmark
; --------------------------------
(= (TupleCount $tuple) 
   (if (== $tuple ()) 
       0 
       (+ 1 (TupleCount (cdr-atom $tuple)))))

!(let* (
    ($start (current_time))
    ($result (TupleCount (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)))
    ($end (current_time))
    ($time (- $end $start))
  )
  (assertEqualToResult (< $time 1) (True))
)

; Example 2: ListCount Benchmark
; -------------------------------
(= (ListCount Nil) 0)
(= (ListCount (Cons $head $tail)) 
   (+ 1 (ListCount $tail)))

!(let* (
    ($start (current_time))
    ($result (ListCount (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10  
      (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 
      (Cons 21 (Cons 22 (Cons 23 (Cons 24 (Cons 25 (Cons 26 (Cons 27 (Cons 28 (Cons 29 (Cons 30 Nil))))))))))))))))))))))))))))))))))
    ($end (current_time))
    ($time (- $end $start))
  )
  (assertEqualToResult (< $time 1) (True))
)

; Example 3: BuildList Benchmark
; -------------------------------
(= (BuildList $w $c) 
   (if (> $c 0) 
       (BuildList (Cons 1 $w) (- $c 1)) 
       $w))

!(let* (
    ($start (current_time))
    ($result (BuildList Nil 30))
    ($end (current_time))
    ($time (- $end $start))
  )
  (assertEqualToResult (< $time 1) (True))
)

; Example 4: BuildTuple Benchmark
; -------------------------------
(= (BuildTuple $w $c) 
   (if (> $c 0) 
       (BuildTuple (1 $w) (- $c 1)) 
       $w))

!(let* (
    ($start (current_time))
    ($result (BuildTuple () 30))
    ($end (current_time))
    ($time (- $end $start))
  )
  (assertEqualToResult (< $time 1) (True))
)

; Summary:
; - Benchmarks TupleCount, ListCount, BuildList, and BuildTuple functions for 30 elements.
; - Asserts that execution time for each function is less than 1 second.
