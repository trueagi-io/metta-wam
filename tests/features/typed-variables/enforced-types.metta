
; =============================================================
; Purpose: Define predicate 'is-even' to check if a number is even.
; Operation: Uses modulo operator to determine if a number is divisible by 2 without remainder.
; Usage: General utility function for checking evenness of numbers.
; =============================================================
; Define 'is-even' predicate
(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))


(= (: $v EvenNumber) (if (is-even $v) () (empty)))

!(assertEqual 
 (sequential 
  ((== (get-type $even) EvenNumber)
   (let $even (superpose (1 2 3 4 5 6)) $even))
 (superpose (2 4 6)))
