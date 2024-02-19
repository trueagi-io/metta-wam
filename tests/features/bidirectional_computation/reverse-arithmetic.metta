;; probes whether functions can be executed in reverse. This involves inferring the
;; input values from given output values through inverse operations or pattern matching,

!(pragma! compile full)
!(pragma! table false)

(= (f $x) (* 2 $x))

!(print (match &self (= (f 1) $y) $y))
!(assertEqual (match &self (= (f 1) $y) $y) (superpose (2)))
; Expected Output: [2]

!(print (match &self (= (f $x) 2) $x))
!(assertEqual (match &self (= (f $x) 2) $x) (superpose (1)))
; Expected Output: [1]



