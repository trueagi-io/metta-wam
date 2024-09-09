!(assertEqualToResult (sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b))))) ((quote (= ($a $x $c) ($b)))))  

!(assertEqualToResult (sealed ($x $y) (quote (= ($y $z)))) ((quote (= ($y $z)))))

!(assertEqualToResult (let $x (input $y) (output $x)) ((output (input $y))))

!(assertEqual (let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x)))) 
    (let $sv (input $x) $st)) (output (input $x)))

(: callsealer (-> Number  Number Expression))

(= (callsealer $a $b) (sealed () (quote ((* (+ 40 $a) (/ 100 $b))))))

!(assertEqualToResult (callsealer  $x $y) ((quote ((* (+ 40 $x) (/ 100 $y))))))  ; no sealing

!(assertEqualToResult (callsealer  10 25) ((quote ((* (+ 40 10) (/ 100 25))))))

(: callsealer1 (-> Number  Number Expression))
(= (callsealer1 $a $b)  (sealed ($a) (quote ((* (+ 40 $a) (/ 100 $b))))))

!(assertEqualToResult (callsealer1  5 $y) ((quote ((* (+ 40 5) (/ 100 $y))))))

!(assertEqualToResult (callsealer1  $x $y) ((quote ((* (+ 40 $a) (/ 100 $y)))))) ; $x should be sealed

(: callsealer2 (-> Number  Number Expression))
(= (callsealer2 $a $b) (sealed ($a $b) (quote ((* (+ 40 $a) (/ 100 $b))))))

!(assertEqualToResult (callsealer2  $x 4 ) ((quote ((* (+ 40 $a) (/ 100 4 ))))))   ;$x sealed

!(assertEqualToResult (callsealer2  $x $y) ((quote ((* (+ 40 $a) (/ 100 $b))))))  ;both $x and $y sealed




