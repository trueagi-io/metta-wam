(= (Truth_Deduction ($f1 $c1) ($f2 $c2))
   ((* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))

!(Truth_Deduction (1.0 0.9) (1.0 0.9))
