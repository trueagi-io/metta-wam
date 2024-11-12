  ; fn superpose_op_superposed_with_collapse() 
  (= (f) A)
  (= (f) B)
  !(assertEqualToResult (let $x (collapse (f)) (superpose $x)) ( A B ))     