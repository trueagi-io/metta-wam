; fn superpose_op_multiple_interpretations
 (= (f) A)
 (= (f) B)
 (= (g) C)
 (= (g) D)
 !(assertEqualToResult (superpose ((f) (g))) ( A B C D ))     