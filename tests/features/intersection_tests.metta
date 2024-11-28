; 16:28 18-8-2024
; !(intersect (superpose (a b c d )) (superpose ( c d e f )))


!(assertEqualToResults (intersect (superpose (a b c d )) (superpose ( c d e f ))) (c d))
