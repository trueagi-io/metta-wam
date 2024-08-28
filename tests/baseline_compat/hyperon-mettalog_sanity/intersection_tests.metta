; 16:28 18-8-2024
; !(intersect (superpose (a b c d )) (superpose ( c d e f )))


!(assertEqual (intersection (superpose (a b c d )) (superpose ( c d e f ))) (superpose (c d)))
