; 16:28 18-8-2024


;!(assertEqualToResults (repr atoma_tag) "atoma_tag")
;!(assertEqualToResults (parse "x_string_tag") x_string_tag)


; !(intersection (superpose (a b c d )) (superpose ( c d e f )))
!(assertEqualToResult (intersect (superpose (a b c d )) (superpose ( c d e f ))) (c d))

; !(union (superpose (a b c d )) (superpose ( c d e f )))
!(assertEqualToResult (union (superpose (a b c d )) (superpose ( c d e f ))) (a b c d e f))
