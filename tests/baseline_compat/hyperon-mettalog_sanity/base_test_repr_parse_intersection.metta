
; intersection

!(assertEqualToResult (intersection (superpose (a b (+ 2 3 ) c)) (superpose (b c (+ 2 3) c c d))) (b 5 c))
; assertEqualToResult doesnt work
!(assertEqual (intersection (superpose (a b (+ 2 3 ) c)) (superpose (b c (+ 2 3) c c d)))  (superpose (b (+ 2 3) c)))

; repr
!(repr aswde)
; this works 
!(assertEqualToResult (repr aswde ) ("aswde" ))

; parse
!(parse "kjh")
; this works
!(assertEqualToResult (parse "kjh" ) (kjh ))



