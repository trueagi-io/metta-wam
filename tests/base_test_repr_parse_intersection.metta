
; intersection

!(intersection (superpose (a b (+ 2 3 ) c)) (superpose (b c (+ 2 3) c c d)))
!
; assertEqualToResults doesnt work


; repr
!(repr aswde)
; this works 
!(assertEqualToResults (repr aswde ) "aswde" )

; parse
!(parse "kjh")
; this works
!(assertEqualToResults (parse "kjh" ) kjh )



