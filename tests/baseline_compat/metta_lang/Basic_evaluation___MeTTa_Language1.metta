
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; This line will be ignored.
; Hello 
; This symbol will be added to the Space
; (Hello World) 
; This expression will also be added
;! (+ 1 2) ; This expression will be immediatedly evaluated
;! (Hi there) ; as well as this one

; ready works
!(assertEqualToResult ( + 1 2 )  ( 3 )  ) 


; ready works
!(assertEqualToResult ( Hi there )  ( (Hi there) )  ) 
