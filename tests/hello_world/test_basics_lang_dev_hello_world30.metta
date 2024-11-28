
(Parent Bob Ann)
; This match will be successful
! (match &self (Parent Bob Ann) (Bob is Ann`s father))
; The following line will return []
! (match &self (Parent Bob Joe) (Bob is Joe`s father))


;[(Bob is Ann`s father)]
;[]

;[(Bob is Ann`s father)]
;[]
;% 14,219 inferences, 0.020 CPU in 0.021 seconds (96% CPU, 711736 Lips)