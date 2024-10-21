


(= (only-a A) (Input A is accepted))
! (match &self (= (only-a A) $result) $result)
! (match &self (= (only-a B) $result) $result)
! (match &self (= (only-a $x) $result) $result)

;[(Input A is accepted)]
;[]
;[(Input A is accepted)]

;[(Input A is accepted)]
;[]
;[(Input A is accepted)]
;% 19,600 inferences, 0.015 CPU in 0.015 seconds (95% CPU, 1341354 Lips)
;true.