

(= (length ()) 0)
(= (length (:: $x $xs))
   (+ 1 (length $xs)))
! (length (:: A (:: B (:: C ()))))

;[3]

;[3]
;% 15,627 inferences, 0.023 CPU in 0.023 seconds (97% CPU, 689213 Lips)
;true.