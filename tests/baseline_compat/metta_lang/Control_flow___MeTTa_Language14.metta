
; 20241014 r.v.v converted metta-lang.dev to metta 

(Sam is 34)
(= (age++ $who)
   (let* (($age (match &self ($who is $a) $a))
          ( ()  (println! (WAS: ($who is $age))))
          ( ()  (remove-atom &self ($who is $age)))
          ( ()  (add-reduct &self ($who is (+ $age 1))))
          ($upd (match &self ($who is $a) $a))
          ( ()  (println! (NOW: ($who is $upd)))))
        $upd
   )
)

; ! (age++ Sam) 
; 35


!(println "xx 19 ") 
; notready
!(assertEqualToResult ( age++ Sam ) ( 35 ) ) 

;[STDOUT] (WAS: (Sam is 34))
;[STDOUT] (NOW: (Sam is 35))
;[35]


