
; 20241014 r.v.v converted metta-lang.dev to metta 

(Sam is 34)
(Tom is 14)

(= (person-by-age $age)
   (match &self ($who is $age) $who))

(= (persons-of-age $a $b)
   (let* ((($age-min $age-max $diff)
           (if (< $a $b)
               ($a $b (- $b $a))
               ($b $a (- $a $b))))
          ($younger (person-by-age $age-min))
          ($older   (person-by-age $age-max))
         )
         ($younger is younger than $older by $diff years))
)


;! (persons-of-age 34 14)
;[(Tom is younger than Sam by 20 years)]


!(println "xx 18 ") 
; ready works 
!(assertEqualToResult ( persons-of-age 34 14 ) ( (Tom is younger than Sam by 20 years) ) ) 
