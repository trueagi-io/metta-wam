
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Sam is 34)
(Tom is 14)
(= (person-by-age $age)
   (match &amp;self ($who is $age) $who))
(= (persons-of-age $a $b)
   (let* ((($age-min $age-max $diff)
           (if (&lt; $a $b)
               ($a $b (- $b $a))
               ($b $a (- $a $b))))
          ($younger (person-by-age $age-min))
          ($older   (person-by-age $age-max))
         )
         ($younger is younger than $older by $diff years))
)

;  !(assertEqualToResult ( )  (  )  ) 
