
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 44../../../hyperon-experimental/python/tests/test_examples.py :95
; <pre>

            (= (croaks Fritz) True)
            (= (chirps Tweety) True)
            (= (yellow Tweety) True)
            (= (eats_flies Tweety) True)
            (= (eats_flies Fritz) True)
        
;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br> lastline 101 <br> 
; <pre>
; !(if (and (croaks $x) (eats_flies $x)) (= (frog $x) True) nop)
; Added
; rne add
;(= ( (frog $x) True  )
;  (and (croaks $x) (eats_flies $x) )
;   (croaks $x) 
;  )
;</pre>
; <br> lastline 103 <br> 
; <pre>
;        self.assertEqual(metta.parse_all('
;  (= (frog Fritz) True)'), fritz_frog)
; !(match (frog Fritz) &er )
;!(match &self (= frog Tweety) $value)

;!(match &self (= (frog Fritz) $res)  $res)
; !(if (and (croaks $x) (eats_flies $x)) (= (frog $x) True) nop)
; !(assertEqualToResult ( )  (  )  )
; ready works
!(assertEqualToResult 
  ( if (and (croaks $x) (eats_flies $x)) (= (frog $x) True) nop )   
  ( (= (frog Fritz) True)  )
  )

;!(assertEqualToResult (= (frog Fritz ) True )  ( True )  )

;!(assertEqualToResult (frog Fritz )  ( True )  )
; !(assertEqualToResult (= frog Fritz )  ( True )  )

;!(match &self (= (frog $qq) $res)  $res)
;!(assertEqualToResult (= frog Fritz)  (True)  )
;!(= (frog Fritz) True)
;!(match (frog Fritz) $Result)
;(match &self (state $var $y)
;!(print "result:")
;!(print $value)


;</pre>

; <br>  lastline 107 <br> 
