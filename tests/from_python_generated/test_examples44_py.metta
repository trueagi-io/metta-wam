
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 44../../../hyperon-experimental/python/tests/test_examples.py :95
; <pre>

            (= (croaks Fritz) True)
            (= (chirps Tweety) True)
            (= (yellow Tweety) True)
            (= (eats_flies Tweety) True)
            (= (eats_flies Fritz) True)
        
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 101 <br> 
 

; <pre>
!(if (and (croaks $x) (eats_flies $x)) (= (frog $x) True) nop)
;</pre>

; <br> lastline 103 <br> 
 

; <pre>
        self.assertEqual(metta.parse_all('(= (frog Fritz) True)'), fritz_frog)
;</pre>

; <br>  lastline 107 <br> 
