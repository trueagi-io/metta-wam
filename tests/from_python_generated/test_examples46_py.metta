
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 46../../../hyperon-experimental/python/tests/test_examples.py :113
; <pre>


        
;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br> lastline 118 <br> 
 

; <pre>
 (= (: (apply $f $x) $r) (and (: $f (=> $a $r)) (: $x $a)))
 (= (: reverse (=> String String)) True)
 (= (: "Hello" String) True)

!(if (: (apply reverse "Hello") $t) $t Wrong)

!(assertEqualToResult (if (: (apply reverse "Hello") $t) $t Wrong)  ( String )  )
;</pre>

; <br> lastline 120 <br> 
 

; <pre>
 ;       self.assertEqualMettaRunnerResults(output, [[S('String')]])
;</pre>

; <br>  lastline 126 <br> 
