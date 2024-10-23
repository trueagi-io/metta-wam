
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 90../../../hyperon-experimental/python/tests/test_modules.py :24
; <pre>
;        result = metta.run("""
            (three isprime)
            !(match &self ($x isprime) $x)
 ;            !(include test_include)
 ;           !(match &self ($x isprime) $x)
;        """)
;</pre>
; ready works !!
!(assertEqualToResult ( match &self ($x isprime) $x )  ( three )  )
; <br>  lastline 33 <br> 
