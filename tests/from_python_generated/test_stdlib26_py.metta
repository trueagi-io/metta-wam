
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 26../../../hyperon-experimental/python/tests/test_stdlib.py :32
; <pre>

;! (bind! &pow (py-atom math.pow (-> Number Number Number)))
;! (bind! &math (py-atom math))

;!(assertEqualToResult ( )  (  )  )

;! (&pow 2 3)
;! ((py-dot &math floor) (py-dot &math pi))
;! ((py-dot (py-atom "{5: \'f\'}") get) 5)
;! ((py-atom str) 5)
;! ((py-dot (py-atom os) path.commonpath)
;(py-atom "['/usr/lib', '/usr/local/lib']"))


