
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
metta = MeTTa()
metta.run(
)
pattern = metta.parse_single()
print(metta.space().query(pattern)) # [{ $x &lt;- Pam }, { $x &lt;- Tom }]

;  !(assertEqualToResult ( )  (  )  ) 
