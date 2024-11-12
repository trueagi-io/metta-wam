
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 18../../../hyperon-experimental/python/tests/test_metta.py :22
; <pre>
;        program = '''
  ;          (= (And T T) T)
   ;         (= (frog $x)
   ;             (And (croaks $x)
    ;                 (eat_flies $x)))
     ;       (= (croaks Fritz) T)
     ;       (= (eat_flies Fritz) T)
     ;       (= (green $x) (frog $x))
      ;      !(green Fritz)
  ;      '''
   ;     runner = MeTTa(env_builder=Environment.test_env())
   ;     result = runner.run(program)

        
;<br>5 20241007 R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  )
;<br>
;([[S('T')]], result)

 ;   def test_metta_evaluate_atom(self):
  ;      program = '''
            (= (And T T) T)
            (= (frog $x)
                (And (croaks $x)
                     (eat_flies $x)))
            (= (croaks Fritz) T)
            (= (eat_flies Fritz) T)
            (= (green $x) (frog $x))
   ;     '''
   ;     runner = MeTTa(env_builder=Environment.test_env())
;</pre>
; ready works 
!(assertEqualToResult ( green Fritz )  ( T )  )
; <br>  lastline 51 <br> 
