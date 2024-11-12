
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 22../../../hyperon-experimental/python/tests/test_metta.py :89
; <pre>
        program = '''
          ; True is used as a Python grounded object
          (grounded True)
          ; import! is used as a Rust grounded object which can be
          ; received from Python
          !(match &self (grounded import!) Ok)
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        result = runner.run(program)

        
;<br>7 20241007 R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  )
;<br>
([[]], result)

;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br>  lastline 10 <br> 
