
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 20../../../hyperon-experimental/python/tests/test_metta.py :54
; <pre>
        program = '''
            !(+ 1 (+ 2 (+ 3 4)))
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        runner_state = RunnerState(runner, program)

        step_count = 0
        while not runner_state.is_complete():
            runner_state.run_step()
            step_count += 1

        results = runner_state.current_results()
        
;<br>6 20241007 R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  )
;<br>
(repr(results), "[[10]]")

    def test_gnd_type_error(self):
        program = '''
          !(+ 2 "String")
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        result = runner.run(program)

        self.assertEqual([[E(S('Error'), ValueAtom('String'), S('BadType'))]], result)

    def test_runner_error(self):
        program = '''
          !(+ 2 3
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br>  lastline 86 <br> 
