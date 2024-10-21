; 20241007 R.v.Vessum converted python source to metta 
; FILE: 52../../../hyperon-experimental/python/tests/test_examples.py :286
; <pre>
 (= (perform (visit $x)) (perform (lunch-order $x)))
 (= (perform (visit $x)) (perform (health-check $x)))

  (impl (is-achieved (visit $x))
                (And (is-achieved (lunch-order $x)) (is-achieved (health-check $x))))

            (= (achieve $goal)
                (match &self (impl (is-achieved $goal)
                                (And (is-achieved $subgoal1) (is-achieved $subgoal2)))
                    (do $subgoal1 $subgoal2)))

            (= (achieve (health-check Kim)) True)
            (= (achieve (lunch-order Kim)) False)
        
;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br> lastline 300 <br> 
; <pre>

            !(perform (visit Kim))
            !(achieve (visit Kim))
            
;</pre>
; <br> lastline 307 <br> 
; <pre>
            (= (do $goal1 $goal2) (achieve $goal1))
            (= (do $goal1 $goal2) (achieve $goal2))
        
;</pre>
; <br> lastline 311 <br> 
; <pre>
; 14. 20241007 R.v.Vessum converted python source to metta 
!(assertEqualToResult (achieve (visit Kim)) metta.parse_all('False True') &nbsp &nbsp ) 
;</pre>

; <br>  lastline 317 <br> 
