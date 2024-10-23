; 20241007 R.v.Vessum converted python source to metta 
; FILE: 52../../../hyperon-experimental/python/tests/test_examples.py :286
; <pre>

 ;( Parent bob mary )
 ;(= Parent bob (mary) )

 (= (perform (visit $x)) (perform (lunch-order $x)))
 (= (perform (visit $x)) (perform (health-check $x)))

 (= (do $goal1 $goal2) (achieve $goal1))
 (= (do $goal1 $goal2) (achieve $goal2))

 (= (achieve (health-check Kim)) True)
 ;(= (achieve (lunch-order Kim)) False)
 ; (= (achieve (lunch-order Kim)) True)
 ; (= (is-achieved (lunch-order Kim)) False)
 ; (= (is-achieved (health-check Kim)) True)
 (perform (lunch-order Kim)) 
 (perform (health-check Kim))
 
 (do (lunch-order Kim) (health-check Kim))


 (impl (is-achieved (visit $x))
                (And (is-achieved (lunch-order $x)) (is-achieved (health-check $x))))
 (= (achieve $goal)
                (match &self (impl (is-achieved $goal)
                                (And (is-achieved $subgoal1) (is-achieved $subgoal2)))
                    (do $subgoal1 $subgoal2)))
; !(perform (visit Kim))
; !(achieve (visit Kim))
!(assertEqualToResult ( (achieve (visit Kim)) )  ( False True )  )


; <br> lastline 300 <br> 


; <br> lastline 307 <br> 
; <br> lastline 311 <br> 
; 14. 20241007 R.v.V converted python source to metta 
; !(assertEqualToResult (achieve (visit Kim)) ( True ) ) 
; <br>  lastline 317 <br> 
