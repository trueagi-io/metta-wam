(= (test-func2 $b)  (let $a (, hello $b) $a))

!(assertResultEqualTo 
     (test-func2 world) 
       ((, hello world)))



