


;; the comma only works in match 

(= (test-func2 $b)  (let $a (, hello $b) $a))

!(assertEqualToResult 
     (test-func2 world) 
       ((, hello world)))



