;     fn superpose_op_consumes_interpreter_errors() 
; this test  causes a bug in mettalog 
   (: f (-> A B))
   (= (f $x) $x)
   (: a A)
   (: b B)
   !(superpose ( (f (superpose ())) (f a) (f b) ) )
   
