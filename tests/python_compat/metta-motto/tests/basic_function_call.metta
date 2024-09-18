!(import! &self motto)

(= (doc func_test)
   (Doc
     (description "call-me")
     (parameters
      (arg "argument=" (A B C D))
     ))
)

(= (func_test ("A") $msgs)
   "SUCCESS")

!(assertEqual
  ((echo-agent)
   (user "Please execute call-me with argument=A")
   (Function func_test))
  "SUCCESS")
