


!(bind! &ps1
  (eval-for $pid (process-create! (bash) (:: -c "MeTTa server1-with-some-data.metta server2-with-some-data.metta -G \'(create-vspace-server &self 3021)\')")) (:: (process $pid))))

!(assertEqualToResult
  (remote-eval "localhost:3021"
   (match &self
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop))
  ((Color of Planet Mars is Red)))

!(process-kill! &ps1)


