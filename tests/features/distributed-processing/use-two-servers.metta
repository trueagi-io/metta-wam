
!(bind! &ps1
  (eval-for $pid (process-create! (bash) (:: -c "MeTTa server1-with-some-data.metta  -G \'(create-vspace-server &self 3021)\')")) (:: (process $pid))))

!(bind! &ps2
  (eval-for $pid (process-create! (bash) (:: -c "MeTTa server2-with-some-data.metta -G \'(create-vspace-server &self 3025)\')")) (:: (process $pid))))


!(assertEqualToResult
  (remote-eval "localhost:3021"
   (match &self
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop)))
  ((Color of Planet Mars is Red)))


!(assertEqualToResult
  (remote-eval "localhost:3025"
   (match &self
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop)))
  ())

!(process-kill! &ps1)

!(process-kill! &ps2)


