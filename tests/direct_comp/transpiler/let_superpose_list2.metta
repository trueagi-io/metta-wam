(= (wuzaL $x $L)
   (let* (($y (* 2 $x))
            ($z (superpose $L)))
        ($y $z)))


!(assertEqual
  (wuzaL 2 (3 4))
  (superpose ((4 3)(4 4))))
