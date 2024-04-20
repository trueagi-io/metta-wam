

(: wazaL (-> $a (List $a) (List $a)))

(= (wuzaL $x $L)
   (let* (($y (* 2 $x))
            ($z (superpose $L)))
        ($y $z)))

!(compile! wuzaL)

!(assertEqual
  (wuzaL 2 (3 4))
  (superpose ((4 3)(4 4))))
