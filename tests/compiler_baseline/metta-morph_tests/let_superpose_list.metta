; !(pragma! compile True)

; !(compile-space! &self)

!(pfcWatch!)

; (: waza (-> $a (List $a)))

(= (wuza $x)
   (let* (($y (* 2 $x))
		  ($z (superpose (4 5))))
		 ($y $z)))

!(compile! wuza)

!(wuza 5)

!(assertEqual
  (wuza 5)
  (superpose ((10 4) (10 5))))
