!(extend-py! mettamorph)
!(compile! "
(= (f1 $x) (+ 2 $x))
(= (use1 $f $arg)
   ($f $arg))
")

(= (f2 $x) (+ 1 $x))
(= (use2 $f $arg)
   ($f $arg))

!(+ 1 (use1 f2 40))
!(+ 1 (use1 f1 39))
!(+ 1 (use2 f2 40))
!(+ 1 (use2 f1 39))
