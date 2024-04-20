!(pfcWatch!)
(= (g $x) $x)
(= (g $x) (* 2 $x))
(= (g test ($y $z)) (* (* 1 $y) $z))
(= (g 3 ($y $z)) (* $y $z))
(= (g $x ($y $z)) (* (* $x $y) 42))
(= (g $x ($y $z)) (* (* 42 $x) $y))
(= (g a ($y w)) $y)

!(listing! g)

!(g 42 (1 2))

!(assertEqualToResult
  (g 42
	(1 2))
  (1764 1764))


!(assertEqualToResult
  (g 42)
  (42 84))



   
!(listing! g)
