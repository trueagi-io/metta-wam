!(extend-py! mettamorph)
!(compile! (
(= (test1) 42)
(: test2 (-> Number Number))
(= (test2 $x) 42)
))

!(test1)
!(test2 33)
