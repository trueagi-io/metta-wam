(: f (-> Number Number))
(= (f $x) $x)
!(compile! f)
; !(repl!)
!(f 42)
!(f (+ 4 2))


(: fs (-> Char Char))
(= (fs $x) $x)
!(compile! fs)
!(fs 42)
!(fs 'X')



!(pfcWatch!)
!(repl!)

