
!(assertEqual (py-list ()) (py-eval "[]"))
!(assertEqual (py-tuple ()) (py-eval "()"))
!(assertEqual (py-dict ()) (py-eval "{}"))
!(assertEqual (py-tuple (1 (2 (3 "3")) (py-atom list))), (py-eval "(1,(2,(3, "3")), list)"))
!(assertEqual (py-list (1 2 (4.5 3))) (py-eval "[1,2,[4.5,3]]"))
!(assertEqual (py-list (1 2 (py-tuple (3 4)))) (py-eval "[1,2, (3,4)]"))

!(assertEqual (py-dict ((a "b") ("b" "c"))) (py-eval '{"a":"b", "b":"c"}'))

!(assertNotEqual (py-list (a "b" c)) (py-list (a b c)))

!(assertNotEqual (py-nth! (py-list (a "b" c)) 1) (py-nth! (py-list (a 'b' c)) 1))


; We need py-chain for langchain, but we test with bitwise operation | (1 | 2 | 3 | 4 = 7) and logical OR
!(assertEqual (py-chain (1 2 3 4)) (py-eval "7"))
!(assertEqualToResult (py-chain (True False)) (True))
!(assertEqualToResult (py-chain (False False)) (False))

!(bind! &pow (py-atom math.pow (-> Number Number Number)))
!(bind! &math (py-atom math))

!(assertEqual (&pow 2 3) ((py-dot &math pow) 2 3))

!(assertEqual 3 ((py-dot &math floor) (py-dot &math pi)))

!(assertEqual (py-type! (py-nth! (py-list (a "b" c)) 1)) str)

!(assertEqual (py-type! (py-nth! (py-list (a b c)) 1)) SymbolAtom)



