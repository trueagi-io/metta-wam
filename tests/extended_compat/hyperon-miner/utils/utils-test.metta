!(register-module! ../../hyperon-miner)
!(import! &self hyperon-miner:utils:MinerUtils)

!(assertEqualToResult 
    (Debruijn2var (Inheritance (VarIdx Z) human) (Cons $Xvar (Cons $Yvar Nil)))
    ((Inheritance $Xvar human)))

!(assertEqualToResult 
    (Debruijn2var (Inheritance (VarIdx Z) (VarIdx (S Z))) (Cons $Xvar (Cons $Yvar Nil)))
    ((Inheritance $Xvar $Yvar)))

!(assertEqualToResult 
    (Deb2var (, (Inheritance (VarIdx Z) human) (Inheritance (VarIdx Z) man)) (Cons $Xvar (Cons $Yvar Nil)))
    ((, (Inheritance $Xvar human) (Inheritance $Xvar man))))

!(assertEqualToResult
     (tuple-count (1 2 3))
     (3))
