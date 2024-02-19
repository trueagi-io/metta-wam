(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))
!(extend-py! mettamorph)
!(compile! "
!(bind! &schemespace (new-space))
(= (add-atom-scheme $x) (add-atom &schemespace $x))
(= (get-atoms-scheme) (get-atoms &schemespace))
")
!(bind! &mettaspace (new-space))
(= (add-atom-metta $x) (add-atom &mettaspace $x))
(= (get-atoms-metta) (get-atoms &mettaspace))

!(add-atom-metta 42)
!(add-atom-metta 43)
!(add-atom-scheme 44)
!(add-atom-scheme 45)
!(superpose ((get-atoms-metta) (get-atoms-scheme)))


