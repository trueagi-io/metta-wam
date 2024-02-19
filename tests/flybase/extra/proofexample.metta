
; rule 1 (link(X Y)-> Path(X Y))
; rule 2 (Path(X Y), (link Y Z) -> Path(X Z))

; kb (Link A B) (Link B C)

!(import! &self ../../../../hyperon-pln/metta/synthesis/Synthesize.metta)

!(bind! &db (new-space))
!(add-atom &db (link a b))
!(add-atom &db (link b c))

(: kb (-> Atom))
(= (kb) (superpose ( (: ab (link a b))
                     (: bc (link b c)))))

(= (rule1)
  (: r1 (-> (link $x $y) (path $x $y))))
(= (rule2)
  (: r2 (-> (path $x $y) (link $y $z) (path $x $z))))

(: rb (-> Atom))
(= (rb) (superpose ((rule2)
                    (rule1))))

!(synthesize (: $proof (path a c)) kb rb (fromNumber 3)) ;; (: (r2 (r1 ab) bc) (path a c))
