

!(bind! &ex1 (new-space))
!(add-atom &ex1 (y y (ɑ 0)))
!(add-atom &ex1 (x z (ɑ 0)))
!(add-atom &ex1 ((ɑ 0) 1 A))

(= (name $x) (case $x (
  ((ɑ $n) (format-args "ɑ{}" ($n)))
  ($k (format-args "n{}" ($k)))
)))
(= (graphviz-triple ($x $p $y))
  (let* (($xn (name $x)) ($pn (name $p)) ($yn (name $y)))
    (format-args "{} -> {} [label=\"{}\"]"
      ($xn $yn $pn)
)))
(= (graphviz $space)
  (println! (graphviz-triple (get-atoms $space)))
)
!(graphviz &ex1)

