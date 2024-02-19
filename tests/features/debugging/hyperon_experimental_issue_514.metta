

; https://github.com/trueagi-io/hyperon-experimental/issues/514

(= (loop) (loop))

; instead we can rewrite and via if

(: and2 (-> Atom Atom Bool))
(= (and2 $X $Y)
  (if $X $Y False))

!(and2 False (loop))

!(and False (loop))
; Evaluating the second argument will harm the performance even if the second argument doesn't contain infinite loop

