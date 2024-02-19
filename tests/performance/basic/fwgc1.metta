(: State (-> Side Side Side Side))
(: safe (-> Side Side Side Side Bool))

(: move-farmer-alone (-> Side Side Side Side State))
(: move-farmer-wolf (-> Side Side Side Side State))
(: move-farmer-goat (-> Side Side Side Side State))
(: move-farmer-cabbage (-> Side Side Side Side State))

; Define which states are safe
(= (safe $f $w $g $c) (or (eq $f $w) (and (neq $f $g) (neq $f $c))))

; Move rules
(= (move-farmer-alone $f $w $g $c) (if (safe (not $f) $w $g $c) (State (not $f) $w $g $c) invalid))
(= (move-farmer-wolf $f $w $g $c) (if (and (eq $f $w) (safe (not $f) (not $w) $g $c)) (State (not $f) (not $w) $g $c) invalid))
(= (move-farmer-goat $f $w $g $c) (if (and (eq $f $g) (safe (not $f) $w (not $g) $c)) (State (not $f) $w (not $g) $c) invalid))
(= (move-farmer-cabbage $f $w $g $c) (if (and (eq $f $c) (safe (not $f) $w $g (not $c))) (State (not $f) $w $g (not $c)) invalid))

(: solve (-> State State Bool))
(= (solve $start $goal) (if (eq $start $goal) True (or (solve (move-farmer-alone $start) $goal)
                                                       (solve (move-farmer-wolf $start) $goal)
                                                       (solve (move-farmer-goat $start) $goal)
                                                       (solve (move-farmer-cabbage $start) $goal))))

; Initial state: All on one side of the river
!(let start (State True True True True))

; Goal state: All on the other side of the river
!(let goal (State False False False False))

!(assertEqual (solve start goal) True)

