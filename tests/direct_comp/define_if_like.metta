









(= (if-like True $then $else) $then)
(= (if-like False $then $else) $else)

!(bind! &pass (new-state :uncalled))
!(bind! &fail (new-state :uncalled))


(= (set-pass) (let $ (change-state! &pass :called) :pass))
(= (set-fail) (let $ (change-state! &fail :called) :fail))

(= (is-true) True)

!(bind! pass-or-fail (if-like (is-true) (set-pass) (set-fail)))

!(assertEqual pass-or-fail :pass)
!(1)
!(assertEqual (get-state &pass) :called)
!(assertEqual (get-state &fail) :called)
!(2)
;; we need to not be too eager to eval arguements
(= (pass-or-fail-f) (if-like (is-true) (set-pass) (superpose (set-fail))))
!(3)

!(change-state! &pass :uncalled)
!(change-state! &fail :uncalled)
!(4)
!(assertEqual (pass-or-fail-f) :pass)
!(5)
!(assertEqual (get-state &pass) :called)
!(assertEqual (get-state &fail) :uncalled)


