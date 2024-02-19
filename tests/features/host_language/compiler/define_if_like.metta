
(= (if-like True $then $else) $then)
(= (if-like False $then $else) $else)

!(bind! &pass (new-state :uncalled))
!(bind! &fail (new-state :uncalled))


(= (set-pass) (let $ (update-state! &pass :called) :pass))
(= (set-pass) (let $ (update-state! &fail :called) :fail))

(= (is-true) True)

!(bind! pass-or-fail (if-like (is-true) (set-pass) (set-fail)))

!(assertEqual pass-or-fail :pass)
!(assertEqual (get-state &pass) :called)
!(assertEqual (get-state &fail) :uncalled)

;; we need to not be too eager to eval arguements
(= pass-or-fail-f (if-like (is-true) (set-pass) (superpose (set-fail))))

!(update-state! &pass :uncalled)
!(update-state! &fail :uncalled)

!(assertEqual (pass-or-fail-f) :pass)
!(assertEqual (get-state &pass) :uncalled)
!(assertEqual (get-state &fail) :uncalled)

