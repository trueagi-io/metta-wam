
; Check whether atom is space comparing its type with type of the &self atom
(: is-space (-> Atom Bool))
(= (is-space $atom)
   (let* (($type (get-type $atom)) ($space (get-type &self))) (== $type $space)))


!(assertEqual (is-space &self) True)

!(assertEqual (is-space &corelib) True)

!(assertEqual (is-space 666) False)
