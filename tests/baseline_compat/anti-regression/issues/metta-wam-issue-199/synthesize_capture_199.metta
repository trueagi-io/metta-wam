
;; Define synthesize
(: synthesize (-> Atom Atom Atom))
(= (synthesize $query $axiom) (case $axiom (($query $query))))

