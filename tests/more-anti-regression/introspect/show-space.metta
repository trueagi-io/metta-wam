
;(: = (-> Atom Atom))
;(: : (-> Atom Atom))
(: = (-> Atom Atom Atom))
(: : (-> Atom Atom Atom))

(: my-println (-> Atom Atom))
(= (my-println $info) 
   (let (quote $u) $info (trace! $u True)))


(: show-space (-> hyperon::space::DynSpace Atom))
(= (show-space $space)
	(show-space-helper-3 $space $space 0))

(: show-space-helper-3 (-> Atom Atom Number Atom))
(= (show-space-helper-3 $orig $obj $depth)
  (nop (collapase (let $objtype (get-type $obj)
	 (show-space-helper-4 $orig $obj $objtype $depth)))))

(: show-space-helper-4 (-> Atom Atom Atom Number Atom))
(= (show-space-helper-4 $orig $obj $objtype $depth) 
 (if  (== $objtype hyperon::space::DynSpace)
   (let $_0 (my-println (quote ($orig = space $obj)))
	 (let $_1 (collapase (let $e (get-atoms $obj)
						  (show-space-helper-3 $obj $e (+ 1 $depth)))) ()))    
  (my-println (quote ($orig = $obj)))))


