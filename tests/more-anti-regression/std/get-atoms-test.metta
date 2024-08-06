

; !(pragma! compile full)

!(import! &kb ./../chaining/go_rel)


(assertEqualToResult
  (get-atoms &kb)

  (&stdlib &corelib
	(: go-level
	  (->
		(go_gene
		  (ontology_term $o_2)
		  (gene $g_3))
		(member
		  (gene $g_3)
		  (ontology_term $o_2) Z)))
	(: go-level
	  (->
		(go_parent
		  (ontology_term $o_4)
		  (ontology_term $x_5))
		(->
		  (member
			(gene $g_6)
			(ontology_term $x_5) $k_7)
		  (member
			(gene $g_6)
			(ontology_term $o_4)
			(S $k_7)))))
	(: go_gene-a-g
	  (go_gene
		(ontology_term a)
		(gene g)))
	(: go_gene-b-g
	  (go_gene
		(ontology_term b)
		(gene g)))
	(: go_parent-c-b
	  (go_parent
		(ontology_term c)
		(ontology_term b)))
	(: go_parent-d-c
	  (go_parent
		(ontology_term d)
		(ontology_term c)))))

