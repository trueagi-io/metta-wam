(: go-level
    (-> (go_gene (ontology_term $o) (gene $g))
        (member (gene $g) (ontology_term $o) Z)))

(: go-level 
    (-> (go_parent (ontology_term $o) (ontology_term $x))
        (-> (member (gene $g) (ontology_term $x) $k)
            (member (gene $g) (ontology_term $o) (S $k)))))

(: go_gene-a-g (go_gene (ontology_term a) (gene g)))
(: go_gene-b-g (go_gene (ontology_term b) (gene g)))

(: go_parent-c-b (go_parent (ontology_term c) (ontology_term b)))
(: go_parent-d-c (go_parent (ontology_term d) (ontology_term c)))