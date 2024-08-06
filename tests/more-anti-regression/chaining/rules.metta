;R1: gene a transcribes transcript t and transcript t is translated to protein p => gene a codes for protein p
(: gene-codes-protein 
    (-> (transcribed_to $g $t)
        (-> (translates_to $t $p)
                (codes_for $g $p))))  


;R2: gene a is closest gene to snp s, gene a is in tad with gene b => snp s is in tad with gene b
(: snp-gene-tad 
     (-> (in_tad_region $g1 $t)
          (-> (in_tad_region $g2 $t)
               (-> (closest_gene $s $g1)
                    (in_tad_with $s $g2)))))


;R3: gene a is in tad-with snp s, gene a has eQTL association with snp s => gene a is relevant for snp s
(: tad-eqtl
     (-> (in_tad_with $s $g1)
          (-> (eqtl $s $g1)
               (relevant_gene $g1 $s))))

;R4: gene a is coexpressed with gene b => gene b is coexpressed with gene a
(: communicative-coexpressed
     (-> (coexpressed_with $g1 $g2) (coexpressed_with $g2 $g1)))

;R5 protein a interacts_with protein b => protein b interacts_with protein a
(: communicative-interaction
     (-> (interacts_with $p1 $p2) (interacts_with $p2 $p1)))

;R6: gene a is relevant for snp s, gene b is co-expressed with gene a => gene b is relevant for snp s
(: relevant-coexpression
     (-> (relevant_gene $g1 $s)
          (-> (coexpressed_with $g1 $g2)
               (relevant_gene $g2 $s))))

;R7: gene a codes for protein p1, gene b codes for protein p2, p1 interacts with p1 => gene a interacts with gene b
; (: gene-gene-interaction
;      (-> (codes_for $g1 $p1)
;           (-> (codes_for $g2 $p2)
;                (-> (interacts_with $p1 $p2)
;                     (interacts_with $g1 $g2)))))

;R8: gene a is relevant for snp s, gene b is interacts with gene a => gene b is relevant for snp s
; (: relevant-interaction
;      (-> (relevant_gene $g1 $s)
;           (-> (interacts_with $g1 $g2)
;                (relevant_gene $g2 $s))))

;R9: protein p member of GO C, gene g codes for protein p => gene g member of GO C
(: gene-go
     (-> (codes_for $g $p)
          (-> (go_gene_product $go $p)
               (member $g $go))))

;R10: gene g is member of GO C1, GO C1 is parent of GO C2 => gene g is member of GO C2
; (: gene-go-parent
;      (-> (member $g $go1)
;           (-> (rel_type (ontology_relationship $go1 $go2) subclass)
;                (member $g $go2))))

;R11: gene a is relevant for snp s, gene a is in GO category C => GO category C is relevant for snp s
(: relevant-go-category
     (-> (relevant_gene $g1 $s)
          (-> (member $g1 $go)
               (relevant_go $go $s))))

;R12: snp s1 is in ld_with snp s2, gene a is relevant for snp s1, gene a has eQTL association with snp s2  => gene b is relevant for snp s2
; (: relevant_gene-ld-snp
;      (-> (in_ld_with $s1 $s2)
;           (-> (relevant_gene $g $s1)
;                (-> (eqtl $s1 $g1)
;                     (relevant_gene $g $s2)))))

;R13: snp s1 in motif m, gene a binds to motif m, gene a regulates gene b, gene b has eQTL association with snp s1 => gene b is relevant for snp s1
; (-> : relevant-motif-snp-gene
;      (-> (disrupts $s $g1)
;           (-> (interacts_with $g1 $g2)
;                (-> (relevant $g2 $s)
;                     (-> (member $g2 $go)
;                          (relevant $go $s))))))