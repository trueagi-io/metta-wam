;Get properties of gene ENSG00000177508
(match &self ($prop (gene ENSG00000177508) $val)
    ($prop $val))

;Find the transcripts of gene ENSG00000177508
(match &self (transcribed_to (gene ENSG00000177508) $transcript)
    $transcript)

;What are the proteins that gene ENSG00000177508 codes for
(match &self (, (transcribed_to (gene ENSG00000177508) $transcript)
                (translated_to $transcript $protein))
                $protein)

;Find the Gene Ontology (GO) categories associated with protein A0A024RBG1
(match &self (go_gene_product $ontology (protein P78415))
    $ontology)

;Find the GO categories associated with gene ENSG00000177508
(match &self (, (transcribed_to (gene ENSG00000177508) $transcript)
                (translated_to $transcript $protein)
                (go_gene_product $ontology $protein))
                $ontology)

;Find biological process GO categories associated with gene ENSG00000177508
(match &self (, (transcribed_to (gene ENSG00000177508) $transcript)
                (translated_to $transcript $protein)
                (go_gene_product $ontology $protein)
                (subontology $ontology biological_process))
                $ontology)

;Find pathways that gene ENSG00000177508 is a subset of
(match &self (genes_pathways (gene ENSG00000177508) $p)
        $p)

;Find pathways that gene IRX3 is a subset of (use the gene HGNC symbol instead of ensembl id)
(match &self (, (gene_name (gene $ens) IRX3)
                (genes_pathways (gene $ens) $p))
        $p)

;Find parent pathways of the pathways that 
;gene IRX3 is a subset of (use the gene HGNC symbol instead of ensembl id)
(match &self (, (gene_name (gene $ens) IRX3)
                (genes_pathways (gene $ens) $p1)
                (parent_pathway_of $p2 $p1))
        $p2)

;What variants have eqtl association with gene IRX3
(match &self (, (gene_name (gene $ens) IRX3)
                (eqtl $seq $ens))
                $seq)

;What variants have eqtl association with gene IRX3 and return the 
;properties of the association
(match &self (, (gene_name (gene $ens) IRX3)
                (eqtl $seq $ens)
                ($prop (eqtl $seq $ens) $val))
           ($prop (eqtl $seq $ens) $val))
