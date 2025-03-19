
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query1 - BRCA2 Gene Interactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query1 - BRCA2 Gene Interactions"
  "Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "BRCA2")) ;; single-var => best index usage, used 3 times
      (neo $Gene:gene transcribed_to $Tx:transcript)  ;; sum usage=5 ($Gene(3) + $Tx(2))
      (neo $Gene:gene genes_pathways $Pathway:pathway) ;; sum usage=4 ($Gene(3) + $Pathway(1)) - tie w/ next
      (neo $Tx:transcript translates_to $Prot:protein) ;; sum usage=4 ($Tx(2) + $Prot(2)) - tie
      (neo $Prot:protein interacts_with $Prot2:protein) ;; sum usage=3
    )
    (result $Gene $Tx $Prot $Prot2 $Pathway)))
  (neo4j-info (execution_time "stopped_after(4980)")
              (result_count 151956) (node_count 1908) (edge_count 1882)))

(compare-query "Query1 (With Properties) - BRCA2 Gene Interactions"
  "Find all interactions involving the BRCA2 gene, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name (startsWith "BRCA2") $PropList1)   ;; single-var => best index usage
      (neo_P $Gene:gene transcribed_to $Tx:transcript $PropList2)    ;; sum usage=5
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $PropList5)  ;; sum usage=4
      (neo_P $Tx:transcript translates_to $Prot:protein $PropList3)  ;; sum usage=4
      (neo_P $Prot:protein interacts_with $Prot2:protein $PropList4) ;; sum usage=3
    )
    (result $Gene $Tx $Prot $Prot2 $Pathway
            $PropList1 $PropList2 $PropList3 $PropList4 $PropList5)))
  (neo4j-info (execution_time "stopped_after(4980)")
              (result_count 151956) (node_count 1908) (edge_count 1882)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query2 - IGF2 Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query2 - IGF2 Components"
  "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF2"))            ;; single-var => best index usage
      (neo $Gene:gene genes_pathways $Pway:pathway)             ;; sum usage=6 ($Gene(4) + $Pway(2))
      (neo $Prom:promoter associated_with $Gene:gene)           ;; sum usage=5 ($Prom(1)+$Gene(4))
      (neo $Enh:enhancer associated_with $Gene:gene)            ;; sum usage=5 ($Enh(1)+$Gene(4))
      (neo $Child:pathway child_pathway_of $Pway:pathway)       ;; sum usage=3 ($Child(1)+$Pway(2))
    )
    (result $Prom $Gene $Enh $Pway $Child)))
  (neo4j-info (execution_time "about(180)")
              (result_count 41536) (node_count 144) (edge_count 143)))

(compare-query "Query2 (With Properties) - IGF2 Components"
  "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name (startsWith "IGF2") $PropList1)    ;; single-var => best index usage
      (neo_P $Gene:gene genes_pathways $Pway:pathway $PropList4)     ;; sum usage=6
      (neo_P $Prom:promoter associated_with $Gene:gene $PropList2)   ;; sum usage=5
      (neo_P $Enh:enhancer associated_with $Gene:gene $PropList3)    ;; sum usage=5
      (neo_P $Child:pathway child_pathway_of $Pway:pathway $PropList5) ;; sum usage=3
    )
    (result $Prom $Gene $Enh $Pway $Child
            $PropList1 $PropList2 $PropList3 $PropList4 $PropList5)))
  (neo4j-info (execution_time "about(180)")
              (result_count 41536) (node_count 144) (edge_count 143)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query3 - Gene Interactions and GO Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query3 - Gene Interactions and GO Terms"
  "Find gene interactions and associated GO terms including proteins and transcripts."
  (match &neo4j_out_v3
    (,
      (neo $Prot1:protein translation_of $Tx:transcript)  ;; sum usage=6 ($Prot1(3)+$Tx(3)), highest
      (neo $Gene:gene transcribed_to $Tx:transcript)      ;; sum usage=4, tie among rest
      (neo $Tx:transcript includes $Exon:exon)            ;; sum usage=4
      (neo $Prot1:protein interacts_with $Prot2:protein)  ;; sum usage=4
      (neo $GO:go go_gene_product $Prot1:protein)         ;; sum usage=4
    )
    (result $Gene $Tx $Exon $Prot1 $Prot2 $GO)))
  (neo4j-info (execution_time "stopped_after(1200)")
              (result_count "too_many_results")
              (node_count "unknown")
              (edge_count "unknown")))

(compare-query "Query3 (With Properties) - Gene Interactions and GO Terms"
  "Find gene interactions and associated GO terms (proteins/transcripts), returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Prot1:protein translation_of $Tx:transcript $P3) ;; sum usage=6
      (neo_P $Gene:gene transcribed_to $Tx:transcript $P1)     ;; sum usage=4
      (neo_P $Tx:transcript includes $Exon:exon $P2)           ;; sum usage=4
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P4) ;; sum usage=4
      (neo_P $GO:go go_gene_product $Prot1:protein $P5)        ;; sum usage=4
    )
    (result $Gene $Tx $Exon $Prot1 $Prot2 $GO
            $P1 $P2 $P3 $P4 $P5)))
  (neo4j-info (execution_time "stopped_after(1200)")
              (result_count "too_many_results")
              (node_count "unknown")
              (edge_count "unknown")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query4 - Interactions Involving 1433B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query4 - Interactions Involving 1433B"
  "Find interactions involving 1433B protein, including transcripts, exons, and GO terms."
  (match &neo4j_out_v3
    (,
      (neo $Prot1:protein protein_name (startsWith "1433B")) ;; single-var => best index usage
      (neo $Prot1:protein translation_of $Tx:transcript)      ;; sum usage=7 ($Prot1(4)+$Tx(3))
      (neo $Prot1:protein interacts_with $Prot2:protein)      ;; sum usage=5 ($Prot1(4)+$Prot2(1))
      (neo $GO:go go_gene_product $Prot1:protein)             ;; sum usage=5 ($GO(1)+$Prot1(4))
      (neo $Gene:gene transcribed_to $Tx:transcript)          ;; sum usage=4 ($Gene(1)+$Tx(3))
      (neo $Tx:transcript includes $Exon:exon)                ;; sum usage=4 ($Tx(3)+$Exon(1))
    )
    (result $Gene $Tx $Exon $Prot1 $Prot2 $GO)))
  (neo4j-info (execution_time "stopped_after(2400)")
              (result_count 794742) (node_count 2693) (edge_count 2698)))

(compare-query "Query4 (With Properties) - Interactions Involving 1433B"
  "Find interactions involving 1433B protein, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Prot1:protein protein_name (startsWith "1433B") $P1) ;; single-var => best index usage
      (neo_P $Prot1:protein translation_of $Tx:transcript $P4)      ;; sum usage=7
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P5)      ;; sum usage=5
      (neo_P $GO:go go_gene_product $Prot1:protein $P6)             ;; sum usage=5
      (neo_P $Gene:gene transcribed_to $Tx:transcript $P2)          ;; sum usage=4
      (neo_P $Tx:transcript includes $Exon:exon $P3)                ;; sum usage=4
    )
    (result $Gene $Tx $Exon $Prot1 $Prot2 $GO
            $P1 $P2 $P3 $P4 $P5 $P6)))
  (neo4j-info (execution_time "stopped_after(2400)")
              (result_count 794742) (node_count 2693) (edge_count 2698)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query5 - Components Associated with IGF1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query5 - Components Associated with IGF1"
  "Find enhancers, pathways, and transcripts associated with the IGF1 gene."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")                      ;; single-var => best index usage
      (neo $Tx:transcript transcribed_from $Gene:gene)       ;; sum usage=6 ($Gene(4)+$Tx(2))
      (neo $Enh:enhancer associated_with $Gene:gene)         ;; sum usage=5 ($Enh(1)+$Gene(4))
      (neo $Gene:gene genes_pathways $Pathway:pathway)       ;; sum usage=5 ($Gene(4)+$Pathway(1))
      (neo $Tx:transcript translates_to $Prot:protein)       ;; sum usage=3 ($Tx(2)+$Prot(1))
    )
    (result $Gene $Pathway $Enh $Tx $Prot)))
  (neo4j-info (execution_time 25) (result_count 1932) (node_count 45) (edge_count 49)))

(compare-query "Query5 (With Properties) - Components Associated with IGF1"
  "Find enhancers, pathways, and transcripts associated with IGF1, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $P1)                 ;; single-var => best index usage
      (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)   ;; sum usage=6
      (neo_P $Enh:enhancer associated_with $Gene:gene $P3)     ;; sum usage=5
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)   ;; sum usage=5
      (neo_P $Tx:transcript translates_to $Prot:protein $P5)   ;; sum usage=3
    )
    (result $Gene $Pathway $Enh $Tx $Prot
            $P1 $P2 $P3 $P4 $P5)))
  (neo4j-info (execution_time 25) (result_count 1932) (node_count 45) (edge_count 49)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query6 - Pathways and Protein Interactions for IGF1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query6 - Pathways and Protein Interactions for IGF1"
  "Find pathways and interacting proteins for the IGF1 gene including all associated components."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")                    ;; single-var => best index usage
      (neo $Tx:transcript transcribed_from $Gene:gene)     ;; sum usage=6
      (neo $Enh:enhancer associated_with $Gene:gene)       ;; sum usage=5
      (neo $Gene:gene genes_pathways $Pathway:pathway)     ;; sum usage=5
      (neo $Tx:transcript translates_to $Prot1:protein)    ;; sum usage=4
      (neo $Prot1:protein interacts_with $Prot2:protein)   ;; sum usage=3
    )
    (result $Gene $Pathway $Enh $Tx $Prot1 $Prot2)))
  (neo4j-info (execution_time "squiggly_mark")
              (result_count 5790204)
              (node_count 3042)
              (edge_count 3046)))

(compare-query "Query6 (With Properties) - Pathways and Protein Interactions for IGF1"
  "Find pathways and interacting proteins for the IGF1 gene, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $P1)                   ;; single-var => best index usage
      (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)     ;; sum usage=6
      (neo_P $Enh:enhancer associated_with $Gene:gene $P3)       ;; sum usage=5
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)     ;; sum usage=5
      (neo_P $Tx:transcript translates_to $Prot1:protein $P5)    ;; sum usage=4
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P6)   ;; sum usage=3
    )
    (result $Gene $Pathway $Enh $Tx $Prot1 $Prot2
            $P1 $P2 $P3 $P4 $P5 $P6)))
  (neo4j-info (execution_time "squiggly_mark")
              (result_count 5790204)
              (node_count 3042)
              (edge_count 3046)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query7 - Transcripts and Exons for TP73-AS1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query7 - Transcripts and Exons for TP73-AS1"
  "Find transcripts and exons associated with the TP73-AS1 gene."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (endsWith "TP73-AS1"))  ;; single-var => best index usage
      (neo $Tx:transcript transcribed_from $Gene:gene)  ;; references $Gene + $Tx
      (neo $Tx:transcript includes $Exon:exon)          ;; references $Tx + $Exon
    )
    (result $Tx $Exon $Gene)))
  (neo4j-info (execution_time 8) (result_count 142) (node_count 106) (edge_count 284)))

(compare-query "Query7 (With Properties) - Transcripts and Exons for TP73-AS1"
  "Find transcripts and exons associated with the TP73-AS1 gene, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name (endsWith "TP73-AS1") $P3)   ;; single-var => best index usage
      (neo_P $Tx:transcript transcribed_from $Gene:gene $P2)   ;; references $Gene + $Tx
      (neo_P $Tx:transcript includes $Exon:exon $P1)           ;; references $Tx + $Exon
    )
    (result $Tx $Exon $Gene $P1 $P2 $P3)))
  (neo4j-info (execution_time 8) (result_count 142) (node_count 106) (edge_count 284)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query8 - Interactions Involving 1433S Protein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query8 - Interactions Involving 1433S Protein"
  "Find proteins interacting with 1433S and associated GO terms."
  (match &neo4j_out_v3
    (,
      (neo $Prot1:protein protein_name (stringEqual "1433S")) ;; single-var => best index usage
      (neo $Prot1:protein interacts_with $Prot2:protein)       ;; references two vars ($Prot1, $Prot2)
      (neo $GO:go go_gene_product $Prot1:protein)              ;; references two vars ($GO, $Prot1)
    )
    (result $GO $Prot1 $Prot2)))
  (neo4j-info (execution_time 420) (result_count 62248) (node_count 2040) (edge_count 4047)))

(compare-query "Query8 (With Properties) - Interactions Involving 1433S Protein"
  "Find proteins interacting with 1433S and their GO terms, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Prot1:protein protein_name (stringEqual "1433S") $P1) ;; single-var => best index usage
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P3)       ;; references two vars
      (neo_P $GO:go go_gene_product $Prot1:protein $P2)              ;; references two vars
    )
    (result $GO $Prot1 $Prot2 $P1 $P2 $P3)))
  (neo4j-info (execution_time 420) (result_count 62248) (node_count 2040) (edge_count 4047))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query9 - IGF1 Expression in Tissues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query9 - IGF1 Expression in Tissues"
  "Find IGF1 expression in tissues and related transcripts."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))   ;; single-var => best index usage
      (neo $Gene:gene expressed_in $Uberon:uberon)     ;; references two vars ($Gene, $Uberon)
      (neo $Gene:gene transcribed_to $Tx:transcript)   ;; references two vars ($Gene, $Tx)
    )
    (result $Gene $Uberon $Tx)))
  (neo4j-info (execution_time 6) (result_count 1827) (node_count 269) (edge_count 268)))

(compare-query "Query9 (With Properties) - IGF1 Expression in Tissues"
  "Find IGF1 expression in tissues and related transcripts, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name (startsWith "IGF1") $P1)  ;; single-var => best index usage
      (neo_P $Gene:gene expressed_in $Uberon:uberon $P2)     ;; references two vars
      (neo_P $Gene:gene transcribed_to $Tx:transcript $P3)   ;; references two vars
    )
    (result $Gene $Uberon $Tx $P1 $P2 $P3)))
  (neo4j-info (execution_time 6) (result_count 1827) (node_count 269) (edge_count 268))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query10 - Transcripts & Exons on Chromosome 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query10 - Transcripts & Exons on Chromosome 1"
  "Find transcripts, exons (on chr1), and interacting proteins."
  (match &neo4j_out_v3
    (,
      (neo $Exon:exon chr chr1)                ;; single-var => best index usage
      (neo $Tx:transcript includes $Exon:exon)  ;; references two vars ($Tx, $Exon)
      (neo $Tx:transcript translates_to $P1:protein) ;; references two vars ($Tx, $P1)
      (neo $P2:protein interacts_with $P1:protein)   ;; references two vars ($P2, $P1)
    )
    (result $Tx $Exon $P1 $P2)))
  (neo4j-info (execution_time "stopped_after(3000)")
              (result_count 40172978)
              (node_count 51599)
              (edge_count 1427846)))

(compare-query "Query10 (With Properties) - Transcripts & Exons on Chromosome 1"
  "Find transcripts, exons (on chr1), and interacting proteins, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Exon:exon chr chr1 $P2)                 ;; single-var => best index usage
      (neo_P $Tx:transcript includes $Exon:exon $P1)   ;; references two vars
      (neo_P $Tx:transcript translates_to $P3:protein $P3Props) ;; references two vars
      (neo_P $P4:protein interacts_with $P3:protein $P4Props)   ;; references two vars
    )
    (result $Tx $Exon $P3 $P4 $P1 $P2 $P3Props $P4Props)))
  (neo4j-info (execution_time "stopped_after(3000)")
              (result_count 40172978)
              (node_count 51599)
              (edge_count 1427846))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query11 - IGF1 Gene Expression in Cell Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query11 - IGF1 Gene Expression in Cell Lines"
  "Find IGF1 gene expression in cell lines and related subclass relationships."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")          ;; single-var => best index usage
      (neo $Gene:gene expressed_in $CL1:cl)      ;; 5 REFS
      (neo $CL2:cl subclass_of $CL1:cl)          ;; 3 REFS
    )
    (result $Gene $CL1 $CL2)))
  (neo4j-info (execution_time 1)
              (result_count 42) (node_count 52) (edge_count 51)))

(compare-query "Query11 (With Properties) - IGF1 Gene Expression in Cell Lines"
  "Find IGF1 gene expression in cell lines, plus subclass relationships, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $P1)     ;; single-var => best index usage
      (neo_P $Gene:gene expressed_in $CL1:cl $P2) ;; references two vars
      (neo_P $CL2:cl subclass_of $CL1:cl $P3)     ;; references two vars
    )
    (result $Gene $CL1 $CL2 $P1 $P2 $P3)))
  (neo4j-info (execution_time 1)
              (result_count 42) (node_count 52) (edge_count 51))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query12 - IGF1 Gene Regulation by SNP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query12 - IGF1 Gene Regulation by SNP"
  "Find regulation of the IGF1 gene by SNP activity."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))     ;; single-var => best index usage
      (neo $Snp:snp activity_by_contact $Gene:gene)      ;; references two vars ($Snp, $Gene)
    )
    (result $Snp $Gene)))
  (neo4j-info (execution_time 3)
              (result_count 445) (node_count 446) (edge_count 445)))

(compare-query "Query12 (With Properties) - IGF1 Gene Regulation by SNP"
  "Find regulation of the IGF1 gene by SNP activity, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name (startsWith "IGF1") $P1)  ;; single-var => best index usage
      (neo_P $Snp:snp activity_by_contact $Gene:gene $P2)    ;; references two vars
    )
    (result $Snp $Gene $P1 $P2)))
  (neo4j-info (execution_time 3)
              (result_count 445) (node_count 446) (edge_count 445))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query13 - IGF1 Gene Interactions and Regulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query13 - IGF1 Gene Interactions and Regulations"
  "Find IGF1 gene interactions, regulations, transcripts, proteins, etc."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))  ;; single-var => best index usage
      (neo $GeneReg:gene regulates $Gene:gene)        ;; sum usage=5 ($Gene(3) + $GeneReg(2))
      (neo $Gene:gene expressed_in $CL1:cl)           ;; sum usage=5 ($Gene(3) + $CL1(2))
      (neo $GeneReg:gene transcribed_to $Tx:transcript) ;; sum usage=4 ($GeneReg(2) + $Tx(2))
      (neo $Tx:transcript translates_to $Prot1:protein)  ;; sum usage=4 ($Tx(2) + $Prot1(2))
      (neo $Prot2:protein interacts_with $Prot1:protein)  ;; sum usage=3 ($Prot2(1)+$Prot1(2))
      (neo $CL2:cl subclass_of $CL1:cl)                  ;; sum usage=3 ($CL2(1)+$CL1(2))
    )
    (result $Gene $CL1 $CL2 $GeneReg $Tx $Prot1 $Prot2)))
  (neo4j-info (execution_time "unknown")
              (result_count 58854894)
              (node_count 18900)
              (edge_count 377127)))

(compare-query "Query13 (With Properties) - IGF1 Gene Interactions and Regulations"
  "Find IGF1 gene interactions, regulations, transcripts, proteins, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name (startsWith "IGF1") $P1)    ;; single-var => best index usage
      (neo_P $GeneReg:gene regulates $Gene:gene $P4)          ;; sum usage=5
      (neo_P $Gene:gene expressed_in $CL1:cl $P2)             ;; sum usage=5
      (neo_P $GeneReg:gene transcribed_to $Tx:transcript $P5) ;; sum usage=4
      (neo_P $Tx:transcript translates_to $Prot1:protein $P6) ;; sum usage=4
      (neo_P $Prot2:protein interacts_with $Prot1:protein $P7) ;; sum usage=3
      (neo_P $CL2:cl subclass_of $CL1:cl $P3)                 ;; sum usage=3
    )
    (result $Gene $CL1 $CL2 $GeneReg $Tx $Prot1 $Prot2
            $P1 $P2 $P3 $P4 $P5 $P6 $P7)))
  (neo4j-info (execution_time "unknown")
              (result_count 58854894)
              (node_count 18900)
              (edge_count 377127))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query14 - Pathway Associations for SNAP25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "Query14 - Pathway Associations for SNAP25"
  "Locate SNAP25 in pathways with other genes."
  (match &neo4j_out_v3
    (,
      (neo $Gene1:gene gene_name "SNAP25")             ;; single-var => best index usage
      (neo $Gene1:gene genes_pathways $Pathway:pathway) ;; references two vars ($Gene1, $Pathway)
      (neo $Gene2:gene genes_pathways $Pathway:pathway) ;; references two vars ($Gene2, $Pathway)
    )
    (result $Gene1 $Pathway $Gene2)))
  (neo4j-info (execution_time 5) (result_count 11967) (node_count 5665) (edge_count 11996)))

(compare-query "Query14 (With Properties) - Pathway Associations for SNAP25"
  "Locate SNAP25 in pathways with other genes, returning property lists."
  (match &neo4j_out_v3
    (,
      (neo_P $Gene1:gene gene_name "SNAP25" $P1)         ;; single-var => best index usage
      (neo_P $Gene1:gene genes_pathways $Pathway:pathway $P2) ;; references two vars
      (neo_P $Gene2:gene genes_pathways $Pathway:pathway $P3) ;; references two vars
    )
    (result $Gene1 $Pathway $Gene2 $P1 $P2 $P3)))
  (neo4j-info (execution_time 5) (result_count 11967) (node_count 5665) (edge_count 11996)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2a - Transcript-Exon Relationship
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s2a - Transcript-Exon Relationship"
  "Find transcripts (by transcript_id) and their included exons."
  (match &neo4j_out_v3
	(,
	  (neo $T:transcript transcript_id "ENST00000472835.1") ;; single-var => best index usage
	  (neo $T:transcript includes $E:exon)                  ;; references two variables ($T, $E)
	)
	(result $T $E)))
  (neo4j-info (execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2a (With Properties) - Transcript-Exon Relationship
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s2a (With Properties) - Transcript-Exon Relationship"
  "Find transcripts (by transcript_id) and their included exons, returning property lists."
  (match &neo4j_out_v3
	(,
	  (neo_P $T:transcript transcript_id "ENST00000472835.1" $PropList1) ;; single-var => best index usage
	  (neo_P $T:transcript includes $E:exon $PropList2)                  ;; references two variables ($T, $E)
	)
	(result $T $E $PropList1 $PropList2)))
  (neo4j-info (execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2b - Transcript & Exon Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s2b - Transcript & Exon Names"
  "Find transcripts (by transcript_id) with transcript_name and exons with exon_id."
  (match &neo4j_out_v3
	(,
	  (neo $T:transcript transcript_id "ENST00000472835.1")             ;; single-var => best index usage
	  (neo $E:exon exon_id $ExonID)                              ;; single-var => best index usage
	  (neo $T:transcript includes $E:exon)                              ;; references two variables ($T, $E)
	  (neo $T:transcript transcript_name $TranscriptName) ;; references one var ($T) + property
	)
	(result $TranscriptName $ExonID)))
  (neo4j-info (execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2b (With Properties) - Transcript & Exon Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s2b (With Properties) - Transcript & Exon Names"
  "Find transcripts (by transcript_id) with transcript_name and exons with exon_id, returning property lists."
  (match &neo4j_out_v3
	(,
	  (neo_P $Gene1:transcript transcript_id "ENST00000472835.1" $PropList2) ;; single-var => best index usage
	  (neo_P $Exon1:exon exon_id $ExonID $PropList6)                   ;; single-var => best index usage
	  (neo_P $Gene1:transcript includes $Exon1:exon $PropList4)           ;; references two variables ($Gene1, $Exon1)
	  (neo_P $Gene1:transcript transcript_name $TranscriptName $PropList5) ;; references $Gene1 + property
	)
	(result $TranscriptName $ExonID
			$Gene1 $PropList2 $Exon1 $PropList4 $PropList5 $PropList6)))
  (neo4j-info (execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502a - Regulators of FTO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s502a - Regulators of FTO"
  "Find all genes that regulate the FTO gene (1-hop)."
  (match &neo4j_out_v3
	(,
	  (neo $GeneFTO:gene gene_name "FTO")          ;; single-var => best index usage
	  (neo $GeneReg:gene regulates $GeneFTO:gene)  ;; references two variables ($GeneReg, $GeneFTO)
	)
	(result $GeneFTO $GeneReg)))
  (neo4j-info (execution_time 0.018) (result_count 502) (node_count 503) (edge_count 503)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502a (With Properties) - Regulators of FTO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s502a (With Properties) - Regulators of FTO"
  "Find all genes that regulate the FTO gene (1-hop), returning property lists."
  (match &neo4j_out_v3
	(,
	  (neo_P $GeneFTO:gene gene_name "FTO" $PropList1)                ;; single-var => best index usage
	  (neo_P $GeneReg:gene regulates $GeneFTO:gene $PropList2)         ;; references two variables ($GeneReg, $GeneFTO)
	)
	(result $GeneFTO $GeneReg $PropList1 $PropList2)))
  (neo4j-info (execution_time 0.018) (result_count 502) (node_count 503) (edge_count 503)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502b - Regulators of FTO + gene_name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s502b - Regulators of FTO + gene_name"
  "Find all genes that regulate FTO, returning regulator and target gene_names."
  (match &neo4j_out_v3
	(,
	  (neo $FTO:gene gene_name "FTO")                 ;; single-var => best index usage
	  (neo $Reg:gene gene_name $Regulator:regulator)  ;; single-var => best index usage
	  (neo $Reg:gene regulates $FTO:gene)             ;; references two vars ($Reg, $FTO)
	  (neo $FTO:gene gene_name $Target:target)        ;; references one var ($FTO) + property
	)
	(result $Regulator $Target)))
  (neo4j-info (execution_time 0.028) (result_count 502) (node_count 503) (edge_count 503)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502b (With Properties) - Regulators of FTO + gene_name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compare-query "s502b (With Properties) - Regulators of FTO + gene_name"
  "Find all genes that regulate FTO, returning regulator and target gene_names, plus property lists."
  (match &neo4j_out_v3
	(,
	  (neo_P $PropList1:gene gene_name "FTO" $PropList2)                   ;; single-var => best index usage
	  (neo_P $PropList3:gene gene_name $Regulator:regulator $PropList6)    ;; single-var => best index usage
	  (neo_P $PropList3:gene regulates $PropList1:gene $PropList4)         ;; references two vars ($PropList3, $PropList1)
	  (neo_P $PropList1:gene gene_name $Target:target $PropList5)          ;; references $PropList1 + property
	)
	(result $Regulator $Target
			$PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6)))
  (neo4j-info (execution_time 0.028) (result_count 502) (node_count 503) (edge_count 503)))


