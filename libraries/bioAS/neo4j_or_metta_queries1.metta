
(arg1Isa isa-test-query PropertyListAtom)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query1 - BRCA2 Gene Interactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query1 - BRCA2 Gene Interactions"
   :query-number "1"
   :description "Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name (startsWith "BRCA2")) ;; single-var => best index usage
       (neo $Gene:gene transcribed_to $Tx:transcript)  ;; sum usage=5
       (neo $Gene:gene genes_pathways $Pathway:pathway) ;; sum usage=4
       (neo $Tx:transcript translates_to $Prot:protein) ;; sum usage=4
       (neo $Prot:protein interacts_with $Prot2:protein) ;; sum usage=3
     )
     (result $Gene $Tx $Prot $Prot2 $Pathway))

   :neo4j-info (( execution_time "stopped_after(4980)")
               (result_count 151956) (node_count 1908) (edge_count 1882))))


(isa-test-query
  (:name "Query1 (With Properties) - BRCA2 Gene Interactions"
   :query-number "1-with-properties"
   :description "Find all interactions involving the BRCA2 gene, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name (startsWith "BRCA2") $PropList1)
       (neo_P $Gene:gene transcribed_to $Tx:transcript $PropList2)
       (neo_P $Gene:gene genes_pathways $Pathway:pathway $PropList5)
       (neo_P $Tx:transcript translates_to $Prot:protein $PropList3)
       (neo_P $Prot:protein interacts_with $Prot2:protein $PropList4)
     )
     (result $Gene $Tx $Prot $Prot2 $Pathway
             $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))

   :neo4j-info (( execution_time "stopped_after(4980)")
               (result_count 151956) (node_count 1908) (edge_count 1882))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query2 - IGF2 Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query2 - IGF2 Components"
   :query-number "2"
   :description "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name (startsWith "IGF2"))            ;; single-var => best index usage
       (neo $Gene:gene genes_pathways $Pway:pathway)             ;; sum usage=6
       (neo $Prom:promoter associated_with $Gene:gene)           ;; sum usage=5
       (neo $Enh:enhancer associated_with $Gene:gene)            ;; sum usage=5
       (neo $Child:pathway child_pathway_of $Pway:pathway)       ;; sum usage=3
     )
     (result $Prom $Gene $Enh $Pway $Child))

   :neo4j-info (( execution_time "about(180)")
               (result_count 41536) (node_count 144) (edge_count 143))))


(isa-test-query
  (:name "Query2 (With Properties) - IGF2 Components"
   :query-number "2-with-properties"
   :description "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name (startsWith "IGF2") $PropList1)
       (neo_P $Gene:gene genes_pathways $Pway:pathway $PropList4)
       (neo_P $Prom:promoter associated_with $Gene:gene $PropList2)
       (neo_P $Enh:enhancer associated_with $Gene:gene $PropList3)
       (neo_P $Child:pathway child_pathway_of $Pway:pathway $PropList5)
     )
     (result $Prom $Gene $Enh $Pway $Child
             $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))

   :neo4j-info (( execution_time "about(180)")
               (result_count 41536) (node_count 144) (edge_count 143))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query3 - Gene Interactions and GO Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query3 - Gene Interactions and GO Terms"
   :query-number "3"
   :description "Find gene interactions and associated GO terms including proteins and transcripts."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Prot1:protein translation_of $Tx:transcript)  ;; sum usage=6
       (neo $Gene:gene transcribed_to $Tx:transcript)      ;; sum usage=4
       (neo $Tx:transcript includes $Exon:exon)            ;; sum usage=4
       (neo $Prot1:protein interacts_with $Prot2:protein)  ;; sum usage=4
       (neo $GO:go go_gene_product $Prot1:protein)         ;; sum usage=4
     )
     (result $Gene $Tx $Exon $Prot1 $Prot2 $GO))

   :neo4j-info (( execution_time "stopped_after(1200)")
               (result_count "too_many_results")
               (node_count "unknown")
               (edge_count "unknown"))))


(isa-test-query
  (:name "Query3 (With Properties) - Gene Interactions and GO Terms"
   :query-number "3-with-properties"
   :description "Find gene interactions and associated GO terms (proteins/transcripts), returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Prot1:protein translation_of $Tx:transcript $P3)
       (neo_P $Gene:gene transcribed_to $Tx:transcript $P1)
       (neo_P $Tx:transcript includes $Exon:exon $P2)
       (neo_P $Prot1:protein interacts_with $Prot2:protein $P4)
       (neo_P $GO:go go_gene_product $Prot1:protein $P5)
     )
     (result $Gene $Tx $Exon $Prot1 $Prot2 $GO
             $P1 $P2 $P3 $P4 $P5))

   :neo4j-info (( execution_time "stopped_after(1200)")
               (result_count "too_many_results")
               (node_count "unknown")
               (edge_count "unknown"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query4 - Interactions Involving 1433B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query4 - Interactions Involving 1433B"
   :query-number "4"
   :description "Find interactions involving 1433B protein, including transcripts, exons, and GO terms."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Prot1:protein protein_name (startsWith "1433B")) ;; single-var => best index usage
       (neo $Prot1:protein translation_of $Tx:transcript)      ;; sum usage=7
       (neo $Prot1:protein interacts_with $Prot2:protein)      ;; sum usage=5
       (neo $GO:go go_gene_product $Prot1:protein)             ;; sum usage=5
       (neo $Gene:gene transcribed_to $Tx:transcript)          ;; sum usage=4
       (neo $Tx:transcript includes $Exon:exon)                ;; sum usage=4
     )
     (result $Gene $Tx $Exon $Prot1 $Prot2 $GO))

   :neo4j-info ((execution_time "stopped_after(2400)")
                (result_count 794742) (node_count 2693) (edge_count 2698))))


(isa-test-query
  (:name "Query4 - @xabush order Interactions Involving 1433B"
   :query-number "4x"
   :description "@xabush order Find interactions involving 1433B protein, including transcripts, exons, and GO terms."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Protein1:protein protein_name (startsWith "1433B"))
       (neo $Protein1:protein translation_of $Transcript:transcript)
       (neo $Gene:gene transcribed_to $Transcript:transcript)
       (neo $Transcript:transcript includes $Exon:exon)
       (neo $Protein1:protein interacts_with $Protein2:protein)
       (neo $GO1Term:go go_gene_product $Protein1:protein))
     (result $Gene $Tx $Exon $Prot1 $Prot2 $GO))
   
   :neo4j-info ((execution_time "stopped_after(2400)")
                (result_count 794742) (node_count 2693) (edge_count 2698))))


(isa-test-query
  (:name "Query4 (With Properties) - Interactions Involving 1433B"
   :query-number "4-with-properties"
   :description "Find interactions involving 1433B protein, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Prot1:protein protein_name (startsWith "1433B") $P1)
       (neo_P $Prot1:protein translation_of $Tx:transcript $P4)
       (neo_P $Prot1:protein interacts_with $Prot2:protein $P5)
       (neo_P $GO:go go_gene_product $Prot1:protein $P6)
       (neo_P $Gene:gene transcribed_to $Tx:transcript $P2)
       (neo_P $Tx:transcript includes $Exon:exon $P3)
     )
     (result $Gene $Tx $Exon $Prot1 $Prot2 $GO
             $P1 $P2 $P3 $P4 $P5 $P6))

   :neo4j-info (( execution_time "stopped_after(2400)")
               (result_count 794742) (node_count 2693) (edge_count 2698))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query5 - Components Associated with IGF1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query5 - Components Associated with IGF1"
   :query-number "5"
   :description "Find enhancers, pathways, and transcripts associated with the IGF1 gene."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name "IGF1")                      ;; single-var => best index usage
       (neo $Tx:transcript transcribed_from $Gene:gene)       ;; sum usage=6
       (neo $Enh:enhancer associated_with $Gene:gene)         ;; sum usage=5
       (neo $Gene:gene genes_pathways $Pathway:pathway)       ;; sum usage=5
       (neo $Tx:transcript translates_to $Prot:protein)       ;; sum usage=3
     )
     (result $Gene $Pathway $Enh $Tx $Prot))

   :neo4j-info (( execution_time 25) (result_count 1932) (node_count 45) (edge_count 49))))


(isa-test-query
  (:name "Query5 (With Properties) - Components Associated with IGF1"
   :query-number "5-with-properties"
   :description "Find enhancers, pathways, and transcripts associated with IGF1, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name "IGF1" $P1)
       (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)
       (neo_P $Enh:enhancer associated_with $Gene:gene $P3)
       (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)
       (neo_P $Tx:transcript translates_to $Prot:protein $P5)
     )
     (result $Gene $Pathway $Enh $Tx $Prot
             $P1 $P2 $P3 $P4 $P5))

   :neo4j-info (( execution_time 25) (result_count 1932) (node_count 45) (edge_count 49))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query6 - Pathways and Protein Interactions for IGF1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query6 - Pathways and Protein Interactions for IGF1"
   :query-number "6"
   :description "Find pathways and interacting proteins for the IGF1 gene including all associated components."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name "IGF1")                    ;; single-var => best index usage
       (neo $Tx:transcript transcribed_from $Gene:gene)     ;; sum usage=6
       (neo $Enh:enhancer associated_with $Gene:gene)       ;; sum usage=5
       (neo $Gene:gene genes_pathways $Pathway:pathway)     ;; sum usage=5
       (neo $Tx:transcript translates_to $Prot1:protein)    ;; sum usage=4
       (neo $Prot1:protein interacts_with $Prot2:protein)   ;; sum usage=3
     )
     (result $Gene $Pathway $Enh $Tx $Prot1 $Prot2))

   :neo4j-info (( execution_time "squiggly_mark")
               (result_count 5790204)
               (node_count 3042)
               (edge_count 3046))))


(isa-test-query
  (:name "Query6 (With Properties) - Pathways and Protein Interactions for IGF1"
   :query-number "6-with-properties"
   :description "Find pathways and interacting proteins for the IGF1 gene, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name "IGF1" $P1)
       (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)
       (neo_P $Enh:enhancer associated_with $Gene:gene $P3)
       (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)
       (neo_P $Tx:transcript translates_to $Prot1:protein $P5)
       (neo_P $Prot1:protein interacts_with $Prot2:protein $P6)
     )
     (result $Gene $Pathway $Enh $Tx $Prot1 $Prot2
             $P1 $P2 $P3 $P4 $P5 $P6))

   :neo4j-info (( execution_time "squiggly_mark")
               (result_count 5790204)
               (node_count 3042)
               (edge_count 3046))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query7 - Transcripts and Exons for TP73-AS1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query7 - Transcripts and Exons for TP73-AS1"
   :query-number "7"
   :description "Find transcripts and exons associated with the TP73-AS1 gene."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name (endsWith "TP73-AS1"))  ;; single-var => best index usage
       (neo $Tx:transcript transcribed_from $Gene:gene)  ;; references $Gene + $Tx
       (neo $Tx:transcript includes $Exon:exon)          ;; references $Tx + $Exon
     )
     (result $Tx $Exon $Gene))

   :neo4j-info (( execution_time 8) (result_count 142) (node_count 106) (edge_count 284))))


(isa-test-query
  (:name "Query7 (With Properties) - Transcripts and Exons for TP73-AS1"
   :query-number "7-with-properties"
   :description "Find transcripts and exons associated with the TP73-AS1 gene, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name (endsWith "TP73-AS1") $P3)
       (neo_P $Tx:transcript transcribed_from $Gene:gene $P2)
       (neo_P $Tx:transcript includes $Exon:exon $P1)
     )
     (result $Tx $Exon $Gene $P1 $P2 $P3))

   :neo4j-info (( execution_time 8) (result_count 142) (node_count 106) (edge_count 284))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query8 - Interactions Involving 1433S Protein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query8 - Interactions Involving 1433S Protein"
   :query-number "8"
   :description "Find proteins interacting with 1433S and associated GO terms."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Prot1:protein protein_name (stringEqual "1433S")) ;; single-var => best index usage
       (neo $Prot1:protein interacts_with $Prot2:protein)       ;; references two vars ($Prot1, $Prot2)
       (neo $GO:go go_gene_product $Prot1:protein)              ;; references two vars ($GO, $Prot1)
     )
     (result $GO $Prot1 $Prot2))

   :neo4j-info (( execution_time 420) (result_count 62248) (node_count 2040) (edge_count 4047))))


(isa-test-query
  (:name "Query8 (With Properties) - Interactions Involving 1433S Protein"
   :query-number "8-with-properties"
   :description "Find proteins interacting with 1433S and their GO terms, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Prot1:protein protein_name (stringEqual "1433S") $P1)
       (neo_P $Prot1:protein interacts_with $Prot2:protein $P3)
       (neo_P $GO:go go_gene_product $Prot1:protein $P2)
     )
     (result $GO $Prot1 $Prot2 $P1 $P2 $P3))

   :neo4j-info (( execution_time 420) (result_count 62248) (node_count 2040) (edge_count 4047))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query9 - IGF1 Expression in Tissues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query9 - IGF1 Expression in Tissues"
   :query-number "9"
   :description "Find IGF1 expression in tissues and related transcripts."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name (startsWith "IGF1"))   ;; single-var => best index usage
       (neo $Gene:gene expressed_in $Uberon:uberon)     ;; references two vars
       (neo $Gene:gene transcribed_to $Tx:transcript)   ;; references two vars
     )
     (result $Gene $Uberon $Tx))

   :neo4j-info (( execution_time 6) (result_count 1827) (node_count 269) (edge_count 268))))


(isa-test-query
  (:name "Query9 (With Properties) - IGF1 Expression in Tissues"
   :query-number "9-with-properties"
   :description "Find IGF1 expression in tissues and related transcripts, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name (startsWith "IGF1") $P1)
       (neo_P $Gene:gene expressed_in $Uberon:uberon $P2)
       (neo_P $Gene:gene transcribed_to $Tx:transcript $P3)
     )
     (result $Gene $Uberon $Tx $P1 $P2 $P3))

   :neo4j-info (( execution_time 6) (result_count 1827) (node_count 269) (edge_count 268))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query10 - Transcripts & Exons on Chromosome 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query10 - Transcripts & Exons on Chromosome 1"
   :query-number "10"
   :description "Find transcripts, exons (on chr1), and interacting proteins."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Exon:exon chr chr1)                ;; single-var => best index usage
       (neo $Tx:transcript includes $Exon:exon)  ;; references two vars ($Tx, $Exon)
       (neo $Tx:transcript translates_to $P1:protein) ;; references two vars ($Tx, $P1)
       (neo $P2:protein interacts_with $P1:protein)   ;; references two vars ($P2, $P1)
     )
     (result $Tx $Exon $P1 $P2))

   :neo4j-info (( execution_time "stopped_after(3000)")
               (result_count 40172978)
               (node_count 51599)
               (edge_count 1427846))))


(isa-test-query
  (:name "Query10 (With Properties) - Transcripts & Exons on Chromosome 1"
   :query-number "10-with-properties"
   :description "Find transcripts, exons (on chr1), and interacting proteins, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Exon:exon chr chr1 $P2)
       (neo_P $Tx:transcript includes $Exon:exon $P1)
       (neo_P $Tx:transcript translates_to $P3:protein $P3Props)
       (neo_P $P4:protein interacts_with $P3:protein $P4Props)
     )
     (result $Tx $Exon $P3 $P4 $P1 $P2 $P3Props $P4Props))

   :neo4j-info (( execution_time "stopped_after(3000)")
               (result_count 40172978)
               (node_count 51599)
               (edge_count 1427846))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query11 - IGF1 Gene Expression in Cell Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query11 - IGF1 Gene Expression in Cell Lines"
   :query-number "11"
   :description "Find IGF1 gene expression in cell lines and related subclass relationships."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name "IGF1")          ;; single-var => best index usage
       (neo $Gene:gene expressed_in $CL1:cl)      ;; references two vars
       (neo $CL2:cl subclass_of $CL1:cl)          ;; references two vars
     )
     (result $Gene $CL1 $CL2))

   :neo4j-info (( execution_time 1)
               (result_count 42) (node_count 52) (edge_count 51))))


(isa-test-query
  (:name "Query11 (With Properties) - IGF1 Gene Expression in Cell Lines"
   :query-number "11-with-properties"
   :description "Find IGF1 gene expression in cell lines, plus subclass relationships, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name "IGF1" $P1)
       (neo_P $Gene:gene expressed_in $CL1:cl $P2)
       (neo_P $CL2:cl subclass_of $CL1:cl $P3)
     )
     (result $Gene $CL1 $CL2 $P1 $P2 $P3))

   :neo4j-info (( execution_time 1)
               (result_count 42) (node_count 52) (edge_count 51))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query12 - IGF1 Gene Regulation by SNP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query12 - IGF1 Gene Regulation by SNP"
   :query-number "12"
   :description "Find regulation of the IGF1 gene by SNP activity."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name (startsWith "IGF1"))     ;; single-var => best index usage
       (neo $Snp:snp activity_by_contact $Gene:gene)      ;; references two vars
     )
     (result $Snp $Gene))

   :neo4j-info (( execution_time 3)
               (result_count 445) (node_count 446) (edge_count 445))))


(isa-test-query
  (:name "Query12 (With Properties) - IGF1 Gene Regulation by SNP"
   :query-number "12-with-properties"
   :description "Find regulation of the IGF1 gene by SNP activity, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name (startsWith "IGF1") $P1)
       (neo_P $Snp:snp activity_by_contact $Gene:gene $P2)
     )
     (result $Snp $Gene $P1 $P2))

   :neo4j-info (( execution_time 3)
               (result_count 445) (node_count 446) (edge_count 445))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query13 - IGF1 Gene Interactions and Regulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query13 - IGF1 Gene Interactions and Regulations"
   :query-number "13"
   :description "Find IGF1 gene interactions, regulations, transcripts, proteins, etc."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene:gene gene_name (startsWith "IGF1"))  ;; single-var => best index usage
       (neo $GeneReg:gene regulates $Gene:gene)        ;; sum usage=5
       (neo $Gene:gene expressed_in $CL1:cl)           ;; sum usage=5
       (neo $GeneReg:gene transcribed_to $Tx:transcript) ;; sum usage=4
       (neo $Tx:transcript translates_to $Prot1:protein)  ;; sum usage=4
       (neo $Prot2:protein interacts_with $Prot1:protein)  ;; sum usage=3
       (neo $CL2:cl subclass_of $CL1:cl)                  ;; sum usage=3
     )
     (result $Gene $CL1 $CL2 $GeneReg $Tx $Prot1 $Prot2))

   :neo4j-info (( execution_time "unknown")
               (result_count 58854894)
               (node_count 18900)
               (edge_count 377127))))


(isa-test-query
  (:name "Query13 (With Properties) - IGF1 Gene Interactions and Regulations"
   :query-number "13-with-properties"
   :description "Find IGF1 gene interactions, regulations, transcripts, proteins, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene:gene gene_name (startsWith "IGF1") $P1)
       (neo_P $GeneReg:gene regulates $Gene:gene $P4)
       (neo_P $Gene:gene expressed_in $CL1:cl $P2)
       (neo_P $GeneReg:gene transcribed_to $Tx:transcript $P5)
       (neo_P $Tx:transcript translates_to $Prot1:protein $P6)
       (neo_P $Prot2:protein interacts_with $Prot1:protein $P7)
       (neo_P $CL2:cl subclass_of $CL1:cl $P3)
     )
     (result $Gene $CL1 $CL2 $GeneReg $Tx $Prot1 $Prot2
             $P1 $P2 $P3 $P4 $P5 $P6 $P7))

   :neo4j-info (( execution_time "unknown")
               (result_count 58854894)
               (node_count 18900)
               (edge_count 377127))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query14 - Pathway Associations for SNAP25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "Query14 - Pathway Associations for SNAP25"
   :query-number "14"
   :description "Locate SNAP25 in pathways with other genes."

   :metta
   (match &neo4j_out_v3
     (,
       (neo $Gene1:gene gene_name "SNAP25")             ;; single-var => best index usage
       (neo $Gene1:gene genes_pathways $Pathway:pathway) ;; references two vars
       (neo $Gene2:gene genes_pathways $Pathway:pathway) ;; references two vars
     )
     (result $Gene1 $Pathway $Gene2))

   :neo4j-info ((execution_time 5) (result_count 11967) (node_count 5665) (edge_count 11996))))



(isa-test-query
  (:name "Query14 (With Properties) - Pathway Associations for SNAP25"
   :query-number "14-with-properties"
   :description "Locate SNAP25 in pathways with other genes, returning property lists."

   :metta
   (match &neo4j_out_v3
     (,
       (neo_P $Gene1:gene gene_name "SNAP25" $P1)
       (neo_P $Gene1:gene genes_pathways $Pathway:pathway $P2)
       (neo_P $Gene2:gene genes_pathways $Pathway:pathway $P3)
     )
     (result $Gene1 $Pathway $Gene2 $P1 $P2 $P3))

   :neo4j-info ((execution_time 5) (result_count 11967) (node_count 5665) (edge_count 11996))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 21) Counts of genes and their number of transcripts, proteins, and GO terms
;;;    in decreasing order (62700 records)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query 
  (:name "Counts of Genes/Transcripts/Proteins/GO in Desc Order"
   :query-number "21"
   :description "Counts of genes and their number of transcripts, proteins, and GO terms in decreasing order."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript translates_to $P:protein)
	   (neo $P:protein go_gene_product $GO:go)
	 )
	 (aggregate $G
	   (count $T numTranscripts)
	   (count $P numProteins)
	   (count $GO numGOterms)
	   (sum (+ numTranscripts numProteins numGOterms) total))
	 (result $G $numTranscripts $numProteins $numGOterms $total))

   :cypher
   "MATCH (g:gene)-[:transcribed_to]->(t:transcript)
	OPTIONAL MATCH (t)-[:translates_to]->(p:protein) <-[:go_gene_product]-(go:go)
	WITH g,
		 COUNT(DISTINCT t) AS numTranscripts,
		 COUNT(DISTINCT p) AS numProteins,
		 COUNT(DISTINCT go) AS numGOterms,
		 (COUNT(DISTINCT t) + COUNT(DISTINCT p) + COUNT(DISTINCT go)) AS total
	RETURN g.gene_name AS gene, numTranscripts, numProteins, numGOterms, total
	ORDER BY total DESC"

   :neo4j-info ((result_count 62700))))


(isa-test-query 
  (:name "LIMIT 1 Counts of Genes/Transcripts/Proteins/GO in Desc Order"
   :query-number "21-LIMIT-1"
   :description "Counts of genes and their number of transcripts, proteins, and GO terms in decreasing order."

   :metta
   (limit 1 (sort
			   
	 (match &neo4j_out_v3
	 (,
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript translates_to $P:protein)
	   (neo $P:protein go_gene_product $GO:go)
	 )
	 (aggregate $G
	   (count $T numTranscripts)
	   (count $P numProteins)
	   (count $GO numGOterms)
	   (sum (+ numTranscripts numProteins numGOterms) total))
	 (result $total $G $numTranscripts $numProteins $numGOterms))))

   :cypher
   "MATCH (g:gene)-[:transcribed_to]->(t:transcript)
	OPTIONAL MATCH (t)-[:translates_to]->(p:protein) <-[:go_gene_product]-(go:go)
	WITH g,
		 COUNT(DISTINCT t) AS numTranscripts,
		 COUNT(DISTINCT p) AS numProteins,
		 COUNT(DISTINCT go) AS numGOterms,
		 (COUNT(DISTINCT t) + COUNT(DISTINCT p) + COUNT(DISTINCT go)) AS total
	RETURN g.gene_name AS gene, numTranscripts, numProteins, numGOterms, total
	ORDER BY total DESC 
	LIMIT 1"

   :neo4j-info ((result_count 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22) All transcripts, proteins, and GO terms of the TTN (Titin) gene (225 records)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "TTN: All transcripts, proteins, and GO terms"
   :query-number "22"
   :description "Find all transcripts, proteins, and GO terms connected to the TTN gene."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $G:gene gene_name "TTN")
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript translates_to $P:protein)
	   (neo $P:protein go_gene_product $GO:go)
	 )
	 (result $G $T $P $GO))

   :neo4j-info ((result_count 225))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23) The above query but returning property names only (225 records)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "TTN: gene_name, transcript_name, protein_name, GO term_name"
   :query-number "23"
   :description "Same TTN query but returning property fields (225 records)."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $G:gene gene_name "TTN")
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript translates_to $P:protein)
	   (neo $P:protein go_gene_product $GO:go)
	   ;; Grab property names:
	   (neo $G:gene gene_name      $GeneName)
	   (neo $T:transcript transcript_name $TxName)
	   (neo $P:protein  protein_name      $ProtName)
	   (neo $GO:go      term_name         $GoName)
	 )
	 (result $GeneName $TxName $ProtName $GoName))

   :neo4j-info ((result_count 225))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24) Transcripts, proteins, and GO terms for ALL genes (62700 records)
;;;    (Similar pattern but no restriction on gene_name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "All genes: gene_name, transcript_name, protein_name, GO term_name"
   :query-number "24"
   :description "Find transcripts, proteins, and GO terms for all genes, returning property names."

   :metta
   (match &neo4j_out_v3
	 (,	   
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript translates_to $P:protein)
	   (neo $P:protein go_gene_product $GO:go)
	   ;; Grab property names:
	   (neo $G:gene gene_name      $GeneName)
	   (neo $T:transcript transcript_name $TxName)
	   (neo $P:protein  protein_name      $ProtName)
	   (neo $GO:go      term_name         $GoName)
	 )
	 (result $GeneName $TxName $ProtName $GoName))

   :neo4j-info ((result_count 62700))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25) Transcripts and exons of a given gene (TTN): 1700 records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "TTN: Transcripts and Exons"
   :query-number "25"
   :description "Find transcripts and exons for TTN, returning property fields (1700 records)."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $G:gene gene_name "TTN")
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript includes $E:exon)

	   ;; Grab property names:
	   (neo $T:transcript transcript_name $TxName)
	   (neo $G:gene gene_name $GeneName)
	   (neo $E:exon exon_id $ExonID)
	 )
	 (result $GeneName $TxName $ExonID))

   :neo4j-info ((result_count 1700))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 26) Transcripts and exons of ALL genes: 1,649,476 records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "All Genes: Transcripts and Exons"
   :query-number "26"
   :description "Find transcripts and exons for all genes, returning property fields (1,649,476 records)."

   :metta
   (match &neo4j_out_v3
	 (,	   
	   (neo $G:gene transcribed_to $T:transcript)
	   (neo $T:transcript includes $E:exon)

	   ;; Grab property names:
	   (neo $E:exon exon_id $ExonID)                
	   (neo $T:transcript transcript_name $TxName)
	   (neo $G:gene gene_name $GeneName)
	 )
	 (result $GeneName $TxName $ExonID))

   :neo4j-info ((result_count 1649476))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2a - Transcript-Exon Relationship
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s2a - Transcript-Exon Relationship"
   :query-number "s2a"
   :description "Find transcripts (by transcript_id) and their included exons."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $T:transcript transcript_id "ENST00000472835.1") ;; single-var => best index usage
	   (neo $T:transcript includes $E:exon)                  ;; references two variables ($T, $E)
	 )
	 (result $T $E))

   :neo4j-info (( execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2a (With Properties) - Transcript-Exon Relationship
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s2a (With Properties) - Transcript-Exon Relationship"
   :query-number "s2a-with-properties"
   :description "Find transcripts (by transcript_id) and their included exons, returning property lists."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo_P $T:transcript transcript_id "ENST00000472835.1" $PropList1) ;; single-var => best index usage
	   (neo_P $T:transcript includes $E:exon $PropList2)                  ;; references two variables ($T, $E)
	 )
	 (result $T $E $PropList1 $PropList2))

   :neo4j-info (( execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2b - Transcript & Exon Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s2b - Transcript & Exon Names"
   :query-number "s2b"
   :description "Find transcripts (by transcript_id) with transcript_name and exons with exon_id."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $T:transcript transcript_id "ENST00000472835.1")              ;; single-var => best index usage
	   (neo $E:exon exon_id $ExonID:exonid)                               ;; single-var => best index usage
	   (neo $T:transcript includes $E:exon)                               ;; references two variables ($T, $E)
	   (neo $T:transcript transcript_name $TranscriptName:transcriptname) ;; references one var ($T) + property
	 )
	 (result $TranscriptName:transcriptname $ExonID:exonid))

   :neo4j-info (( execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s2b (With Properties) - Transcript & Exon Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s2b (With Properties) - Transcript & Exon Names"
   :query-number "s2b-with-properties"
   :description "Find transcripts (by transcript_id) with transcript_name and exons with exon_id, returning property lists."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo_P $PropList1:transcript transcript_id "ENST00000472835.1" $PropList2) ;; single-var => best index usage
	   (neo_P $PropList3:exon exon_id $ExonID:exonid $PropList6)                   ;; single-var => best index usage
	   (neo_P $PropList1:transcript includes $PropList3:exon $PropList4)           ;; references two variables ($PropList1, $PropList3)
	   (neo_P $PropList1:transcript transcript_name $TranscriptName:transcriptname $PropList5) ;; references $PropList1 + property
	 )
	 (result $TranscriptName:transcriptname $ExonID:exonid
			 $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))

   :neo4j-info (( execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502a - Regulators of FTO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s502a - Regulators of FTO"
   :query-number "s502a"
   :description "Find all genes that regulate the FTO gene (1-hop)."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $GeneFTO:gene gene_name "FTO")          ;; single-var => best index usage
	   (neo $GeneReg:gene regulates $GeneFTO:gene)  ;; references two variables ($GeneReg, $GeneFTO)
	 )
	 (result $GeneFTO:gene $GeneReg:gene))

   :neo4j-info (( execution_time 0.018) (result_count 502) (node_count 503) (edge_count 503))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502a (With Properties) - Regulators of FTO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s502a (With Properties) - Regulators of FTO"
   :query-number "s502a-with-properties"
   :description "Find all genes that regulate the FTO gene (1-hop), returning property lists."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo_P $GeneFTO:gene gene_name "FTO" $PropList1)         ;; single-var => best index usage
	   (neo_P $GeneReg:gene regulates $GeneFTO:gene $PropList2)  ;; references two variables ($GeneReg, $GeneFTO)
	 )
	 (result $GeneFTO:gene $GeneReg:gene $PropList1 $PropList2))

   :neo4j-info (( execution_time 0.018) (result_count 502) (node_count 503) (edge_count 503))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502b - Regulators of FTO + gene_name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s502b - Regulators of FTO + gene_name"
   :query-number "s502b"
   :description "Find all genes that regulate FTO, returning regulator and target gene_names."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo $FTO:gene gene_name "FTO")                 ;; single-var => best index usage
	   (neo $Reg:gene gene_name $Regulator:regulator)  ;; single-var => best index usage
	   (neo $Reg:gene regulates $FTO:gene)             ;; references two vars ($Reg, $FTO)
	   (neo $FTO:gene gene_name $Target:target)        ;; references one var ($FTO) + property
	 )
	 (result $Regulator:regulator $Target:target))

   :neo4j-info (( execution_time 0.028) (result_count 502) (node_count 503) (edge_count 503))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s502b (With Properties) - Regulators of FTO + gene_name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa-test-query
  (:name "s502b (With Properties) - Regulators of FTO + gene_name"
   :query-number "s502b-with-properties"
   :description "Find all genes that regulate FTO, returning regulator and target gene_names, plus property lists."

   :metta
   (match &neo4j_out_v3
	 (,
	   (neo_P $PropList1:gene gene_name "FTO" $PropList2)                ;; single-var => best index usage
	   (neo_P $PropList3:gene gene_name $Regulator:regulator $PropList6) ;; single-var => best index usage
	   (neo_P $PropList3:gene regulates $PropList1:gene $PropList4)       ;; references two vars ($PropList3, $PropList1)
	   (neo_P $PropList1:gene gene_name $Target:target $PropList5)        ;; references $PropList1 + property
	 )
	 (result $Regulator:regulator $Target:target
			 $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))

   :neo4j-info (( execution_time 0.028) (result_count 502) (node_count 503) (edge_count 503))))


(isa-test-query
  (:name "Same Start-End Pair with Directed Relation"
   :query-number "custom-same-start-end"
   :description "Find pairs of nodes A and B that share the same 'start' and 'end' property values, where A ≠ B and A links to B via some relationship."
   :minimum-time 30
   :metta
   (match &neo4j_out_v3
     (,
       (each-different ($A $B))
       (neo $A start $S)
       (neo $B start $S)
       (neo $A end $E)
       (neo $B end $E)
       (neo_P $A $R $B $EdgeProps)
     )
     (result $A $B $S $E $R $EdgeProps))

   :prolog
   "neo(A, start, S),
    neo(B, start, S),
    A \\== B,
    neo(A, end, E),
    neo(B, end, E),
    neo(A, R, B)."

   :cypher
   "MATCH (a)-[r]->(b)
     WHERE a.start = b.start AND
           a.end = b.end AND
           id(a) <> id(b)
     RETURN DISTINCT
       a.id AS A_ID,
       b.id AS B_ID,
       a.start AS Start,
       a.end AS End,
       type(r) AS RelationType,
       properties(r) AS RelationProperties;"

   :neo4j-info ((execution_time "unknown")
                (result_count "dynamic")
                (node_count "unknown")
                (edge_count "unknown"))))

(isa-test-query
  (:name "5-edge chain: $R1–$R5"
   :description "Chain of 5 edges using $R1,$R2,$R3,$R4,$R5."
	:minimum-time 60
	:cypher
	 "MATCH (A)
	   WHERE A.R1 IS NOT NULL OR (A)-[:R1]->()
	   WITH A
	   MATCH (A)-[R1]->(B)
	   WHERE (type(R1) IS NOT NULL OR A.R1 IS NOT NULL) 
		 AND id(A) <> id(B)
	   WITH A, B, R1
	   MATCH (B)-[R2]->(C)
	   WHERE (type(R2) IS NOT NULL OR B.R2 IS NOT NULL) 
		 AND id(A) <> id(B) AND id(A) <> id(C) 
		 AND id(B) <> id(C)
		 AND type(R1) <> type(R2)
	   WITH A, B, C, R1, R2
	   MATCH (C)-[R3]->(D)
	   WHERE (type(R3) IS NOT NULL OR C.R3 IS NOT NULL) 
		 AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D) 
		 AND id(B) <> id(C) AND id(B) <> id(D) 
		 AND id(C) <> id(D)
		 AND type(R1) <> type(R3) AND type(R2) <> type(R3)
	   WITH A, B, C, D, R1, R2, R3
	   MATCH (D)-[R4]->(E)
	   WHERE (type(R4) IS NOT NULL OR D.R4 IS NOT NULL) 
		 AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D) AND id(A) <> id(E)
		 AND id(B) <> id(C) AND id(B) <> id(D) AND id(B) <> id(E)
		 AND id(C) <> id(D) AND id(C) <> id(E)
		 AND id(D) <> id(E)
		 AND type(R1) <> type(R4) AND type(R2) <> type(R4) AND type(R3) <> type(R4)
	   WITH A, B, C, D, E, R1, R2, R3, R4
	   MATCH (E)-[R5]->(F)
	   WHERE (type(R5) IS NOT NULL OR E.R5 IS NOT NULL) 
		 AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D) AND id(A) <> id(E) AND id(A) <> id(F)
		 AND id(B) <> id(C) AND id(B) <> id(D) AND id(B) <> id(E) AND id(B) <> id(F)
		 AND id(C) <> id(D) AND id(C) <> id(E) AND id(C) <> id(F)
		 AND id(D) <> id(E) AND id(D) <> id(F)
		 AND id(E) <> id(F)
		 AND type(R1) <> type(R5) AND type(R2) <> type(R5) AND type(R3) <> type(R5) AND type(R4) <> type(R5)
	   RETURN DISTINCT 
		   A, 
		   coalesce(type(R1), 'Property') AS R1, 
		   B, 
		   coalesce(type(R2), 'Property') AS R2, 
		   C, 
		   coalesce(type(R3), 'Property') AS R3, 
		   D, 
		   coalesce(type(R4), 'Property') AS R4, 
		   E, 
		   coalesce(type(R5), 'Property') AS R5, 
		   F
	   LIMIT 3"


   :metta	 
	 (limit 3
	   (match &neo4j_out_v3
		 (,
		   ;; Edges must differ:
		   (each-different ($R1 $R2 $R3 $R4 $R5))
		   ;; Nodes must differ:
		   (each-different  ($A $B $C $D $E $F $G $H $I $J $K))
		   (neo $A $R1 $B)
		   (neo $B $R2 $C)
		   (neo $C $R3 $D)
		   (neo $D $R4 $E)
		   (neo $E $R5 $F)
		 )
		 (result $A $R1 $B $R2 $C $R3 $D $R4 $E $R5 $F)))

  ;; Provide any additional info if desired:

   :neo4j-info ((execution_time "unknown")
			   (result_count "dynamic")
			   (node_count "unknown")
			   (edge_count "unknown"))))



(isa-test-query
  (:name "Ten-Step Path from $R1 to $R10"
   :query-number "custom-r1-r10"
   :description "Find paths with exactly ten relationships, from $R1 to $R10."

   :cypher
   "MATCH (A)
    WHERE A.R1 IS NOT NULL OR (A)-[:R1]->()
    WITH A
    MATCH (A)-[R1]->(B)
    WHERE (type(R1) IS NOT NULL OR A.R1 IS NOT NULL)
      AND id(A) <> id(B)
    WITH A, B, R1
    MATCH (B)-[R2]->(C)
    WHERE (type(R2) IS NOT NULL OR B.R2 IS NOT NULL)
      AND id(A) <> id(B) AND id(A) <> id(C) 
      AND id(B) <> id(C)
      AND type(R1) <> type(R2)
    WITH A, B, C, R1, R2
    MATCH (C)-[R3]->(D)
    WHERE (type(R3) IS NOT NULL OR C.R3 IS NOT NULL)
      AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D)
      AND id(B) <> id(C) AND id(B) <> id(D)
      AND id(C) <> id(D)
      AND type(R1) <> type(R3) AND type(R2) <> type(R3)
    WITH A, B, C, D, R1, R2, R3
    MATCH (D)-[R4]->(E)
    WHERE (type(R4) IS NOT NULL OR D.R4 IS NOT NULL)
      AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D) AND id(A) <> id(E)
      AND id(B) <> id(C) AND id(B) <> id(D) AND id(B) <> id(E)
      AND id(C) <> id(D) AND id(C) <> id(E)
      AND id(D) <> id(E)
      AND type(R1) <> type(R4) AND type(R2) <> type(R4) AND type(R3) <> type(R4)
    WITH A, B, C, D, E, R1, R2, R3, R4
    MATCH (E)-[R5]->(F)
    WHERE (type(R5) IS NOT NULL OR E.R5 IS NOT NULL)
      AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D) AND id(A) <> id(E) AND id(A) <> id(F)
      AND id(B) <> id(C) AND id(B) <> id(D) AND id(B) <> id(E) AND id(B) <> id(F)
      AND id(C) <> id(D) AND id(C) <> id(E) AND id(C) <> id(F)
      AND id(D) <> id(E) AND id(D) <> id(F)
      AND id(E) <> id(F)
      AND type(R1) <> type(R5) AND type(R2) <> type(R5) AND type(R3) <> type(R5) AND type(R4) <> type(R5)
    WITH A, B, C, D, E, F, R1, R2, R3, R4, R5
    MATCH (F)-[R6]->(G)
    WHERE (type(R6) IS NOT NULL OR F.R6 IS NOT NULL)
      AND id(A) <> id(B) AND id(A) <> id(C) AND id(A) <> id(D) AND id(A) <> id(E) AND id(A) <> id(F) AND id(A) <> id(G)
      AND id(B) <> id(C) AND id(B) <> id(D) AND id(B) <> id(E) AND id(B) <> id(F) AND id(B) <> id(G)
      AND id(C) <> id(D) AND id(C) <> id(E) AND id(C) <> id(F) AND id(C) <> id(G)
      AND id(D) <> id(E) AND id(D) <> id(F) AND id(D) <> id(G)
      AND id(E) <> id(F) AND id(E) <> id(G)
      AND id(F) <> id(G)
      AND type(R1) <> type(R6) AND type(R2) <> type(R6) AND type(R3) <> type(R6) AND type(R4) <> type(R6) AND type(R5) <> type(R6)
    WITH A, B, C, D, E, F, G, R1, R2, R3, R4, R5, R6
    MATCH (G)-[R7]->(H)
    MATCH (H)-[R8]->(I)
    MATCH (I)-[R9]->(J)
    MATCH (J)-[R10]->(K)
    RETURN DISTINCT 
      A, coalesce(type(R1), 'Property') AS R1, 
      B, coalesce(type(R2), 'Property') AS R2, 
      C, coalesce(type(R3), 'Property') AS R3, 
      D, coalesce(type(R4), 'Property') AS R4, 
      E, coalesce(type(R5), 'Property') AS R5, 
      F, coalesce(type(R6), 'Property') AS R6, 
      G, coalesce(type(R7), 'Property') AS R7, 
      H, coalesce(type(R8), 'Property') AS R8, 
      I, coalesce(type(R9), 'Property') AS R9, 
      J, coalesce(type(R10), 'Property') AS R10, 
      K
    LIMIT 3"

   :metta
     (limit 3
       (match &neo4j_out_v3
         (,
		   (each-different ($A $B $C $D $E $F $G $H $I $J $K))
		   (each-different ($R1 $R2 $R3 $R4 $R5 $R6 $R7 $R8 $R9 $R10))
           (neo $A $R1 $B)
           (neo $B $R2 $C)
           (neo $C $R3 $D)
           (neo $D $R4 $E)
           (neo $E $R5 $F)
           (neo $F $R6 $G)
           (neo $G $R7 $H)
           (neo $H $R8 $I)
           (neo $I $R9 $J)
           (neo $J $R10 $K)
         )
        (result $A $R1 $B $R2 $C $R3 $D $R4 $E $R5 $F $R6 $G $R7 $H $R8 $I $R9 $J $R10 $K)))

   :neo4j-info ((execution_time "unknown")
               (result_count "dynamic")
               (node_count "unknown")
               (edge_count "unknown"))))




(isa-test-query
  (:name "Dynamic Query 9 relations"
   :query-number "custom-dynamic-query-9-relations"
   :description "Finds dynamic relationships with properties in both Neo4j (Cypher) and Prolog, ensuring all result values are distinct."

   :metta
   (match &neo4j_out_v3
     (,
	   (each-different (cl_0000540:cl $VAR1 $VAR2 $VAR3 $VAR4 $VAR5 $VAR6 $VAR7 $VAR8 q13394))
	   (each-different ($R1 $R2 $R3 $R4 $R5 $R6 $R7 $R8 $R9))
       (neo cl_0000540:cl $R1 $VAR1)
       (neo $VAR1 $R2 $VAR2 $P3)
       (neo $VAR2 $R3 $VAR3 $P4)
       (neo $VAR3 $R4 $VAR4 $P5)
       (neo $VAR4 $R5 $VAR5 $P6)
       (neo $VAR5 $R6 $VAR6 $P7)
       (neo $VAR6 $R7 $VAR7 $P8)
       (neo $VAR7 $R8 $VAR8 $P9)
       (neo $VAR8 $R9 q13394)
     )

     (result $Class $VAR1 $VAR2 $VAR3 $VAR4 $VAR5 $VAR6 $VAR7 $VAR8 $Q13394
             $R1 $R2 $R3 $R4 $R5 $R6 $R7 $R8 $R9))


	:prolog
	"m(neo(cl_0000540:cl, R1, VAR1),
		  neo(VAR1, R2, VAR2),
		  neo(VAR2, R3, VAR3),
		  neo(VAR3, R4, VAR4),
		  neo(VAR4, R5, VAR5),
		  neo(VAR5, R6, VAR6),
		  neo(VAR6, R7, VAR7),
		  neo(VAR7, R8, VAR8),
		  neo(VAR8, R9, q13394))"

	:cypher 
	"MATCH 
	  (c {id: \"cl_0000540\"})-[r1]->(v1)-[r2]->(v2)-[r3]->(v3)-[r4]->(v4)
	  -[r5]->(v5)-[r6]->(v6)-[r7]->(v7)-[r8]->(v8)-[r9]->({id: \"q13394\"} q)
	WHERE 
	  c <> v1 AND v1 <> v2 AND v2 <> v3 AND v3 <> v4 AND v4 <> v5 AND 
	  v5 <> v6 AND v6 <> v7 AND v7 <> v8 AND v8 <> q
	RETURN DISTINCT 
		   c, v1, v2, v3, v4, v5, v6, v7, v8, q, 
		   type(r1) AS R1_Label, properties(r1) AS R1_CypherProperties,
		   type(r2) AS R2_Label, properties(r2) AS R2_CypherProperties,
		   type(r3) AS R3_Label, properties(r3) AS R3_CypherProperties,
		   type(r4) AS R4_Label, properties(r4) AS R4_CypherProperties,
		   type(r5) AS R5_Label, properties(r5) AS R5_CypherProperties,
		   type(r6) AS R6_Label, properties(r6) AS R6_CypherProperties,
		   type(r7) AS R7_Label, properties(r7) AS R7_CypherProperties,
		   type(r8) AS R8_Label, properties(r8) AS R8_CypherProperties,
		   type(r9) AS R9_Label, properties(r9) AS R9_CypherProperties;"

   :neo4j-info ((execution_time "unknown")
               (result_count "dynamic")
               (node_count "unknown")
               (edge_count "unknown"))))



