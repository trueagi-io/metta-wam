(compare-query "s2a - Transcript-Exon Relationship"
  "Find transcripts and their included exons."
  (match &neo4j_out_v3
    (,
      (neo $T:transcript transcript_id "ENST00000472835.1")
      (neo $T:transcript includes $E:exon))
    (result $T $E)))
  (neo4j-info (execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2)))

(compare-query "s2b - Transcript-Exon Relationship (With Names) - **Updated**"
  "Retrieve transcript names and their associated exon IDs."
  (match &neo4j_out_v3
    (,
      (neo $T:transcript transcript_id "ENST00000472835.1")
      (neo $T:transcript transcript_name $TranscriptName)
      (neo $T:transcript includes $E:exon)
      (neo $E:exon exon_id $ExonID))
    (result $TranscriptName $ExonID)))
  (neo4j-info (execution_time 0.0001) (result_count 2) (node_count 3) (edge_count 2)))

(compare-query "s502a - Gene Regulation Query"
  "Find gene regulations related to FTO."
  (match &neo4j_out_v3
    (,
      (neo $Gene1:gene gene_name "FTO")
      (neo $Gene2:gene regulates $Gene1))
    (result $Gene1 $Gene2)))
  (neo4j-info (execution_time 0.018) (result_count 502) (node_count 503) (edge_count 503)))

(compare-query "s502b - Gene Regulation Query (With Names) - **Updated**"
  "Find gene regulations with named identifiers."
  (match &neo4j_out_v3
    (,
      (neo $Gene1:gene gene_name "FTO")
      (neo $Gene1:gene gene_name $Target)
      (neo $Gene2:gene regulates $Gene1)
      (neo $Gene2:gene gene_name $Regulator))
    (result $Regulator $Target)))
  (neo4j-info (execution_time 0.028) (result_count 502) (node_count 503) (edge_count 503)))

(compare-query "1. BRCA2 Interactions"
  "Find interactions involving the BRCA2 gene, including transcripts, proteins, and pathways."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "BRCA2"))
      (neo $Gene:gene transcribed_to $Transcript)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein1 interacts_with $Protein2)
      (neo $Gene genes_pathways $Pathway))
    (result $Gene $Transcript $Protein1 $Protein2 $Pathway)))
  (neo4j-info (execution_time 180) (result_count 41536) (node_count 144) (edge_count 143)))

(compare-query "2. IGF2 Associations - **Updated**"
  "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF2"))
      (neo $Gene:gene genes_pathways $Pathway)
      (neo $ChildPathway:pathway child_pathway_of $Pathway)
      (neo $Promoter:promoter associated_with $Gene)
      (neo $Enhancer:enhancer associated_with $Gene))
    (result $Promoter $Gene $Enhancer $Pathway $ChildPathway)))
  (neo4j-info (execution_time 2400) (result_count 794742) (node_count 2693) (edge_count 2698)))

(compare-query "3. IGF1 Gene Interactions and Regulations - **Updated**"
  "Find IGF1 gene interactions, regulations, and pathways including transcripts and proteins."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $Gene:gene expressed_in $CellLine1)
      (neo $CellLine2 subclass_of $CellLine1)
      (neo $RegulatingGene regulates $Gene)
      (neo $RegulatingGene transcribed_to $Transcript)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Gene $CellLine1 $CellLine2 $RegulatingGene $Transcript $Protein1 $Protein2)))
  (neo4j-info (execution_time 58.9) (result_count 18665440) (node_count unknown) (edge_count unknown)))

(compare-query "4. IGF1 Gene Expression in Tissues - **Updated**"
  "Find IGF1 gene expression in tissues and related transcripts."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $Gene:gene expressed_in $Uberon)
      (neo $Gene:gene transcribed_to $Transcript))
    (result $Gene $Uberon $Transcript)))
  (neo4j-info (execution_time 6) (result_count 1827) (node_count 269) (edge_count 268)))

(compare-query "5. Pathway Associations for SNAP25 - **Updated**"
  "Locate SNAP25 in pathways with other genes."
  (match &neo4j_out_v3
    (,
      (neo $Gene1:gene gene_name "SNAP25")
      (neo $Gene1:gene genes_pathways $Pathway)
      (neo $Gene2:gene genes_pathways $Pathway))
    (result $Gene1 $Pathway $Gene2)))
  (neo4j-info (execution_time 5) (result_count 11967) (node_count 5665) (edge_count 11996)))

(compare-query "6. IGF1 Gene Regulation by SNP Activity"
  "Find regulation of the IGF1 gene by SNP activity."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $SNP:snp activity_by_contact $Gene))
    (result $SNP $Gene)))
  (neo4j-info (execution_time 0.15) (result_count 4285) (node_count unknown) (edge_count unknown)))

(compare-query "7. Transcripts and Exons on Chromosome 1 - **Updated**"
  "Find transcripts, exons, and interacting proteins located on chromosome 1."
  (match &neo4j_out_v3
    (,
      (neo $Exon:exon chr "chr1")
      (neo $Transcript:transcript includes $Exon)
      (neo $Transcript:transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Transcript $Exon $Protein1 $Protein2)))
  (neo4j-info (execution_time 58.82) (result_count 23736036) (node_count unknown) (edge_count unknown)))


(compare-query "8. IGF1 Gene Expression in Cell Lines - **Updated**"
  "Find IGF1 gene expression in cell lines and related subclass relationships."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")
      (neo $Gene:gene expressed_in $CellLine1)
      (neo $CellLine2 subclass_of $CellLine1))
    (result $Gene $CellLine1 $CellLine2)))
  (neo4j-info (execution_time 0.03) (result_count 3135) (node_count unknown) (edge_count unknown)))

(compare-query "9. IGF1 Gene Regulation by SNP Activity"
  "Find regulation of the IGF1 gene by SNP activity."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $SNP:snp activity_by_contact $Gene))
    (result $SNP $Gene)))
  (neo4j-info (execution_time 0.15) (result_count 4285) (node_count unknown) (edge_count unknown)))

(compare-query "10. IGF1 Gene Interactions and Regulations - **Updated**"
  "Find IGF1 gene interactions, regulations, and pathways including transcripts, proteins, and subclass relationships."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $Gene:gene expressed_in $CL1:cl)
      (neo $CL2:cl subclass_of $CL1:cl)
      (neo $RegulatingGene regulates $Gene)
      (neo $RegulatingGene transcribed_to $Transcript)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Gene $CL1 $CL2 $RegulatingGene $Transcript $Protein1 $Protein2)))
  (neo4j-info (execution_time 58.99) (result_count 19676097) (node_count unknown) (edge_count unknown)))

(compare-query "11. Pathway Associations for SNAP25 - **Updated**"
  "Locate SNAP25 in pathways with other genes, ensuring that SNAP25 and other genes are distinct."
  (match &neo4j_out_v3
    (,
      (neo $Gene1:gene gene_name "SNAP25")
      (neo $Gene1:gene genes_pathways $Pathway)
      (neo $Gene2:gene genes_pathways $Pathway)
      (different $Gene1 $Gene2))
    (result $Gene1 $Pathway $Gene2)))
  (neo4j-info (execution_time 0.42) (result_count 97865) (node_count unknown) (edge_count unknown)))

(compare-query "12. IGF1 Gene Expression in Tissues and Transcripts - **Updated**"
  "Find IGF1 gene expression in tissues and related transcripts."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $Gene:gene expressed_in $Uberon)
      (neo $Gene:gene transcribed_to $Transcript))
    (result $Gene $Uberon $Transcript)))
  (neo4j-info (execution_time 1.60) (result_count 49357) (node_count unknown) (edge_count unknown)))

(compare-query "13. Transcripts and Exons on Chromosome 1 - **Updated**"
  "Find transcripts, exons, and interacting proteins located on chromosome 1."
  (match &neo4j_out_v3
    (,
      (neo $Exon:exon chr "chr1")
      (neo $Transcript:transcript includes $Exon)
      (neo $Transcript:transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Transcript $Exon $Protein1 $Protein2)))
  (neo4j-info (execution_time 58.82) (result_count 23736036) (node_count unknown) (edge_count unknown)))

(compare-query "14. IGF1 Gene Expression in Tissues and Transcripts - **Updated**"
  "Find IGF1 gene expression in tissues and related transcripts."
  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name (startsWith "IGF1"))
      (neo $Gene:gene expressed_in $Uberon)
      (neo $Gene:gene transcribed_to $Transcript))
    (result $Gene $Uberon $Transcript)))
  (neo4j-info (execution_time 1.60) (result_count 49357) (node_count unknown) (edge_count unknown)))

