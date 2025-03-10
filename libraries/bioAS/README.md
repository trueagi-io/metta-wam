
```no-wrap
Line Count       File Path
-------------    --------------------------------------------------------
        7,199    ./neo4j_out_v3/cell_ontology/edges_cl_subclass_of.csv
        4,087    ./neo4j_out_v3/cell_ontology/edges_cl_part_of.csv
        1,301    ./neo4j_out_v3/cell_ontology/edges_cl_capable_of.csv
        4,873    ./neo4j_out_v3/cell_ontology/nodes_cl.csv
   37,302,979    ./neo4j_out_v3/dbsnp/nodes_snp.csv
      827,037    ./neo4j_out_v3/dgv/nodes_structural_variant.csv
    3,554,858    ./neo4j_out_v3/roadmap/chromatin_state/edges_chromatin_state.csv
       26,681    ./neo4j_out_v3/roadmap/dhs/edges_in_dnase_i_hotspot.csv
    1,124,719    ./neo4j_out_v3/roadmap/h3_mark/edges_histone_modification.csv
      635,269    ./neo4j_out_v3/gaf/edges_go_gene_product.csv
      329,075    ./neo4j_out_v3/brenda_tissue_ontology/edges_bto_subclass_of.csv
      239,869    ./neo4j_out_v3/brenda_tissue_ontology/nodes_bto.csv
   36,495,831    ./neo4j_out_v3/bgee/edges_expressed_in.csv
           59    ./neo4j_out_v3/graph_info.json
       20,429    ./neo4j_out_v3/uniprot/nodes_protein.csv
       50,073    ./neo4j_out_v3/uniprot/edges_translation_of.csv
       50,073    ./neo4j_out_v3/uniprot/edges_translates_to.csv
  257,746,971    ./neo4j_out_v3/coxpressdb/edges_coexpressed_with.csv
   20,140,695    ./neo4j_out_v3/peregrine/edges_enhancer_gene.csv
    1,085,539    ./neo4j_out_v3/peregrine/nodes_enhancer.csv
      144,395    ./neo4j_out_v3/uberon/edges_uberon_subclass_of.csv
       72,554    ./neo4j_out_v3/uberon/nodes_uberon.csv
   67,561,010    ./neo4j_out_v3/gtex/expression/edges_expressed_in.csv
   67,561,010    ./neo4j_out_v3/gtex/eqtl/edges_gtex_variant_gene.csv
   23,396,248    ./neo4j_out_v3/refseq/edges_closest_gene.csv
      211,389    ./neo4j_out_v3/cell_line_ontology/edges_clo_subclass_of.csv
      114,245    ./neo4j_out_v3/cell_line_ontology/nodes_clo.csv
   13,356,025    ./neo4j_out_v3/string/edges_interacts_with.csv
   35,856,052    ./neo4j_out_v3/abc/edges_activity_by_contact.csv
    6,398,916    ./neo4j_out_v3/tflink/edges_tf_gene.csv
      233,376    ./neo4j_out_v3/experimental_factor_ontology/nodes_efo.csv
      321,433    ./neo4j_out_v3/experimental_factor_ontology/edges_efo_subclass_of.csv
      327,933    ./neo4j_out_v3/fabian/edges_tfbs_snp.csv
       62,701    ./neo4j_out_v3/gencode/gene/nodes_gene.csv
    1,649,477    ./neo4j_out_v3/gencode/exon/nodes_exon.csv
    1,649,477    ./neo4j_out_v3/gencode/exon/edges_includes.csv
      252,836    ./neo4j_out_v3/gencode/transcript/nodes_transcript.csv
      252,836    ./neo4j_out_v3/gencode/transcript/edges_transcribed_to.csv
      252,836    ./neo4j_out_v3/gencode/transcript/edges_transcribed_from.csv
      991,805    ./neo4j_out_v3/reactome/edges_genes_pathways.csv
        2,674    ./neo4j_out_v3/reactome/nodes_pathway.csv
        2,692    ./neo4j_out_v3/reactome/edges_child_pathway_of.csv
        2,692    ./neo4j_out_v3/reactome/edges_parent_pathway_of.csv
       29,599    ./neo4j_out_v3/epd/nodes_promoter.csv
       28,134    ./neo4j_out_v3/epd/edges_promoter_gene.csv
    7,578,823    ./neo4j_out_v3/enhancer_atlas/edges_enhancer_gene.csv
      193,219    ./neo4j_out_v3/enhancer_atlas/nodes_enhancer.csv
       81,757    ./neo4j_out_v3/gene_ontology/edges_go_subclass_of.csv
       51,767    ./neo4j_out_v3/gene_ontology/nodes_go.csv
   24,296,266    ./neo4j_out_v3/dbvar/nodes_structural_variant.csv
        3,037    ./neo4j_out_v3/tadmap/nodes_tad.csv
       18,957    ./neo4j_out_v3/tadmap/edges_in_tad_region.csv
    2,268,769    ./neo4j_out_v3/cadd/nodes_snp.csv
       46,413    ./neo4j_out_v3/rna_central/edges_go_rna.csv
      662,841    ./neo4j_out_v3/rna_central/nodes_non_coding_rna.csv
      385,656    ./neo4j_out_v3/gwas/ingene/edges_snp_in_gene.csv
      188,113    ./neo4j_out_v3/gwas/downstream/edges_snp_downstream_gene.csv
      188,120    ./neo4j_out_v3/gwas/upstream/edges_snp_upstream_gene.csv
       62,284    ./neo4j_out_v3/dbsuper/edges_super_enhancer_gene.csv
       69,071    ./neo4j_out_v3/dbsuper/nodes_super_enhancer.csv
   10,565,631    ./neo4j_out_v3/tfbs/nodes_tfbs.csv
   10,489,066    ./neo4j_out_v3/tfbs/edges_gene_tfbs.csv
-------------    --------------------------------------------------------
  637,529,752    Total


```



true.

88 ?- time(sample_query).




### Saulo - returns 2 answers
```no-wrap
=================================================================
        MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.id, e.id

  (match &neo4j_out_v3
    (,
      (neo $T:transcript transcript_id ENST00000472835.1)
      (neo $T:transcript includes $E:exon))
    (result $T $E))

enst00000472835                                                                       ense00001880441
enst00000472835                                                                       ense00001885560
 MeTTaLog Execution time: 0.00 seconds
% 952 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 4068376 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 2
=================================================================
```




### Saulo - returns 2 answers  (With Properties)
```no-wrap
=================================================================
        MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.id, e.id (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $T:transcript transcript_id ENST00000472835.1 $PropList1)
      (neo_P $T:transcript includes $E:exon $PropList2))
    (result $T $E $PropList1 $PropList2))

enst00000472835                            ense00001880441                            ()                                         ()
enst00000472835                            ense00001885560                            ()                                         ()
 MeTTaLog Execution time: 0.00 seconds
% 1,277 inferences, 0.000 CPU in 0.000 seconds (52% CPU, 8948844 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 2
=================================================================
```




### Saulo - returns 2 answers
```no-wrap
=================================================================
        MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.transcript_name, e.exon_id

  (match &neo4j_out_v3
    (,
      (neo $0:transcript transcript_id ENST00000472835.1)
      (neo $0:transcript includes $1:exon)
      (neo $0:transcript transcript_name $TranscriptName)
      (neo $1:exon exon_id $ExonID))
    (result $TranscriptName $ExonID))

FTO-207                                                                               ENSE00001880441
FTO-207                                                                               ENSE00001885560
 MeTTaLog Execution time: 0.00 seconds
% 1,868 inferences, 0.000 CPU in 0.000 seconds (90% CPU, 12664407 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 2
=================================================================
```




### Saulo - returns 2 answers  (With Properties)
```no-wrap
=================================================================
        MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.transcript_name, e.exon_id (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $PropList1:transcript transcript_id ENST00000472835.1 $PropList2)
      (neo_P $PropList1:transcript includes $PropList3:exon $PropList4)
      (neo_P $PropList1:transcript transcript_name $TranscriptName $PropList5)
      (neo_P $PropList3:exon exon_id $ExonID $PropList6))
    (result $TranscriptName $ExonID $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))

FTO-207              ENSE00001880441      enst00000472835      ()                   ense00001880441      ()                   ()                   ()
FTO-207              ENSE00001885560      enst00000472835      ()                   ense00001885560      ()                   ()                   ()
 MeTTaLog Execution time: 0.00 seconds
% 3,278 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 11583039 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 2
=================================================================
```




### Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc
```no-wrap
=================================================================
        MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target

  (match &neo4j_out_v3
    (,
      (neo $Gene1 gene_name "FTO")
      (neo $Gene2 regulates $Gene1))
    (result $Gene1 $Gene2))

ensg00000140718:gene                                                                  ensg00000257923:gene
ensg00000140718:gene                                                                  ensg00000137871:gene
ensg00000140718:gene                                                                  ensg00000104064:gene
ensg00000140718:gene                                                                  ensg00000126746:gene
ensg00000140718:gene                                                                  ensg00000164190:gene
ensg00000140718:gene                                                                  ensg00000185811:gene
ensg00000140718:gene                                                                  ensg00000101076:gene
 MeTTaLog Execution time: 0.00 seconds
% 31,538 inferences, 0.002 CPU in 0.002 seconds (97% CPU, 17348589 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 502
=================================================================
```




### Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc (With Properties)
```no-wrap
=================================================================
        MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene1 gene_name "FTO" $PropList1)
      (neo_P $Gene2 regulates $Gene1 $PropList2))
    (result $Gene1 $Gene2 $PropList1 $PropList2))

ensg00000140718:gene    ensg00000257923:gene    ()                                          ...
    [evidence=['pubmed:23341774','pubmed:29087512','pubmed:29126285'],databases=['ReMap','TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator']
ensg00000140718:gene    ensg00000137871:gene    ()                                          ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay']
ensg00000140718:gene    ensg00000104064:gene    ()                                          ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay']
ensg00000140718:gene    ensg00000126746:gene    ()                                          ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay']
ensg00000140718:gene    ensg00000164190:gene    ()                                          ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay']
ensg00000140718:gene    ensg00000185811:gene    ()                                          ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay']
ensg00000140718:gene    ensg00000101076:gene    ()                                          ...
    [evidence=['pubmed:18971253','pubmed:27924024','pubmed:26578589'],databases=['GTRD','ORegAnno','PAZAR'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator']
 MeTTaLog Execution time: 0.00 seconds
% 36,537 inferences, 0.002 CPU in 0.002 seconds (96% CPU, 20991038 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 502
=================================================================
```




### Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc
```no-wrap
=================================================================
        MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target

  (match &neo4j_out_v3
    (,
      (neo $0 gene_name "FTO")
      (neo $1 regulates $0)
      (neo $0 gene_name $Target)
      (neo $1 gene_name $Regulator))
    (result $Regulator $Target))

CUX1                                                                                  FTO
ZNF280D                                                                               FTO
GABPB1                                                                                FTO
ZNF384                                                                                FTO
NIPBL                                                                                 FTO
IKZF1                                                                                 FTO
HNF4A                                                                                 FTO
 MeTTaLog Execution time: 0.01 seconds
% 336,570 inferences, 0.014 CPU in 0.015 seconds (99% CPU, 23214126 Lips)
 Last answer found: 0.01 seconds
 Number of answers: 502
=================================================================
```




### Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc (With Properties)
```no-wrap
=================================================================
        MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $PropList1 gene_name "FTO" $PropList2)
      (neo_P $PropList3 regulates $PropList1 $PropList4)
      (neo_P $PropList1 gene_name $Target $PropList5)
      (neo_P $PropList3 gene_name $Regulator $PropList6))
    (result $Regulator $Target $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))

CUX1                 FTO                  ensg00000140718:gene    ()                   ensg00000257923:gene     ...
    [evidence=['pubmed:23341774','pubmed:29087512','pubmed:29126285'],databases=['ReMap','TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()                   ()
ZNF280D              FTO                  ensg00000140718:gene    ()                   ensg00000137871:gene     ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                   ()      
GABPB1               FTO                  ensg00000140718:gene    ()                   ensg00000104064:gene     ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                   ()      
ZNF384               FTO                  ensg00000140718:gene    ()                   ensg00000126746:gene     ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                   ()      
NIPBL                FTO                  ensg00000140718:gene    ()                   ensg00000164190:gene     ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                   ()      
IKZF1                FTO                  ensg00000140718:gene    ()                   ensg00000185811:gene     ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                   ()      
HNF4A                FTO                  ensg00000140718:gene    ()                   ensg00000101076:gene     ...
    [evidence=['pubmed:18971253','pubmed:27924024','pubmed:26578589'],databases=['GTRD','ORegAnno','PAZAR'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator'] ()                   ()
 MeTTaLog Execution time: 0.02 seconds
% 360,972 inferences, 0.020 CPU in 0.020 seconds (99% CPU, 17961934 Lips)
 Last answer found: 0.02 seconds
 Number of answers: 502
=================================================================
```




### 1. Find Interactions of BRCA2 Gene
```no-wrap
=================================================================
        Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "BRCA2"))
      (neo $Gene transcribed_to $Transcript)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein1 interacts_with $Protein2)
      (neo $Gene genes_pathways $Pathway))
    (result $Gene $Transcript $Protein1 $Protein2 $Pathway))

ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q7z407:protein    'r-hsa-1474165':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q7z407:protein    'r-hsa-1500620':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q7z407:protein    'r-hsa-1640170':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q92887:protein    'r-hsa-9701192':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q92887:protein    'r-hsa-9701193':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q92887:protein    'r-hsa-9704646':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    p09661:protein    'r-hsa-73894':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    p38159:protein    'r-hsa-1500620':pathway
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    p07949:protein    'r-hsa-5693567':pathway
 MeTTaLog Execution time: 0.50 seconds
% 12,182,824 inferences, 0.498 CPU in 0.500 seconds (99% CPU, 24478071 Lips)
 Last answer found: 0.38 seconds
 Number of answers: 152,442
=================================================================
```




### 1. Find Interactions of BRCA2 Gene (With Properties)
```no-wrap
=================================================================
        Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "BRCA2") $PropList1)
      (neo_P $Gene transcribed_to $Transcript $PropList2)
      (neo_P $Transcript translates_to $Protein1 $PropList3)
      (neo_P $Protein1 interacts_with $Protein2 $PropList4)
      (neo_P $Gene genes_pathways $Pathway $PropList5))
    (result $Gene $Transcript $Protein1 $Protein2 $Pathway $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))

ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q7z407:protein    'r-hsa-1474165':pathway    ()               ()               ()                ...
    [score=0.403]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q7z407:protein    'r-hsa-1500620':pathway    ()               ()               ()                ...
    [score=0.403]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q7z407:protein    'r-hsa-1640170':pathway    ()               ()               ()                ...
    [score=0.403]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q92887:protein    'r-hsa-9701192':pathway    ()               ()               ()                ...
    [score=0.165]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q92887:protein    'r-hsa-9701193':pathway    ()               ()               ()                ...
    [score=0.165]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    q92887:protein    'r-hsa-9704646':pathway    ()               ()               ()                ...
    [score=0.165]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    p09661:protein    'r-hsa-73894':pathway    ()               ()               ()                ...
    [score=0.432]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    p38159:protein    'r-hsa-1500620':pathway    ()               ()               ()                ...
    [score=0.283]    ()
ensg00000139618:gene    enst00000544455:transcript    p51587:protein    p07949:protein    'r-hsa-5693567':pathway    ()               ()               ()                ...
    [score=0.705]    ()
 MeTTaLog Execution time: 0.56 seconds
% 15,257,547 inferences, 0.557 CPU in 0.562 seconds (99% CPU, 27375649 Lips)
 Last answer found: 0.44 seconds
 Number of answers: 152,442
=================================================================
```




### 2. Find Components Associated with IGF2
```no-wrap
=================================================================
        Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (startsWith "IGF2"))
      (neo $Promoter:promoter associated_with $Gene:gene)
      (neo $Enhancer:enhancer associated_with $Gene:gene)
      (neo $Gene:gene genes_pathways $Pathway:pathway)
      (neo $ChildPathway:pathway child_pathway_of $Pathway:pathway))
    (result $Promoter $Gene $Enhancer $Pathway $ChildPathway))

chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185644521_185644840_grch38   r-hsa-8953854                     r-hsa-194441
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185644521_185644840_grch38   r-hsa-8953854                     r-hsa-428359
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185644521_185644840_grch38   r-hsa-8953854                     r-hsa-429914
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185049221_185051590_grch38   r-hsa-8953854                     r-hsa-72086
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185049221_185051590_grch38   r-hsa-8953854                     r-hsa-72203
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185049221_185051590_grch38   r-hsa-8953854                     r-hsa-72312
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185404981_185407140_grch38   r-hsa-8953854                     r-hsa-75067
chr3_185821121_185821180_grch38   ensg00000073792                   chr3_185404981_185407140_grch38   r-hsa-8953854                     r-hsa-72086
chr3_185825028_185825087_grch38   ensg00000073792                   chr3_185266321_185268330_grch38   r-hsa-8953854                     r-hsa-72086
chr11_2138632_2138691_grch38      ensg00000167244                   chr11_2024289_2024603_grch38      r-hsa-109582                      r-hsa-202733
 MeTTaLog Execution time: 8.58 seconds
% 91,144,574 inferences, 8.584 CPU in 8.873 seconds (97% CPU, 10618181 Lips)
 Last answer found: 8.57 seconds
 Number of answers: 664,061
=================================================================
```




### 2. Find Components Associated with IGF2 (With Properties)
```no-wrap
=================================================================
        Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (startsWith "IGF2") $PropList1)
      (neo_P $Promoter:promoter associated_with $Gene:gene $PropList2)
      (neo_P $Enhancer:enhancer associated_with $Gene:gene $PropList3)
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $PropList4)
      (neo_P $ChildPathway:pathway child_pathway_of $Pathway:pathway $PropList5))
    (result $Promoter $Gene $Enhancer $Pathway $ChildPathway $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))

chr3_185821121_185821180_grch38 ensg00000073792  chr3_185644521_185644840_grch38 r-hsa-8953854    r-hsa-194441     ()               ()                ...
    [biological_context='UBERON_0000006',score=0.845829] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185644521_185644840_grch38 r-hsa-8953854    r-hsa-428359     ()               ()                ...
    [biological_context='UBERON_0000006',score=0.845829] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185644521_185644840_grch38 r-hsa-8953854    r-hsa-429914     ()               ()                ...
    [biological_context='UBERON_0000006',score=0.845829] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185049221_185051590_grch38 r-hsa-8953854    r-hsa-72086      ()               ()                ...
    [biological_context='CLO_0007599',score=2.086826] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185049221_185051590_grch38 r-hsa-8953854    r-hsa-72203      ()               ()                ...
    [biological_context='CLO_0007599',score=2.086826] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185049221_185051590_grch38 r-hsa-8953854    r-hsa-72312      ()               ()                ...
    [biological_context='CLO_0007599',score=2.086826] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185404981_185407140_grch38 r-hsa-8953854    r-hsa-75067      ()               ()                ...
    [biological_context='CL_0000515',score=0.505124] ()               ()
chr3_185821121_185821180_grch38 ensg00000073792  chr3_185404981_185407140_grch38 r-hsa-8953854    r-hsa-72086      ()               ()                ...
    [biological_context='BTO_0000764',score=0.765] ()               ()
chr3_185825028_185825087_grch38 ensg00000073792  chr3_185266321_185268330_grch38 r-hsa-8953854    r-hsa-72086      ()               ()                ...
    [biological_context='CLO_0009258',score=1.753719] ()               ()
chr11_2138632_2138691_grch38 ensg00000167244  chr11_2024289_2024603_grch38 r-hsa-109582     r-hsa-202733     ()               ()                ...
    [biological_context='CL_2000043'] ()               ()
 MeTTaLog Execution time: 8.83 seconds
% 105,199,702 inferences, 8.832 CPU in 8.907 seconds (99% CPU, 11911263 Lips)
 Last answer found: 8.82 seconds
 Number of answers: 664,061
=================================================================
```




### 3. Gene Interactions and GO Terms
```no-wrap
=================================================================
        Find gene interactions and associated GO terms including proteins and transcripts.

  (match &neo4j_out_v3
    (,
      (neo $Gene transcribed_to $Transcript)
      (neo $Transcript includes $Exon)
      (neo $Protein1 translation_of $Transcript)
      (neo $Protein1 interacts_with $Protein2)
      (neo $GOTerm go_gene_product $Protein1))
    (result $Gene $Transcript $Exon $Protein1 $Protein2 $GOTerm))

ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p35626:protein    go_0004930:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p35626:protein    go_0005515:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p35626:protein    go_0007186:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p32121:protein    go_0005886:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p32121:protein    go_0004984:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p17612:protein    go_0005515:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q9p1a6:protein    go_0007186:go
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p62805:protein    go_0005886:go
ensg00000187634:gene    enst00000342066:transcript    ense00003902988:exon    q96nu1:protein    p08100:protein    go_0005515:go
ensg00000188976:gene    enst00000327044:transcript    ense00001380352:exon    q9y3t9:protein    q8nbu5:protein    go_0005730:go
ensg00000188976:gene    enst00000327044:transcript    ense00003554589:exon    q9y3t9:protein    q969t9:protein    go_0005654:go
ensg00000188157:gene    enst00000379370:transcript    ense00001650172:exon    o00468:protein    q5t1h1:protein    go_0005604:go
Time limit 60 exceeded for 3. Gene Interactions and GO Terms
% 1,380,313,155 inferences, 58.820 CPU in 60.000 seconds (98% CPU, 23466844 Lips)
 Last answer found: 58.82 seconds
 Number of answers: 25,245,987
=================================================================
```




### 3. Gene Interactions and GO Terms (With Properties)
```no-wrap
=================================================================
        Find gene interactions and associated GO terms including proteins and transcripts. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene transcribed_to $Transcript $PropList1)
      (neo_P $Transcript includes $Exon $PropList2)
      (neo_P $Protein1 translation_of $Transcript $PropList3)
      (neo_P $Protein1 interacts_with $Protein2 $PropList4)
      (neo_P $GOTerm go_gene_product $Protein1 $PropList5))
    (result $Gene $Transcript $Exon $Protein1 $Protein2 $GOTerm $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))

ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p35626:protein    go_0004930:go    ()             ()             ()              ...
    [score=0.67]    ...
    [qualifier=[enables],db_reference=['GO_REF:0000043'],evidence='IEA']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p35626:protein    go_0005515:go    ()             ()             ()              ...
    [score=0.67]    ...
    [qualifier=[enables],db_reference=['PMID:32296183'],evidence='IPI']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p35626:protein    go_0007186:go    ()             ()             ()              ...
    [score=0.67]    ...
    [qualifier=[involved_in],db_reference=['GO_REF:0000043'],evidence='IEA']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p32121:protein    go_0005886:go    ()             ()             ()              ...
    [score=0.679]   ...
    [qualifier=[located_in],db_reference=['GO_REF:0000044'],evidence='IEA']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p32121:protein    go_0004984:go    ()             ()             ()              ...
    [score=0.679]   ...
    [qualifier=[enables],db_reference=['GOREF:0000033'],evidence='IBA']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p17612:protein    go_0005515:go    ()             ()             ()              ...
    [score=0.65]    ...
    [qualifier=[enables],db_reference=['PMID:32296183'],evidence='IPI']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q9p1a6:protein    go_0007186:go    ()             ()             ()              ...
    [score=0.248]   ...
    [qualifier=[involved_in],db_reference=['GO_REF:0000043'],evidence='IEA']
ensg00000284733:gene    enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p62805:protein    go_0005886:go    ()             ()             ()              ...
    [score=0.214]   ...
    [qualifier=[located_in],db_reference=['GO_REF:0000044'],evidence='IEA']
ensg00000187634:gene    enst00000342066:transcript    ense00003902988:exon    q96nu1:protein    p08100:protein    go_0005515:go    ()             ()             ()              ...
    [score=0.173]   ...
    [qualifier=[enables],db_reference=['PMID:32296183'],evidence='IPI']
ensg00000188976:gene    enst00000327044:transcript    ense00001380352:exon    q9y3t9:protein    q8nbu5:protein    go_0005730:go    ()             ()             ()              ...
    [score=0.176]   ...
    [qualifier=[is_active_in],db_reference=['GOREF:0000033'],evidence='IBA']
ensg00000188976:gene    enst00000327044:transcript    ense00003554589:exon    q9y3t9:protein    q969t9:protein    go_0005654:go    ()             ()             ()              ...
    [score=0.228]   ...
    [qualifier=[located_in],db_reference=['PMID:20123734'],evidence='IDA']
ensg00000188157:gene    enst00000379370:transcript    ense00001650172:exon    o00468:protein    q5t1h1:protein    go_0005604:go    ()             ()             ()              ...
    [score=0.179]   ...
    [qualifier=[located_in],db_reference=['PMID:9405491'],evidence='IDA']
Time limit 60 exceeded for 3. Gene Interactions and GO Terms (With Properties)
% 1,626,450,996 inferences, 58.902 CPU in 60.000 seconds (98% CPU, 27613057 Lips)
 Last answer found: 58.90 seconds
 Number of answers: 22,001,896
=================================================================
```




### 4. Interactions Involving 1433B Protein
```no-wrap
=================================================================
        Find interactions involving 1433B protein including transcripts, exons, and GO terms.

  (match &neo4j_out_v3
    (,
      (neo $Protein1 protein_name
        (startsWith "1433B"))
      (neo $Gene transcribed_to $Transcript)
      (neo $Transcript includes $Exon)
      (neo $Protein1 translation_of $Transcript)
      (neo $Protein1 interacts_with $Protein2)
      (neo $GOTerm go_gene_product $Protein1))
    (result $Gene $Transcript $Exon $Protein1 $Protein2 $GOTerm))

ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0004860:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p30044:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p63151:protein    go_0005829:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    q16576:protein    go_0005829:go
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    q13363:protein    go_0005515:go
ensg00000166913:gene    enst00000353703:transcript    ense00003798043:exon    p31946:protein    q8wum4:protein    go_0005829:go
ensg00000166913:gene    enst00000372839:transcript    ense00003798043:exon    p31946:protein    o14810:protein    go_0005515:go
 MeTTaLog Execution time: 20.03 seconds
% 417,778,945 inferences, 20.032 CPU in 20.399 seconds (98% CPU, 20855912 Lips)
 Last answer found: 19.25 seconds
 Number of answers: 7,441,850
=================================================================
```




### 4. Interactions Involving 1433B Protein (With Properties)
```no-wrap
=================================================================
        Find interactions involving 1433B protein including transcripts, exons, and GO terms. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Protein1 protein_name
        (startsWith "1433B") $PropList1)
      (neo_P $Gene transcribed_to $Transcript $PropList2)
      (neo_P $Transcript includes $Exon $PropList3)
      (neo_P $Protein1 translation_of $Transcript $PropList4)
      (neo_P $Protein1 interacts_with $Protein2 $PropList5)
      (neo_P $GOTerm go_gene_product $Protein1 $PropList6))
    (result $Gene $Transcript $Exon $Protein1 $Protein2 $GOTerm $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))

ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0004860:go    ()            ()            ()            ()             ...
    [score=0.39]   ...
    [qualifier=[enables],db_reference=['PMID:11555644'],evidence='IDA']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.39]   ...
    [qualifier=[enables],db_reference=['PMID:10713667'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.39]   ...
    [qualifier=[enables],db_reference=['PMID:10958686'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.39]   ...
    [qualifier=[enables],db_reference=['PMID:31980649'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.39]   ...
    [qualifier=[enables],db_reference=['PMID:32707033'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p20290:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.39]   ...
    [qualifier=[enables],db_reference=['PMID:32707033'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p30044:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.187]  ...
    [qualifier=[enables],db_reference=['PMID:24255178'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    p63151:protein    go_0005829:go    ()            ()            ()            ()             ...
    [score=0.18]   ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-5632738'],evidence='TAS']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    q16576:protein    go_0005829:go    ()            ()            ()            ()             ...
    [score=0.322]  ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-5672972'],evidence='TAS']
ensg00000166913:gene    enst00000353703:transcript    ense00001891800:exon    p31946:protein    q13363:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.165]  ...
    [qualifier=[enables],db_reference=['PMID:30833792'],evidence='IPI']
ensg00000166913:gene    enst00000353703:transcript    ense00003798043:exon    p31946:protein    q8wum4:protein    go_0005829:go    ()            ()            ()            ()             ...
    [score=0.179]  ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-9656211'],evidence='TAS']
ensg00000166913:gene    enst00000372839:transcript    ense00003798043:exon    p31946:protein    o14810:protein    go_0005515:go    ()            ()            ()            ()             ...
    [score=0.182]  ...
    [qualifier=[enables],db_reference=['PMID:32707033'],evidence='IPI']
 MeTTaLog Execution time: 23.27 seconds
% 605,785,515 inferences, 23.269 CPU in 23.806 seconds (98% CPU, 26034178 Lips)
 Last answer found: 22.72 seconds
 Number of answers: 7,441,850
=================================================================
```




### 5. Components Associated with IGF1
```no-wrap
=================================================================
        Find enhancers, pathways, and transcripts associated with the IGF1 gene.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")
      (neo $Gene:gene genes_pathways $Pathway)
      (neo $Enhancer:enhancer associated_with $Gene:gene)
      (neo $Transcript transcribed_from $Gene:gene)
      (neo $Transcript translates_to $Protein:protein))
    (result $Gene $Pathway $Enhancer $Transcript $Protein))

ensg00000017427                   'r-hsa-109582':pathway    chr12_102848221_102849260_grch38  enst00000337514:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102848221_102849260_grch38  enst00000392904:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102848221_102849260_grch38  enst00000644491:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102498523_102498856_grch38  enst00000392905:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102498523_102498856_grch38  enst00000307046:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102498523_102498856_grch38  enst00000392904:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102410004_102410767_grch38  enst00000644491:transcript    p05019
ensg00000017427                   'r-hsa-109582':pathway    chr12_102418398_102418675_grch38  enst00000392905:transcript    p05019
ensg00000017427                   'r-hsa-114608':pathway    chr12_102452575_102452904_grch38  enst00000392905:transcript    p05019
 MeTTaLog Execution time: 33.57 seconds
% 14,564,769 inferences, 33.567 CPU in 34.176 seconds (98% CPU, 433906 Lips)
 Last answer found: 33.57 seconds
 Number of answers: 80,808
=================================================================
```




### 5. Components Associated with IGF1 (With Properties)
```no-wrap
=================================================================
        Find enhancers, pathways, and transcripts associated with the IGF1 gene. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $PropList1)
      (neo_P $Gene:gene genes_pathways $Pathway $PropList2)
      (neo_P $Enhancer:enhancer associated_with $Gene:gene $PropList3)
      (neo_P $Transcript transcribed_from $Gene:gene $PropList4)
      (neo_P $Transcript translates_to $Protein:protein $PropList5))
    (result $Gene $Pathway $Enhancer $Transcript $Protein $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))

ensg00000017427  'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0007947',score=0.699872] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000392904:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0007947',score=0.699872] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000644491:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0007947',score=0.699872] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102498523_102498856_grch38 enst00000392905:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0001654'] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102498523_102498856_grch38 enst00000307046:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0001654'] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102498523_102498856_grch38 enst00000392904:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0002785'] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102410004_102410767_grch38 enst00000644491:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0002785'] ()               ()
ensg00000017427  'r-hsa-109582':pathway    chr12_102418398_102418675_grch38 enst00000392905:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0009047'] ()               ()
ensg00000017427  'r-hsa-114608':pathway    chr12_102452575_102452904_grch38 enst00000392905:transcript    p05019           ()               ()                ...
    [biological_context='CLO_0009054'] ()               ()
 MeTTaLog Execution time: 33.24 seconds
% 16,709,305 inferences, 33.236 CPU in 34.020 seconds (98% CPU, 502743 Lips)
 Last answer found: 33.24 seconds
 Number of answers: 80,808
=================================================================
```




### 6. Pathways and Protein Interactions for IGF1
```no-wrap
=================================================================
        Find pathways and interacting proteins for the IGF1 gene including all associated components.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")
      (neo $Gene:gene genes_pathways $Pathway)
      (neo $Enhancer:enhancer associated_with $Gene:gene)
      (neo $Transcript transcribed_from $Gene:gene)
      (neo $Transcript translates_to $Protein1:protein)
      (neo $Protein1:protein interacts_with $Protein2:protein))
    (result $Gene $Pathway $Enhancer $Transcript $Protein1 $Protein2))

ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      o43541
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      p04141
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      q12879
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      p46939
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      q16656
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      o43741
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      q9bxn2
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019                      o75173
ensg00000017427             'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000424202:transcript    p05019                      q05086
ensg00000017427             'r-hsa-109582':pathway    chr12_102427022_102427223_grch38 enst00000392905:transcript    p05019                      q8tf66
ensg00000017427             'r-hsa-109582':pathway    chr12_102407709_102408725_grch38 enst00000424202:transcript    p05019                      p14317
ensg00000017427             'r-hsa-109582':pathway    chr12_102421772_102422722_grch38 enst00000424202:transcript    p05019                      p05362
Time limit 60 exceeded for 6. Pathways and Protein Interactions for IGF1
% 2,081,921,527 inferences, 58.991 CPU in 60.000 seconds (98% CPU, 35292321 Lips)
 Last answer found: 58.99 seconds
 Number of answers: 15,080,242
=================================================================
```




### 6. Pathways and Protein Interactions for IGF1 (With Properties)
```no-wrap
=================================================================
        Find pathways and interacting proteins for the IGF1 gene including all associated components. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $PropList1)
      (neo_P $Gene:gene genes_pathways $Pathway $PropList2)
      (neo_P $Enhancer:enhancer associated_with $Gene:gene $PropList3)
      (neo_P $Transcript transcribed_from $Gene:gene $PropList4)
      (neo_P $Transcript translates_to $Protein1:protein $PropList5)
      (neo_P $Protein1:protein interacts_with $Protein2:protein $PropList6))
    (result $Gene $Pathway $Enhancer $Transcript $Protein1 $Protein2 $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))

ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        o43541        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.225]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        p04141        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.541]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        q12879        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.25]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        p46939        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.282]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        q16656        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.271]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        o43741        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.167]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        q9bxn2        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.227]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000337514:transcript    p05019        o75173        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.373]
ensg00000017427 'r-hsa-109582':pathway    chr12_102848221_102849260_grch38 enst00000424202:transcript    p05019        q05086        ()            ()             ...
    [biological_context='CLO_0007947',score=0.699872] ()            ()             ...
    [score=0.2]
ensg00000017427 'r-hsa-109582':pathway    chr12_102427022_102427223_grch38 enst00000392905:transcript    p05019        q8tf66        ()            ()             ...
    [biological_context='CLO_0002177'] ()            ()             ...
    [score=0.185]
ensg00000017427 'r-hsa-109582':pathway    chr12_102407709_102408725_grch38 enst00000424202:transcript    p05019        p14317        ()            ()             ...
    [biological_context='CLO_0037076'] ()            ()             ...
    [score=0.195]
ensg00000017427 'r-hsa-109582':pathway    chr12_102421772_102422722_grch38 enst00000424202:transcript    p05019        p05362        ()            ()             ...
    [biological_context='CLO_0009054'] ()            ()             ...
    [score=0.694]
Time limit 60 exceeded for 6. Pathways and Protein Interactions for IGF1 (With Properties)
% 2,206,926,315 inferences, 58.965 CPU in 60.000 seconds (98% CPU, 37427500 Lips)
 Last answer found: 58.97 seconds
 Number of answers: 13,617,920
=================================================================
```




### 7. Transcripts and Exons for TP73-AS1
```no-wrap
=================================================================
        Find transcripts and exons associated with the TP73-AS1 gene.

  (match &neo4j_out_v3
    (,
      (neo $Transcript includes $Exon)
      (neo $Transcript transcribed_from $Gene)
      (neo $Gene gene_name
        (endsWith "TP73-AS1")))
    (result $Transcript $Exon $Gene))

enst00000648526:transcript                               ense00003836919:exon                                     ensg00000227372:gene
enst00000648526:transcript                               ense00001787305:exon                                     ensg00000227372:gene
enst00000648526:transcript                               ense00001734384:exon                                     ensg00000227372:gene
enst00000636630:transcript                               ense00003798426:exon                                     ensg00000227372:gene
enst00000636630:transcript                               ense00003791729:exon                                     ensg00000227372:gene
enst00000636630:transcript                               ense00003798500:exon                                     ensg00000227372:gene
 MeTTaLog Execution time: 22.42 seconds
% 531,137,524 inferences, 22.417 CPU in 22.753 seconds (99% CPU, 23693882 Lips)
 Last answer found: 0.11 seconds
 Number of answers: 142
=================================================================
```




### 7. Transcripts and Exons for TP73-AS1 (With Properties)
```no-wrap
=================================================================
        Find transcripts and exons associated with the TP73-AS1 gene. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Transcript includes $Exon $PropList1)
      (neo_P $Transcript transcribed_from $Gene $PropList2)
      (neo_P $Gene gene_name
        (endsWith "TP73-AS1") $PropList3))
    (result $Transcript $Exon $Gene $PropList1 $PropList2 $PropList3))

enst00000648526:transcript    ense00003836919:exon    ensg00000227372:gene    ()                          ()                          ()
enst00000648526:transcript    ense00001787305:exon    ensg00000227372:gene    ()                          ()                          ()
enst00000648526:transcript    ense00001734384:exon    ensg00000227372:gene    ()                          ()                          ()
enst00000636630:transcript    ense00003798426:exon    ensg00000227372:gene    ()                          ()                          ()
enst00000636630:transcript    ense00003791729:exon    ensg00000227372:gene    ()                          ()                          ()
enst00000636630:transcript    ense00003798500:exon    ensg00000227372:gene    ()                          ()                          ()
 MeTTaLog Execution time: 23.23 seconds
% 534,441,674 inferences, 23.235 CPU in 23.596 seconds (98% CPU, 23001889 Lips)
 Last answer found: 0.10 seconds
 Number of answers: 142
=================================================================
```




### 8. Interactions Involving 1433S Protein
```no-wrap
=================================================================
        Find proteins interacting with 1433S and associated GO terms.

  (match &neo4j_out_v3
    (,
      (neo $Protein1 protein_name
        (stringEqual "1433S"))
      (neo $GOTerm go_gene_product $Protein1)
      (neo $Protein1 interacts_with $Protein2))
    (result $GOTerm $Protein1 $Protein2))

go_0005515:go                                            p31947:protein                                           p68032:protein
go_0005515:go                                            p31947:protein                                           o43278:protein
go_0005515:go                                            p31947:protein                                           p08922:protein
go_0005515:go                                            p31947:protein                                           p15407:protein
go_0005515:go                                            p31947:protein                                           p19838:protein
go_0005515:go                                            p31947:protein                                           p37023:protein
go_0005515:go                                            p31947:protein                                           q8ivw1:protein
go_0005515:go                                            p31947:protein                                           q96nj6:protein
go_0005515:go                                            p31947:protein                                           o95382:protein
 MeTTaLog Execution time: 0.58 seconds
% 15,135,842 inferences, 0.581 CPU in 0.587 seconds (99% CPU, 26048736 Lips)
 Last answer found: 0.56 seconds
 Number of answers: 243,331
=================================================================
```




### 8. Interactions Involving 1433S Protein (With Properties)
```no-wrap
=================================================================
        Find proteins interacting with 1433S and associated GO terms. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Protein1 protein_name
        (stringEqual "1433S") $PropList1)
      (neo_P $GOTerm go_gene_product $Protein1 $PropList2)
      (neo_P $Protein1 interacts_with $Protein2 $PropList3))
    (result $GOTerm $Protein1 $Protein2 $PropList1 $PropList2 $PropList3))

go_0005515:go    p31947:protein    p68032:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.386]
go_0005515:go    p31947:protein    o43278:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.238]
go_0005515:go    p31947:protein    p08922:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.198]
go_0005515:go    p31947:protein    p15407:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.218]
go_0005515:go    p31947:protein    p19838:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.273]
go_0005515:go    p31947:protein    p37023:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.181]
go_0005515:go    p31947:protein    q8ivw1:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.347]
go_0005515:go    p31947:protein    q96nj6:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI']  ...
    [score=0.156]
go_0005515:go    p31947:protein    o95382:protein    ()                           ...
    [qualifier=[enables],db_reference=['PMID:15657067'],evidence='IPI']  ...
    [score=0.198]
 MeTTaLog Execution time: 0.62 seconds
% 18,057,360 inferences, 0.618 CPU in 0.624 seconds (99% CPU, 29206921 Lips)
 Last answer found: 0.60 seconds
 Number of answers: 243,331
=================================================================
```




### 9. IGF1 Expression in Tissues and Transcripts
```no-wrap
=================================================================
        Find IGF1 expression in tissues and related transcripts.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "IGF1"))
      (neo $Gene expressed_in $Uberon)
      (neo $Gene transcribed_to $Transcript))
    (result $Gene $Uberon $Transcript))

ensg00000017427:gene                                     cl_0000015:cl                                            enst00000337514:transcript
ensg00000017427:gene                                     cl_0000015:cl                                            enst00000481539:transcript
ensg00000017427:gene                                     cl_0000015:cl                                            enst00000392904:transcript
ensg00000017427:gene                                     uberon_0000002:uberon                                    enst00000392904:transcript
ensg00000017427:gene                                     uberon_0000002:uberon                                    enst00000644491:transcript
ensg00000017427:gene                                     uberon_0000002:uberon                                    enst00000392905:transcript
ensg00000017427:gene                                     uberon_0000057:uberon                                    enst00000644491:transcript
ensg00000017427:gene                                     uberon_0000451:uberon                                    enst00000307046:transcript
ensg00000017427:gene                                     uberon_0010414:uberon                                    enst00000424202:transcript
 MeTTaLog Execution time: 1.60 seconds
% 6,817,334 inferences, 1.604 CPU in 1.621 seconds (99% CPU, 4249796 Lips)
 Last answer found: 1.48 seconds
 Number of answers: 49,357
=================================================================
```




### 9. IGF1 Expression in Tissues and Transcripts (With Properties)
```no-wrap
=================================================================
        Find IGF1 expression in tissues and related transcripts. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "IGF1") $PropList1)
      (neo_P $Gene expressed_in $Uberon $PropList2)
      (neo_P $Gene transcribed_to $Transcript $PropList3))
    (result $Gene $Uberon $Transcript $PropList1 $PropList2 $PropList3))

ensg00000017427:gene    cl_0000015:cl    enst00000337514:transcript    ()                           ...
    [score=79.15,p_value=0.0033457544413306] ()
ensg00000017427:gene    cl_0000015:cl    enst00000481539:transcript    ()                           ...
    [score=79.15,p_value=0.0033457544413306] ()
ensg00000017427:gene    cl_0000015:cl    enst00000392904:transcript    ()                           ...
    [score=79.15,p_value=0.0033457544413306] ()
ensg00000017427:gene    uberon_0000002:uberon    enst00000392904:transcript    ()                           ...
    [score=89.21,p_value=5.212849615285199e-12] ()
ensg00000017427:gene    uberon_0000002:uberon    enst00000644491:transcript    ()                           ...
    [score=89.21,p_value=5.212849615285199e-12] ()
ensg00000017427:gene    uberon_0000002:uberon    enst00000392905:transcript    ()                           ...
    [score=89.21,p_value=5.212849615285199e-12] ()
ensg00000017427:gene    uberon_0000057:uberon    enst00000644491:transcript    ()                           ...
    [score=94.68,p_value=0.0016728772206653] ()
ensg00000017427:gene    uberon_0000451:uberon    enst00000307046:transcript    ()                           ...
    [score=64.76,p_value=0.0029235940742491] ()
ensg00000017427:gene    uberon_0010414:uberon    enst00000424202:transcript    ()                           ...
    [score=96.52,p_value=1.0e-14] ()
 MeTTaLog Execution time: 1.58 seconds
% 7,427,090 inferences, 1.585 CPU in 1.606 seconds (99% CPU, 4685903 Lips)
 Last answer found: 1.47 seconds
 Number of answers: 49,357
=================================================================
```




### 10. Transcripts and Exons on Chromosome 1
```no-wrap
=================================================================
        Find transcripts, exons, and interacting proteins located on chromosome 1.

  (match &neo4j_out_v3
    (,
      (neo $Transcript includes $Exon)
      (neo $Exon chr chr1)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Transcript $Exon $Protein1 $Protein2))

enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q9h098:protein
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p50548:protein
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q9h720:protein
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q8n1m1:protein
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q8n1m1:protein
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q15735:protein
enst00000332831:transcript    ense00004001351:exon    q6iey1:protein    q8n660:protein
enst00000342066:transcript    ense00003902988:exon    q96nu1:protein    q9uh03:protein
enst00000342066:transcript    ense00003915575:exon    q96nu1:protein    q9np77:protein
enst00000379370:transcript    ense00001605257:exon    o00468:protein    q6gph4:protein
enst00000379370:transcript    ense00001385905:exon    o00468:protein    p25025:protein
enst00000435064:transcript    ense00003562382:exon    q5ta45:protein    q8izn3:protein
Time limit 60 exceeded for 10. Transcripts and Exons on Chromosome 1
% 1,587,641,190 inferences, 58.821 CPU in 60.000 seconds (98% CPU, 26991263 Lips)
 Last answer found: 58.82 seconds
 Number of answers: 23,736,036
=================================================================
```




### 10. Transcripts and Exons on Chromosome 1 (With Properties)
```no-wrap
=================================================================
        Find transcripts, exons, and interacting proteins located on chromosome 1. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Transcript includes $Exon $PropList1)
      (neo_P $Exon chr chr1 $PropList2)
      (neo_P $Transcript translates_to $Protein1 $PropList3)
      (neo_P $Protein2 interacts_with $Protein1 $PropList4))
    (result $Transcript $Exon $Protein1 $Protein2 $PropList1 $PropList2 $PropList3 $PropList4))

enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q9h098:protein    ()                   ()                   ()                    ...
    [score=0.173]
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    p50548:protein    ()                   ()                   ()                    ...
    [score=0.222]
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q9h720:protein    ()                   ()                   ()                    ...
    [score=0.325]
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q8n1m1:protein    ()                   ()                   ()                    ...
    [score=0.305]
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q8n1m1:protein    ()                   ()                   ()                    ...
    [score=0.311]
enst00000426406:transcript    ense00003989331:exon    q6iey1:protein    q15735:protein    ()                   ()                   ()                    ...
    [score=0.287]
enst00000332831:transcript    ense00004001351:exon    q6iey1:protein    q8n660:protein    ()                   ()                   ()                    ...
    [score=0.383]
enst00000342066:transcript    ense00003902988:exon    q96nu1:protein    q9uh03:protein    ()                   ()                   ()                    ...
    [score=0.154]
enst00000342066:transcript    ense00003915575:exon    q96nu1:protein    q9np77:protein    ()                   ()                   ()                    ...
    [score=0.237]
enst00000379370:transcript    ense00001605257:exon    o00468:protein    q6gph4:protein    ()                   ()                   ()                    ...
    [score=0.17]
enst00000379370:transcript    ense00001385905:exon    o00468:protein    p25025:protein    ()                   ()                   ()                    ...
    [score=0.18]
enst00000435064:transcript    ense00003562382:exon    q5ta45:protein    q8izn3:protein    ()                   ()                   ()                    ...
    [score=0.159]
Time limit 60 exceeded for 10. Transcripts and Exons on Chromosome 1 (With Properties)
% 1,748,200,923 inferences, 58.846 CPU in 60.000 seconds (98% CPU, 29707899 Lips)
 Last answer found: 58.85 seconds
 Number of answers: 21,007,212
=================================================================
```




### 11. IGF1 Gene Expression in Cell Lines
```no-wrap
=================================================================
        Find IGF1 gene expression in cell lines and related subclass relationships.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name "IGF1")
      (neo $Gene expressed_in $CellLine1)
      (neo $CellLine2 subclass_of $CellLine1))
    (result $Gene $CellLine1 $CellLine2))

ensg00000017427:gene                                     cl_0000015:cl                                            cl_0000018:cl
ensg00000017427:gene                                     cl_0000015:cl                                            cl_0000020:cl
ensg00000017427:gene                                     cl_0000015:cl                                            cl_0000017:cl
ensg00000017427:gene                                     cl_0002328:cl                                            cl_1000312:cl
ensg00000017427:gene                                     cl_0002328:cl                                            cl_1000349:cl
ensg00000017427:gene                                     cl_0002328:cl                                            cl_4033010:cl
ensg00000017427:gene                                     uberon_0000043:uberon                                    uberon_0003701:uberon
ensg00000017427:gene                                     uberon_0000996:uberon                                    uberon_0013524:uberon
 MeTTaLog Execution time: 0.04 seconds
% 642,781 inferences, 0.038 CPU in 0.039 seconds (99% CPU, 16815943 Lips)
 Last answer found: 0.04 seconds
 Number of answers: 3,135
=================================================================
```




### 11. IGF1 Gene Expression in Cell Lines (With Properties)
```no-wrap
=================================================================
        Find IGF1 gene expression in cell lines and related subclass relationships. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name "IGF1" $PropList1)
      (neo_P $Gene expressed_in $CellLine1 $PropList2)
      (neo_P $CellLine2 subclass_of $CellLine1 $PropList3))
    (result $Gene $CellLine1 $CellLine2 $PropList1 $PropList2 $PropList3))

ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ()                           ...
    [score=79.15,p_value=0.0033457544413306] ()
ensg00000017427:gene    cl_0000015:cl    cl_0000020:cl    ()                           ...
    [score=79.15,p_value=0.0033457544413306] ()
ensg00000017427:gene    cl_0000015:cl    cl_0000017:cl    ()                           ...
    [score=79.15,p_value=0.0033457544413306] ()
ensg00000017427:gene    cl_0002328:cl    cl_1000312:cl    ()                           ...
    [score=57.27,p_value=0.024552789258141] ()
ensg00000017427:gene    cl_0002328:cl    cl_1000349:cl    ()                           ...
    [score=57.27,p_value=0.024552789258141] ()
ensg00000017427:gene    cl_0002328:cl    cl_4033010:cl    ()                           ...
    [score=57.27,p_value=0.024552789258141] ()
ensg00000017427:gene    uberon_0000043:uberon    uberon_0003701:uberon    ()                           ...
    [score=92.51,p_value=0.0016728772206653] ()
ensg00000017427:gene    uberon_0000996:uberon    uberon_0013524:uberon    ()                           ...
    [score=71.39,p_value=0.0022196102432029] ()
 MeTTaLog Execution time: 0.03 seconds
% 688,604 inferences, 0.027 CPU in 0.027 seconds (99% CPU, 25608848 Lips)
 Last answer found: 0.03 seconds
 Number of answers: 3,135
=================================================================
```




### 12. IGF1 Gene Regulation by SNP Activity
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "IGF1"))
      (neo $SNP activity_by_contact $Gene))
    (result $SNP $Gene))

rs1019731:snp                                                                         ensg00000017427:gene
rs10507135:snp                                                                        ensg00000017427:gene
rs10507135:snp                                                                        ensg00000017427:gene
rs114119866:snp                                                                       ensg00000017427:gene
rs114119866:snp                                                                       ensg00000017427:gene
rs114160584:snp                                                                       ensg00000017427:gene
rs184405224:snp                                                                       ensg00000017427:gene
rs10163105:snp                                                                        ensg00000140443:gene
 MeTTaLog Execution time: 0.17 seconds
% 4,515,233 inferences, 0.169 CPU in 0.171 seconds (99% CPU, 26659351 Lips)
 Last answer found: 0.06 seconds
 Number of answers: 4,285
=================================================================
```




### 12. IGF1 Gene Regulation by SNP Activity (With Properties)
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "IGF1") $PropList1)
      (neo_P $SNP activity_by_contact $Gene $PropList2))
    (result $SNP $Gene $PropList1 $PropList2))

rs1019731:snp    ensg00000017427:gene    ()                                         [score=0.01959,biological_context='CL_0000103']
rs10507135:snp    ensg00000017427:gene    ()                                         [score=0.027607,biological_context='UBERON_0000992']
rs10507135:snp    ensg00000017427:gene    ()                                         [score=0.030463,biological_context='CLO_0007599']
rs114119866:snp    ensg00000017427:gene    ()                                         [score=0.085288,biological_context='CL_0001070']
rs114119866:snp    ensg00000017427:gene    ()                                         [score=0.056618,biological_context='CL_0000448']
rs114160584:snp    ensg00000017427:gene    ()                                         [score=0.020362,biological_context='CLO_0037116']
rs184405224:snp    ensg00000017427:gene    ()                                         [score=0.020066,biological_context='CLO_0001605']
rs10163105:snp    ensg00000140443:gene    ()                                         [score=0.022901,biological_context='CLO_0037281']
 MeTTaLog Execution time: 0.15 seconds
% 4,550,604 inferences, 0.146 CPU in 0.147 seconds (99% CPU, 31074700 Lips)
 Last answer found: 0.03 seconds
 Number of answers: 4,285
=================================================================
```




### 13. IGF1 Gene Interactions and Regulations
```no-wrap
=================================================================
        Find IGF1 gene interactions, regulations, and pathways including transcripts and proteins.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "IGF1"))
      (neo $Gene expressed_in $CellLine1)
      (neo $CellLine2 subclass_of $CellLine1)
      (neo $RegulatingGene regulates $Gene)
      (neo $RegulatingGene transcribed_to $Transcript)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Gene $CellLine1 $CellLine2 $RegulatingGene $Transcript $Protein1 $Protein2))

ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q96ru2:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q16654:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p11233:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p18146:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q13207:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p15173:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q8n302:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q92841:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000091831:gene    enst00000338799:transcript    p03372:protein    p21754:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000100888:gene    enst00000645929:transcript    q9hck8:protein    p49459:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000120948:gene    enst00000240185:transcript    q13148:protein    p02766:protein
ensg00000017427:gene    cl_0000015:cl    cl_0000408:cl    ensg00000068305:gene    enst00000558812:transcript    q02078:protein    q9c0f3:protein
Time limit 60 exceeded for 13. IGF1 Gene Interactions and Regulations
% 1,469,817,160 inferences, 58.899 CPU in 60.000 seconds (98% CPU, 24954953 Lips)
 Last answer found: 58.90 seconds
 Number of answers: 18,665,440
=================================================================
```




### 13. IGF1 Gene Interactions and Regulations (With Properties)
```no-wrap
=================================================================
        Find IGF1 gene interactions, regulations, and pathways including transcripts and proteins. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "IGF1") $PropList1)
      (neo_P $Gene expressed_in $CellLine1 $PropList2)
      (neo_P $CellLine2 subclass_of $CellLine1 $PropList3)
      (neo_P $RegulatingGene regulates $Gene $PropList4)
      (neo_P $RegulatingGene transcribed_to $Transcript $PropList5)
      (neo_P $Transcript translates_to $Protein1 $PropList6)
      (neo_P $Protein2 interacts_with $Protein1 $PropList7))
    (result $Gene $CellLine1 $CellLine2 $RegulatingGene $Transcript $Protein1 $Protein2 $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6 $PropList7))

ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q96ru2:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.177]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q16654:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.157]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p11233:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.178]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p18146:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.22]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q13207:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.161]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p15173:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.189]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q8n302:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.157]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q92841:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.173]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000091831:gene    enst00000338799:transcript    p03372:protein    p21754:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285','pubmed:27924024','pubmed:17202159'],databases=['GTRD','ReMap','TRED'],evidence_type=small_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator'] ()          ()           ...
    [score=0.215]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000100888:gene    enst00000645929:transcript    q9hck8:protein    p49459:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285'],databases=['ReMap'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()          ()           ...
    [score=0.25]
ensg00000017427:gene    cl_0000015:cl    cl_0000018:cl    ensg00000120948:gene    enst00000240185:transcript    q13148:protein    p02766:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285'],databases=['ReMap'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()          ()           ...
    [score=0.272]
ensg00000017427:gene    cl_0000015:cl    cl_0000408:cl    ensg00000068305:gene    enst00000558812:transcript    q02078:protein    q9c0f3:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285'],databases=['ReMap'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()          ()           ...
    [score=0.159]
Time limit 60 exceeded for 13. IGF1 Gene Interactions and Regulations (With Properties)
% 1,824,499,179 inferences, 58.917 CPU in 60.000 seconds (98% CPU, 30967098 Lips)
 Last answer found: 58.92 seconds
 Number of answers: 17,089,701
=================================================================
```




### 14. Pathway Associations for SNAP25
```no-wrap
=================================================================
        Locate SNAP25 in pathways with other genes, including cases where the other gene may be SNAP25 itself.

  (match &neo4j_out_v3
    (,
      (neo $Gene1 gene_name
        (startsWith "SNAP25"))
      (neo $Gene1 genes_pathways $Pathway)
      (neo $Gene2 genes_pathways $Pathway))
    (result $Gene1 $Pathway $Gene2))

ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensg00000005379:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensg00000008056:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensg00000010379:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensp00000355920:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensp00000355930:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensp00000360549:gene
ensg00000132639:gene                                     'r-hsa-112315':pathway                                   ensg00000117676:gene
ensg00000132639:gene                                     'r-hsa-112315':pathway                                   ensp00000335592:gene
ensg00000132639:gene                                     'r-hsa-1280215':pathway                                  ensp00000445409:gene
 MeTTaLog Execution time: 0.68 seconds
% 10,400,748 inferences, 0.676 CPU in 0.862 seconds (78% CPU, 15378242 Lips)
 Last answer found: 0.56 seconds
 Number of answers: 97,865
=================================================================
```




### 14. Pathway Associations for SNAP25 (With Properties)
```no-wrap
=================================================================
        Locate SNAP25 in pathways with other genes, including cases where the other gene may be SNAP25 itself. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene1 gene_name
        (startsWith "SNAP25") $PropList1)
      (neo_P $Gene1 genes_pathways $Pathway $PropList2)
      (neo_P $Gene2 genes_pathways $Pathway $PropList3))
    (result $Gene1 $Pathway $Gene2 $PropList1 $PropList2 $PropList3))

ensg00000132639:gene    'r-hsa-112310':pathway    ensg00000005379:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensg00000008056:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensg00000010379:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensp00000355920:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensp00000355930:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensp00000360549:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112315':pathway    ensg00000117676:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112315':pathway    ensp00000335592:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-1280215':pathway    ensp00000445409:gene    ()                          ()                          ()
 MeTTaLog Execution time: 0.39 seconds
% 11,577,965 inferences, 0.389 CPU in 0.391 seconds (100% CPU, 29725695 Lips)
 Last answer found: 0.28 seconds
 Number of answers: 97,865
=================================================================
```




### 14a. Pathway Associations for SNAP25 - Distinct Genes
```no-wrap
=================================================================
        Locate SNAP25 in pathways with other genes, ensuring that SNAP25 and other genes are distinct.

  (match &neo4j_out_v3
    (,
      (neo $Gene1 gene_name
        (startsWith "SNAP25"))
      (neo $Gene1 genes_pathways $Pathway)
      (neo $Gene2 genes_pathways $Pathway)
      (different $Gene1 $Gene2))
    (result $Gene1 $Pathway $Gene2))

ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensg00000005379:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensg00000008056:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensg00000010379:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensp00000355920:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensp00000355930:gene
ensg00000132639:gene                                     'r-hsa-112310':pathway                                   ensp00000360549:gene
ensg00000132639:gene                                     'r-hsa-112315':pathway                                   ensg00000117676:gene
ensg00000132639:gene                                     'r-hsa-112315':pathway                                   ensp00000335592:gene
ensg00000132639:gene                                     'r-hsa-1280215':pathway                                  ensp00000445409:gene
 MeTTaLog Execution time: 0.42 seconds
% 11,085,871 inferences, 0.415 CPU in 0.418 seconds (99% CPU, 26702821 Lips)
 Last answer found: 0.31 seconds
 Number of answers: 97,865
=================================================================
```




### 14a. Pathway Associations for SNAP25 - Distinct Genes (With Properties)
```no-wrap
=================================================================
        Locate SNAP25 in pathways with other genes, ensuring that SNAP25 and other genes are distinct. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene1 gene_name
        (startsWith "SNAP25") $PropList1)
      (neo_P $Gene1 genes_pathways $Pathway $PropList2)
      (neo_P $Gene2 genes_pathways $Pathway $PropList3)
      (different $Gene1 $Gene2))
    (result $Gene1 $Pathway $Gene2 $PropList1 $PropList2 $PropList3))

ensg00000132639:gene    'r-hsa-112310':pathway    ensg00000005379:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensg00000008056:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensg00000010379:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensp00000355920:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensp00000355930:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112310':pathway    ensp00000360549:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112315':pathway    ensg00000117676:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-112315':pathway    ensp00000335592:gene    ()                          ()                          ()
ensg00000132639:gene    'r-hsa-1280215':pathway    ensp00000445409:gene    ()                          ()                          ()
 MeTTaLog Execution time: 0.42 seconds
% 12,263,088 inferences, 0.422 CPU in 0.427 seconds (99% CPU, 29054059 Lips)
 Last answer found: 0.31 seconds
 Number of answers: 97,865
=================================================================
```
% 16,374,058,425 inferences, 652.128 CPU in 664.588 seconds (98% CPU, 25108663 Lips)
true.

89 ?-
|
|    make.
% MW-NOTE: time(use_mw_directory(neo4j_out_v3_mw)).
% MW-NOTE: time(load_mw_data(neo4j_out_v3_mw)).
% MW-NOTE: time(load_mw_info(neo4j_out_v3_mw)).
% MW-NOTE: time(show_linked).
% MW-NOTE: time(show_linked(1)).
% MW-NOTE: time(show_three).
% MW-NOTE: time(link_mw_data(neo4j_out_v3_mw)).
% MW-NOTE: time(mw_stats).
% MW-NOTE: time(sample_query).
true.

90 ?- time(sample_query(16)).




### 12. IGF1 Gene Regulation by SNP Activity
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "IGF1"))
      (neo $SNP activity_by_contact $Gene))
    (result $SNP $Gene))

rs1019731:snp                                                                         ensg00000017427:gene
rs10507135:snp                                                                        ensg00000017427:gene
rs10507135:snp                                                                        ensg00000017427:gene
rs114119866:snp                                                                       ensg00000017427:gene
rs114119866:snp                                                                       ensg00000017427:gene
rs114160584:snp                                                                       ensg00000017427:gene
rs184405224:snp                                                                       ensg00000017427:gene
rs10163105:snp                                                                        ensg00000140443:gene
 MeTTaLog Execution time: 0.15 seconds
% 4,515,233 inferences, 0.148 CPU in 0.149 seconds (99% CPU, 30450064 Lips)
 Last answer found: 0.03 seconds
 Number of answers: 4,285
=================================================================
```




### 12. IGF1 Gene Regulation by SNP Activity (With Properties)
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "IGF1") $PropList1)
      (neo_P $SNP activity_by_contact $Gene $PropList2))
    (result $SNP $Gene $PropList1 $PropList2))

rs1019731:snp    ensg00000017427:gene    ()                                         [score=0.01959,biological_context='CL_0000103']
rs10507135:snp    ensg00000017427:gene    ()                                         [score=0.027607,biological_context='UBERON_0000992']
rs10507135:snp    ensg00000017427:gene    ()                                         [score=0.030463,biological_context='CLO_0007599']
rs114119866:snp    ensg00000017427:gene    ()                                         [score=0.085288,biological_context='CL_0001070']
rs114119866:snp    ensg00000017427:gene    ()                                         [score=0.056618,biological_context='CL_0000448']
rs114160584:snp    ensg00000017427:gene    ()                                         [score=0.020362,biological_context='CLO_0037116']
rs184405224:snp    ensg00000017427:gene    ()                                         [score=0.020066,biological_context='CLO_0001605']
rs10163105:snp    ensg00000140443:gene    ()                                         [score=0.022901,biological_context='CLO_0037281']
 MeTTaLog Execution time: 0.15 seconds
% 4,550,604 inferences, 0.146 CPU in 0.146 seconds (99% CPU, 31262157 Lips)
 Last answer found: 0.03 seconds
 Number of answers: 4,285
=================================================================
```
% 9,071,449 inferences, 0.296 CPU in 0.297 seconds (99% CPU, 30698165 Lips)
true.

91 ?-
|    make.
% MW-NOTE: time(use_mw_directory(neo4j_out_v3_mw)).
% MW-NOTE: time(load_mw_data(neo4j_out_v3_mw)).
% MW-NOTE: time(load_mw_info(neo4j_out_v3_mw)).
% MW-NOTE: time(show_linked).
% MW-NOTE: time(show_linked(1)).
% MW-NOTE: time(show_three).
% MW-NOTE: time(link_mw_data(neo4j_out_v3_mw)).
% MW-NOTE: time(mw_stats).
% MW-NOTE: time(sample_query).
true.

92 ?- time(sample_query(16)).




### 12. IGF1 Gene Regulation by SNP Activity
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "IGF1"))
      (neo $SNP activity_by_contact $Gene))
    (result $SNP $Gene))

rs1019731:snp                                                                         ensg00000017427:gene
rs10507135:snp                                                                        ensg00000017427:gene
rs10507135:snp                                                                        ensg00000017427:gene
rs114119866:snp                                                                       ensg00000017427:gene
rs114119866:snp                                                                       ensg00000017427:gene
rs114160584:snp                                                                       ensg00000017427:gene
rs184405224:snp                                                                       ensg00000017427:gene
rs10163105:snp                                                                        ensg00000140443:gene
 MeTTaLog Execution time: 0.15 seconds
% 4,515,233 inferences, 0.153 CPU in 0.156 seconds (98% CPU, 29471550 Lips)
 Last answer found: 0.03 seconds
 Number of answers: 4,285
=================================================================
```




### 12. IGF1 Gene Regulation by SNP Activity (With Properties)
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "IGF1") $PropList1)
      (neo_P $SNP activity_by_contact $Gene $PropList2))
    (result $SNP $Gene $PropList1 $PropList2))

rs1019731:snp    ensg00000017427:gene    ()                                         [score=0.01959,biological_context='CL_0000103']
rs10507135:snp    ensg00000017427:gene    ()                                         [score=0.027607,biological_context='UBERON_0000992']
rs10507135:snp    ensg00000017427:gene    ()                                         [score=0.030463,biological_context='CLO_0007599']
rs114119866:snp    ensg00000017427:gene    ()                                         [score=0.085288,biological_context='CL_0001070']
rs114119866:snp    ensg00000017427:gene    ()                                         [score=0.056618,biological_context='CL_0000448']
rs114160584:snp    ensg00000017427:gene    ()                                         [score=0.020362,biological_context='CLO_0037116']
rs184405224:snp    ensg00000017427:gene    ()                                         [score=0.020066,biological_context='CLO_0001605']
rs10163105:snp    ensg00000140443:gene    ()                                         [score=0.022901,biological_context='CLO_0037281']
 MeTTaLog Execution time: 0.15 seconds
% 4,550,604 inferences, 0.145 CPU in 0.147 seconds (99% CPU, 31288639 Lips)
 Last answer found: 0.03 seconds
 Number of answers: 4,285
=================================================================
```
% 9,071,449 inferences, 0.300 CPU in 0.304 seconds (99% CPU, 30262393 Lips)
true.

93 ?- time(sample_query(17)).




### 13. IGF1 Gene Interactions and Regulations
```no-wrap
=================================================================
        Find IGF1 gene interactions, regulations, and pathways including transcripts and proteins.

  (match &neo4j_out_v3
    (,
      (neo $Gene gene_name
        (startsWith "IGF1"))
      (neo $Gene expressed_in $CL1:cl)
      (neo $CL2:cl subclass_of $CL1:cl)
      (neo $Gene2Regulating regulates $Gene)
      (neo $Gene2Regulating transcribed_to $Transcript)
      (neo $Transcript translates_to $Protein1)
      (neo $Protein2 interacts_with $Protein1))
    (result $Gene $CL1 $CL2 $Gene2Regulating $Transcript $Protein1 $Protein2))

ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q96ru2:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q16654:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p11233:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p18146:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q13207:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p15173:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q8n302:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q92841:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000091831:gene    enst00000338799:transcript    p03372:protein    p21754:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000100888:gene    enst00000645929:transcript    q9hck8:protein    p49459:protein
ensg00000017427:gene    cl_0000015              cl_0000018              ensg00000120948:gene    enst00000240185:transcript    q13148:protein    p02766:protein
ensg00000017427:gene    cl_0000015              cl_0000408              ensg00000068305:gene    enst00000558812:transcript    q02078:protein    q9c0f3:protein
Time limit 60 exceeded for 13. IGF1 Gene Interactions and Regulations
% 1,541,991,089 inferences, 58.990 CPU in 60.000 seconds (98% CPU, 26139859 Lips)
 Last answer found: 58.99 seconds
 Number of answers: 19,676,097
=================================================================
```




### 13. IGF1 Gene Interactions and Regulations (With Properties)
```no-wrap
=================================================================
        Find IGF1 gene interactions, regulations, and pathways including transcripts and proteins. (With Properties)

  (match &neo4j_out_v3
    (,
      (neo_P $Gene gene_name
        (startsWith "IGF1") $PropList1)
      (neo_P $Gene expressed_in $CL1:cl $PropList2)
      (neo_P $CL2:cl subclass_of $CL1:cl $PropList3)
      (neo_P $Gene2Regulating regulates $Gene $PropList4)
      (neo_P $Gene2Regulating transcribed_to $Transcript $PropList5)
      (neo_P $Transcript translates_to $Protein1 $PropList6)
      (neo_P $Protein2 interacts_with $Protein1 $PropList7))
    (result $Gene $CL1 $CL2 $Gene2Regulating $Transcript $Protein1 $Protein2 $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6 $PropList7))

ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q96ru2:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.177]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q16654:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.157]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p11233:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.178]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p18146:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.22]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q13207:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.161]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    p15173:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.189]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q8n302:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.157]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000139687:gene    enst00000267163:transcript    p06400:protein    q92841:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ()          ()           ...
    [score=0.173]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000091831:gene    enst00000338799:transcript    p03372:protein    p21754:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285','pubmed:27924024','pubmed:17202159'],databases=['GTRD','ReMap','TRED'],evidence_type=small_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator'] ()          ()           ...
    [score=0.215]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000100888:gene    enst00000645929:transcript    q9hck8:protein    p49459:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285'],databases=['ReMap'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()          ()           ...
    [score=0.25]
ensg00000017427:gene    cl_0000015  cl_0000018  ensg00000120948:gene    enst00000240185:transcript    q13148:protein    p02766:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285'],databases=['ReMap'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()          ()           ...
    [score=0.272]
ensg00000017427:gene    cl_0000015  cl_0000408  ensg00000068305:gene    enst00000558812:transcript    q02078:protein    q9c0f3:protein    ()           ...
    [score=79.15,p_value=0.0033457544413306] ()           ...
    [evidence=['pubmed:29126285'],databases=['ReMap'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()          ()           ...
    [score=0.159]
Time limit 60 exceeded for 13. IGF1 Gene Interactions and Regulations (With Properties)
% 1,837,445,533 inferences, 58.978 CPU in 60.000 seconds (98% CPU, 31154550 Lips)
 Last answer found: 58.98 seconds
 Number of answers: 17,272,372
=================================================================

```

## % 3,379,448,545 inferences, 117.970 CPU in 120.002 seconds (98% CPU, 28646727 Lips)

```no-wrap
root@HOSTAGE:~/metta-wam/libraries/bioAS# wc -l neo4j_out_v3_mw/global_links/*.pl
    8335666 neo4j_out_v3_mw/global_links/activity_by_contact.pl
    3121079 neo4j_out_v3_mw/global_links/associated_with.pl
      46413 neo4j_out_v3_mw/global_links/belongs_to.pl
   10489066 neo4j_out_v3_mw/global_links/binds_to.pl
        359 neo4j_out_v3_mw/global_links/capable_of.pl
       2692 neo4j_out_v3_mw/global_links/child_pathway_of.pl
    2407227 neo4j_out_v3_mw/global_links/chromatin_state.pl
   23396248 neo4j_out_v3_mw/global_links/closest_gene.pl
  257586456 neo4j_out_v3_mw/global_links/coexpressed_with.pl
     107700 neo4j_out_v3_mw/global_links/downstream_of.pl
   13117246 neo4j_out_v3_mw/global_links/eqtl_association.pl
    7326858 neo4j_out_v3_mw/global_links/expressed_in.pl
     889906 neo4j_out_v3_mw/global_links/genes_pathways.pl
     288639 neo4j_out_v3_mw/global_links/go_gene_product.pl
     618298 neo4j_out_v3_mw/global_links/histone_modification.pl
      23760 neo4j_out_v3_mw/global_links/in_dnase_i_hotspot.pl
      18957 neo4j_out_v3_mw/global_links/in_tad_region.pl
    1649477 neo4j_out_v3_mw/global_links/includes.pl
   13247695 neo4j_out_v3_mw/global_links/interacts_with.pl
     177707 neo4j_out_v3_mw/global_links/located_in.pl
       2692 neo4j_out_v3_mw/global_links/parent_pathway_of.pl
       1304 neo4j_out_v3_mw/global_links/part_of.pl
    6394608 neo4j_out_v3_mw/global_links/regulates.pl
     329233 neo4j_out_v3_mw/global_links/subclass_of.pl
      96375 neo4j_out_v3_mw/global_links/tfbs_snp.pl
     252836 neo4j_out_v3_mw/global_links/transcribed_from.pl
     252836 neo4j_out_v3_mw/global_links/transcribed_to.pl
      50073 neo4j_out_v3_mw/global_links/translates_to.pl
      50073 neo4j_out_v3_mw/global_links/translation_of.pl
     107707 neo4j_out_v3_mw/global_links/upstream_of.pl
  350,89,186 total
root@HOSTAGE:~/metta-wam/libraries/bioAS#


```




```no-wrap

86 ?- time(findall(X,sample_query(X),L)),length(L,R).
% 3,492,804 inferences, 0.165 CPU in 0.165 seconds (100% CPU, 21213366 Lips)
L = [[chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-194441'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-428359'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-429914'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-450531'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72086'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72203'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72306'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72312'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-75067'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-75072'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-927802'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-9836573'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-194441'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-428359'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-429914'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-450531'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72086'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72203'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72306'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72312'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-75067'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-75072'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-927802'], [chr3_185821121_185821180_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-9836573'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-194441'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-428359'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-429914'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-450531'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72086'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72203'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72306'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-72312'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-75067'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-75072'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-927802'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185821121_185821180_grch38, 'r-hsa-8953854', 'r-hsa-9836573'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-194441'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-428359'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-429914'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-450531'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72086'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72203'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72306'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-72312'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-75067'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-75072'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-927802'], [chr3_185825028_185825087_grch38, ensg00000073792, chr3_185825028_185825087_grch38, 'r-hsa-8953854', 'r-hsa-9836573'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249', 'r-hsa-1222556'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249', 'r-hsa-166658'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249', 'r-hsa-168643'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249', 'r-hsa-168898'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249', 'r-hsa-168928'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249', 'r-hsa-1834949'], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38, 'r-hsa-168249'|...], [chr6_159969036_159969095_grch38, ensg00000197081, chr6_159969036_159969095_grch38|...], [chr6_159969036_159969095_grch38, ensg00000197081|...], [chr6_159969036_159969095_grch38|...], [...|...]|...],
R = 7547.

87 ?- listing(sample_query).
sample_query([Promoter, Gene, Enhancer, Pathway, ChildPathway]) :-
    neo(Gene, gene_name, fnL(atom_contains, _, "IGF2")),
    neo(Promoter, associated_with, Gene),
    neo(Enhancer, associated_with, Gene),
    neo(Gene, genes_pathways, Pathway),
    neo(ChildPathway, child_pathway_of, Pathway).



```



```no-wrap
root@HOSTAGE:~/metta-wam/libraries/bioAS# ./prepare.sh neo4j_out_v3.rdf 100M
./prepare.sh2: line 1: #!/bin/bash: No such file or directory
🚀 Splitting 'neo4j_out_v3.rdf' (~25 GB) into ~100M chunks in 'neo4j_out_v3_split_rdf/'...
🎉 Total files created: 258 in 3m 51s.4j_out_v3_part_000258.rdf 🦅 (elapsed: 3m 51s)                       ✅

```


```no-wrap

╒═════════╤═════════╕
│nodeCount│relCount │
╞═════════╪═════════╡
│72801737 │110427747

root@HOSTAGE:~/metta-wam/libraries/bioAS# swipl -l gene_queries1.pl
\U0001F680 Mounting persistence at datastore_neo4j_out_v3
% Restoring 1 snapshots using 1 concurrent workers
datastore_neo4j_out_v3................................... 1 of 1 graphs
% Loaded 1 graphs (6,154,340 triples) in 7.56 sec. (98% CPU = 7.44 sec.)
?-load_rdf_db_directory
Welcome to SWI-Prolog (threaded, 64 bits, version 9.3.19-42-g7a8a2ecb8-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- load_rdf_db_directory.
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000001.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2548779: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000001.rdf" in 13.29 sec; 1,209,940 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000001.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000002.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2576329: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000002.rdf" in 11.83 sec; 2,353,547 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000002.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000003.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2501798: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000003.rdf" in 15.76 sec; 3,988,378 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000003.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000004.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2453529: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000004.rdf" in 13.69 sec; 5,062,555 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000004.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000005.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1959983: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000005.rdf" in 13.81 sec; 6,154,340 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000005.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000006.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2794979: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000006.rdf" in 16.63 sec; 7,436,905 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000006.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000007.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2952934: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000007.rdf" in 18.14 sec; 8,698,490 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000007.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000008.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953957: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000008.rdf" in 20.15 sec; 9,962,340 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000008.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000009.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2957855: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000009.rdf" in 21.65 sec; 11,233,370 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000009.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000010.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954833: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000010.rdf" in 22.96 sec; 12,499,290 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000010.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000011.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2958597: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000011.rdf" in 23.94 sec; 13,764,996 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000011.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000012.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2957501: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000012.rdf" in 25.32 sec; 15,032,432 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000012.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000013.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2939295: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000013.rdf" in 27.42 sec; 16,294,358 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000013.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000014.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2765182: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000014.rdf" in 31.45 sec; 17,827,494 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000014.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000015.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2575775: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000015.rdf" in 37.54 sec; 19,739,519 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000015.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000016.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2582887: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000016.rdf" in 40.14 sec; 21,659,187 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000016.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000017.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2615587: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000017.rdf" in 45.75 sec; 23,593,456 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000017.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000018.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2588540: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000018.rdf" in 44.77 sec; 25,514,775 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000018.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000019.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2550593: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000019.rdf" in 44.79 sec; 27,372,584 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000019.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000020.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2538726: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000020.rdf" in 46.74 sec; 29,236,058 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000020.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000021.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2570453: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000021.rdf" in 49.37 sec; 31,081,849 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000021.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000022.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2575707: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000022.rdf" in 50.25 sec; 32,966,224 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000022.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000023.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2595071: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000023.rdf" in 54.11 sec; 34,857,390 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000023.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000024.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2567272: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000024.rdf" in 54.92 sec; 36,764,950 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000024.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000025.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2582425: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000025.rdf" in 57.38 sec; 38,659,868 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000025.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000026.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2611280: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000026.rdf" in 58.78 sec; 40,604,704 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000026.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000027.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2549700: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000027.rdf" in 59.19 sec; 42,508,000 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000027.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000028.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2685227: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000028.rdf" in 63.09 sec; 44,534,100 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000028.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000029.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746402: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000029.rdf" in 70.74 sec; 46,659,124 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000029.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000030.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740260: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000030.rdf" in 74.71 sec; 48,839,776 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000030.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000031.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740706: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000031.rdf" in 73.73 sec; 51,017,007 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000031.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000032.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2749937: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000032.rdf" in 75.03 sec; 53,172,575 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000032.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000033.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744658: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000033.rdf" in 75.69 sec; 55,339,763 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000033.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000034.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738230: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000034.rdf" in 78.39 sec; 57,523,317 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000034.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000035.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738711: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000035.rdf" in 80.95 sec; 59,706,017 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000035.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000036.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739529: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000036.rdf" in 81.99 sec; 61,889,799 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000036.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000037.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738978: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000037.rdf" in 83.78 sec; 64,073,825 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000037.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000038.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746879: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000038.rdf" in 84.36 sec; 66,221,400 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000038.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000039.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738258: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000039.rdf" in 90.17 sec; 68,407,214 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000039.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000040.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738191: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000040.rdf" in 90.91 sec; 70,592,454 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000040.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000041.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737495: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000041.rdf" in 94.01 sec; 72,778,204 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000041.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000042.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748563: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000042.rdf" in 92.08 sec; 74,859,342 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000042.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000043.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2751172: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000043.rdf" in 95.22 sec; 76,929,727 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000043.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000044.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748393: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000044.rdf" in 100.04 sec; 79,036,921 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000044.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000045.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2750807: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000045.rdf" in 101.62 sec; 81,155,676 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000045.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000046.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748454: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000046.rdf" in 103.19 sec; 83,313,487 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000046.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000047.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748432: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000047.rdf" in 110.62 sec; 85,487,410 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000047.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000048.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2747401: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000048.rdf" in 113.66 sec; 87,663,959 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000048.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000049.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2751355: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000049.rdf" in 114.90 sec; 89,784,588 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000049.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000050.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2797650: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000050.rdf" in 84.98 sec; 90,870,918 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000050.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000051.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2562947: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000051.rdf" in 86.75 sec; 92,083,016 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000051.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000052.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2387168: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000052.rdf" in 93.45 sec; 93,664,807 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000052.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000053.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2407436: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000053.rdf" in 96.07 sec; 95,287,935 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000053.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000054.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2397715: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000054.rdf" in 99.96 sec; 96,894,283 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000054.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000055.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2392829: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000055.rdf" in 103.98 sec; 98,526,528 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000055.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000056.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2345859: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000056.rdf" in 102.19 sec; 100,045,101 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000056.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000057.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2355365: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000057.rdf" in 101.87 sec; 101,576,609 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000057.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000058.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2302163: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000058.rdf" in 100.41 sec; 103,087,693 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000058.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000059.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1862041: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000059.rdf" in 88.40 sec; 104,247,194 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000059.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000060.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1816027: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000060.rdf" in 87.89 sec; 105,391,578 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000060.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000061.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1801787: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000061.rdf" in 87.82 sec; 106,544,915 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000061.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000062.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1854249: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000062.rdf" in 91.16 sec; 107,700,380 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000062.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000063.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1832916: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000063.rdf" in 103.55 sec; 109,095,072 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000063.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000064.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2384339: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000064.rdf" in 115.33 sec; 110,786,723 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000064.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000065.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2610611: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000065.rdf" in 106.94 sec; 111,884,992 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000065.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000066.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2721902: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000066.rdf" in 101.11 sec; 112,885,371 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000066.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000067.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2527651: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000067.rdf" in 123.45 sec; 114,695,140 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000067.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000068.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2549013: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000068.rdf" in 128.94 sec; 116,532,304 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000068.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000069.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2498826: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000069.rdf" in 129.85 sec; 118,342,051 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000069.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000070.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2442119: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000070.rdf" in 135.29 sec; 120,123,900 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000070.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000071.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2486273: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000071.rdf" in 134.40 sec; 121,952,684 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000071.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000072.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2503999: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000072.rdf" in 134.39 sec; 123,775,773 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000072.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000073.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2553487: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000073.rdf" in 135.21 sec; 125,632,267 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000073.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000074.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2290352: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000074.rdf" in 127.96 sec; 127,376,517 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000074.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000075.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1466343: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000075.rdf" in 107.02 sec; 128,795,388 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000075.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000076.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1468222: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000076.rdf" in 110.00 sec; 130,212,970 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000076.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000077.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1481816: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000077.rdf" in 107.67 sec; 131,622,477 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000077.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000078.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1482675: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000078.rdf" in 104.33 sec; 133,031,580 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000078.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000079.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1484325: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000079.rdf" in 103.91 sec; 134,450,568 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000079.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000080.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1485197: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000080.rdf" in 103.83 sec; 135,888,209 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000080.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000081.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1487662: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000081.rdf" in 104.60 sec; 137,324,535 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000081.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000082.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1501023: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000082.rdf" in 107.57 sec; 138,752,820 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000082.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000083.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1502162: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000083.rdf" in 105.27 sec; 140,180,315 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000083.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000084.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1489420: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000084.rdf" in 102.33 sec; 141,604,296 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000084.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000085.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1592013: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000085.rdf" in 105.12 sec; 143,032,152 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000085.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000086.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2661824: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000086.rdf" in 123.77 sec; 144,528,590 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000086.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000087.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2674382: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000087.rdf" in 124.40 sec; 146,017,963 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000087.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000088.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2657482: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000088.rdf" in 119.59 sec; 147,512,081 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000088.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000089.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2670784: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000089.rdf" in 119.39 sec; 149,003,799 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000089.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000090.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2674474: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000090.rdf" in 120.70 sec; 150,495,556 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000090.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000091.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2059357: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000091.rdf" in 112.27 sec; 151,949,570 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000091.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000092.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1474032: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000092.rdf" in 102.25 sec; 153,363,620 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000092.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000093.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1483821: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000093.rdf" in 104.40 sec; 154,771,865 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000093.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000094.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1482000: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000094.rdf" in 106.87 sec; 156,181,094 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000094.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000095.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2119884: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000095.rdf" in 124.78 sec; 157,916,126 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000095.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000096.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2231907: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000096.rdf" in 128.13 sec; 159,698,066 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000096.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000097.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2258508: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000097.rdf" in 135.59 sec; 161,526,731 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000097.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000098.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2255090: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000098.rdf" in 137.52 sec; 163,342,852 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000098.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000099.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2147519: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000099.rdf" in 132.25 sec; 164,960,349 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000099.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000100.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2222666: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000100.rdf" in 139.11 sec; 166,721,795 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000100.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000101.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2092516: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000101.rdf" in 123.47 sec; 168,044,889 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000101.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000102.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1470481: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000102.rdf" in 117.51 sec; 169,439,437 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000102.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000103.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1476774: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000103.rdf" in 114.09 sec; 170,789,704 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000103.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000104.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1440867: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000104.rdf" in 114.82 sec; 172,161,463 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000104.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000105.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1420623: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000105.rdf" in 115.08 sec; 173,520,769 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000105.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000106.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1450451: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000106.rdf" in 116.45 sec; 174,851,649 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000106.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000107.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1486151: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000107.rdf" in 111.31 sec; 176,060,699 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000107.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000108.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1618390: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000108.rdf" in 114.85 sec; 177,338,364 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000108.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000109.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1405975: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000109.rdf" in 120.01 sec; 178,731,949 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000109.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000110.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406609: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000110.rdf" in 117.02 sec; 180,125,076 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000110.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000111.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406770: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000111.rdf" in 114.90 sec; 181,518,451 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000111.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000112.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406858: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000112.rdf" in 115.27 sec; 182,912,118 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000112.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000113.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406532: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000113.rdf" in 114.90 sec; 184,305,930 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000113.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000114.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1689777: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000114.rdf" in 129.91 sec; 185,800,587 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000114.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000115.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2446457: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000115.rdf" in 153.34 sec; 187,671,482 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000115.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000116.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2439242: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000116.rdf" in 152.75 sec; 189,507,112 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000116.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000117.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2478844: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000117.rdf" in 157.10 sec; 191,410,862 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000117.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000118.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2352648: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000118.rdf" in 154.57 sec; 193,231,958 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000118.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000119.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2447799: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000119.rdf" in 164.83 sec; 195,121,736 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000119.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000120.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2499532: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000120.rdf" in 176.18 sec; 197,062,425 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000120.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000121.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2576716: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000121.rdf" in 189.07 sec; 198,967,345 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000121.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000122.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742332: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000122.rdf" in 193.40 sec; 200,919,072 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000122.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000123.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742050: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000123.rdf" in 202.21 sec; 202,981,469 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000123.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000124.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740993: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000124.rdf" in 195.95 sec; 205,061,523 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000124.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000125.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738718: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000125.rdf" in 192.29 sec; 207,166,110 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000125.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000126.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2741473: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000126.rdf" in 202.52 sec; 209,284,249 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000126.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000127.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738870: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000127.rdf" in 208.04 sec; 211,418,961 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000127.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000128.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739899: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000128.rdf" in 214.57 sec; 213,551,366 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000128.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000129.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740369: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000129.rdf" in 223.66 sec; 215,682,051 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000129.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000130.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738682: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000130.rdf" in 224.34 sec; 217,810,209 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000130.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000131.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739227: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000131.rdf" in 227.88 sec; 219,928,544 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000131.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000132.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739583: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000132.rdf" in 221.41 sec; 222,044,931 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000132.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000133.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740113: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000133.rdf" in 233.70 sec; 224,155,431 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000133.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000134.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738269: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000134.rdf" in 233.69 sec; 226,236,796 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000134.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000135.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740555: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000135.rdf" in 229.89 sec; 228,287,534 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000135.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000136.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2741914: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000136.rdf" in 222.51 sec; 230,323,635 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000136.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000137.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743465: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000137.rdf" in 214.83 sec; 232,250,230 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000137.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000138.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742089: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000138.rdf" in 221.16 sec; 234,283,863 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000138.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000139.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737441: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000139.rdf" in 223.54 sec; 236,425,240 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000139.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000140.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739180: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000140.rdf" in 226.81 sec; 238,567,186 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000140.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000141.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739111: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000141.rdf" in 234.81 sec; 240,708,658 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000141.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000142.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737576: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000142.rdf" in 236.23 sec; 242,850,212 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000142.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000143.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737288: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000143.rdf" in 234.26 sec; 244,991,847 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000143.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000144.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737471: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000144.rdf" in 240.87 sec; 247,133,470 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000144.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000145.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738363: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000145.rdf" in 231.51 sec; 249,275,976 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000145.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000146.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738225: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000146.rdf" in 228.96 sec; 251,418,209 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000146.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000147.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736836: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000147.rdf" in 234.20 sec; 253,559,683 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000147.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000148.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736964: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000148.rdf" in 249.91 sec; 255,701,801 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000148.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000149.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737243: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000149.rdf" in 247.60 sec; 257,843,094 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000149.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000150.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737842: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000150.rdf" in 242.15 sec; 259,985,079 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000150.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000151.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736749: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000151.rdf" in 238.98 sec; 262,127,045 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000151.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000152.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740076: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000152.rdf" in 284.63 sec; 264,269,524 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000152.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000153.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737633: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000153.rdf" in 274.27 sec; 266,411,717 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000153.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000154.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737813: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000154.rdf" in 276.82 sec; 268,553,172 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000154.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000155.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739305: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000155.rdf" in 293.46 sec; 270,694,907 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000155.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000156.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739271: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000156.rdf" in 293.58 sec; 272,836,200 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000156.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000157.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737710: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000157.rdf" in 288.32 sec; 274,978,089 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000157.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000158.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737816: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000158.rdf" in 300.69 sec; 277,120,144 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000158.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000159.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736805: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000159.rdf" in 297.35 sec; 279,261,950 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000159.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000160.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738055: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000160.rdf" in 303.19 sec; 281,404,298 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000160.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000161.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737288: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000161.rdf" in 306.24 sec; 283,546,167 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000161.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000162.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737589: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000162.rdf" in 308.12 sec; 285,672,833 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000162.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000163.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742389: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000163.rdf" in 308.90 sec; 287,725,183 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000163.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000164.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743605: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000164.rdf" in 312.40 sec; 289,744,261 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000164.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000165.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736433: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000165.rdf" in 322.05 sec; 291,895,409 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000165.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000166.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736826: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000166.rdf" in 346.29 sec; 294,046,365 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000166.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000167.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736583: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000167.rdf" in 378.14 sec; 296,198,416 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000167.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000168.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737247: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000168.rdf" in 388.12 sec; 298,350,144 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000168.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000169.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737809: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000169.rdf" in 376.65 sec; 300,501,972 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000169.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000170.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738187: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000170.rdf" in 399.13 sec; 302,654,286 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000170.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000171.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737880: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000171.rdf" in 395.13 sec; 304,806,554 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000171.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000172.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737273: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000172.rdf" in 366.15 sec; 306,956,907 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000172.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000173.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737006: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000173.rdf" in 346.39 sec; 309,106,663 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000173.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000174.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736281: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000174.rdf" in 349.00 sec; 311,256,660 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000174.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000175.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736810: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000175.rdf" in 365.22 sec; 313,406,727 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000175.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000176.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738331: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000176.rdf" in 360.47 sec; 315,556,374 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000176.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000177.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2735435: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000177.rdf" in 372.99 sec; 317,707,286 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000177.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000178.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736662: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000178.rdf" in 374.34 sec; 319,858,423 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000178.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000179.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736572: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000179.rdf" in 379.45 sec; 322,010,691 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000179.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000180.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737314: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000180.rdf" in 376.68 sec; 324,162,885 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000180.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000181.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736901: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000181.rdf" in 388.63 sec; 326,314,354 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000181.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000182.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740902: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000182.rdf" in 392.01 sec; 328,394,779 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000182.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000183.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745127: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000183.rdf" in 383.74 sec; 330,272,729 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000183.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000184.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2747217: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000184.rdf" in 383.04 sec; 332,230,178 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000184.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000185.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746129: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000185.rdf" in 381.53 sec; 334,137,378 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000185.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000186.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746417: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000186.rdf" in 386.80 sec; 336,029,758 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000186.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000187.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744793: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000187.rdf" in 410.10 sec; 337,922,371 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000187.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000188.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742348: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000188.rdf" in 402.69 sec; 339,939,280 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000188.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000189.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745329: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000189.rdf" in 408.08 sec; 341,963,607 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000189.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000190.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743085: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000190.rdf" in 418.26 sec; 343,965,536 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000190.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000191.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745443: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000191.rdf" in 417.68 sec; 345,884,887 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000191.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000192.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746874: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000192.rdf" in 433.20 sec; 347,837,304 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000192.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000193.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744757: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000193.rdf" in 454.16 sec; 349,804,770 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000193.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000194.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743228: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000194.rdf" in 456.01 sec; 351,852,613 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000194.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000195.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746152: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000195.rdf" in 482.76 sec; 353,931,571 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000195.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000196.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745210: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000196.rdf" in 482.82 sec; 356,026,954 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000196.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000197.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2747490: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000197.rdf" in 482.29 sec; 358,118,035 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000197.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000198.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744570: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000198.rdf" in 472.62 sec; 360,200,211 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000198.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000199.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744426: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000199.rdf" in 417.61 sec; 362,283,071 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000199.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000200.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744127: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000200.rdf" in 402.31 sec; 364,368,020 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000200.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000201.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746536: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000201.rdf" in 406.20 sec; 366,456,944 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000201.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000202.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743734: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000202.rdf" in 465.08 sec; 368,543,216 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000202.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000203.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743449: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000203.rdf" in 474.72 sec; 370,628,684 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000203.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000204.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746964: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000204.rdf" in 470.62 sec; 372,717,361 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000204.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000205.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744665: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000205.rdf" in 447.33 sec; 374,804,360 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000205.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000206.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743543: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000206.rdf" in 381.07 sec; 376,875,171 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000206.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000207.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746997: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000207.rdf" in 412.51 sec; 378,915,902 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000207.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000208.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2749912: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000208.rdf" in 416.87 sec; 380,876,064 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000208.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000209.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2922809: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000209.rdf" in 362.57 sec; 382,319,633 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000209.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000210.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2966871: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000210.rdf" in 286.70 sec; 383,590,250 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000210.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000211.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2947676: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000211.rdf" in 313.07 sec; 384,852,949 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000211.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000212.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2943144: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000212.rdf" in 308.53 sec; 386,114,365 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000212.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000213.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2952389: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000213.rdf" in 292.21 sec; 387,378,054 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000213.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000214.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953884: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000214.rdf" in 288.30 sec; 388,641,657 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000214.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000215.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954160: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000215.rdf" in 354.66 sec; 389,904,906 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000215.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000216.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955356: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000216.rdf" in 428.31 sec; 391,168,109 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000216.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000217.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2950300: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000217.rdf" in 438.42 sec; 392,427,957 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000217.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000218.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2949062: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000218.rdf" in 448.49 sec; 393,686,822 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000218.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000219.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2948617: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000219.rdf" in 412.05 sec; 394,946,157 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000219.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000220.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953640: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000220.rdf" in 412.95 sec; 396,209,357 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000220.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000221.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2950846: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000221.rdf" in 459.73 sec; 397,472,157 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000221.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000222.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2961660: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000222.rdf" in 422.45 sec; 398,740,245 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000222.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000223.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954919: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000223.rdf" in 426.94 sec; 400,005,840 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000223.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000224.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2960061: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000224.rdf" in 366.93 sec; 401,275,503 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000224.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000225.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953736: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000225.rdf" in 420.21 sec; 402,543,884 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000225.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000226.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955881: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000226.rdf" in 383.26 sec; 403,813,915 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000226.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000227.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2961703: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000227.rdf" in 379.79 sec; 405,090,368 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000227.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000228.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2951217: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000228.rdf" in 398.34 sec; 406,362,203 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000228.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000229.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2969167: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000229.rdf" in 375.33 sec; 407,633,256 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000229.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000230.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954906: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000230.rdf" in 355.30 sec; 408,897,860 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000230.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000231.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2949679: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000231.rdf" in 351.82 sec; 410,160,720 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000231.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000232.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2951679: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000232.rdf" in 427.94 sec; 411,423,942 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000232.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000233.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955544: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000233.rdf" in 525.37 sec; 412,688,716 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000233.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000234.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2956514: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000234.rdf" in 527.80 sec; 413,952,051 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000234.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000235.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2970175: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000235.rdf" in 545.31 sec; 415,221,748 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000235.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000236.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2950279: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000236.rdf" in 558.30 sec; 416,483,186 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000236.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000237.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2959209: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000237.rdf" in 574.79 sec; 417,748,664 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000237.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000238.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954802: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000238.rdf" in 652.18 sec; 419,013,509 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000238.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000239.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2963042: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000239.rdf" in 653.64 sec; 420,283,024 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000239.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000240.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2944955: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000240.rdf" in 625.12 sec; 421,544,613 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000240.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000241.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2966057: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000241.rdf" in 650.58 sec; 422,816,297 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000241.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000242.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955693: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000242.rdf" in 656.17 sec; 424,083,767 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000242.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000243.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2945529: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000243.rdf" in 669.13 sec; 425,347,583 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000243.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000244.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2940818: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000244.rdf" in 584.91 sec; 426,610,213 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000244.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000245.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2936908: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000245.rdf" in 555.79 sec; 427,871,531 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000245.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000246.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2942066: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000246.rdf" in 529.30 sec; 429,135,385 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000246.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000247.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2933088: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000247.rdf" in 596.14 sec; 430,395,949 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000247.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000248.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2947419: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000248.rdf" in 550.34 sec; 431,661,274 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000248.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000249.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2934380: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000249.rdf" in 519.41 sec; 432,920,859 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000249.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000250.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2868113: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000250.rdf" in 483.78 sec; 433,952,687 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000250.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000251.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811485: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000251.rdf" in 445.54 sec; 434,665,213 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000251.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000252.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811418: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000252.rdf" in 424.60 sec; 435,377,789 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000252.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000253.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811518: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000253.rdf" in 426.45 sec; 436,090,306 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000253.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000254.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811604: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000254.rdf" in 444.97 sec; 436,802,798 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000254.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000255.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811550: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000255.rdf" in 449.11 sec; 437,515,302 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000255.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000256.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811241: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000256.rdf" in 435.93 sec; 438,228,025 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000256.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000257.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2291258: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000257.rdf" in 473.12 sec; 439,306,461 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000257.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000258.rdf
% Parsed "neo4j_out_v3_part_000258.rdf" in 492.32 sec; 440,318,102 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000258.rdf
true.

?- rdf_graph_property(neo4j_out_v3,Y).
Y = hash('5617e3db40e363dd9e916d1aefc81299') ;
Y = modified(false) ;
Y = source('file:///root/metta-wam/libraries/bioAS/neo4j_out_v3_split_rdf/neo4j_out_v3_part_000258.rdf') ;
Y = source_last_modified(1740796112.9243655) ;
Y = triples(440_318_102) ;
Y = persistent(true).

```


```no-wrap
root@HOSTAGE:~/metta-wam/libraries/bioAS# swipl -l gene_queries1.pl
\U0001F680 Mounting persistence at datastore_neo4j_out_v3
% Restoring 2 snapshots using 2 concurrent workers
datastore_neo4j_out_v3................................... 1 of 2 graphs
neo4j_out_v3 2 of 2 graphs
% Loaded 2 graphs (446,472,442 triples) in 4554.54 sec. (91% CPU = 4140.39 sec.)
?-load_rdf_db_directory
Welcome to SWI-Prolog (threaded, 64 bits, version 9.3.19-42-g7a8a2ecb8-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).


    PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND
 132458 root      20   0   67.4g  61.3g   6540 S   0.0  48.8  69:00.65 swipl

?- rdf(X,Y,Z), rdf(X,A,B).


X = 'neo4j://graph.individuals#13441',
Y = 'neo4j://graph.schema#start',
Z = literal('25120014') ;
X = 'neo4j://graph.individuals#14209',
Y = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
Z = 'neo4j://graph.schema#gene' ;
X = 'neo4j://graph.individuals#6747',
Y = 'neo4j://graph.schema#gene_type',
Z = literal(protein_coding) ;
X = 'neo4j://graph.individuals#22552398',
Y = 'neo4j://graph.schema#end',
Z = literal('152668911') ;
X = 'neo4j://graph.individuals#24572',
Y = 'neo4j://graph.schema#gene_name',
Z = literal('GIMAP7') ;
X = 'neo4j://graph.individuals#21575610',
Y = 'neo4j://graph.schema#phred_score',
Z = literal('6.14') ;
X = 'neo4j://graph.individuals#16634',
Y = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
Z = 'neo4j://graph.schema#gene' .

?-

```




```no-wrap

        62,284  neo4j_out_v4/dbsuper/edges_super_enhancer_gene.csv
        69,071  neo4j_out_v4/dbsuper/nodes_super_enhancer.csv
         1,088  neo4j_out_v4/cell_ontology/edges_cl_part_of.csv
         2,811  neo4j_out_v4/cell_ontology/nodes_cl.csv
           368  neo4j_out_v4/cell_ontology/edges_cl_capable_of.csv
         3,945  neo4j_out_v4/cell_ontology/edges_cl_subclass_of.csv
    14,096,202  neo4j_out_v4/top_ld/EUR/edges_in_ld_with.csv
       188,120  neo4j_out_v4/gwas/upstream/edges_snp_upstream_gene.csv
       385,656  neo4j_out_v4/gwas/ingene/edges_snp_in_gene.csv
       188,113  neo4j_out_v4/gwas/downstream/edges_snp_downstream_gene.csv
   257,746,971  neo4j_out_v4/coxpressdb/edges_coexpressed_with.csv
    13,356,025  neo4j_out_v4/string/edges_interacts_with.csv
        12,622  neo4j_out_v4/gene_ontology/molecular_function/edges_molecular_function_subclass_of.csv
        10,147  neo4j_out_v4/gene_ontology/molecular_function/nodes_molecular_function.csv
        46,512  neo4j_out_v4/gene_ontology/biological_process/edges_biological_process_subclass_of.csv
        26,468  neo4j_out_v4/gene_ontology/biological_process/nodes_biological_process.csv
         4,618  neo4j_out_v4/gene_ontology/cellular_component/edges_cellular_component_subclass_of.csv
         4,023  neo4j_out_v4/gene_ontology/cellular_component/nodes_cellular_component.csv
           400  neo4j_out_v4/hocomoco/nodes_motif.csv
       991,805  neo4j_out_v4/reactome/edges_genes_pathways.csv
         2,692  neo4j_out_v4/reactome/edges_parent_pathway_of.csv
         2,692  neo4j_out_v4/reactome/edges_child_pathway_of.csv
         2,674  neo4j_out_v4/reactome/nodes_pathway.csv
    67,561,010  neo4j_out_v4/gtex/eqtl/edges_gtex_variant_gene.csv
    67,561,010  neo4j_out_v4/gtex/expression/edges_expressed_in.csv
        38,090  neo4j_out_v4/uberon/edges_uberon_subclass_of.csv
        26,997  neo4j_out_v4/uberon/nodes_uberon.csv
 3,152,174,545  neo4j_out_v4/roadmap/chromatin_state/edges_chromatin_state.csv
   832,811,103  neo4j_out_v4/roadmap/h3_mark/edges_histone_modification.csv
    26,821,418  neo4j_out_v4/roadmap/dhs/edges_in_dnase_i_hotspot.csv
       662,841  neo4j_out_v4/rna_central/nodes_non_coding_rna.csv
        18,898  neo4j_out_v4/rna_central/edges_cellular_component_rna.csv
        10,359  neo4j_out_v4/rna_central/edges_molecular_function_rna.csv
        17,035  neo4j_out_v4/rna_central/edges_biological_process_rna.csv
    37,302,979  neo4j_out_v4/dbsnp/nodes_snp.csv
     7,177,505  neo4j_out_v4/bgee/edges_expressed_in.csv
    23,396,248  neo4j_out_v4/refseq/edges_closest_gene.csv
       134,098  neo4j_out_v4/gaf/edges_biological_process_gene_product.csv
        62,235  neo4j_out_v4/gaf/edges_cellular_component_gene_product_located_in.csv
        70,800  neo4j_out_v4/gaf/edges_molecular_function_gene_product.csv
        10,265  neo4j_out_v4/gaf/edges_cellular_component_gene_product_part_of.csv
    29,184,677  neo4j_out_v4/polyphen-2/nodes_snp.csv
        50,073  neo4j_out_v4/uniprot/edges_translation_of.csv
        20,429  neo4j_out_v4/uniprot/nodes_protein.csv
        50,073  neo4j_out_v4/uniprot/edges_translates_to.csv
    10,489,066  neo4j_out_v4/tfbs/edges_gene_tfbs.csv
    10,565,631  neo4j_out_v4/tfbs/nodes_tfbs.csv
       827,037  neo4j_out_v4/dgv/nodes_structural_variant.csv
    24,296,266  neo4j_out_v4/dbvar/nodes_structural_variant.csv
     7,578,823  neo4j_out_v4/enhancer_atlas/edges_enhancer_gene.csv
       193,219  neo4j_out_v4/enhancer_atlas/nodes_enhancer.csv
     6,398,916  neo4j_out_v4/tflink/edges_tf_gene.csv
        62,701  neo4j_out_v4/gencode/gene/nodes_gene.csv
       565,117  neo4j_out_v4/gencode/exon/edges_includes.csv
     1,649,477  neo4j_out_v4/gencode/exon/nodes_exon.csv
       191,211  neo4j_out_v4/gencode/transcript/edges_transcribed_to.csv
       191,211  neo4j_out_v4/gencode/transcript/edges_transcribed_from.csv
       191,211  neo4j_out_v4/gencode/transcript/nodes_transcript.csv
    20,140,695  neo4j_out_v4/peregrine/edges_enhancer_gene.csv
     1,085,539  neo4j_out_v4/peregrine/nodes_enhancer.csv
    35,856,052  neo4j_out_v4/abc/edges_activity_by_contact.csv
        74,257  neo4j_out_v4/experimental_factor_ontology/edges_efo_subclass_of.csv
       118,736  neo4j_out_v4/experimental_factor_ontology/nodes_efo.csv
         6,558  neo4j_out_v4/brenda_tissue_ontology/nodes_bto.csv
         4,600  neo4j_out_v4/brenda_tissue_ontology/edges_bto_subclass_of.csv
        29,599  neo4j_out_v4/epd/nodes_promoter.csv
        28,134  neo4j_out_v4/epd/edges_promoter_gene.csv
        45,673  neo4j_out_v4/cell_line_ontology/edges_clo_subclass_of.csv
        45,502  neo4j_out_v4/cell_line_ontology/nodes_clo.csv
            47  neo4j_out_v4/graph_info.json
         3,037  neo4j_out_v4/tadmap/nodes_tad.csv
        18,957  neo4j_out_v4/tadmap/edges_in_tad_region.csv
    74,460,817  neo4j_out_v4/cadd/nodes_snp.csv
       327,933  neo4j_out_v4/fabian/edges_tfbs_snp.csv
 4,727,814,017  total


```
