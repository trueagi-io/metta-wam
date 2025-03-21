true.

137 ?- rtq(120).




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name
          (startsWith "BRCA2"))
        (neo $Gene:gene transcribed_to $Tx:transcript)
        (neo $Gene:gene genes_pathways $Pathway:pathway)
        (neo $Tx:transcript translates_to $Prot:protein)
        (neo $Prot:protein interacts_with $Prot2:protein))
      (result $Gene $Tx $Prot $Prot2 $Pathway))
prolog=(neo(Gene:gene,gene_name,startsWith("BRCA2")),neo(Gene:gene,transcribed_to,Tx:transcript),neo(Gene:gene,genes_pathways,Pathway:pathway),neo(Tx:transcript,translates_to,Prot:protein),neo(Prot:protein,interacts_with,Prot2:protein),true)




### Query1 - BRCA2 Gene Interactions
```no-wrap
=================================================================
        Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (startsWith "BRCA2"))
      (neo $Gene:gene transcribed_to $Tx:transcript)
      (neo $Gene:gene genes_pathways $Pathway:pathway)
      (neo $Tx:transcript translates_to $Prot:protein)
      (neo $Prot:protein interacts_with $Prot2:protein))
    (result $Prot2 $Prot $Pathway $Tx $Gene))

q7z407                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
o75376                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
q15059                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
q13489                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
p48594                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
o14646                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
p33991                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
o43543                        p51587                        r-hsa-1474165                 enst00000544455               ensg00000139618
q0d2j5                        p51587                        r-hsa-5685942                 enst00000544455               ensg00000139618
 MeTTaLog Execution time: 0.76 seconds
% 18,649,776 inferences, 0.756 CPU in 0.760 seconds (99% CPU, 24674783 Lips)
 Last answer found: 0.67 seconds
 Number of answers: 152,442
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name
          (startsWith "BRCA2") $PropList1)
        (neo_P $Gene:gene transcribed_to $Tx:transcript $PropList2)
        (neo_P $Gene:gene genes_pathways $Pathway:pathway $PropList5)
        (neo_P $Tx:transcript translates_to $Prot:protein $PropList3)
        (neo_P $Prot:protein interacts_with $Prot2:protein $PropList4))
      (result $Gene $Tx $Prot $Prot2 $Pathway $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))
prolog=(neo_P(Gene:gene,gene_name,startsWith("BRCA2"),PropList1),neo_P(Gene:gene,transcribed_to,Tx:transcript,PropList2),neo_P(Gene:gene,genes_pathways,Pathway:pathway,PropList5),neo_P(Tx:transcript,translates_to,Prot:protein,PropList3),neo_P(Prot:protein,interacts_with,Prot2:protein,PropList4),true)




### Query1 (With Properties) - BRCA2 Gene Interactions
```no-wrap
=================================================================
        Find all interactions involving the BRCA2 gene, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (startsWith "BRCA2") $PropList1)
      (neo_P $Gene:gene transcribed_to $Tx:transcript $PropList2)
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $PropList5)
      (neo_P $Tx:transcript translates_to $Prot:protein $PropList3)
      (neo_P $Prot:protein interacts_with $Prot2:protein $PropList4))
    (result $PropList4 $Prot2 $PropList3 $Prot $PropList5 $Pathway $PropList2 $Tx $PropList1 $Gene))

[score=0.403]  q7z407         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.347]  o75376         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.158]  q15059         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.186]  q13489         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.164]  p48594         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.411]  o14646         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.386]  p33991         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.982]  o43543         ()             p51587         ()             r-hsa-1474165  ()             enst00000544455 ()             ensg00000139618
[score=0.193]  q0d2j5         ()             p51587         ()             r-hsa-5685942  ()             enst00000544455 ()             ensg00000139618
 MeTTaLog Execution time: 0.55 seconds
% 18,661,792 inferences, 0.554 CPU in 0.556 seconds (100% CPU, 33698063 Lips)
 Last answer found: 0.53 seconds
 Number of answers: 152,442
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name
          (startsWith "IGF2"))
        (neo $Gene:gene genes_pathways $Pway:pathway)
        (neo $Prom:promoter associated_with $Gene:gene)
        (neo $Enh:enhancer associated_with $Gene:gene)
        (neo $Child:pathway child_pathway_of $Pway:pathway))
      (result $Prom $Gene $Enh $Pway $Child))
prolog=(neo(Gene:gene,gene_name,startsWith("IGF2")),neo(Gene:gene,genes_pathways,Pway:pathway),neo(Prom:promoter,associated_with,Gene:gene),neo(Enh:enhancer,associated_with,Gene:gene),neo(Child:pathway,child_pathway_of,Pway:pathway),true)




### Query2 - IGF2 Components
```no-wrap
=================================================================
        Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (startsWith "IGF2"))
      (neo $Gene:gene genes_pathways $Pway:pathway)
      (neo $Prom:promoter associated_with $Gene:gene)
      (neo $Enh:enhancer associated_with $Gene:gene)
      (neo $Child:pathway child_pathway_of $Pway:pathway))
    (result $Child $Enh $Prom $Pway $Gene))

r-hsa-194441                  chr3_185644521_185644840_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-428359                  chr3_185644521_185644840_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-429914                  chr3_185644521_185644840_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-72086                   chr3_185049221_185051590_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-72203                   chr3_185049221_185051590_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-72312                   chr3_185049221_185051590_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-75067                   chr3_185404981_185407140_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-72086                   chr3_185404981_185407140_grch38 chr3_185821121_185821180_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-72086                   chr3_185266321_185268330_grch38 chr3_185825028_185825087_grch38 r-hsa-8953854                 ensg00000073792             
r-hsa-1236394                 chr11_2004087_2004243_grch38  chr11_2129566_2129625_grch38  r-hsa-9006934                 ensg00000167244
 MeTTaLog Execution time: 8.19 seconds
% 85,383,971 inferences, 8.195 CPU in 8.236 seconds (100% CPU, 10419418 Lips)
 Last answer found: 8.18 seconds
 Number of answers: 664,061
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name
          (startsWith "IGF2") $PropList1)
        (neo_P $Gene:gene genes_pathways $Pway:pathway $PropList4)
        (neo_P $Prom:promoter associated_with $Gene:gene $PropList2)
        (neo_P $Enh:enhancer associated_with $Gene:gene $PropList3)
        (neo_P $Child:pathway child_pathway_of $Pway:pathway $PropList5))
      (result $Prom $Gene $Enh $Pway $Child $PropList1 $PropList2 $PropList3 $PropList4 $PropList5))
prolog=(neo_P(Gene:gene,gene_name,startsWith("IGF2"),PropList1),neo_P(Gene:gene,genes_pathways,Pway:pathway,PropList4),neo_P(Prom:promoter,associated_with,Gene:gene,PropList2),neo_P(Enh:enhancer,associated_with,Gene:gene,PropList3),neo_P(Child:pathway,child_pathway_of,Pway:pathway,PropList5),true)




### Query2 (With Properties) - IGF2 Components
```no-wrap
=================================================================
        Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (startsWith "IGF2") $PropList1)
      (neo_P $Gene:gene genes_pathways $Pway:pathway $PropList4)
      (neo_P $Prom:promoter associated_with $Gene:gene $PropList2)
      (neo_P $Enh:enhancer associated_with $Gene:gene $PropList3)
      (neo_P $Child:pathway child_pathway_of $Pway:pathway $PropList5))
    (result $PropList5 $Child $PropList3 $Enh $PropList2 $Prom $PropList4 $Pway $PropList1 $Gene))

()             r-hsa-194441   [biological_context='UBERON_0000006',score=0.845829] chr3_185644521_185644840_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-428359   [biological_context='UBERON_0000006',score=0.845829] chr3_185644521_185644840_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-429914   [biological_context='UBERON_0000006',score=0.845829] chr3_185644521_185644840_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-72086    [biological_context='CLO_0007599',score=2.086826] chr3_185049221_185051590_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-72203    [biological_context='CLO_0007599',score=2.086826] chr3_185049221_185051590_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-72312    [biological_context='CLO_0007599',score=2.086826] chr3_185049221_185051590_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-75067    [biological_context='CL_0000515',score=0.505124] chr3_185404981_185407140_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-72086    [biological_context='BTO_0000764',score=0.765] chr3_185404981_185407140_grch38 ()             chr3_185821121_185821180_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()             r-hsa-72086    [biological_context='CLO_0009258',score=1.753719] chr3_185266321_185268330_grch38 ()             chr3_185825028_185825087_grch38 ()             r-hsa-8953854  ()             ensg00000073792
()               r-hsa-1236394    [biological_context='CLO_0001654'] chr11_2004087_2004243_grch38 ()               chr11_2129566_2129625_grch38 ()      4    ()               ensg00000167244
 MeTTaLog Execution time: 8.22 seconds
% 87,759,986 inferences, 8.215 CPU in 8.251 seconds (100% CPU, 10682490 Lips)
 Last answer found: 8.20 seconds
 Number of answers: 664,061
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Prot1:protein translation_of $Tx:transcript)
        (neo $Gene:gene transcribed_to $Tx:transcript)
        (neo $Tx:transcript includes $Exon:exon)
        (neo $Prot1:protein interacts_with $Prot2:protein)
        (neo $GO:go go_gene_product $Prot1:protein))
      (result $Gene $Tx $Exon $Prot1 $Prot2 $GO))
prolog=(neo(Prot1:protein,translation_of,Tx:transcript),neo(Gene:gene,transcribed_to,Tx:transcript),neo(Tx:transcript,includes,Exon:exon),neo(Prot1:protot2:protein),neo(GO:go,go_gene_product,Prot1:protein),true)




### Query3 - Gene Interactions and GO Terms
```no-wrap
=================================================================
        Find gene interactions and associated GO terms including proteins and transcripts.

  (match &neo4j_out_v3
    (,
      (neo $Prot1:protein translation_of $Tx:transcript)
      (neo $Gene:gene transcribed_to $Tx:transcript)
      (neo $Tx:transcript includes $Exon:exon)
      (neo $Prot1:protein interacts_with $Prot2:protein)
      (neo $GO:go go_gene_product $Prot1:protein))
    (result $GO $Prot2 $Exon $Gene $Tx $Prot1))

go_0004860                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p30044                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005829                  p63151                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005829                  q16576                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  q13363                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005829                  q8wum4                      ense00003798043             ensg00000166913             enst00000353703             p31946
go_0005515                  o14810                      ense00003798043             ensg00000166913             enst00000372839             p31946
 Time limit 120 seconds exceeded!
% 4,384,300,156 inferences, 119.495 CPU in 120.000 seconds (100% CPU, 36690269 Lips)
 Last answer found: 119.49 seconds
 Number of answers: 35,221,859
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Prot1:protein translation_of $Tx:transcript $P3)
        (neo_P $Gene:gene transcribed_to $Tx:transcript $P1)
        (neo_P $Tx:transcript includes $Exon:exon $P2)
        (neo_P $Prot1:protein interacts_with $Prot2:protein $P4)
        (neo_P $GO:go go_gene_product $Prot1:protein $P5))
      (result $Gene $Tx $Exon $Prot1 $Prot2 $GO $P1 $P2 $P3 $P4 $P5))
prolog=(neo_P(Prot1:protein,translation_of,Tx:transcript,P3),neo_P(Gene:gene,transcribed_to,Tx:transcript,P1),neo_P(Tx:transcript,includes,Exon:exon,P2)interacts_with,Prot2:protein,P4),neo_P(GO:go,go_gene_product,Prot1:protein,P5),true)




### Query3 (With Properties) - Gene Interactions and GO Terms
```no-wrap
=================================================================
        Find gene interactions and associated GO terms (proteins/transcripts), returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Prot1:protein translation_of $Tx:transcript $P3)
      (neo_P $Gene:gene transcribed_to $Tx:transcript $P1)
      (neo_P $Tx:transcript includes $Exon:exon $P2)
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P4)
      (neo_P $GO:go go_gene_product $Prot1:protein $P5))
    (result $P5 $GO $P4 $Prot2 $P2 $Exon $P1 $Gene $P3 $Tx $Prot1))

[qualifier=[enables],db_reference=['PMID:11555644'],evidence='IDA'] go_0004860     [score=0.39]   p20290         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:10713667'],evidence='IPI'] go_0005515     [score=0.39]   p20290         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:10958686'],evidence='IPI'] go_0005515     [score=0.39]   p20290         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:31980649'],evidence='IPI'] go_0005515     [score=0.39]   p20290         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:32707033'],evidence='IPI'] go_0005515     [score=0.39]   p20290         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:32707033'],evidence='IPI'] go_0005515     [score=0.39]   p20290         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:24255178'],evidence='IPI'] go_0005515     [score=0.187]  p30044         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[located_in],db_reference=['Reactome:R-HSA-5632738'],evidence='TAS'] go_0005829     [score=0.18]   p63151         ()             ense00001891sg00000166913 ()             enst00000353703 p31946
[qualifier=[located_in],db_reference=['Reactome:R-HSA-5672972'],evidence='TAS'] go_0005829     [score=0.322]  q16576         ()             ense00001891sg00000166913 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:30833792'],evidence='IPI'] go_0005515     [score=0.165]  q13363         ()             ense00001891800 ()      3 ()             enst00000353703 p31946
[qualifier=[located_in],db_reference=['Reactome:R-HSA-9656211'],evidence='TAS'] go_0005829     [score=0.179]  q8wum4         ()             ense00003798sg00000166913 ()             enst00000353703 p31946
[qualifier=[enables],db_reference=['PMID:32707033'],evidence='IPI'] go_0005515     [score=0.182]  o14810         ()             ense00003798043 ()      3 ()             enst00000372839 p31946
 Time limit 120 seconds exceeded!
% 4,390,176,183 inferences, 119.502 CPU in 120.000 seconds (100% CPU, 36737282 Lips)
 Last answer found: 119.50 seconds
 Number of answers: 35,247,842
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Prot1:protein protein_name
          (startsWith "1433B"))
        (neo $Prot1:protein translation_of $Tx:transcript)
        (neo $Prot1:protein interacts_with $Prot2:protein)
        (neo $GO:go go_gene_product $Prot1:protein)
        (neo $Gene:gene transcribed_to $Tx:transcript)
        (neo $Tx:transcript includes $Exon:exon))
      (result $Gene $Tx $Exon $Prot1 $Prot2 $GO))
prolog=(neo(Prot1:protein,protein_name,startsWith("1433B")),neo(Prot1:protein,translation_of,Tx:transcript),neo(Prot1:protein,interacts_with,Prot2:prote_product,Prot1:protein),neo(Gene:gene,transcribed_to,Tx:transcript),neo(Tx:transcript,includes,Exon:exon),true)




### Query4 - Interactions Involving 1433B
```no-wrap
=================================================================
        Find interactions involving 1433B protein, including transcripts, exons, and GO terms.

  (match &neo4j_out_v3
    (,
      (neo $Prot1:protein protein_name
        (startsWith "1433B"))
      (neo $Prot1:protein translation_of $Tx:transcript)
      (neo $Prot1:protein interacts_with $Prot2:protein)
      (neo $GO:go go_gene_product $Prot1:protein)
      (neo $Gene:gene transcribed_to $Tx:transcript)
      (neo $Tx:transcript includes $Exon:exon))
    (result $Exon $Gene $GO $Prot2 $Tx $Prot1))

ense00001891800             ensg00000166913             go_0004860                  p20290                      enst00000353703             p31946
ense00003798043             ensg00000166913             go_0004860                  p20290                      enst00000353703             p31946
ense00003550189             ensg00000166913             go_0004860                  p20290                      enst00000353703             p31946
ense00003515039             ensg00000166913             go_0005515                  p20290                      enst00000353703             p31946
ense00001458785             ensg00000166913             go_0005515                  p20290                      enst00000353703             p31946
ense00003798043             ensg00000166913             go_0005515                  p20290                      enst00000353703             p31946
ense00003550189             ensg00000166913             go_0005515                  p20290                      enst00000353703             p31946
ense00003515039             ensg00000166913             go_0005829                  p20290                      enst00000353703             p31946
ense00003515039             ensg00000166913             go_0005829                  q9but1                      enst00000353703             p31946
ense00003550189             ensg00000166913             go_0005515                  p51617                      enst00000353703             p31946
ense00003515039             ensg00000166913             go_0005829                  q6uuv7                      enst00000353703             p31946
ense00003798043             ensg00000166913             go_0005829                  q9ny65                      enst00000372839             p31946
 MeTTaLog Execution time: 30.86 seconds
% 1,151,850,114 inferences, 30.861 CPU in 30.990 seconds (100% CPU, 37323233 Lips)
 Last answer found: 30.58 seconds
 Number of answers: 7,441,850
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Protein1:protein protein_name
          (startsWith "1433B"))
        (neo $Protein1:protein translation_of $Transcript:transcript)
        (neo $Gene:gene transcribed_to $Transcript:transcript)
        (neo $Transcript:transcript includes $Exon:exon)
        (neo $Protein1:protein interacts_with $Protein2:protein)
        (neo $GO1Term:go go_gene_product $Protein1:protein))
      (result $Gene $Tx $Exon $Prot1 $Prot2 $GO))
prolog=(neo(Protein1:protein,protein_name,startsWith("1433B")),neo(Protein1:protein,translation_of,Transcript:transcript),neo(Gene:gene,transcribed_to,T,neo(Transcript:transcript,includes,Exon:exon),neo(Protein1:protein,interacts_with,Protein2:protein),neo(GO1Term:go,go_gene_product,Protein1:protein),tr




### Query4 - @xabush order Interactions Involving 1433B
```no-wrap
=================================================================
        @xabush order Find interactions involving 1433B protein, including transcripts, exons, and GO terms.

  (match &neo4j_out_v3
    (,
      (neo $Protein1:protein protein_name
        (startsWith "1433B"))
      (neo $Protein1:protein translation_of $Transcript:transcript)
      (neo $Gene:gene transcribed_to $Transcript:transcript)
      (neo $Transcript:transcript includes $Exon:exon)
      (neo $Protein1:protein interacts_with $Protein2:protein)
      (neo $GO1Term:go go_gene_product $Protein1:protein))
    (result $GO1Term $Protein2 $Exon $Gene $Transcript $Protein1))

go_0004860                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p20290                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  p30044                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005829                  p63151                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005829                  q16576                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005515                  q13363                      ense00001891800             ensg00000166913             enst00000353703             p31946
go_0005829                  q8wum4                      ense00003798043             ensg00000166913             enst00000353703             p31946
go_0005515                  o14810                      ense00003798043             ensg00000166913             enst00000372839             p31946
 MeTTaLog Execution time: 24.99 seconds
% 927,376,145 inferences, 24.993 CPU in 25.100 seconds (100% CPU, 37105457 Lips)
 Last answer found: 24.96 seconds
 Number of answers: 7,441,850
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Prot1:protein protein_name
          (startsWith "1433B") $P1)
        (neo_P $Prot1:protein translation_of $Tx:transcript $P4)
        (neo_P $Prot1:protein interacts_with $Prot2:protein $P5)
        (neo_P $GO:go go_gene_product $Prot1:protein $P6)
        (neo_P $Gene:gene transcribed_to $Tx:transcript $P2)
        (neo_P $Tx:transcript includes $Exon:exon $P3))
      (result $Gene $Tx $Exon $Prot1 $Prot2 $GO $P1 $P2 $P3 $P4 $P5 $P6))
prolog=(neo_P(Prot1:protein,protein_name,startsWith("1433B"),P1),neo_P(Prot1:protein,translation_of,Tx:transcript,P4),neo_P(Prot1:protein,interacts_witho_P(GO:go,go_gene_product,Prot1:protein,P6),neo_P(Gene:gene,transcribed_to,Tx:transcript,P2),neo_P(Tx:transcript,includes,Exon:exon,P3),true)




### Query4 (With Properties) - Interactions Involving 1433B
```no-wrap
=================================================================
        Find interactions involving 1433B protein, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Prot1:protein protein_name
        (startsWith "1433B") $P1)
      (neo_P $Prot1:protein translation_of $Tx:transcript $P4)
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P5)
      (neo_P $GO:go go_gene_product $Prot1:protein $P6)
      (neo_P $Gene:gene transcribed_to $Tx:transcript $P2)
      (neo_P $Tx:transcript includes $Exon:exon $P3))
    (result $P3 $Exon $P2 $Gene $P6 $GO $P5 $Prot2 $P4 $Tx $P1 $Prot1))

()            ense00001891800 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:11555644'],evidence='IDA'] go_0004860     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003798043 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:11555644'],evidence='IDA'] go_0004860     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003550189 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:11555644'],evidence='IDA'] go_0004860     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003515039 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:15023544'],evidence='IPI'] go_0005515     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00001458785 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:15023544'],evidence='IPI'] go_0005515     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003798043 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:15159416'],evidence='IPI'] go_0005515     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003550189 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:24947832'],evidence='IPI'] go_0005515     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003515039 ()            ensg00000166913  ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-6802918'],evidence='TAS'] go_0005829     ...
    [score=0.39]  p20290        ()            enst00000353703 ()            p31946
()            ense00003515039 ()            ensg00000166913  ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-6802921'],evidence='TAS'] go_0005829     ...
    [score=0.168] q9but1        ()            enst00000353703 ()            p31946
()            ense00003550189 ()            ensg00000166913  ...
    [qualifier=[enables],db_reference=['PMID:26496610'],evidence='IPI'] go_0005515     ...
    [score=0.22]  p51617        ()            enst00000353703 ()            p31946
()            ense00003515039 ()            ensg00000166913  ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-6802942'],evidence='TAS'] go_0005829     ...
    [score=0.424] q6uuv7        ()            enst00000353703 ()            p31946
()            ense00003798043 ()            ensg00000166913  ...
    [qualifier=[located_in],db_reference=['Reactome:R-HSA-9657603'],evidence='TAS'] go_0005829     ...
    [score=0.199] q9ny65        ()            enst00000372839 ()            p31946
 MeTTaLog Execution time: 31.83 seconds
% 1,190,872,668 inferences, 31.830 CPU in 31.966 seconds (100% CPU, 37414110 Lips)
 Last answer found: 31.79 seconds
 Number of answers: 7,441,850
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name "IGF1")
        (neo $Tx:transcript transcribed_from $Gene:gene)
        (neo $Enh:enhancer associated_with $Gene:gene)
        (neo $Gene:gene genes_pathways $Pathway:pathway)
        (neo $Tx:transcript translates_to $Prot:protein))
      (result $Gene $Pathway $Enh $Tx $Prot))
prolog=(neo(Gene:gene,gene_name,"IGF1"),neo(Tx:transcript,transcribed_from,Gene:gene),neo(Enh:enhancer,associated_with,Gene:gene),neo(Gene:gene,genes_pay),neo(Tx:transcript,translates_to,Prot:protein),true)




### Query5 - Components Associated with IGF1
```no-wrap
=================================================================
        Find enhancers, pathways, and transcripts associated with the IGF1 gene.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")
      (neo $Tx:transcript transcribed_from $Gene:gene)
      (neo $Enh:enhancer associated_with $Gene:gene)
      (neo $Gene:gene genes_pathways $Pathway:pathway)
      (neo $Tx:transcript translates_to $Prot:protein))
    (result $Prot $Pathway $Enh $Tx $Gene))

p05019                            r-hsa-109582                      chr12_102848221_102849260_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-114608                      chr12_102848221_102849260_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-162582                      chr12_102848221_102849260_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-162582                      chr12_102498523_102498856_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-2404192                     chr12_102498523_102498856_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-2428928                     chr12_102498523_102498856_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-422085                      chr12_102427022_102427223_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-2428933                     chr12_102410004_102410767_grch38  enst00000337514                   ensg00000017427
p05019                            r-hsa-2428924                     chr12_102452575_102452904_grch38  enst00000337514                   ensg00000017427
 MeTTaLog Execution time: 0.54 seconds
% 17,551,286 inferences, 0.544 CPU in 0.547 seconds (99% CPU, 32276156 Lips)
 Last answer found: 0.52 seconds
 Number of answers: 80,808
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name "IGF1" $P1)
        (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)
        (neo_P $Enh:enhancer associated_with $Gene:gene $P3)
        (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)
        (neo_P $Tx:transcript translates_to $Prot:protein $P5))
      (result $Gene $Pathway $Enh $Tx $Prot $P1 $P2 $P3 $P4 $P5))
prolog=(neo_P(Gene:gene,gene_name,"IGF1",P1),neo_P(Tx:transcript,transcribed_from,Gene:gene,P4),neo_P(Enh:enhancer,associated_with,Gene:gene,P3),neo_P(Gys,Pathway:pathway,P2),neo_P(Tx:transcript,translates_to,Prot:protein,P5),true)




### Query5 (With Properties) - Components Associated with IGF1
```no-wrap
=================================================================
        Find enhancers, pathways, and transcripts associated with IGF1, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $P1)
      (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)
      (neo_P $Enh:enhancer associated_with $Gene:gene $P3)
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)
      (neo_P $Tx:transcript translates_to $Prot:protein $P5))
    (result $P5 $Prot $P2 $Pathway $P3 $Enh $P4 $Tx $P1 $Gene))

()               p05019           ()               r-hsa-109582      ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()               enst00000337514  ()               ensg0000001742
()               p05019           ()               r-hsa-114608      ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()               enst00000337514  ()               ensg0000001742
()               p05019           ()               r-hsa-162582      ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()               enst00000337514  ()               ensg0000001742
()               p05019           ()               r-hsa-162582     [biological_context='CLO_0007366'] chr12_102498523_102498856_grch38 ()                            ensg00000017427
()               p05019           ()               r-hsa-2404192    [biological_context='CLO_0007366'] chr12_102498523_102498856_grch38 ()                            ensg00000017427
()               p05019           ()               r-hsa-2428928    [biological_context='CLO_0007366'] chr12_102498523_102498856_grch38 ()                            ensg00000017427
()               p05019           ()               r-hsa-422085     [biological_context='CLO_0009054'] chr12_102427022_102427223_grch38 ()                            ensg00000017427
()               p05019           ()               r-hsa-2428933    [biological_context='CLO_0002177'] chr12_102410004_102410767_grch38 ()                            ensg00000017427
()               p05019           ()               r-hsa-2428924    [biological_context='CLO_0006951'] chr12_102452575_102452904_grch38 ()                            ensg00000017427
 MeTTaLog Execution time: 0.55 seconds
% 19,272,215 inferences, 0.548 CPU in 0.551 seconds (99% CPU, 35185155 Lips)
 Last answer found: 0.55 seconds
 Number of answers: 80,808
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name "IGF1")
        (neo $Tx:transcript transcribed_from $Gene:gene)
        (neo $Enh:enhancer associated_with $Gene:gene)
        (neo $Gene:gene genes_pathways $Pathway:pathway)
        (neo $Tx:transcript translates_to $Prot1:protein)
        (neo $Prot1:protein interacts_with $Prot2:protein))
      (result $Gene $Pathway $Enh $Tx $Prot1 $Prot2))
prolog=(neo(Gene:gene,gene_name,"IGF1"),neo(Tx:transcript,transcribed_from,Gene:gene),neo(Enh:enhancer,associated_with,Gene:gene),neo(Gene:gene,genes_pay),neo(Tx:transcript,translates_to,Prot1:protein),neo(Prot1:protein,interacts_with,Prot2:protein),true)




### Query6 - Pathways and Protein Interactions for IGF1
```no-wrap
=================================================================
        Find pathways and interacting proteins for the IGF1 gene including all associated components.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")
      (neo $Tx:transcript transcribed_from $Gene:gene)
      (neo $Enh:enhancer associated_with $Gene:gene)
      (neo $Gene:gene genes_pathways $Pathway:pathway)
      (neo $Tx:transcript translates_to $Prot1:protein)
      (neo $Prot1:protein interacts_with $Prot2:protein))
    (result $Prot2 $Prot1 $Pathway $Enh $Tx $Gene))

o43541                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
p04141                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
q12879                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
p46939                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
q16656                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
o43741                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
q9bxn2                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
o75173                      p05019                      r-hsa-109582                chr12_102848221_102849260_grch38 enst00000337514             ensg000
q05086                      p05019                      r-hsa-2404192               chr12_102848221_102849260_grch38 enst00000337514             ensg000
q8tf66                      p05019                      r-hsa-76005                 chr12_102498523_102498856_grch38 enst00000337514             ensg000
p14317                      p05019                      r-hsa-76002                 chr12_102498523_102498856_grch38 enst00000337514             ensg000
p05362                      p05019                      r-hsa-9006934               chr12_102415197_102415568_grch38 enst00000337514             ensg000
 Time limit 120 seconds exceeded!
% 4,441,521,017 inferences, 119.501 CPU in 120.000 seconds (100% CPU, 37167145 Lips)
 Last answer found: 119.50 seconds
 Number of answers: 35,798,961
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name "IGF1" $P1)
        (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)
        (neo_P $Enh:enhancer associated_with $Gene:gene $P3)
        (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)
        (neo_P $Tx:transcript translates_to $Prot1:protein $P5)
        (neo_P $Prot1:protein interacts_with $Prot2:protein $P6))
      (result $Gene $Pathway $Enh $Tx $Prot1 $Prot2 $P1 $P2 $P3 $P4 $P5 $P6))
prolog=(neo_P(Gene:gene,gene_name,"IGF1",P1),neo_P(Tx:transcript,transcribed_from,Gene:gene,P4),neo_P(Enh:enhancer,associated_with,Gene:gene,P3),neo_P(Gys,Pathway:pathway,P2),neo_P(Tx:transcript,translates_to,Prot1:protein,P5),neo_P(Prot1:protein,interacts_with,Prot2:protein,P6),true)




### Query6 (With Properties) - Pathways and Protein Interactions for IGF1
```no-wrap
=================================================================
        Find pathways and interacting proteins for the IGF1 gene, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $P1)
      (neo_P $Tx:transcript transcribed_from $Gene:gene $P4)
      (neo_P $Enh:enhancer associated_with $Gene:gene $P3)
      (neo_P $Gene:gene genes_pathways $Pathway:pathway $P2)
      (neo_P $Tx:transcript translates_to $Prot1:protein $P5)
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P6))
    (result $P6 $Prot2 $P5 $Prot1 $P2 $Pathway $P3 $Enh $P4 $Tx $P1 $Gene))

[score=0.225] o43541        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.541] p04141        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.25]  q12879        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.282] p46939        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.271] q16656        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.167] o43741        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.227] q9bxn2        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.373] o75173        ()            p05019        ()            r-hsa-109582   ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.2]   q05086        ()            p05019        ()            r-hsa-2404192  ...
    [biological_context='CLO_0007947',score=0.699872] chr12_102848221_102849260_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.185] q8tf66        ()            p05019        ()            r-hsa-76005    ...
    [biological_context='CLO_0037076'] chr12_102498523_102498856_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.195] p14317        ()            p05019        ()            r-hsa-76002    ...
    [biological_context='CLO_0050119'] chr12_102498523_102498856_grch38 ()            enst00000337514 ()            ensg00000017427
[score=0.694] p05362        ()            p05019        ()            r-hsa-9006934  ...
    [biological_context='CLO_0008089'] chr12_102415197_102415568_grch38 ()            enst00000337514 ()            ensg00000017427
 Time limit 120 seconds exceeded!
% 4,403,889,749 inferences, 119.427 CPU in 120.000 seconds (100% CPU, 36875159 Lips)
 Last answer found: 119.43 seconds
 Number of answers: 35,492,255
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name
          (endsWith "TP73-AS1"))
        (neo $Tx:transcript transcribed_from $Gene:gene)
        (neo $Tx:transcript includes $Exon:exon))
      (result $Tx $Exon $Gene))
prolog=(neo(Gene:gene,gene_name,endsWith("TP73-AS1")),neo(Tx:transcript,transcribed_from,Gene:gene),neo(Tx:transcript,includes,Exon:exon),true)




### Query7 - Transcripts and Exons for TP73-AS1
```no-wrap
=================================================================
        Find transcripts and exons associated with the TP73-AS1 gene.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (endsWith "TP73-AS1"))
      (neo $Tx:transcript transcribed_from $Gene:gene)
      (neo $Tx:transcript includes $Exon:exon))
    (result $Exon $Tx $Gene))

ense00003836919                                   enst00000648526                                   ensg00000227372
ense00001787305                                   enst00000648526                                   ensg00000227372
ense00001734384                                   enst00000648526                                   ensg00000227372
ense00003798426                                   enst00000636630                                   ensg00000227372
ense00003791729                                   enst00000636630                                   ensg00000227372
ense00003798500                                   enst00000636630                                   ensg00000227372
 MeTTaLog Execution time: 0.17 seconds
% 2,334,985 inferences, 0.172 CPU in 0.173 seconds (99% CPU, 13613977 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 142
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name
          (endsWith "TP73-AS1") $P3)
        (neo_P $Tx:transcript transcribed_from $Gene:gene $P2)
        (neo_P $Tx:transcript includes $Exon:exon $P1))
      (result $Tx $Exon $Gene $P1 $P2 $P3))
prolog=(neo_P(Gene:gene,gene_name,endsWith("TP73-AS1"),P3),neo_P(Tx:transcript,transcribed_from,Gene:gene,P2),neo_P(Tx:transcript,includes,Exon:exon,P1),true)




### Query7 (With Properties) - Transcripts and Exons for TP73-AS1
```no-wrap
=================================================================
        Find transcripts and exons associated with the TP73-AS1 gene, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (endsWith "TP73-AS1") $P3)
      (neo_P $Tx:transcript transcribed_from $Gene:gene $P2)
      (neo_P $Tx:transcript includes $Exon:exon $P1))
    (result $P1 $Exon $P2 $Tx $P3 $Gene))

()                       ense00003836919          ()                       enst00000648526          ()                       ensg00000227372
()                       ense00001787305          ()                       enst00000648526          ()                       ensg00000227372
()                       ense00001734384          ()                       enst00000648526          ()                       ensg00000227372
()                       ense00003798426          ()                       enst00000636630          ()                       ensg00000227372
()                       ense00003791729          ()                       enst00000636630          ()                       ensg00000227372
()                       ense00003798500          ()                       enst00000636630          ()                       ensg00000227372
 MeTTaLog Execution time: 0.07 seconds
% 2,337,142 inferences, 0.074 CPU in 0.074 seconds (100% CPU, 31598244 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 142
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Prot1:protein protein_name
          (stringEqual "1433S"))
        (neo $Prot1:protein interacts_with $Prot2:protein)
        (neo $GO:go go_gene_product $Prot1:protein))
      (result $GO $Prot1 $Prot2))
prolog=(neo(Prot1:protein,protein_name,stringEqual("1433S")),neo(Prot1:protein,interacts_with,Prot2:protein),neo(GO:go,go_gene_product,Prot1:protein),true)




### Query8 - Interactions Involving 1433S Protein
```no-wrap
=================================================================
        Find proteins interacting with 1433S and associated GO terms.

  (match &neo4j_out_v3
    (,
      (neo $Prot1:protein protein_name
        (stringEqual "1433S"))
      (neo $Prot1:protein interacts_with $Prot2:protein)
      (neo $GO:go go_gene_product $Prot1:protein))
    (result $GO $Prot2 $Prot1))

go_0005515                                        p68032                                            p31947
go_0005515                                        p68032                                            p31947
go_0005515                                        p68032                                            p31947
go_2000647                                        p68032                                            p31947
go_0005615                                        p68032                                            p31947
go_0005829                                        p68032                                            p31947
go_0005515                                        q99102                                            p31947
go_0005515                                        q9gzv4                                            p31947
go_0042802                                        p31350                                            p31947
 MeTTaLog Execution time: 0.66 seconds
% 17,881,509 inferences, 0.661 CPU in 0.664 seconds (100% CPU, 27060129 Lips)
 Last answer found: 0.62 seconds
 Number of answers: 243,331
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Prot1:protein protein_name
          (stringEqual "1433S") $P1)
        (neo_P $Prot1:protein interacts_with $Prot2:protein $P3)
        (neo_P $GO:go go_gene_product $Prot1:protein $P2))
      (result $GO $Prot1 $Prot2 $P1 $P2 $P3))
prolog=(neo_P(Prot1:protein,protein_name,stringEqual("1433S"),P1),neo_P(Prot1:protein,interacts_with,Prot2:protein,P3),neo_P(GO:go,go_gene_product,Prot1:protein,P2),true)




### Query8 (With Properties) - Interactions Involving 1433S Protein
```no-wrap
=================================================================
        Find proteins interacting with 1433S and their GO terms, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Prot1:protein protein_name
        (stringEqual "1433S") $P1)
      (neo_P $Prot1:protein interacts_with $Prot2:protein $P3)
      (neo_P $GO:go go_gene_product $Prot1:protein $P2))
    (result $P2 $GO $P3 $Prot2 $P1 $Prot1))

[qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI'] go_0005515               [score=0.386]            p68032                   ()                       p31947
[qualifier=[enables],db_reference=['PMID:11574543'],evidence='IPI'] go_0005515               [score=0.386]            p68032                   ()                       p31947
[qualifier=[enables],db_reference=['PMID:11697890'],evidence='IPI'] go_0005515               [score=0.386]            p68032                   ()                       p31947
[qualifier=[involved_in],db_reference=['GO_REF:0000107'],evidence='IEA'] go_2000647               [score=0.386]            p68032                   ()                       p31947
[qualifier=[located_in],db_reference=['PMID:8515476'],evidence='TAS'] go_0005615               [score=0.386]            p68032                   ()                       p31947
[qualifier=[located_in],db_reference=['GO_REF:0000052'],evidence='IDA'] go_0005829               [score=0.386]            p68032                   ()                       p31947
[qualifier=[enables],db_reference=['PMID:15778465'],evidence='IPI'] go_0005515               [score=0.245]            q99102                   ()                       p31947
[qualifier=[enables],db_reference=['PMID:20179209'],evidence='IPI'] go_0005515               [score=0.193]            q9gzv4                   ()                       p31947
[qualifier=[enables],db_reference=['PMID:15778465'],evidence='IPI'] go_0042802               [score=0.153]            p31350                   ()                       p31947
 MeTTaLog Execution time: 0.68 seconds
% 17,916,892 inferences, 0.685 CPU in 0.688 seconds (100% CPU, 26156138 Lips)
 Last answer found: 0.65 seconds
 Number of answers: 243,331
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name
          (startsWith "IGF1"))
        (neo $Gene:gene expressed_in $Uberon:uberon)
        (neo $Gene:gene transcribed_to $Tx:transcript))
      (result $Gene $Uberon $Tx))
prolog=(neo(Gene:gene,gene_name,startsWith("IGF1")),neo(Gene:gene,expressed_in,Uberon:uberon),neo(Gene:gene,transcribed_to,Tx:transcript),true)




### Query9 - IGF1 Expression in Tissues
```no-wrap
=================================================================
        Find IGF1 expression in tissues and related transcripts.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (startsWith "IGF1"))
      (neo $Gene:gene expressed_in $Uberon:uberon)
      (neo $Gene:gene transcribed_to $Tx:transcript))
    (result $Tx $Uberon $Gene))

enst00000337514                                   uberon_0000473                                    ensg00000017427
enst00000481539                                   uberon_0000473                                    ensg00000017427
enst00000392904                                   uberon_0000473                                    ensg00000017427
enst00000392904                                   uberon_0000006                                    ensg00000017427
enst00000644491                                   uberon_0000006                                    ensg00000017427
enst00000392905                                   uberon_0000006                                    ensg00000017427
enst00000644491                                   uberon_0000082                                    ensg00000017427
enst00000307046                                   uberon_0000473                                    ensg00000017427
enst00000424202                                   uberon_0010533                                    ensg00000017427
 MeTTaLog Execution time: 1.38 seconds
% 5,880,382 inferences, 1.377 CPU in 1.386 seconds (99% CPU, 4270054 Lips)
 Last answer found: 1.36 seconds
 Number of answers: 47,862
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name
          (startsWith "IGF1") $P1)
        (neo_P $Gene:gene expressed_in $Uberon:uberon $P2)
        (neo_P $Gene:gene transcribed_to $Tx:transcript $P3))
      (result $Gene $Uberon $Tx $P1 $P2 $P3))
prolog=(neo_P(Gene:gene,gene_name,startsWith("IGF1"),P1),neo_P(Gene:gene,expressed_in,Uberon:uberon,P2),neo_P(Gene:gene,transcribed_to,Tx:transcript,P3),true)




### Query9 (With Properties) - IGF1 Expression in Tissues
```no-wrap
=================================================================
        Find IGF1 expression in tissues and related transcripts, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (startsWith "IGF1") $P1)
      (neo_P $Gene:gene expressed_in $Uberon:uberon $P2)
      (neo_P $Gene:gene transcribed_to $Tx:transcript $P3))
    (result $P3 $Tx $P2 $Uberon $P1 $Gene))

()                       enst00000337514          [score=90.6,p_value=2.112928143343311e-07] uberon_0000473           ()                       ensg00000017427
()                       enst00000481539          [score=90.6,p_value=2.112928143343311e-07] uberon_0000473           ()                       ensg00000017427
()                       enst00000392904          [score=90.6,p_value=2.112928143343311e-07] uberon_0000473           ()                       ensg00000017427
()                       enst00000392904          [score=50.09,p_value=1.2104808235114428e-05] uberon_0000006           ()                       ensg00000017427
()                       enst00000644491          [score=50.09,p_value=1.2104808235114428e-05] uberon_0000006           ()                       ensg00000017427
()                       enst00000392905          [score=50.09,p_value=1.2104808235114428e-05] uberon_0000006           ()                       ensg00000017427
()                       enst00000644491          [score=70.43,p_value=2.38570677626595e-07] uberon_0000082           ()                       ensg00000017427
()                       enst00000307046          [score=77.78,p_value=0.0016728772206653] uberon_0000473           ()                       ensg00000017427
()                       enst00000424202          [score=53.89,p_value=0.0002903593083101] uberon_0010533           ()                       ensg00000017427
 MeTTaLog Execution time: 1.23 seconds
% 5,946,820 inferences, 1.231 CPU in 1.241 seconds (99% CPU, 4829326 Lips)
 Last answer found: 1.21 seconds
 Number of answers: 47,862
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Exon:exon chr chr1)
        (neo $Tx:transcript includes $Exon:exon)
        (neo $Tx:transcript translates_to $P1:protein)
        (neo $P2:protein interacts_with $P1:protein))
      (result $Tx $Exon $P1 $P2))
prolog=(neo(Exon:exon,chr,chr1),neo(Tx:transcript,includes,Exon:exon),neo(Tx:transcript,translates_to,P1:protein),neo(P2:protein,interacts_with,P1:protein),true)




### Query10 - Transcripts & Exons on Chromosome 1
```no-wrap
=================================================================
        Find transcripts, exons (on chr1), and interacting proteins.

  (match &neo4j_out_v3
    (,
      (neo $Exon:exon chr chr1)
      (neo $Tx:transcript includes $Exon:exon)
      (neo $Tx:transcript translates_to $P1:protein)
      (neo $P2:protein interacts_with $P1:protein))
    (result $P2 $P1 $Tx $Exon))

q9h098                                q6iey1                                enst00000426406                       ense00003989331
p50548                                q6iey1                                enst00000426406                       ense00003989331
q9h720                                q6iey1                                enst00000426406                       ense00003989331
q8n1m1                                q6iey1                                enst00000426406                       ense00003989331
q8n1m1                                q6iey1                                enst00000426406                       ense00003989331
q15735                                q6iey1                                enst00000426406                       ense00003989331
q8n660                                q6iey1                                enst00000332831                       ense00004001351
q9uh03                                q96nu1                                enst00000342066                       ense00003915575
q9np77                                q96nu1                                enst00000342066                       ense00003477353
q6gph4                                o00468                                enst00000379370                       ense00001641464
p25025                                o00468                                enst00000379370                       ense00003686258
q8izn3                                q5ta45                                enst00000545578                       ense00003493216
 Time limit 120 seconds exceeded!
% 3,555,779,537 inferences, 119.443 CPU in 120.000 seconds (100% CPU, 29769765 Lips)
 Last answer found: 119.44 seconds
 Number of answers: 40,264,188
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Exon:exon chr chr1 $P2)
        (neo_P $Tx:transcript includes $Exon:exon $P1)
        (neo_P $Tx:transcript translates_to $P3:protein $P3Props)
        (neo_P $P4:protein interacts_with $P3:protein $P4Props))
      (result $Tx $Exon $P3 $P4 $P1 $P2 $P3Props $P4Props))
prolog=(neo_P(Exon:exon,chr,chr1,P2),neo_P(Tx:transcript,includes,Exon:exon,P1),neo_P(Tx:transcript,translates_to,P3:protein,P3Props),neo_P(P4:protein,interacts_with,P3:protein,P4Props),true)




### Query10 (With Properties) - Transcripts & Exons on Chromosome 1
```no-wrap
=================================================================
        Find transcripts, exons (on chr1), and interacting proteins, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Exon:exon chr chr1 $P2)
      (neo_P $Tx:transcript includes $Exon:exon $P1)
      (neo_P $Tx:transcript translates_to $P3:protein $P3Props:p)
      (neo_P $P4:protein interacts_with $P3:protein $P4Props:p))
    (result $P4Props:p $P4 $P3Props:p $P3 $P1 $Tx $P2 $Exon))

[score=0.173]      q9h098             ()                 q6iey1             ()                 enst00000426406    ()                 ense00003989331
[score=0.222]      p50548             ()                 q6iey1             ()                 enst00000426406    ()                 ense00003989331
[score=0.325]      q9h720             ()                 q6iey1             ()                 enst00000426406    ()                 ense00003989331
[score=0.305]      q8n1m1             ()                 q6iey1             ()                 enst00000426406    ()                 ense00003989331
[score=0.311]      q8n1m1             ()                 q6iey1             ()                 enst00000426406    ()                 ense00003989331
[score=0.287]      q15735             ()                 q6iey1             ()                 enst00000426406    ()                 ense00003989331
[score=0.383]      q8n660             ()                 q6iey1             ()                 enst00000332831    ()                 ense00004001351
[score=0.154]      q9uh03             ()                 q96nu1             ()                 enst00000342066    ()                 ense00003915575
[score=0.237]      q9np77             ()                 q96nu1             ()                 enst00000342066    ()                 ense00003477353
[score=0.17]       q6gph4             ()                 o00468             ()                 enst00000379370    ()                 ense00001641464
[score=0.18]       p25025             ()                 o00468             ()                 enst00000379370    ()                 ense00003686258
[score=0.159]      q8izn3             ()                 q5ta45             ()                 enst00000545578    ()                 ense00003493216
 Time limit 120 seconds exceeded!
% 3,521,108,957 inferences, 119.406 CPU in 120.000 seconds (100% CPU, 29488571 Lips)
 Last answer found: 119.41 seconds
 Number of answers: 39,821,596
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name "IGF1")
        (neo $Gene:gene expressed_in $CL1:cl)
        (neo $CL2:cl subclass_of $CL1:cl))
      (result $Gene $CL1 $CL2))
prolog=(neo(Gene:gene,gene_name,"IGF1"),neo(Gene:gene,expressed_in,CL1:cl),neo(CL2:cl,subclass_of,CL1:cl),true)




### Query11 - IGF1 Gene Expression in Cell Lines
```no-wrap
=================================================================
        Find IGF1 gene expression in cell lines and related subclass relationships.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name "IGF1")
      (neo $Gene:gene expressed_in $CL1:cl)
      (neo $CL2:cl subclass_of $CL1:cl))
    (result $CL2 $CL1 $Gene))

cl_0000018                                        cl_0000015                                        ensg00000017427
cl_0000020                                        cl_0000015                                        ensg00000017427
cl_0000017                                        cl_0000015                                        ensg00000017427
cl_0002332                                        cl_0002328                                        ensg00000017427
cl_1000312                                        cl_0002328                                        ensg00000017427
cl_4033003                                        cl_0002328                                        ensg00000017427
 MeTTaLog Execution time: 0.00 seconds
% 11,588 inferences, 0.004 CPU in 0.004 seconds (97% CPU, 2858976 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 109
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name "IGF1" $P1)
        (neo_P $Gene:gene expressed_in $CL1:cl $P2)
        (neo_P $CL2:cl subclass_of $CL1:cl $P3))
      (result $Gene $CL1 $CL2 $P1 $P2 $P3))
prolog=(neo_P(Gene:gene,gene_name,"IGF1",P1),neo_P(Gene:gene,expressed_in,CL1:cl,P2),neo_P(CL2:cl,subclass_of,CL1:cl,P3),true)




### Query11 (With Properties) - IGF1 Gene Expression in Cell Lines
```no-wrap
=================================================================
        Find IGF1 gene expression in cell lines, plus subclass relationships, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name "IGF1" $P1)
      (neo_P $Gene:gene expressed_in $CL1:cl $P2)
      (neo_P $CL2:cl subclass_of $CL1:cl $P3))
    (result $P3 $CL2 $P2 $CL1 $P1 $Gene))

()                       cl_0000018               [score=79.15,p_value=0.0033457544413306] cl_0000015               ()                       ensg00000017427
()                       cl_0000020               [score=79.15,p_value=0.0033457544413306] cl_0000015               ()                       ensg00000017427
()                       cl_0000017               [score=79.15,p_value=0.0033457544413306] cl_0000015               ()                       ensg00000017427
()                       cl_0002332               [score=57.27,p_value=0.024552789258141] cl_0002328               ()                       ensg00000017427
()                       cl_1000312               [score=57.27,p_value=0.024552789258141] cl_0002328               ()                       ensg00000017427
()                       cl_4033003               [score=57.27,p_value=0.024552789258141] cl_0002328               ()                       ensg00000017427
 MeTTaLog Execution time: 0.00 seconds
% 13,060 inferences, 0.002 CPU in 0.002 seconds (96% CPU, 5962654 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 109
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name
          (startsWith "IGF1"))
        (neo $Snp:snp activity_by_contact $Gene:gene))
      (result $Snp $Gene))
prolog=(neo(Gene:gene,gene_name,startsWith("IGF1")),neo(Snp:snp,activity_by_contact,Gene:gene),true)




### Query12 - IGF1 Gene Regulation by SNP
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (startsWith "IGF1"))
      (neo $Snp:snp activity_by_contact $Gene:gene))
    (result $Snp $Gene))

rs1019731                                                                   ensg00000017427
rs10507135                                                                  ensg00000017427
rs10507135                                                                  ensg00000017427
rs114119866                                                                 ensg00000017427
rs114119866                                                                 ensg00000017427
rs114160584                                                                 ensg00000017427
rs184405224                                                                 ensg00000017427
rs10163105                                                                  ensg00000140443
 MeTTaLog Execution time: 0.12 seconds
% 2,483,042 inferences, 0.119 CPU in 0.121 seconds (98% CPU, 20870863 Lips)
 Last answer found: 0.10 seconds
 Number of answers: 4,285
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name
          (startsWith "IGF1") $P1)
        (neo_P $Snp:snp activity_by_contact $Gene:gene $P2))
      (result $Snp $Gene $P1 $P2))
prolog=(neo_P(Gene:gene,gene_name,startsWith("IGF1"),P1),neo_P(Snp:snp,activity_by_contact,Gene:gene,P2),true)




### Query12 (With Properties) - IGF1 Gene Regulation by SNP
```no-wrap
=================================================================
        Find regulation of the IGF1 gene by SNP activity, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (startsWith "IGF1") $P1)
      (neo_P $Snp:snp activity_by_contact $Gene:gene $P2))
    (result $P2 $Snp $P1 $Gene))

[score=0.01959,biological_context='CL_0000103'] rs1019731                             ()                                    ensg00000017427             
[score=0.027607,biological_context='UBERON_0000992'] rs10507135                            ()                                    ensg00000017427        
[score=0.030463,biological_context='CLO_0007599'] rs10507135                            ()                                    ensg00000017427           
[score=0.085288,biological_context='CL_0001070'] rs114119866                           ()                                    ensg00000017427            
[score=0.056618,biological_context='CL_0000448'] rs114119866                           ()                                    ensg00000017427            
[score=0.020362,biological_context='CLO_0037116'] rs114160584                           ()                                    ensg00000017427           
[score=0.020066,biological_context='CLO_0001605'] rs184405224                           ()                                    ensg00000017427           
[score=0.022901,biological_context='CLO_0037281'] rs10163105                            ()                                    ensg00000140443           
 MeTTaLog Execution time: 0.08 seconds
% 2,483,980 inferences, 0.082 CPU in 0.082 seconds (99% CPU, 30465834 Lips)
 Last answer found: 0.06 seconds
 Number of answers: 4,285
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene:gene gene_name
          (startsWith "IGF1"))
        (neo $GeneReg:gene regulates $Gene:gene)
        (neo $Gene:gene expressed_in $CL1:cl)
        (neo $GeneReg:gene transcribed_to $Tx:transcript)
        (neo $Tx:transcript translates_to $Prot1:protein)
        (neo $Prot2:protein interacts_with $Prot1:protein)
        (neo $CL2:cl subclass_of $CL1:cl))
      (result $Gene $CL1 $CL2 $GeneReg $Tx $Prot1 $Prot2))
prolog=(neo(Gene:gene,gene_name,startsWith("IGF1")),neo(GeneReg:gene,regulates,Gene:gene),neo(Gene:gene,expressed_in,CL1:cl),neo(GeneReg:gene,transcribed_to,Tx:transcript),neo(Tx:transcript,translates_to,Prot1:protein),neo(Prot2:protein,interacts_with,Prot1:protein),neo(CL2:cl,subclass_of,CL1:cl),true)




### Query13 - IGF1 Gene Interactions and Regulations
```no-wrap
=================================================================
        Find IGF1 gene interactions, regulations, transcripts, proteins, etc.

  (match &neo4j_out_v3
    (,
      (neo $Gene:gene gene_name
        (startsWith "IGF1"))
      (neo $GeneReg:gene regulates $Gene:gene)
      (neo $Gene:gene expressed_in $CL1:cl)
      (neo $GeneReg:gene transcribed_to $Tx:transcript)
      (neo $Tx:transcript translates_to $Prot1:protein)
      (neo $Prot2:protein interacts_with $Prot1:protein)
      (neo $CL2:cl subclass_of $CL1:cl))
    (result $CL2 $Prot2 $Prot1 $Tx $CL1 $GeneReg $Gene))

cl_0000018           q96ru2               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000020           q96ru2               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000017           q96ru2               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000020           q12864               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000017           q12864               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000016           q12864               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000017           p25963               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000016           p10767               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000408           q86y37               p06400               enst00000267163      cl_0000015           ensg00000139687      ensg00000017427
cl_0000017           p02774               p03372               enst00000206249      cl_0000015           ensg00000091831      ensg00000017427
cl_0000842           p59768               p03372               enst00000440973      cl_0000738           ensg00000091831      ensg00000017427
cl_0000576           p26599               p38398               enst00000491747      cl_0000842           ensg00000012048      ensg00000017427
 Time limit 120 seconds exceeded!
% 4,519,440,452 inferences, 119.439 CPU in 120.000 seconds (100% CPU, 37838977 Lips)
 Last answer found: 119.44 seconds
 Number of answers: 28,983,412
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene:gene gene_name
          (startsWith "IGF1") $P1)
        (neo_P $GeneReg:gene regulates $Gene:gene $P4)
        (neo_P $Gene:gene expressed_in $CL1:cl $P2)
        (neo_P $GeneReg:gene transcribed_to $Tx:transcript $P5)
        (neo_P $Tx:transcript translates_to $Prot1:protein $P6)
        (neo_P $Prot2:protein interacts_with $Prot1:protein $P7)
        (neo_P $CL2:cl subclass_of $CL1:cl $P3))
      (result $Gene $CL1 $CL2 $GeneReg $Tx $Prot1 $Prot2 $P1 $P2 $P3 $P4 $P5 $P6 $P7))
prolog=(neo_P(Gene:gene,gene_name,startsWith("IGF1"),P1),neo_P(GeneReg:gene,regulates,Gene:gene,P4),neo_P(Gene:gene,expressed_in,CL1:cl,P2),neo_P(GeneReg:gene,transcribed_to,Tx:transcript,P5),neo_P(Tx:transcript,translates_to,Prot1:protein,P6),neo_P(Prot2:protein,interacts_with,Prot1:protein,P7),neo_P(CL2:cl,subclass_of,CL1:cl,P3),true)




### Query13 (With Properties) - IGF1 Gene Interactions and Regulations
```no-wrap
=================================================================
        Find IGF1 gene interactions, regulations, transcripts, proteins, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene:gene gene_name
        (startsWith "IGF1") $P1)
      (neo_P $GeneReg:gene regulates $Gene:gene $P4)
      (neo_P $Gene:gene expressed_in $CL1:cl $P2)
      (neo_P $GeneReg:gene transcribed_to $Tx:transcript $P5)
      (neo_P $Tx:transcript translates_to $Prot1:protein $P6)
      (neo_P $Prot2:protein interacts_with $Prot1:protein $P7)
      (neo_P $CL2:cl subclass_of $CL1:cl $P3))
    (result $P3 $CL2 $P7 $Prot2 $P6 $Prot1 $P5 $Tx $P2 $CL1 $P4 $GeneReg $P1 $Gene))

()        cl_0000018 [score=0.177] q96ru2    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000020 [score=0.177] q96ru2    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000017 [score=0.177] q96ru2    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000020 [score=0.362] q12864    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000017 [score=0.362] q12864    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000016 [score=0.362] q12864    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000017 [score=0.212] p25963    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000016 [score=0.165] p10767    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000408 [score=0.179] q86y37    ()        p06400    ()        enst00000267163  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:18281476','pubmed:29087512'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000139687 ()        ensg00000017427
()        cl_0000017 [score=0.179] p02774    ()        p03372    ()        enst00000206249  ...
    [score=79.15,p_value=0.0033457544413306] cl_0000015  ...
    [evidence=['pubmed:29126285','pubmed:27924024','pubmed:17202159'],databases=['GTRD','ReMap','TRED'],evidence_type=small_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator'] ensg00000091831 ()        ensg00000017427
()        cl_0000842 [score=0.507] p59768    ()        p03372    ()        enst00000440973  ...
    [score=70.88,p_value=8.0709325461721e-05] cl_0000738  ...
    [evidence=['pubmed:29126285','pubmed:27924024','pubmed:17202159'],databases=['GTRD','ReMap','TRED'],evidence_type=small_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator'] ensg00000091831 ()        ensg00000017427
()        cl_0000576 [score=0.181] p26599    ()        p38398    ()        enst00000491747  ...
    [score=42.65,p_value=0.0068406633443062] cl_0000842  ...
    [evidence=['pubmed:29087512','pubmed:18045956'],databases=['TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000012048 ()        ensg00000017427
 Time limit 120 seconds exceeded!
% 4,503,949,630 inferences, 119.431 CPU in 120.000 seconds (100% CPU, 37711621 Lips)
 Last answer found: 119.43 seconds
 Number of answers: 28,544,983
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $Gene1:gene gene_name "SNAP25")
        (neo $Gene1:gene genes_pathways $Pathway:pathway)
        (neo $Gene2:gene genes_pathways $Pathway:pathway))
      (result $Gene1 $Pathway $Gene2))
prolog=(neo(Gene1:gene,gene_name,"SNAP25"),neo(Gene1:gene,genes_pathways,Pathway:pathway),neo(Gene2:gene,genes_pathways,Pathway:pathway),true)




### Query14 - Pathway Associations for SNAP25
```no-wrap
=================================================================
        Locate SNAP25 in pathways with other genes.

  (match &neo4j_out_v3
    (,
      (neo $Gene1:gene gene_name "SNAP25")
      (neo $Gene1:gene genes_pathways $Pathway:pathway)
      (neo $Gene2:gene genes_pathways $Pathway:pathway))
    (result $Gene2 $Pathway $Gene1))

ensg00000005379                                   r-hsa-112310                                      ensg00000132639
ensg00000008056                                   r-hsa-112310                                      ensg00000132639
ensg00000010379                                   r-hsa-112310                                      ensg00000132639
ensp00000355920                                   r-hsa-112310                                      ensg00000132639
ensp00000355930                                   r-hsa-112310                                      ensg00000132639
ensp00000360549                                   r-hsa-112310                                      ensg00000132639
ensg00000117676                                   r-hsa-112315                                      ensg00000132639
ensp00000335592                                   r-hsa-112315                                      ensg00000132639
ensp00000445409                                   r-hsa-1280215                                     ensg00000132639
 MeTTaLog Execution time: 0.57 seconds
% 6,858,598 inferences, 0.571 CPU in 0.574 seconds (100% CPU, 12002418 Lips)
 Last answer found: 0.57 seconds
 Number of answers: 97,865
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $Gene1:gene gene_name "SNAP25" $P1)
        (neo_P $Gene1:gene genes_pathways $Pathway:pathway $P2)
        (neo_P $Gene2:gene genes_pathways $Pathway:pathway $P3))
      (result $Gene1 $Pathway $Gene2 $P1 $P2 $P3))
prolog=(neo_P(Gene1:gene,gene_name,"SNAP25",P1),neo_P(Gene1:gene,genes_pathways,Pathway:pathway,P2),neo_P(Gene2:gene,genes_pathways,Pathway:pathway,P3),true)




### Query14 (With Properties) - Pathway Associations for SNAP25
```no-wrap
=================================================================
        Locate SNAP25 in pathways with other genes, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $Gene1:gene gene_name "SNAP25" $P1)
      (neo_P $Gene1:gene genes_pathways $Pathway:pathway $P2)
      (neo_P $Gene2:gene genes_pathways $Pathway:pathway $P3))
    (result $P3 $Gene2 $P2 $Pathway $P1 $Gene1))

()                       ensg00000005379          ()                       r-hsa-112310             ()                       ensg00000132639
()                       ensg00000008056          ()                       r-hsa-112310             ()                       ensg00000132639
()                       ensg00000010379          ()                       r-hsa-112310             ()                       ensg00000132639
()                       ensp00000355920          ()                       r-hsa-112310             ()                       ensg00000132639
()                       ensp00000355930          ()                       r-hsa-112310             ()                       ensg00000132639
()                       ensp00000360549          ()                       r-hsa-112310             ()                       ensg00000132639
()                       ensg00000117676          ()                       r-hsa-112315             ()                       ensg00000132639
()                       ensp00000335592          ()                       r-hsa-112315             ()                       ensg00000132639
()                       ensp00000445409          ()                       r-hsa-1280215            ()                       ensg00000132639
 MeTTaLog Execution time: 0.26 seconds
% 6,861,553 inferences, 0.265 CPU in 0.266 seconds (100% CPU, 25919746 Lips)
 Last answer found: 0.26 seconds
 Number of answers: 97,865
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $G:gene transcribed_to $T:transcript)
        (neo $T:transcript translates_to $P:protein)
        (neo $P:protein go_gene_product $GO:go))
      (aggregate $G
        (count $T numTranscripts)
        (count $P numProteins)
        (count $GO numGOterms)
        (sum
          (+ numTranscripts numProteins numGOterms) total))
      (result $G $NumTranscripts $NumProteins $NumGOterms $total))
prolog=(neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,translates_to,P:protein),neo(P:protein,go_gene_product,GO:go),true)




### Counts of Genes/Transcripts/Proteins/GO in Desc Order
```no-wrap
=================================================================
        Counts of genes and their number of transcripts, proteins, and GO terms in decreasing order.

  (match &neo4j_out_v3
    (,
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript translates_to $P:protein)
      (neo $P:protein go_gene_product $GO:go))
    (result $GO $P $T $G))

 MeTTaLog Execution time: 0.77 seconds
% 23,215,699 inferences, 0.770 CPU in 0.773 seconds (99% CPU, 30169363 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (limit 1
      (sort (match &neo4j_out_v3 (, (neo $G:gene transcribed_to $T:transcript) (neo $T:transcript translates_to $P:protein) (neo $P:protein go_gene_product $GO:go)) (aggregate $G (count $T numTranscripts) (count $P numProteins) (count $GO numGOterms) (sum (+ numTranscripts numProteins numGOterms) total)) (result $total $G $NumTranscripts $NumProteins $NumGOterms))))
prolog=(neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,translates_to,P:protein),neo(P:protein,go_gene_product,GO:go),true)




### LIMIT 1 Counts of Genes/Transcripts/Proteins/GO in Desc Order
```no-wrap
=================================================================
        Counts of genes and their number of transcripts, proteins, and GO terms in decreasing order.

  (match &neo4j_out_v3
    (,
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript translates_to $P:protein)
      (neo $P:protein go_gene_product $GO:go))
    (result $GO $P $T $G))

 MeTTaLog Execution time: 0.66 seconds
% 23,215,699 inferences, 0.663 CPU in 0.667 seconds (99% CPU, 34997414 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $G:gene gene_name "TTN")
        (neo $G:gene transcribed_to $T:transcript)
        (neo $T:transcript translates_to $P:protein)
        (neo $P:protein go_gene_product $GO:go))
      (result $G $T $P $GO))
prolog=(neo(G:gene,gene_name,"TTN"),neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,translates_to,P:protein),neo(P:protein,go_gene_product,GO:go),true)




### TTN: All transcripts, proteins, and GO terms
```no-wrap
=================================================================
        Find all transcripts, proteins, and GO terms connected to the TTN gene.

  (match &neo4j_out_v3
    (,
      (neo $G:gene gene_name "TTN")
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript translates_to $P:protein)
      (neo $P:protein go_gene_product $GO:go))
    (result $GO $P $T $G))

 MeTTaLog Execution time: 0.00 seconds
% 1,764 inferences, 0.001 CPU in 0.001 seconds (96% CPU, 1295344 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $G:gene gene_name "TTN")
        (neo $G:gene transcribed_to $T:transcript)
        (neo $T:transcript translates_to $P:protein)
        (neo $P:protein go_gene_product $GO:go)
        (neo $G:gene gene_name $GeneName)
        (neo $T:transcript transcript_name $TxName)
        (neo $P:protein protein_name $ProtName)
        (neo $GO:go term_name $GoName))
      (result $GeneName $TxName $ProtName $GoName))
prolog=(neo(G:gene,gene_name,"TTN"),neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,translates_to,P:protein),neo(P:protein,go_gene_product,GO:go),neo(G:gene,gene_name,GeneName),neo(T:transcript,transcript_name,TxName),neo(P:protein,protein_name,ProtName),neo(GO:go,term_name,GoName),true)




### TTN: gene_name, transcript_name, protein_name, GO term_name
```no-wrap
=================================================================
        Same TTN query but returning property fields (225 records).

  (match &neo4j_out_v3
    (,
      (neo $G:gene gene_name "TTN")
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript translates_to $P:protein)
      (neo $P:protein go_gene_product $GO:go)
      (neo $G:gene gene_name $GeneName:genename)
      (neo $T:transcript transcript_name $TxName:txname)
      (neo $P:protein protein_name $ProtName:protname)
      (neo $GO:go term_name $GoName:goname))
    (result $GoName:goname $ProtName:protname $TxName:txname $GeneName:genename $GO $P $T $G))

 MeTTaLog Execution time: 0.00 seconds
% 2,788 inferences, 0.001 CPU in 0.001 seconds (96% CPU, 2131173 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $G:gene transcribed_to $T:transcript)
        (neo $T:transcript translates_to $P:protein)
        (neo $P:protein go_gene_product $GO:go)
        (neo $G:gene gene_name $GeneName)
        (neo $T:transcript transcript_name $TxName)
        (neo $P:protein protein_name $ProtName)
        (neo $GO:go term_name $GoName))
      (result $GeneName $TxName $ProtName $GoName))
prolog=(neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,translates_to,P:protein),neo(P:protein,go_gene_product,GO:go),neo(G:gene,gene_name,GeneName),neo(T:transcript,transcript_name,TxName),neo(P:protein,protein_name,ProtName),neo(GO:go,term_name,GoName),true)




### All genes: gene_name, transcript_name, protein_name, GO term_name
```no-wrap
=================================================================
        Find transcripts, proteins, and GO terms for all genes, returning property names.

  (match &neo4j_out_v3
    (,
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript translates_to $P:protein)
      (neo $P:protein go_gene_product $GO:go)
      (neo $G:gene gene_name $GeneName:genename)
      (neo $T:transcript transcript_name $TxName:txname)
      (neo $P:protein protein_name $ProtName:protname)
      (neo $GO:go term_name $GoName:goname))
    (result $GoName:goname $ProtName:protname $TxName:txname $GeneName:genename $GO $P $T $G))

 MeTTaLog Execution time: 0.71 seconds
% 25,421,923 inferences, 0.708 CPU in 0.712 seconds (99% CPU, 35913920 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $G:gene gene_name "TTN")
        (neo $G:gene transcribed_to $T:transcript)
        (neo $T:transcript includes $E:exon)
        (neo $T:transcript transcript_name $TxName)
        (neo $G:gene gene_name $GeneName)
        (neo $E:exon exon_id $ExonID))
      (result $GeneName $TxName $ExonID))
prolog=(neo(G:gene,gene_name,"TTN"),neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,includes,E:exon),neo(T:transcript,transcript_name,TxName),neo(G:gene,gene_name,GeneName),neo(E:exon,exon_id,ExonID),true)




### TTN: Transcripts and Exons
```no-wrap
=================================================================
        Find transcripts and exons for TTN, returning property fields (1700 records).

  (match &neo4j_out_v3
    (,
      (neo $G:gene gene_name "TTN")
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript includes $E:exon)
      (neo $T:transcript transcript_name $TxName:txname)
      (neo $G:gene gene_name $GeneName:genename)
      (neo $E:exon exon_id $ExonID:exonid))
    (result $ExonID:exonid $GeneName:genename $TxName:txname $E $T $G))

ENSE00001465556          TTN                      TTN-202                  ense00001465556          enst00000342992          ensg00000155657
ENSE00001465556          TTN                      TTN-202                  ense00001465556          enst00000342992          ensg00000155657
ENSE00001465556          TTN                      TTN-202                  ense00001465556          enst00000342992          ensg00000155657
ENSE00003807606          TTN                      TTN-202                  ense00003807606          enst00000342992          ensg00000155657
ENSE00003807606          TTN                      TTN-202                  ense00003807606          enst00000342992          ensg00000155657
ENSE00003807606          TTN                      TTN-202                  ense00003807606          enst00000342992          ensg00000155657
ENSE00003808716          TTN                      TTN-202                  ense00003808716          enst00000342992          ensg00000155657
ENSE00003804165          TTN                      TTN-202                  ense00003804165          enst00000342992          ensg00000155657
ENSE00003802603          TTN                      TTN-212                  ense00003802603          enst00000460472          ensg00000155657
 MeTTaLog Execution time: 0.16 seconds
% 3,178,141 inferences, 0.157 CPU in 0.158 seconds (100% CPU, 20207425 Lips)
 Last answer found: 0.16 seconds
 Number of answers: 53,912
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $G:gene transcribed_to $T:transcript)
        (neo $T:transcript includes $E:exon)
        (neo $E:exon exon_id $ExonID)
        (neo $T:transcript transcript_name $TxName)
        (neo $G:gene gene_name $GeneName))
      (result $GeneName $TxName $ExonID))
prolog=(neo(G:gene,transcribed_to,T:transcript),neo(T:transcript,includes,E:exon),neo(E:exon,exon_id,ExonID),neo(T:transcript,transcript_name,TxName),neo(G:gene,gene_name,GeneName),true)




### All Genes: Transcripts and Exons
```no-wrap
=================================================================
        Find transcripts and exons for all genes, returning property fields (1,649,476 records).

  (match &neo4j_out_v3
    (,
      (neo $G:gene transcribed_to $T:transcript)
      (neo $T:transcript includes $E:exon)
      (neo $E:exon exon_id $ExonID:exonid)
      (neo $T:transcript transcript_name $TxName:txname)
      (neo $G:gene gene_name $GeneName:genename))
    (result $GeneName:genename $TxName:txname $ExonID:exonid $E $T $G))

DDX11L2                  DDX11L2-202              ENSE00002234944          ense00002234944          enst00000456328          ensg00000290825
DDX11L2                  DDX11L2-202              ENSE00003582793          ense00003582793          enst00000456328          ensg00000290825
DDX11L2                  DDX11L2-202              ENSE00002312635          ense00002312635          enst00000456328          ensg00000290825
ENSG00000241860          ENST00000466557          ENSE00001947087          ense00001947087          enst00000466557          ensg00000241860
ENSG00000241860          ENST00000466557          ENSE00001947087          ense00001947087          enst00000466557          ensg00000241860
ENSG00000241860          ENST00000466557          ENSE00001949945          ense00001949945          enst00000466557          ensg00000241860
ENSG00000290385          ENST00000641845          ENSE00003186556          ense00003186556          enst00000641845          ensg00000290385
ENSG00000230021          ENST00000440196          ENSE00001718533          ense00001718533          enst00000440196          ensg00000230021
LINC01128                LINC01128-205            ENSE00001656290          ense00001656290          enst00000445118          ensg00000228794
KCNAB2                   KCNAB2-205               ENSE00002941030          ense00002941030          enst00000378092          ensg00000069424
KDM1A                    KDM1A-214                ENSE00003930014          ense00003930014          enst00000687202          ensg00000004487
IL23R                    IL23R-201                ENSE00003800467          ense00003800467          enst00000347310          ensg00000162594
 Time limit 120 seconds exceeded!
% 4,025,252,537 inferences, 119.150 CPU in 120.000 seconds (99% CPU, 33783071 Lips)
 Last answer found: 119.15 seconds
 Number of answers: 5,692,705
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $T:transcript transcript_id "ENST00000472835.1")
        (neo $T:transcript includes $E:exon))
      (result $T $E))
prolog=(neo(T:transcript,transcript_id,"ENST00000472835.1"),neo(T:transcript,includes,E:exon),true)




### s2a - Transcript-Exon Relationship
```no-wrap
=================================================================
        Find transcripts (by transcript_id) and their included exons.

  (match &neo4j_out_v3
    (,
      (neo $T:transcript transcript_id "ENST00000472835.1")
      (neo $T:transcript includes $E:exon))
    (result $E $T))

ense00001880441                                                             enst00000472835
ense00001885560                                                             enst00000472835
 MeTTaLog Execution time: 0.00 seconds
% 1,075 inferences, 0.000 CPU in 0.000 seconds (92% CPU, 3737830 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 2
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $T:transcript transcript_id "ENST00000472835.1" $PropList1)
        (neo_P $T:transcript includes $E:exon $PropList2))
      (result $T $E $PropList1 $PropList2))
prolog=(neo_P(T:transcript,transcript_id,"ENST00000472835.1",PropList1),neo_P(T:transcript,includes,E:exon,PropList2),true)




### s2a (With Properties) - Transcript-Exon Relationship
```no-wrap
=================================================================
        Find transcripts (by transcript_id) and their included exons, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $T:transcript transcript_id "ENST00000472835.1" $PropList1)
      (neo_P $T:transcript includes $E:exon $PropList2))
    (result $PropList2 $E $PropList1 $T))

()                                    ense00001880441                       ()                                    enst00000472835
()                                    ense00001885560                       ()                                    enst00000472835
 MeTTaLog Execution time: 0.00 seconds
% 1,479 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 3620563 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 2
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $T:transcript transcript_id "ENST00000472835.1")
        (neo $E:exon exon_id $ExonID:exonid)
        (neo $T:transcript includes $E:exon)
        (neo $T:transcript transcript_name $TranscriptName:transcriptname))
      (result $TranscriptName:transcriptname $ExonID:exonid))
prolog=(neo(T:transcript,transcript_id,"ENST00000472835.1"),neo(E:exon,exon_id,ExonID:exonid),neo(T:transcript,includes,E:exon),neo(T:transcript,transcript_name,TranscriptName:transcriptname),true)




### s2b - Transcript & Exon Names
```no-wrap
=================================================================
        Find transcripts (by transcript_id) with transcript_name and exons with exon_id.

  (match &neo4j_out_v3
    (,
      (neo $T:transcript transcript_id "ENST00000472835.1")
      (neo $E:exon exon_id $ExonID:exonid)
      (neo $T:transcript includes $E:exon)
      (neo $T:transcript transcript_name $TranscriptName:transcriptname))
    (result $TranscriptName $ExonID $E $T))

FTO-207                               ENSE00001880441                       ense00001880441                       enst00000472835
FTO-207                               ENSE00001885560                       ense00001885560                       enst00000472835
 MeTTaLog Execution time: 13.82 seconds
% 373,997,845 inferences, 13.819 CPU in 14.294 seconds (97% CPU, 27063061 Lips)
 Last answer found: 10.63 seconds
 Number of answers: 2
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $PropList1:transcript transcript_id "ENST00000472835.1" $PropList2)
        (neo_P $PropList3:exon exon_id $ExonID:exonid $PropList6)
        (neo_P $PropList1:transcript includes $PropList3:exon $PropList4)
        (neo_P $PropList1:transcript transcript_name $TranscriptName:transcriptname $PropList5))
      (result $TranscriptName:transcriptname $ExonID:exonid $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))
prolog=(neo_P(PropList1:transcript,transcript_id,"ENST00000472835.1",PropList2),neo_P(PropList3:exon,exon_id,ExonID:exonid,PropList6),neo_P(PropList1:transcript,includes,PropList3:exon,PropList4),neo_P(PropList1:transcript,transcript_name,TranscriptName:transcriptname,PropList5),true)




### s2b (With Properties) - Transcript & Exon Names
```no-wrap
=================================================================
        Find transcripts (by transcript_id) with transcript_name and exons with exon_id, returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $PropList1:transcript transcript_id "ENST00000472835.1" $PropList2)
      (neo_P $PropList3:exon exon_id $ExonID:exonid $PropList6)
      (neo_P $PropList1:transcript includes $PropList3:exon $PropList4)
      (neo_P $PropList1:transcript transcript_name $TranscriptName:transcriptname $PropList5))
    (result $PropList5 $TranscriptName $PropList4 $PropList6 $ExonID $PropList3 $PropList2 $PropList1))

()                 FTO-207            ()                 ()                 ENSE00001880441    ense00001880441    ()                 enst00000472835
()                 FTO-207            ()                 ()                 ENSE00001885560    ense00001885560    ()                 enst00000472835
 MeTTaLog Execution time: 26.88 seconds
% 864,845,033 inferences, 26.881 CPU in 27.016 seconds (99% CPU, 32173479 Lips)
 Last answer found: 20.75 seconds
 Number of answers: 2
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $GeneFTO:gene gene_name "FTO")
        (neo $GeneReg:gene regulates $GeneFTO:gene))
      (result $GeneFTO:gene $GeneReg:gene))
prolog=(neo(GeneFTO:gene,gene_name,"FTO"),neo(GeneReg:gene,regulates,GeneFTO:gene),true)




### s502a - Regulators of FTO
```no-wrap
=================================================================
        Find all genes that regulate the FTO gene (1-hop).

  (match &neo4j_out_v3
    (,
      (neo $GeneFTO:gene gene_name "FTO")
      (neo $GeneReg:gene regulates $GeneFTO:gene))
    (result $GeneReg $GeneFTO))

ensg00000257923                                                             ensg00000140718
ensg00000137871                                                             ensg00000140718
ensg00000104064                                                             ensg00000140718
ensg00000126746                                                             ensg00000140718
ensg00000164190                                                             ensg00000140718
ensg00000185811                                                             ensg00000140718
ensg00000101076                                                             ensg00000140718
 MeTTaLog Execution time: 0.01 seconds
% 28,860 inferences, 0.012 CPU in 0.013 seconds (99% CPU, 2327494 Lips)
 Last answer found: 0.01 seconds
 Number of answers: 502
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $GeneFTO:gene gene_name "FTO" $PropList1)
        (neo_P $GeneReg:gene regulates $GeneFTO:gene $PropList2))
      (result $GeneFTO:gene $GeneReg:gene $PropList1 $PropList2))
prolog=(neo_P(GeneFTO:gene,gene_name,"FTO",PropList1),neo_P(GeneReg:gene,regulates,GeneFTO:gene,PropList2),true)




### s502a (With Properties) - Regulators of FTO
```no-wrap
=================================================================
        Find all genes that regulate the FTO gene (1-hop), returning property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $GeneFTO:gene gene_name "FTO" $PropList1)
      (neo_P $GeneReg:gene regulates $GeneFTO:gene $PropList2))
    (result $PropList2 $GeneReg $PropList1 $GeneFTO))

[evidence=['pubmed:23341774','pubmed:29087512','pubmed:29126285'],databases=['ReMap','TRRUST'],evidence_type=small_scale_evidence,detection_method='inferred by curator'] ensg00000257923                       ()                                    ensg00000140718
[evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ensg00000137871                       ()                                    ensg00000140718
[evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ensg00000104064                       ()                                    ensg00000140718
[evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ensg00000126746                       ()                                    ensg00000140718
[evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ensg00000164190                       ()                                    ensg00000140718
[evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ensg00000185811                       ()                                    ensg00000140718
[evidence=['pubmed:18971253','pubmed:27924024','pubmed:26578589'],databases=['GTRD','ORegAnno','PAZAR'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay inferred by curator'] ensg00000101076                       ()                                    ensg00000140718
 MeTTaLog Execution time: 0.00 seconds
% 29,654 inferences, 0.002 CPU in 0.002 seconds (97% CPU, 16088325 Lips)
 Last answer found: 0.00 seconds
 Number of answers: 502
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo $FTO:gene gene_name "FTO")
        (neo $Reg:gene gene_name $Regulator:regulator)
        (neo $Reg:gene regulates $FTO:gene)
        (neo $FTO:gene gene_name $Target:target))
      (result $Regulator:regulator $Target:target))
prolog=(neo(FTO:gene,gene_name,"FTO"),neo(Reg:gene,gene_name,Regulator:regulator),neo(Reg:gene,regulates,FTO:gene),neo(FTO:gene,gene_name,Target:target),true)




### s502b - Regulators of FTO + gene_name
```no-wrap
=================================================================
        Find all genes that regulate FTO, returning regulator and target gene_names.

  (match &neo4j_out_v3
    (,
      (neo $FTO:gene gene_name "FTO")
      (neo $Reg:gene gene_name $Regulator:regulator)
      (neo $Reg:gene regulates $FTO:gene)
      (neo $FTO:gene gene_name $Target:target))
    (result $Target $Regulator $Reg $FTO))

FTO                                   INTS11                                ensg00000127054                       ensg00000140718
FTO                                   SKI                                   ensg00000157933                       ensg00000140718
FTO                                   TP73                                  ensg00000078900                       ensg00000140718
FTO                                   TP63                                  ensg00000073282                       ensg00000140718
FTO                                   HES1                                  ensg00000114315                       ensg00000140718
FTO                                   MXD4                                  ensg00000123933                       ensg00000140718
FTO                                   ELF4                                  ensg00000102034                       ensg00000140718
 MeTTaLog Execution time: 0.89 seconds
% 7,772,588 inferences, 0.886 CPU in 0.891 seconds (99% CPU, 8776190 Lips)
 Last answer found: 0.88 seconds
 Number of answers: 502
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (neo_P $PropList1:gene gene_name "FTO" $PropList2)
        (neo_P $PropList3:gene gene_name $Regulator:regulator $PropList6)
        (neo_P $PropList3:gene regulates $PropList1:gene $PropList4)
        (neo_P $PropList1:gene gene_name $Target:target $PropList5))
      (result $Regulator:regulator $Target:target $PropList1 $PropList2 $PropList3 $PropList4 $PropList5 $PropList6))
prolog=(neo_P(PropList1:gene,gene_name,"FTO",PropList2),neo_P(PropList3:gene,gene_name,Regulator:regulator,PropList6),neo_P(PropList3:gene,regulates,PropList1:gene,PropList4),neo_P(PropList1:gene,gene_name,Target:target,PropList5),true)




### s502b (With Properties) - Regulators of FTO + gene_name
```no-wrap
=================================================================
        Find all genes that regulate FTO, returning regulator and target gene_names, plus property lists.

  (match &neo4j_out_v3
    (,
      (neo_P $PropList1:gene gene_name "FTO" $PropList2)
      (neo_P $PropList3:gene gene_name $Regulator:regulator $PropList6)
      (neo_P $PropList3:gene regulates $PropList1:gene $PropList4)
      (neo_P $PropList1:gene gene_name $Target:target $PropList5))
    (result $PropList5 $Target $PropList4 $PropList6 $Regulator $PropList3 $PropList2 $PropList1))

()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 INTS11             ensg00000127054    ()                 ensg00000140718
()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 SKI                ensg00000157933    ()                 ensg00000140718
()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 TP73               ensg00000078900    ()                 ensg00000140718
()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 TP63               ensg00000073282    ()                 ensg00000140718
()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 HES1               ensg00000114315    ()                 ensg00000140718
()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 MXD4               ensg00000123933    ()                 ensg00000140718
()                 FTO                 ...
    [evidence=['pubmed:27924024'],databases=['GTRD'],evidence_type=large_scale_evidence,detection_method='chromatin immunoprecipitation assay'] ()                 ELF4               ensg00000102034    ()                 ensg00000140718
 MeTTaLog Execution time: 0.35 seconds
% 10,546,491 inferences, 0.352 CPU in 0.353 seconds (100% CPU, 29984988 Lips)
 Last answer found: 0.35 seconds
 Number of answers: 502
=================================================================
```




  ! (let $4122
      ( (apply different
          (each-pair ($R1 $R2 $R3 $R4 $R5))) (apply different (each-pair ($A $B $C $D $E $F $G $H $I $J $K))))
      (limit 3
        (match &neo4j_out_v3
          (,
            (neo $A $R1 $B)
            (neo $B $R2 $C)
            (neo $C $R3 $D)
            (neo $D $R4 $E)
            (neo $E $R5 $F))
          (result $A $R1 $B $R2 $C $R3 $D $R4 $E $R5 $F))))
prolog=(neo(A,R1,B),neo(B,R2,C),neo(C,R3,D),neo(D,R4,E),neo(E,R5,F),true)




### 5-edge chain: $R1\u2013$R5
```no-wrap
=================================================================
        Chain of 5 edges using $R1,$R2,$R3,$R4,$R5.

  (match &neo4j_out_v3
    (,
      (neo $A $R1 $B)
      (neo $B $R2 $C)
      (neo $C $R3 $D)
      (neo $D $R4 $E)
      (neo $E $R5 $F))
    (result $F $R5 $E $R4 $D $R3 $C $R2 $B $R1 $A))

ensg00000109501:gene    transcribed_from enst00000226760:transcript    translation_of o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
ense00001317848:exon    includes     enst00000226760:transcript    translation_of o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
ense00003467547:exon    includes     enst00000226760:transcript    translation_of o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
ATMIN        protein_name o43313:protein    interacts_with o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
['AGR:HGNC:29034','AlphaFoldDB:O43313','Antibodypedia:30434','Bgee:ENSG00000166454','BioGRID-ORCS:23300','BioGRID:116892','BioMuta:ATMIN','CCDS:CCDS32494.1','CCDS:CCDS73917.1','ChiTaRS:ATMIN','CTD:23300','DisGeNET:23300','DisProt:DP01288','DNASU:23300','eggNOG:KOG1721','EMBL:AAH02701.2','EMBL:AB007891','EMBL:AC092718','EMBL:AK290943','EMBL:BAA24861.2','EMBL:BAF83632.1','EMBL:BC002701','EMBL:CAH18291.1','EMBL:CR749457','Ensembl:ENSG00000166454.10','Ensembl:ENSP00000299575.3','Ensembl:ENSP00000455497.1','Ensembl:ENSP00000463478.1','Ensembl:ENST00000299575.5','Ensembl:ENST00000564241.5','Ensembl:ENST00000566488.1','EPD:O43313','ExpressionAtlas:O43313','Gene3D:3.30.160.60','GeneCards:ATMIN','GeneID:23300','GeneTree:ENSGT00390000013091','Genevisible:O43313','GenomeRNAi:23300','GO:GO:0000976','GO:GO:0000981','GO:GO:0001228','GO:GO:0005634','GO:GO:0006357','GO:GO:0006974','GO:GO:0010628','GO:GO:0016604','GO:GO:0044458','GO:GO:0045893','GO:GO:0045944','GO:GO:0046872','GO:GO:0070840','GO:GO:1902857','HGNC:HGNC:29034','HOGENOM:CLU_023902_0_0_1','HPA:ENSG00000166454','InParanoid:O43313','IntAct:O43313','InterPro:IPR013087','iPTMnet:O43313','jPOST:O43313','KEGG:hsa:23300','MANE-Select:ENSP00000299575.3','MANE-Select:ENST00000299575.5','MANE-Select:NM_015251.3','MANE-Select:NP_056066.2','MassIVE:O43313','MaxQB:O43313','MIM:614693','MINT:O43313','neXtProt:NX_O43313','OMA:LCALFQH','OpenTargets:ENSG00000166454','OrthoDB:5363046at2759','PANTHER:PTHR46664','PANTHER:PTHR46664:SF2','PathwayCommons:O43313','PaxDb:9606-ENSP00000299575','PeptideAtlas:O43313','PharmGKB:PA162377191','Pharos:O43313','PhosphoSitePlus:O43313','PhylomeDB:O43313','PIR:T00061','PRO:PR:O43313','PROSITE:PS00028','Proteomes:UP000005640','ProteomicsDB:48890','ProteomicsDB:48891','RefSeq:NM_001300728.1','RefSeq:NM_015251.2','RefSeq:NP_001287657.1','RefSeq:NP_056066.2','RNAct:O43313','SignaLink:O43313','SMART:SM00355','STRING:9606.ENSP00000299575','TreeFam:TF331171','UCSC:uc002ffz.2','VEuPathDB:HostDB:ENSG00000166454'] synonyms     o43313:protein    interacts_with o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
enst00000303400:transcript    translation_of q7l5y9:protein    interacts_with o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
A0A0B4J2F0   accessions   a0a0b4j2f0:protein    interacts_with o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
enst00000477278:transcript    translation_of q9brk3:protein    interacts_with o76024:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
enst00000394267:transcript    translation_of q9ud71:protein    interacts_with p17787:protein    go_gene_product go_0050877:go    subclass_of  go_0019226:go    capable_of   cl_0000540:cl
ense00001326936:exon    includes     enst00000276393:transcript    translation_of p35348:protein    interacts_with p08912:protein    go_gene_product go_0019226:go    capable_of   cl_0000540:cl
uberon_0002037:uberon    expressed_in ensg00000178209:gene    transcribed_from enst00000322810:transcript    translation_of q15149:protein    go_gene_product go_0019226:go    capable_of   cl_0000540:cl
 Time limit 120 seconds exceeded!
% 1,567,996,590 inferences, 117.613 CPU in 120.005 seconds (98% CPU, 13331830 Lips)
 Last answer found: 117.60 seconds
 Number of answers: 1,937,933
=================================================================
```




  ! (limit 3
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
          (neo $J $R10 $K))
        (result $A $R1 $B $R2 $C $R3 $D $R4 $E $R5 $F $R6 $G $R7 $H $R8 $I $R9 $J $R10 $K)))
prolog=('each-different'((A,B,C,D,E,F,G,H,I,J,K,true)),'each-different'((R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,true)),neo(A,R1,B),neo(B,R2,C),neo(C,R3,D),neo(D,R4,E),neo(E,R5,F),neo(F,R6,G),neo(G,R7,H),neo(H,R8,I),neo(I,R9,J),neo(J,R10,K),true)




### Ten-Step Path from $R1 to $R10
```no-wrap
=================================================================
        Find paths with exactly ten relationships, from $R1 to $R10.

  (match &neo4j_out_v3
    (,
      (each-different (, $A (, $B (, $C (, $D (, $E (, $F (, $G (, $H (, $I (, $J (, $K true))))))))))))
      (each-different (, $R1 (, $R2 (, $R3 (, $R4 (, $R5 (, $R6 (, $R7 (, $R8 (, $R9 (, $R10 true)))))))))))
      (neo $A $R1 $B)
      (neo $B $R2 $C)
      (neo $C $R3 $D)
      (neo $D $R4 $E)
      (neo $E $R5 $F)
      (neo $F $R6 $G)
      (neo $G $R7 $H)
      (neo $H $R8 $I)
      (neo $I $R9 $J)
      (neo $J $R10 $K))
    (result $R10 $R9 $R8 $R7 $R6 $R5 $R4 $R3 $R2 $R1 $K $J $I $H $G $F $E $D $C $B $A))


'each-different'((_2436{vn = ..., dif = ...},_2422{vn = ..., dif = ...},_2408{vn = ..., dif = ...},_2394{vn = ..., dif = ...},_2380{vn = ..., dif = ...},_2366{vn = ..., dif = ...},_2352{vn = ..., dif = ...},_2338{vn = ..., dif = ...},_2324{vn = ..., dif = ...},_2310{vn = ..., dif = ...},_2296{vn = ..., dif = ...},true))

'each-different'((_2282{vn = ..., dif = ...},_2268{vn = ..., dif = ...},_2254{vn = ..., dif = ...},_2240{vn = ..., dif = ...},_2226{vn = ..., dif = ...},_2212{vn = ..., dif = ...},_2198{vn = ..., dif = ...},_2184{vn = ..., dif = ...},_2170{vn = ..., dif = ...},_2156{vn = ..., dif = ...},true))
 Time limit 120 seconds exceeded!
% 1,275,762,768 inferences, 119.141 CPU in 120.005 seconds (99% CPU, 10708011 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (match &neo4j_out_v3
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
        (neo $VAR8 $R9 q13394))
      (result $Class $VAR1 $VAR2 $VAR3 $VAR4 $VAR5 $VAR6 $VAR7 $VAR8 $Q13394 $R1 $R2 $R3 $R4 $R5 $R6 $R7 $R8 $R9))
prolog=('each-different'('cl_0000540:cl'(VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,VAR7,VAR8,q13394)),'each-different'((R1,R2,R3,R4,R5,R6,R7,R8,R9,true)),neo('cl_0000540:cl',R1,VAR1),neo(VAR1,R2,VAR2,P3),neo(VAR2,R3,VAR3,P4),neo(VAR3,R4,VAR4,P5),neo(VAR4,R5,VAR5,P6),neo(VAR5,R6,VAR6,P7),neo(VAR6,R7,VAR7,P8),neo(VAR7,R8,VAR8,P9),neo(VAR8,R9,q13394),true)




### Dynamic Query 9 relations
```no-wrap
=================================================================
        Finds dynamic relationships with properties in both Neo4j (Cypher) and Prolog, ensuring all result values are distinct.

  (match &neo4j_out_v3
    (,
      (each-different (cl_0000540:cl $VAR1 $VAR2 $VAR3 $VAR4 $VAR5 $VAR6 $VAR7 $VAR8 q13394))
      (each-different (, $R1 (, $R2 (, $R3 (, $R4 (, $R5 (, $R6 (, $R7 (, $R8 (, $R9 true))))))))))
      (neo cl_0000540:cl $R1 $VAR1)
      (neo $VAR1 $R2 $VAR2 $P3)
      (neo $VAR2 $R3 $VAR3 $P4)
      (neo $VAR3 $R4 $VAR4 $P5)
      (neo $VAR4 $R5 $VAR5 $P6)
      (neo $VAR5 $R6 $VAR6 $P7)
      (neo $VAR6 $R7 $VAR7 $P8)
      (neo $VAR7 $R8 $VAR8 $P9)
      (neo $VAR8 $R9 q13394))
    (result $P9 $P8 $P7 $P6 $P5 $P4 $P3 $R9 $R8 $R7 $R6 $R5 $R4 $R3 $R2 $R1 $VAR8 $VAR7 $VAR6 $VAR5 $VAR4 $VAR3 $VAR2 $VAR1))


'each-different'('cl_0000540:cl'(_72472{vn = ..., dif = ...},_72458{vn = ..., dif = ...},_72444{vn = ..., dif = ...},_72430{vn = ..., dif = ...},_72416{vn = ..., dif = ...},_72402{vn = ..., dif = ...},_72388{vn = ..., dif = ...},_72374{vn = ..., dif = ...},q13394))

'each-different'((_72360{vn = ..., dif = ...},_72346{vn = ..., dif = ...},_72332{vn = ..., dif = ...},_72318{vn = ..., dif = ...},_72304{vn = ..., dif = ...},_72290{vn = ..., dif = ...},_72276{vn = ..., dif = ...},_72262{vn = ..., dif = ...},_72248{vn = ..., dif = ...},true))
 MeTTaLog Execution time: 0.01 seconds
% 18,488 inferences, 0.014 CPU in 0.014 seconds (99% CPU, 1338740 Lips)
 Last answer found: -0.00 seconds
 Number of answers: 0
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (each-different ($A $B))
        (neo $A start $S)
        (neo $B start $S)
        (neo $A end $E)
        (neo $B end $E)
        (neo_P $A $R $B $EdgeProps))
      (result $A $B $S $E $R $EdgeProps))
prolog=('each-different'((A,B,true)),neo(A,start,S),neo(B,start,S),neo(A,end,E),neo(B,end,E),neo_P(A,R,B,EdgeProps),true)




### Same Start-End Pair with Directed Relation
```no-wrap
=================================================================
        Find pairs of nodes A and B that share the same 'start' and 'end' property values, where A \u2260 B and A links to B via some relationship.

  (match &neo4j_out_v3
    (,
      (each-different (, $A (, $B true)))
      (neo $A start $S)
      (neo $B start $S)
      (neo $A end $E)
      (neo $B end $E)
      (neo_P $A $R $B $EdgeProps:edgeprops))
    (result $EdgeProps:edgeprops $R $E $S $B $A))


'each-different'((_1176{vn = ..., dif = ...},_1162{vn = ..., dif = ...},true))
()                       transcribed_to           14409                    11869                    enst00000456328:transcript    ensg00000290825:gene  
()                       transcribed_to           13670                    12010                    enst00000450305:transcript    ensg00000223972:gene  
()                       transcribed_to           29570                    14404                    enst00000488147:transcript    ensg00000227232:gene  
()                       transcribed_to           1_430_255                1_425_871                enst00000378821:transcript    ensg00000205116:gene  
()                       transcribed_to           1_434_573                1_430_539                enst00000430109:transcript    ensg00000225285:gene  
()                       transcribed_to           1_497_848                1_471_765                enst00000673477:transcript    ensg00000160072:gene  
()                       transcribed_to           18_748_866               18_630_846               enst00000420770:transcript    ensg00000009709:gene  
()                       transcribed_to           39_723_627               39_723_566               enst00000516604:transcript    ensg00000252413:gene  
()                       transcribed_to           190_449_901              190_428_655              enst00000354905:transcript    ensg00000198398:gene  
 Time limit 120 seconds exceeded!
% 1,408,073,566 inferences, 66.582 CPU in 120.001 seconds (55% CPU, 21147875 Lips)
 Last answer found: 52.27 seconds
 Number of answers: 48,844
=================================================================
```




  ! (match &neo4j_out_v3
      (,
        (each-different ($A $B))
        (neo $A start $S)
        (neo $B start $S)
        (neo $A end $E)
        (neo $B end $E)
        (neo_P $A $R $B $EdgeProps))
      (result $A $B $S $E $R $EdgeProps))
prolog=('each-different'((A,B,true)),neo(A,start,S),neo(B,start,S),neo(A,end,E),neo(B,end,E),neo_P(A,R,B,EdgeProps),true)




### Same Start-End Pair with Directed Relation
```no-wrap
=================================================================
        Find pairs of nodes A and B that share the same 'start' and 'end' property values, where A \u2260 B and A links to B via some relationship.

  (match &neo4j_out_v3
    (,
      (each-different (, $A (, $B true)))
      (neo $A start $S)
      (neo $B start $S)
      (neo $A end $E)
      (neo $B end $E)
      (neo_P $A $R $B $EdgeProps:edgeprops))
    (result $EdgeProps:edgeprops $R $E $S $B $A))


'each-different'((_56538{vn = ..., dif = ...},_56524{vn = ..., dif = ...},true))
()                       transcribed_to           14409                    11869                    enst00000456328:transcript    ensg00000290825:gene  
()                       transcribed_to           13670                    12010                    enst00000450305:transcript    ensg00000223972:gene  
()                       transcribed_to           29570                    14404                    enst00000488147:transcript    ensg00000227232:gene  
()                       transcribed_to           1_430_255                1_425_871                enst00000378821:transcript    ensg00000205116:gene  
()                       transcribed_to           1_434_573                1_430_539                enst00000430109:transcript    ensg00000225285:gene  
()                       transcribed_to           1_497_848                1_471_765                enst00000673477:transcript    ensg00000160072:gene  
()                       transcribed_to           18_748_866               18_630_846               enst00000420770:transcript    ensg00000009709:gene  
()                       transcribed_to           39_723_627               39_723_566               enst00000516604:transcript    ensg00000252413:gene  
()                       transcribed_to           190_449_901              190_428_655              enst00000354905:transcript    ensg00000198398:gene  
 Time limit 30 seconds exceeded!
% 1,118,218,013 inferences, 29.854 CPU in 30.000 seconds (100% CPU, 37456219 Lips)
 Last answer found: 25.71 seconds
 Number of answers: 40,350
=================================================================
```




  ! (limit 3
      (match &neo4j_out_v3
        (,
          (each-different ($R1 $R2 $R3 $R4 $R5))
          (each-different ($A $B $C $D $E $F $G $H $I $J $K))
          (neo $A $R1 $B)
          (neo $B $R2 $C)
          (neo $C $R3 $D)
          (neo $D $R4 $E)
          (neo $E $R5 $F))
        (result $A $R1 $B $R2 $C $R3 $D $R4 $E $R5 $F)))
prolog=('each-different'((R1,R2,R3,R4,R5,true)),'each-different'((A,B,C,D,E,F,G,H,I,J,K,true)),neo(A,R1,B),neo(B,R2,C),neo(C,R3,D),neo(D,R4,E),neo(E,R5,F),true)




### 5-edge chain: $R1\u2013$R5
```no-wrap
=================================================================
        Chain of 5 edges using $R1,$R2,$R3,$R4,$R5.

  (match &neo4j_out_v3
    (,
      (each-different (, $R1 (, $R2 (, $R3 (, $R4 (, $R5 true))))))
      (each-different (, $A (, $B (, $C (, $D (, $E (, $F (, $G (, $H (, $I (, $J (, $K true))))))))))))
      (neo $A $R1 $B)
      (neo $B $R2 $C)
      (neo $C $R3 $D)
      (neo $D $R4 $E)
      (neo $E $R5 $F))
    (result $K $J $I $H $G $F $E $D $C $B $A $R5 $R4 $R3 $R2 $R1))


'each-different'((_9508{vn = ..., dif = ...},_9494{vn = ..., dif = ...},_9480{vn = ..., dif = ...},_9466{vn = ..., dif = ...},_9452{vn = ..., dif = ...},true))

'each-different'((_9438{vn = ..., dif = ...},_9424{vn = ..., dif = ...},_9410{vn = ..., dif = ...},_9396{vn = ..., dif = ...},_9382{vn = ..., dif = ...},_9368{vn = ..., dif = ...},_9354{vn = ..., dif = ...},_9340{vn = ..., dif = ...},_9326{vn = ..., dif = ...},_9312{vn = ..., dif = ...},_9298{vn = ..., dif = ...},true))
 Time limit 60 seconds exceeded!
% 803,064,762 inferences, 59.611 CPU in 60.000 seconds (99% CPU, 13471800 Lips)
 Last answer found: 59.60 seconds
 Number of answers: 737,163
=================================================================
```
true.

138 ?-

