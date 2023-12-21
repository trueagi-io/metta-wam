;;  Load the VSPACE python space

;; to start in MeTTaLog use    ./MeTTa 8-FlybaseQuestions.metta --repl

!(extend-py! metta_vspace.metta_learner)

!(fb-info!)

;; MeTTaLog only:
!(ensure-loaded! whole_flybase)

;;  define some queries

;; Query: Find the Gene Symbols and Summary Texts for Genes Involved in a Specific Disease
(query-info "Show the gene symbols and summary texts for genes that are associated with a particular disease model."
    (gene-symbols-and-summaries $Disease $GeneSymbol $SummaryText)
   (, (disease_model_annotations! $FBgn $GeneSymbol $1 $2 $Disease $3 $4 $5 $6 $7 $8 $9)
      (automated_gene_summaries! $FBgn $SummaryText))
)

;; Query: Identify Alleles and Their Phenotypes for a Specific Gene
(query-info "List all alleles and their associated phenotypes for a given gene symbol."
    (alleles-and-phenotypes $GeneSymbol $AlleleSymbol $Phenotype)
   (, (fbal_to_fbgn! $1 $AlleleSymbol $2 $GeneSymbol)
      (allele_phenotypic! $AlleleSymbol $3 $Phenotype $4))
)

;; Query: Retrieve Gene Interaction Information and Corresponding Publications
(query-info "Find interactions between genes and the related publications."
    (gene-interactions-and-publications $GeneSymbol1 $GeneSymbol2 $InteractionType $Publication)
   (, (gene_genetic_interactions! $StartingGeneSymbols $1 $InteractingGeneSymbols $2 $InteractionType $PublicationFBrf)
      (fb-member! $GeneSymbol1 $StartingGeneSymbols)
      (fb-member! $GeneSymbol2 $InteractingGeneSymbols)
      (fbrf_pmid_pmcid_doi! $PublicationFBrf $Publication $3 $4 $5 $6 $7))
)

;; Query: Associate Gene Symbols with Their Orthologous Human Genes and Diseases
(query-info "Link Drosophila genes to their human orthologs and associated diseases."
    (gene-orthologs-and-diseases $DmelGeneSymbol $HumanGeneSymbol $Disease)
   (, (dmel_human_orthologs_disease! $1 $DmelGeneSymbol $2 $3 $HumanGeneSymbol $4 $5 $DiseaseIDs)
      (fb-member! $DiseaseID $DiseaseIDs)
      (disease_model_annotations! $6 $7 $HumanGeneSymbol $8 $Disease $9 $10 $11 $12 $13 $14 $15))
)


;; Query: Count the Number of Known RNA Processing Related Genes and Select Those with Less Than 2 Instances
(query-info "Find the number of known RNA processing related genes and select those with less than 2 instances."
    (genes-rna-process-<-two $GeneSymbol $RNAProcess $NumberOf)
    (, (gene_rpkm_matrix! $GeneSymbol $1 $2 $RNAProcess $3)
       (number-of! (gene_rpkm_matrix! $GeneSymbol $4 $5 $RNAProcess $6) $NumberOf)
       (< $NumberOf 2))
)

;; Query: Identify Genes with More Than 4 Physical Interactions
(query-info "Identify genes that have more than 4 physical interactions."
    (genes-physical-interactions-more-than-four $GeneSymbol $PhysicalInteraction $NumberOf)
    (, (physical_interactions_mitab! $GeneSymbol $1 $2 $PhysicalInteraction $3 $4 $5)
       (number-of! (physical_interactions_mitab! $GeneSymbol $6 $7 $PhysicalInteraction $8 $9 $10) $NumberOf)
       (> $NumberOf 4))
)

;; Query: Count Genes Associated with Specific Diseases and Select Those with More Than 3 Associations
(query-info "Count genes associated with specific diseases and select those with more than 3 associations."
    (genes-disease-associations-more-than-three $GeneSymbol $Disease $NumberOf)
    (, (disease_model_annotations! $GeneSymbol $1 $2 $3 $4 $5 $6 $Disease $7 $8 $9)
       (number-of! (disease_model_annotations! $GeneSymbol $10 $11 $12 $13 $14 $15 $Disease $16 $17 $18) $NumberOf)
       (> $NumberOf 3))
)

;; Query: Find the Number of Gene Synonyms and Select Those with Less Than 5 Synonyms
(query-info "Find the number of synonyms for genes and select those with less than 5 synonyms."
    (genes-synonyms-<-five $GeneSymbol $Synonym $NumberOf)
    (, (fb_synonym! $GeneSymbol $1 $2 $Synonym $3)
       (number-of! (fb_synonym! $GeneSymbol $4 $5 $Synonym $6) $NumberOf)
       (< $NumberOf 5))
)

;; Query: Count Genes Involved in Gene Regulation and Exclude Those with Less Than 2 Instances
(query-info "Count genes involved in gene regulation and exclude those with less than 2 instances."
    (genes-regulation-not-<-two $GeneSymbol $GeneRegulation $NumberOf)
    (, (gene_functional_complementation! $GeneSymbol $1 $2 $GeneRegulation $3 $4)
       (number-of! (gene_functional_complementation! $GeneSymbol $5 $6 $GeneRegulation $7 $8) $NumberOf)
       (not (< $NumberOf 2)))
)

;; Query: Identify Genes with Allelic Variants and Select Those with More Than 6 Variants
(query-info "Identify genes with allelic variants and select those with more than 6 variants."
    (genes-allelic-variants-more-than-six $GeneSymbol $AlleleSymbol $NumberOf)
    (, (allele_genetic_interactions! $AlleleSymbol $1 $2 $3 $GeneSymbol)
       (number-of! (allele_genetic_interactions! $AlleleSymbol $4 $5 $6 $GeneSymbol) $NumberOf)
       (> $NumberOf 6))
)



;; Query: Find All Known Paralogs of a Specific Gene and Their Expression Levels
(query-info "Display all paralogs of a given gene and their mean expression levels."
    (gene-paralogs-and-expression $GeneSymbol $ParalogSymbol $MeanExpression)
   (, (dmel_paralogs! $1 $GeneSymbol $2 $ParalogSymbol $3)
      (scRNA-Seq_gene_expression! $ParalogSymbol $4 $MeanExpression $5))
 )

;; Query: Correlate Gene Expression with Specific Stages of Tissue Development
(query-info "Find genes with their expression levels at various stages of tissue development."
    (gene-expression-by-stage $GeneSymbol $TissueStage $MeanExpression)
   (, (scRNA-Seq_gene_expression! $1 $2 $3 $4 $TissueStage $5 $MeanExpression $6)
      (dmel_gene_sequence_ontology_annotations! $GeneSymbol $7 $8))
)

;; Query: Associate Genetic Map Positions with Corresponding Enzyme Activities
(query-info "Relate genetic map positions to the enzyme activities of associated genes."
    (map-position-and-enzyme-activity $MapPosition $GeneSymbol $EnzymeActivity)
   (, (cyto-genetic-seq! $1 $MapPosition $2 $3)
      (Dmel_enzyme! $GeneSymbol $4 $5 $6 $EnzymeActivity))
)

;; Query: Link Alleles with Their Genetic and Physical Interactions
(query-info "Connect alleles to their genetic interactions and the physical interactions they are involved in."
    (allele-genetic-and-physical-interactions $AlleleSymbol $GeneticInteraction $PhysicalInteraction)
   (, (allele_genetic_interactions! $AlleleSymbol $1 $GeneticInteraction $2)
      (physical_interactions_mitab! $3 $4 $5 $6 $7 $PhysicalInteraction))
)

;; Query: Find cDNA Clones Related to Specific Diseases
(query-info "Identify cDNA clones linked to genes associated with certain diseases."
    (cDNA-clones-and-diseases $CloneName $Disease)
   (, (cDNA_clone! $1 $2 $CloneName $3)
      (disease_model_annotations! $4 $5 $6 $7 $Disease $8)
)
)

;; Query: Determine Disease Models Based on Specific Gene Sequences
(query-info "Ascertain disease models associated with particular gene sequences."
    (gene-sequences-and-disease-models $Sequence $DiseaseModel)
   (, (dmel_gene_sequence_ontology_annotations! $1 $2 $3 $Sequence)
      (disease_model_annotations! $4 $5 $6 $7 $DiseaseModel $8))
)

;; Query: Associate Gene Symbols with Known Allele Phenotypes and Interactions
(query-info "Connect gene symbols with the phenotypes and interactions of their alleles."
    (gene-alleles-phenotypes-and-interactions $GeneSymbol $AllelePhenotype $Interaction)
   (, (fbal_to_fbgn! $1 $2 $3 $GeneSymbol)
      (allele_phenotypic! $4 $5 $AllelePhenotype $6)
      (allele_genetic_interactions! $7 $AllelePhenotype $Interaction $8))
)

;; Query: Link Gene Symbols with Their Enzyme Functions and Synonyms
(query-info "Relate gene symbols to their enzyme functions and any known synonyms."
    (gene-enzyme-functions-and-synonyms $GeneSymbol $EnzymeFunction $Synonym)
   (, (Dmel_enzyme! $GeneSymbol $1 $2 $EnzymeFunction $3)
      (fb_synonym! $4 $5 $GeneSymbol $6 $Synonym)
)
)

;; Query: Associate Gene Expression with Specific Cell Types
(query-info "Correlate gene expression levels with specific cell types."
    (gene-expression-and-cell-type $GeneSymbol $CellType $ExpressionLevel)
   (, (scRNA-Seq_gene_expression! $1 $2 $3 $4 $5 $6 $CellType $7 $ExpressionLevel)
      (dmel_gene_sequence_ontology_annotations! $GeneSymbol $8 $9))
)

;; Query: Identify Genes with Specific Protein Isoforms and Their Disease Associations
(query-info "Find genes that have specific protein isoforms and their links to diseases."
    (genes-protein-isoforms-and-diseases $GeneSymbol $ProteinIsoform $Disease)
   (, (dmel_unique_protein_isoforms! $1 $GeneSymbol $2 $ProteinIsoform)
      (disease_model_annotations! $3 $4 $5 $6 $7 $8 $9 $10 $11 $Disease))
)

;; Query: Relate Genomic Clones to Their Expression in Different Tissues
(query-info "Connect genomic clones to their expression profiles in various tissues."
    (clones-and-tissue-expression $CloneName $TissueType $ExpressionLevel)
   (, (genomic_clone! $1 $2 $CloneName $3)
      (gene_rpkm_report! $4 $5 $6 $7 $TissueType $8 $ExpressionLevel))
)

;; Query: Find Genes Involved in Specific Pathways and Their Genetic Interactions
(query-info "Identify genes that are part of certain pathways and their genetic interactions."
    (genes-in-pathways-and-interactions $Pathway $GeneSymbol $Interaction)
   (, (pathway_group! $1 $2 $Pathway $3)
      (gene_genetic_interactions! $4 $GeneSymbol $5 $Interaction $6))
)




;; Query: Determine the Expression of Specific Genes in Different Tissues and Stages
(query-info "Find the mean expression of specific genes across different tissues and developmental stages."
    (gene-expression-across-tissues-and-stages $GeneSymbol $Tissue $Stage $MeanExpression)
   (, (scRNA-Seq_gene_expression! $PubID $1 $2 $3 $Tissue $Stage $4 $ClusterID $5 $6 $7 $GeneID $GeneSymbol $MeanExpression $8)
      (fbgn_annotation_ID! $GeneSymbol $9 $GeneID $10 $11 $12 $13))
)

;; Query: Relate cDNA Clones to Their Corresponding Genes and Accessions
(query-info "Link cDNA clones to their corresponding genes and accessions."
    (cdna-clone-gene-link $CloneName $GeneSymbol $CDNAaccession)
   (, (cDNA_clone! $1 $2 $CloneName $3 $CDNAaccession $4)
      (fbgn_annotation_ID! $GeneSymbol $5 $6 $7 $8 $9))
)

;; Query: Associate Alleles with Genetic Interactions and References
(query-info "Find genetic interactions involving specific alleles and their references."
    (allele-genetic-interactions-and-references $AlleleSymbol $Interaction $Reference)
   (, (allele_genetic_interactions! $AlleleSymbol $1 $Interaction $2 $Reference)
      (fbrf_pmid_pmcid_doi! $Reference $3 $4 $5 $6 $7 $8))
)

;; Query: Explore Paralogous Genes and Their Genomic Locations
(query-info "Identify paralogous genes and their genomic locations."
    (gene-paralogs-and-locations $GeneSymbol $ParalogGeneSymbol $Location)
   (, (dmel_paralogs! $GeneID $GeneSymbol $2 $Location $3 $4 $Paralog_FBgn $Paralog_GeneSymbol $5 $6 $7 $8)
      (fbgn_annotation_ID! $GeneSymbol $9 $GeneID $10 $11 $12))
)


;; Query: Trace Genes to Their Synonyms and Organism Abbreviations
(query-info "Trace genes to their synonyms and organism abbreviations."
    (gene-synonyms-and-organisms $GeneSymbol $Synonym $OrganismAbbreviation)
   (, (fb_synonym! $1 $OrganismAbbreviation $GeneSymbol $2 $Synonym $3 $4)
      (gene_map_table! $OrganismAbbreviation $GeneSymbol $5 $6 $7 $8 $9))
)

;; Query: Link Gene Annotations with Corresponding Protein Accessions
(query-info "Connect gene annotations with their respective protein accessions."
    (gene-annotations-and-protein-accessions $GeneSymbol $AnnotationID $ProteinAccession)
   (, (fbgn_annotation_ID! $GeneSymbol $1 $2 $AnnotationID $3 $4)
      (fbgn_NAseq_Uniprot! $GeneSymbol $5 $2 $6 $7 $ProteinAccession $8 $9 $10))
)

;; Query: Correlate Gene Expression with Specific RNA and Protein Data
(query-info "Correlate gene expression levels with RNA and protein data."
    (gene-expression-rna-protein-correlation $GeneSymbol $ExpressionValue $RNAaccession $ProteinAccession)
   (, (gene_rpkm_matrix! $1 $GeneSymbol $2 $3 $ExpressionValue $4)
      (fbgn_NAseq_Uniprot! $GeneSymbol $5 $1 $RNAaccession $6 $ProteinAccession $7 $8 $9))
)

;; Query: Associate Genes with Disease Models and Relevant Alleles
(query-info "Associate genes with disease models and the alleles used in these models."
    (genes-disease-models-and-alleles $GeneSymbol $Disease $AlleleSymbol)
   (, (disease_model_annotations! $GeneID $GeneSymbol $2 $3 $Disease $4 $5 $6 $AlleleSymbol $7 $8 $9 $10)
      (fbal_to_fbgn! $GeneID $AlleleSymbol $12 $GeneSymbol))
)


;; Query: Match Genes with Their Corresponding Publication References
(query-info "Match genes with their corresponding publication references."
    (gene-publication-matches $GeneSymbol $Publication)
   (, (fbgn_annotation_ID! $GeneSymbol $1 $2 $3 $4 $5)
      (entity_publication! $3 $GeneSymbol $Publication $6))
)

;; Query: Find Alleles and Their Interactions with Genes
(query-info "Find alleles and their interactions with specific genes."
    (alleles-and-gene-interactions $AlleleSymbol $GeneSymbol $Interaction)
   (, (allele_genetic_interactions! $AlleleSymbol $1 $Interaction $2 $3)
      (fbal_to_fbgn! $1 $AlleleSymbol $4 $GeneSymbol))
)







;; Query: Associate Genes with Pathways, Disease Models, and Human Orthologs
(query-info "Link genes to their associated pathways, disease models, and human orthologs."
    (genes-pathways-diseases-orthologs $GeneSymbol $Pathway $Disease $HumanOrtholog)
   (, (gene_groups_HGNC! $GeneSymbol $1 $2 $Pathway)
      (disease_model_annotations! $GeneSymbol $3 $4 $5 $Disease)
      (dmel_human_orthologs_disease! $GeneSymbol $6 $7 $HumanOrtholog))
)

;; Query: Link Alleles to Phenotypes, Genetic Interactions, and Publications
(query-info "Connect alleles to their phenotypes, genetic interactions, and related publications."
    (alleles-phenotypes-interactions-publications $AlleleSymbol $Phenotype $Interaction $Publication)
   (, (allele_phenotypic! $AlleleSymbol $1 $Phenotype)
      (allele_genetic_interactions! $AlleleSymbol $2 $Interaction)
      (fbrf_pmid_pmcid_doi! $Interaction $Publication $3 $4 $5))
)

;; Query: Correlate Gene Expressions with cDNA Clones and Protein Accessions
(query-info "Correlate gene expressions with corresponding cDNA clones and protein accessions."
    (gene-expression-cdna-protein $GeneSymbol $ExpressionValue $CloneName $ProteinAccession)
   (, (gene_rpkm_matrix! $GeneSymbol $1 $2 $ExpressionValue)
      (cDNA_clone! $3 $4 $CloneName $5)
      (fbgn_NAseq_Uniprot! $GeneSymbol $6 $7 $ProteinAccession))
)

;; Query: Determine Gene Functions and Map them to Genetic and Physical Interactions
(query-info "Determine gene functions and map them to genetic and physical interactions."
    (gene-functions-genetic-physical-interactions $GeneSymbol $Function $GeneticInteraction $PhysicalInteraction)
   (, (automated_gene_summaries! $GeneSymbol $Function)
      (gene_genetic_interactions! $GeneSymbol $1 $2 $GeneticInteraction)
      (physical_interactions_mitab! $3 $4 $GeneSymbol $PhysicalInteraction))
)

;; Query: Explore Gene Synonyms, Genetic Map Positions, and Disease Associations
(query-info "Explore gene synonyms, genetic map positions, and disease associations."
    (gene-synonyms-map-diseases $GeneSymbol $Synonym $MapPosition $Disease)
   (, (fb_synonym! $GeneSymbol $1 $2 $Synonym)
      (cyto-genetic-seq! $GeneSymbol $3 $4 $MapPosition)
      (disease_model_annotations! $GeneSymbol $5 $6 $7 $Disease))
)

;; Query: Associate Gene Expression with Cell Types and Developmental Stages
(query-info "Associate gene expression with specific cell types and developmental stages."
    (gene-expression-cell-types-stages $GeneSymbol $ExpressionValue $CellType $Stage)
   (, (gene_rpkm_matrix! $GeneSymbol $1 $2 $ExpressionValue)
      (scRNA-Seq_gene_expression! $3 $4 $5 $6 $Stage $7 $CellType))
)

;; Query: Link Genomic Clones to Genes, Proteins, and Synonyms
(query-info "Link genomic clones to their corresponding genes, proteins, and synonyms."
    (genomic-clone-gene-protein-synonym $CloneName $GeneSymbol $ProteinAccession $Synonym)
   (, (genomic_clone! $CloneName $1 $2 $3)
      (fbgn_NAseq_Uniprot! $GeneSymbol $4 $5 $ProteinAccession)
      (fb_synonym! $6 $7 $GeneSymbol $Synonym))
)

;; Query: Relate Gene Snapshots to Disease Models and Human Orthologs
(query-info "Relate gene snapshots to disease models and their human orthologs."
    (gene-snapshots-diseases-orthologs $GeneSymbol $SnapshotText $Disease $HumanOrtholog)
   (, (gene_snapshots! $GeneSymbol $1 $2 $SnapshotText)
      (disease_model_annotations! $GeneSymbol $3 $4 $5 $Disease)
      (dmel_human_orthologs_disease! $GeneSymbol $6 $7 $HumanOrtholog))
)

;; Query: Connect Genes to Their Enzymatic Functions and Disease Models
(query-info "Connect genes to their enzymatic functions and associated disease models."
    (genes-enzymes-diseases $GeneSymbol $EnzymeFunction $Disease)
   (, (Dmel_enzyme! $GeneSymbol $1 $2 $3 $EnzymeFunction)
      (disease_model_annotations! $GeneSymbol $4 $5 $6 $Disease))
)

;; Query: Associate Pathway Groups with Gene Functions and Physical Interactions
(query-info "Associate pathway groups with gene functions and physical interactions."
    (pathway-gene-function-physical-interaction $Pathway $GeneFunction $PhysicalInteraction)
   (, (pathway_group! $Pathway $1 $2 $3)
      (automated_gene_summaries! $4 $GeneFunction)
      (physical_interactions_mitab! $5 $6 $7 $PhysicalInteraction))
)


;; Query: Associate Disease Models with Gene Transcripts, Protein Isoforms, Gene Summaries, and Snapshots
(query-info "Link disease model annotations with corresponding gene transcripts, unique protein isoforms, the best gene summaries, and gene snapshots."
   (gene-disease-models-transcripts-proteins-summaries-snapshots $GeneID $GeneSymbol $ProteinID $GeneSummaryID $GeneSnapshotID)
   (, (disease_model_annotations! $GeneID $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11)
      (fbgn_fbtr_fbpp_expanded! $TranscriptType $TranscriptID $GeneID $GeneSymbol $GeneFullName $AnnotationID $12 $13 $14 $15 $16)
      (dmel_unique_protein_isoforms! $ProteinID $GeneSymbol $ProteinSymbol $17)
      (best_gene_summary! $GeneSummaryID $GeneSymbol $SummarySource $SummaryText)
      (gene_snapshots! $GeneSnapshotID $GeneSymbol $GeneName $Datestamp $SnapshotText))
)


;; Query: Link Paralogous Genes with Gene Mapping, Transcripts, Protein Isoforms, and Summaries
(query-info "Connect paralogous genes to their gene mapping, transcripts, protein isoforms, and gene summaries."
   (paralogs-gene-mapping-transcripts-proteins-summaries $GeneID $GeneSymbol $ParalogGeneID $MapTableID $ProteinID)
   (, (dmel_paralogs! $GeneID $GeneSymbol $Arm $Location $Strand $ParalogGeneID $ParalogGeneSymbol $17 $18 $19 $20)
      (gene_map_table! $MapTableID $OrganismAbbreviation $GeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (fbgn_fbtr_fbpp_expanded! $TranscriptType $TranscriptID $TranscriptSymbol $MapTableID $21 $22 $23 $24 $25 $26)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $27)
      (best_gene_summary! $GeneSummaryID $ProteinSymbol $SummarySource $SummaryText))
)


;; Query: Correlate Expanded Gene Transcripts with Protein Isoforms, Paralogs, Gene Mapping, and Synonyms
(query-info "Associate expanded gene transcripts with corresponding protein isoforms, paralogs, gene mapping data, and synonyms."
   (transcripts-proteins-paralogs-mapping-synonyms $GeneID $TranscriptID $ProteinID $ParalogGeneID $MapTableID)
   (, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33)
      (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42)
      (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))
)

!"
!(number-of!
   (, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33)
      (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42)
      (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))
"

;; Query: Connect Gene Mapping with Expanded Transcripts, Protein Isoforms, Gene Summaries, and Basic Transcripts
(query-info "Link gene mapping data with expanded transcripts, unique protein isoforms, gene summaries, and basic transcript information."
   (mapping-transcripts-proteins-summaries $MapTableID $GeneSymbol $TranscriptID $ProteinID $GeneSummaryID)
   (, (gene_map_table! $MapTableID $OrganismAbbreviation $GeneSymbol $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (fbgn_fbtr_fbpp_expanded! $TranscriptType $TranscriptID $TranscriptSymbol $OrganismAbbreviation $45 $46 $47 $48 $49 $50)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $51)
      (best_gene_summary! $GeneSummaryID $ProteinSymbol $SummarySource $SummaryText)
      (fbgn_fbtr_fbpp! $GeneSummaryID $52 $53))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query: Associate Gene Expression with Developmental Stages and Publications
(query-info "Link gene expression data to specific developmental stages and corresponding publications."
    (gene-expression-stages-publications $GeneID $ExpressionValue $Stage $Publication)
    (, (gene_rpkm_report! $GeneID $ExpressionValue $1 $2 $3 $Stage $4 $5 $6 $7 $8 $9 $10 $11)
       (fbrf_pmid_pmcid_doi! $12 $Publication $13 $14 $15 $16 $17 $18))
)

;; Query: Link Gene Functional Data with Enzymatic Activities and Disease Models
(query-info "Connect gene functional data with enzymatic activities and their relevance to disease models."
    (gene-function-enzyme-disease $GeneSymbol $Function $EnzymeActivity $Disease)
    (, (automated_gene_summaries! $GeneSymbol $Function)
       (Dmel_enzyme! $GeneSymbol $1 $2 $3 $4 $5 $6 $7 $8 $9 $10)
       (disease_model_annotations! $11 $GeneSymbol $12 $13 $14 $Disease $15 $16 $17 $18 $19 $20))
)

;; Query: Relate Genes to Their Allelic Variants and Phenotypic Effects
(query-info "Relate genes to their allelic variants and the resulting phenotypic effects."
    (genes-alleles-phenotypes $GeneSymbol $AlleleSymbol $Phenotype)
    (, (fbal_to_fbgn! $1 $AlleleSymbol $2 $GeneSymbol)
       (allele_phenotypic! $AlleleSymbol $3 $Phenotype $4))
)

;; Query: Correlate Genes with their Physical Interactions and Pathway Involvements
(query-info "Correlate genes with their physical interactions and pathway involvements."
    (genes-physical-interactions-pathways $GeneID $PhysicalInteraction $Pathway)
    (, (physical_interactions_mitab! $GeneID $1 $2 $PhysicalInteraction $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $16 $17 $18 $19 $20 $21 $22 $23 $24 $25 $26 $27)
       (pathway_group! $Pathway $28 $29 $30 $31 $32 $GeneID $33 $34))
)

;; Query: Connect Genes with Disease Annotations and Relevant Publication Data
(query-info "Connect genes with disease annotations and relevant publication data."
    (genes-disease-annotations-publications $GeneSymbol $DiseaseAnnotation $Publication)
    (, (disease_model_annotations! $1 $GeneSymbol $2 $3 $4 $DiseaseAnnotation $5 $6 $7 $8 $9 $10 $11 $12)
       (fbrf_pmid_pmcid_doi! $13 $Publication $14 $15 $16 $17 $18 $19))
)

;; Query: Associate Gene Sequences with Synonyms and Protein Isoforms
(query-info "Associate gene sequences with their synonyms and protein isoforms."
    (gene-sequences-synonyms-proteins $GeneID $Synonym $ProteinIsoform)
    (, (dmel_gene_sequence_ontology_annotations! $GeneID $1 $2 $3)
       (fb_synonym! $4 $5 $GeneID $6 $Synonym $7)
       (dmel_unique_protein_isoforms! $8 $9 $GeneID $ProteinIsoform $10))
)

;; Query: Link Genotype Data with Phenotypes and Corresponding References
(query-info "Link genotype data with phenotypes and their corresponding references."
    (genotype-phenotypes-references $GenotypeSymbols $Phenotype $Reference)
    (, (genotype_phenotype_data! $GenotypeSymbols $1 $Phenotype $2 $3 $4 $5 $Reference $6)
       (fbrf_pmid_pmcid_doi! $Reference $7 $8 $9 $10 $11 $12 $13))
)

;; Query: Correlate Gene Snapshots with Their Enzymatic Functions and Disease Models
(query-info "Correlate gene snapshots with their enzymatic functions and disease models."
    (gene-snapshots-enzymes-diseases $GeneSymbol $SnapshotText $EnzymeFunction $Disease)
    (, (gene_snapshots! $GeneSymbol $1 $2 $SnapshotText $3)
       (Dmel_enzyme! $GeneSymbol $4 $5 $6 $7 $8 $9 $10 $11)
       (disease_model_annotations! $12 $GeneSymbol $13 $14 $15 $Disease $16 $17 $18 $19 $20 $21))
)

;; Query: Associate Genes with Their cDNA Clones and Disease Models
(query-info "Associate genes with their cDNA clones and relevant disease models."
    (genes-cdna-disease-models $GeneSymbol $CloneName $Disease)
    (, (cDNA_clone! $1 $2 $CloneName $3 $4 $5)
       (gene_map_table! $6 $GeneSymbol $7 $8 $9 $10 $11)
       (disease_model_annotations! $12 $GeneSymbol $13 $14 $15 $Disease $16 $17 $18 $19 $20 $21))
)

;; Query: Connect Genes to Publications and Allelic Interactions
(query-info "Connect genes to relevant publications and their allelic interactions."
    (genes-publications-allelic-interactions $GeneSymbol $Publication $AlleleInteraction)
    (, (entity_publication! $1 $GeneSymbol $Publication $2)
       (allele_genetic_interactions! $3 $4 $AlleleInteraction $5 $GeneSymbol))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ORs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query: Identify Genes Either Involved in Physical Interactions or Associated with Specific Diseases
(query-info "Find genes that are either involved in physical interactions or associated with specific diseases."
    (genes-physical-or-disease $GeneSymbol $PhysicalInteraction $Disease)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (physical_interactions_mitab! $GeneSymbol $6 $7 $PhysicalInteraction $8 $9 $10)
           (disease_model_annotations! $GeneSymbol $11 $12 $13 $14 $15 $16 $Disease $17 $18 $19)))
)

;; Query: Retrieve Gene Snapshots or Enzymatic Functions for Specific Genes
(query-info "Get gene snapshots or enzymatic functions for specific genes."
    (gene-snapshots-or-enzymes $GeneSymbol $SnapshotText $EnzymeFunction)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (gene_snapshots! $GeneSymbol $6 $7 $SnapshotText $8)
           (Dmel_enzyme! $GeneSymbol $9 $10 $11 $EnzymeFunction $12 $13 $14)))
)

;; Query: Find Genes with Either cDNA Clones or Related to Specific Pathways
(query-info "Identify genes that have cDNA clones or are related to specific pathways."
    (genes-cdna-or-pathways $GeneSymbol $CloneName $Pathway)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (cDNA_clone! $CloneName $6 $GeneSymbol $7 $8)
           (pathway_group! $GeneSymbol $9 $10 $11 $Pathway $12 $13)))
)

;; Query: Associate Genes with Synonyms or Protein Isoforms
(query-info "Associate genes with either their synonyms or protein isoforms."
    (genes-synonyms-or-proteins $GeneSymbol $Synonym $ProteinIsoform)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (fb_synonym! $GeneSymbol $6 $7 $Synonym $8)
           (dmel_unique_protein_isoforms! $GeneSymbol $9 $10 $ProteinIsoform $11)))
)

;; Query: Link Alleles to Phenotypes or Genetic Interactions
(query-info "Link alleles to their phenotypes or to their genetic interactions."
    (alleles-phenotypes-or-interactions $AlleleSymbol $Phenotype $Interaction)
    (, (fbal_to_fbgn! $AlleleSymbol $1 $2 $3)
       (or (allele_phenotypic! $AlleleSymbol $4 $Phenotype $5)
           (allele_genetic_interactions! $AlleleSymbol $6 $Interaction $7)))
)

;; Query: Find Genes Involved in RNA Processing or Gene Regulation
(query-info "Find genes involved in RNA processing or gene regulation."
    (genes-rna-or-regulation $GeneSymbol $RNAProcess $GeneRegulation)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (gene_rpkm_matrix! $GeneSymbol $6 $7 $RNAProcess $8)
           (gene_functional_complementation! $GeneSymbol $9 $10 $GeneRegulation $11)))
)

;; Query: Retrieve Publications Related to Genes or Their Allelic Variants
(query-info "Retrieve publications related to genes or their allelic variants."
    (publications-genes-or-alleles $GeneSymbol $Publication $AlleleSymbol)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (entity_publication! $Publication $6 $GeneSymbol $7)
           (allele_genetic_interactions! $AlleleSymbol $8 $9 $10 $GeneSymbol)))
)

;; Query: Correlate Genes with Their Expression in Tissues or During Specific Stages
(query-info "Correlate genes with their expression in tissues or during specific developmental stages."
    (genes-expression-tissues-or-stages $GeneSymbol $Tissue $Stage)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (or (scRNA-Seq_gene_expression! $6 $7 $8 $9 $Tissue $Stage $10 $11)
           (gene_rpkm_report! $GeneSymbol $12 $13 $14 $15 $16 $17 $Stage $18)))
)


;; Query: Find Genes Involved in RNA Processing but Not in Gene Regulation
(query-info "Identify genes involved in RNA processing but not in gene regulation."
    (genes-rna-not-regulation $GeneSymbol $RNAProcess)
    (, (gene_rpkm_matrix! $GeneSymbol $1 $2 $RNAProcess $3)
       (not (gene_functional_complementation! $GeneSymbol $4 $5 $6 $7 $8)))
)

;; Query: Retrieve Publications Related to Specific Genes but Not Their Allelic Variants
(query-info "Retrieve publications related to specific genes but not their allelic variants."
    (publications-genes-not-alleles $GeneSymbol $Publication)
    (, (entity_publication! $Publication $1 $GeneSymbol $2)
       (not (allele_genetic_interactions! $3 $4 $5 $GeneSymbol)))
)

;; Query: Identify Genes with cDNA Clones but Not Involved in Specific Pathways
(query-info "Identify genes that have cDNA clones but are not involved in specific pathways."
    (genes-cdna-not-pathways $GeneSymbol $CloneName)
    (, (cDNA_clone! $CloneName $1 $GeneSymbol $2 $3 $4)
       (not (pathway_group! $GeneSymbol $5 $6 $7 $8 $9 $10)))
)

;; Query: Associate Genes with Synonyms but Exclude Certain Protein Isoforms
(query-info "Associate genes with their synonyms but exclude certain protein isoforms."
    (genes-synonyms-not-proteins $GeneSymbol $Synonym)
    (, (fb_synonym! $GeneSymbol $1 $2 $Synonym $3)
       (not (dmel_unique_protein_isoforms! $GeneSymbol $4 $5 $6)))
)

;; Query: Link Alleles to Phenotypes but Exclude Specific Genetic Interactions
(query-info "Link alleles to their phenotypes but exclude specific genetic interactions."
    (alleles-phenotypes-not-interactions $AlleleSymbol $Phenotype)
    (, (allele_phenotypic! $AlleleSymbol $1 $Phenotype $2)
       (not (allele_genetic_interactions! $AlleleSymbol $3 $4 $5)))
)

;; Query: Find Genes Involved in Physical Interactions but Not Associated with Diseases
(query-info "Find genes involved in physical interactions but not associated with specific diseases."
    (genes-physical-not-disease $GeneSymbol $PhysicalInteraction)
    (, (physical_interactions_mitab! $GeneSymbol $1 $2 $PhysicalInteraction $3 $4 $5 $6 $7)
       (not (disease_model_annotations! $GeneSymbol $8 $9 $10 $11 $12 $13 $14 $15 $16)))
)

;; Query: Retrieve Gene Snapshots but Exclude Those Related to Enzymatic Functions
(query-info "Retrieve gene snapshots but exclude those related to enzymatic functions."
    (gene-snapshots-not-enzymes $GeneSymbol $SnapshotText)
    (, (gene_snapshots! $GeneSymbol $1 $2 $SnapshotText $3)
       (not (Dmel_enzyme! $GeneSymbol $4 $5 $6 $7 $8 $9 $10)))
)

;; Query: Correlate Genes with Expression in Tissues but Not During Specific Stages
(query-info "Correlate genes with their expression in tissues but not during specific developmental stages."
    (genes-expression-tissues-not-stages $GeneSymbol $Tissue)
    (, (scRNA-Seq_gene_expression! $1 $2 $3 $4 $Tissue $5 $6 $7 $GeneSymbol $8 $9)
       (not (gene_rpkm_report! $GeneSymbol $10 $11 $12 $13 $14 $15 $16 $17 $18 $19 $20)))
)


;; Query: Find Genes with Specific Transcripts or Protein Isoforms but Not Involved in Certain Pathways
(query-info "Identify genes that have specific transcripts or protein isoforms but are not involved in certain pathways."
    (genes-transcripts-proteins-not-pathways $GeneSymbol $TranscriptID $ProteinIsoform $ExcludedPathway)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (, (or (fbgn_fbtr_fbpp! $GeneSymbol $TranscriptID $6 $7 $8)
                 (dmel_unique_protein_isoforms! $GeneSymbol $9 $ProteinIsoform $10))
            (not (pathway_group! $GeneSymbol $11 $12 $13 $ExcludedPathway $14 $15))))
)

;; Query: Retrieve Gene Snapshots or Allelic Variants but Exclude Genes with Certain Phenotypes
(query-info "Retrieve gene snapshots or allelic variants but exclude genes associated with certain phenotypes."
    (gene-snapshots-alleles-not-phenotypes $GeneSymbol $SnapshotText $AlleleSymbol $ExcludedPhenotype)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (, (or (gene_snapshots! $GeneSymbol $6 $SnapshotText $7)
                 (allele_genetic_interactions! $AlleleSymbol $8 $9 $10 $GeneSymbol))
            (not (allele_phenotypic! $AlleleSymbol $11 $ExcludedPhenotype $12))))
)

;; Query: Link Genes to Physical Interactions or Publications but Not to Specific Disease Models
(query-info "Link genes to physical interactions or relevant publications but not to specific disease models."
    (genes-interactions-publications-not-disease $GeneSymbol $PhysicalInteraction $Publication $ExcludedDisease)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (, (or (physical_interactions_mitab! $GeneSymbol $6 $7 $PhysicalInteraction $8 $9)
                 (entity_publication! $Publication $10 $GeneSymbol $11))
            (not (disease_model_annotations! $GeneSymbol $12 $13 $14 $15 $16 $17 $ExcludedDisease))))
)

;; Query: Associate Genes with cDNA Clones or Enzymatic Functions but Exclude Certain Synonyms
(query-info "Associate genes with cDNA clones or enzymatic functions but exclude certain synonyms."
    (genes-cdna-enzymes-not-synonyms $GeneSymbol $CloneName $EnzymeFunction $ExcludedSynonym)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (, (or (cDNA_clone! $CloneName $6 $GeneSymbol $7)
                 (Dmel_enzyme! $GeneSymbol $8 $9 $EnzymeFunction $10))
            (not (fb_synonym! $GeneSymbol $11 $12 $ExcludedSynonym $13))))
)

;; Query: Find Genes Involved in RNA Processing or Gene Regulation but Not Linked to Specific Protein Accessions
(query-info "Find genes involved in RNA processing or gene regulation but not linked to specific protein accessions."
    (genes-rna-regulation-not-proteins $GeneSymbol $RNAProcess $GeneRegulation $ExcludedProteinAccession)
    (, (gene_map_table! $GeneSymbol $1 $2 $3 $4 $5)
       (, (or (gene_rpkm_matrix! $GeneSymbol $6 $7 $RNAProcess $8)
                 (gene_functional_complementation! $GeneSymbol $9 $10 $GeneRegulation $11))
            (not (fbgn_NAseq_Uniprot! $GeneSymbol $12 $13 $ExcludedProteinAccession $14 $15 $16))))
)

!(fb-info!)

(: number-of! (-> Atom Number))
(= (number-of! $query $length)
    (length! (match &self $query t) $length))

;; call-nth! is one of our builtins
(: nth-pose! (-> Number Atom))
(= (nth-pose! $length $eval)
   (call-nth! $eval $nth)
   (=< $nth $length))

(= (do-77-queries)
   (match &self
        (query-info $English $template   $query)
	(demo-query $English $query $template)))


(= (demo-query $English $query $template)
   (println!  ($English  "=" $query))
   ;; print no more than the first 5 answers
   (println!  (nth-pose 5 (match &self $query $template)))
   (println! ("time was = " (time! (number-of! $query $count))))
   (println! ("total of query = " $count)))


;; Rust only:
!(metta_learner::vspace-main)

;; !(do-77-queries)

