;; !(do-77-queries)

;;  Load the VSPACE python space

;; to start in MeTTaLog use    ./MeTTa 8-FlybaseQuestions.metta --repl

!(extend-py! mettalog)

!(fb-info!)

;; MeTTaLog only:
!(ensure-loaded! whole_flybase)

;;  define some queries

;; Query: Find the Gene Symbols and Summary Texts for Genes Involved in a Specific Disease
(query-info "Show the gene symbols and summary texts for genes that are associated with a particular disease model."
    (gene-symbols-and-summaries $Disease $GeneSymbol $SummaryText)
   (, (disease_model_annotations! $FBgn $GeneSymbol $_ $_ $Disease $_ $_ $_ $_ $_ $_ $_)
      (automated_gene_summaries! $FBgn $SummaryText))
)

;; Query: Identify Alleles and Their Phenotypes for a Specific Gene
(query-info "List all alleles and their associated phenotypes for a given gene symbol."
    (alleles-and-phenotypes $GeneSymbol $AlleleSymbol $Phenotype)
   (, (fbal_to_fbgn! $_ $AlleleSymbol $_ $GeneSymbol)
      (allele_phenotypic! $AlleleSymbol $_ $Phenotype $_))
)

;; Query: Retrieve Gene Interaction Information and Corresponding Publications
(query-info "Find interactions between genes and the related publications."
    (gene-interactions-and-publications $GeneSymbol1 $GeneSymbol2 $InteractionType $Publication)
   (, (gene_genetic_interactions! $StartingGeneSymbols $_ $InteractingGeneSymbols $_ $InteractionType $PublicationFBrf)
      (fb-member! $GeneSymbol1 $StartingGeneSymbols)
      (fb-member! $GeneSymbol2 $InteractingGeneSymbols)
      (fbrf_pmid_pmcid_doi! $PublicationFBrf $Publication $_ $_ $_ $_ $_))
)

;; Query: Associate Gene Symbols with Their Orthologous Human Genes and Diseases
(query-info "Link Drosophila genes to their human orthologs and associated diseases."
    (gene-orthologs-and-diseases $OrgGeneSymbol $HumanGeneSymbol $Disease)
   (, (dmel_human_orthologs_disease! $_ $OrgGeneSymbol $_ $_ $HumanGeneSymbol $_ $_ $DiseaseIDs)
      (fb-member! $DiseaseID $DiseaseIDs)
      (disease_model_annotations! $_ $_ $HumanGeneSymbol $_ $Disease $_ $_ $_ $_ $_ $_ $_))
)


;; Query: Count the Number of Known RNA Processing Related Genes and Select Those with Less Than 2 Instances
(query-info "Find the number of known RNA processing related genes and select those with less than 2 instances."
    (genes-rna-process-<-two $GeneSymbol $RNAProcess $NumberOf)
    (, (gene_rpkm_matrix! $GeneSymbol $_ $_ $RNAProcess $_)
       (number-of (gene_rpkm_matrix! $GeneSymbol $_ $_ $RNAProcess $_) $NumberOf)
       (< $NumberOf 2))
)

;; Query: Identify Genes with More Than 4 Physical Interactions
(query-info "Identify genes that have more than 4 physical interactions."
    (genes-physical-interactions-more-than-four $GeneSymbol $PhysicalInteraction $NumberOf)
    (, (physical_interactions_mitab! $GeneSymbol $_ $_ $PhysicalInteraction $_ $_ $_)
       (number-of (physical_interactions_mitab! $GeneSymbol $_ $_ $PhysicalInteraction $_ $_ $_) $NumberOf)
       (> $NumberOf 4))
)

;; Query: Count Genes Associated with Specific Diseases and Select Those with More Than 3 Associations
(query-info "Count genes associated with specific diseases and select those with more than 3 associations."
    (genes-disease-associations-more-than-three $GeneSymbol $Disease $NumberOf)
    (, (disease_model_annotations! $GeneSymbol $_ $_ $_ $_ $_ $_ $Disease $_ $_ $_)
       (number-of (disease_model_annotations! $GeneSymbol $_ $_ $_ $_ $_ $_ $Disease $_ $_ $_) $NumberOf)
       (> $NumberOf 3))
)

;; Query: Find the Number of Gene Synonyms and Select Those with Less Than 5 Synonyms
(query-info "Find the number of synonyms for genes and select those with less than 5 synonyms."
    (genes-synonyms-<-five $GeneSymbol $Synonym $NumberOf)
    (, (fb_synonym! $GeneSymbol $_ $_ $Synonym $_)
       (number-of (fb_synonym! $GeneSymbol $_ $_ $Synonym $_) $NumberOf)
       (< $NumberOf 5))
)

;; Query: Count Genes Involved in Gene Regulation and Exclude Those with Less Than 2 Instances
(query-info "Count genes involved in gene regulation and exclude those with less than 2 instances."
    (genes-regulation-not-<-two $GeneSymbol $GeneRegulation $NumberOf)
    (, (gene_functional_complementation! $GeneSymbol $_ $_ $GeneRegulation $_ $_)
       (number-of (gene_functional_complementation! $GeneSymbol $_ $_ $GeneRegulation $_ $_) $NumberOf)
       (not (< $NumberOf 2)))
)

;; Query: Identify Genes with Allelic Variants and Select Those with More Than 6 Variants
(query-info "Identify genes with allelic variants and select those with more than 6 variants."
    (genes-allelic-variants-more-than-six $GeneSymbol $AlleleSymbol $NumberOf)
    (, (allele_genetic_interactions! $AlleleSymbol $_ $_ $_ $GeneSymbol)
       (number-of (allele_genetic_interactions! $AlleleSymbol $_ $_ $_ $GeneSymbol) $NumberOf)
       (> $NumberOf 6))
)



;; Query: Find All Known Paralogs of a Specific Gene and Their Expression Levels
(query-info "Display all paralogs of a given gene and their mean expression levels."
    (gene-paralogs-and-expression $GeneSymbol $ParalogSymbol $MeanExpression)
   (, (dmel_paralogs! $_ $GeneSymbol $_ $ParalogSymbol $_)
      (scRNA-Seq_gene_expression! $ParalogSymbol $_ $MeanExpression $_))
 )

;; Query: Correlate Gene Expression with Specific Stages of Tissue Development
(query-info "Find genes with their expression levels at various stages of tissue development."
    (gene-expression-by-stage $GeneSymbol $TissueStage $MeanExpression)
   (, (scRNA-Seq_gene_expression! $_ $_ $_ $_ $TissueStage $_ $MeanExpression $_)
      (dmel_gene_sequence_ontology_annotations! $GeneSymbol $_ $_))
)

;; Query: Associate Genetic Map Positions with Corresponding Enzyme Activities
(query-info "Relate genetic map positions to the enzyme activities of associated genes."
    (map-position-and-enzyme-activity $MapPosition $GeneSymbol $EnzymeActivity)
   (, (cyto-genetic-seq! $_ $MapPosition $_ $_)
      (Dmel_enzyme! $GeneSymbol $_ $_ $_ $EnzymeActivity))
)

;; Query: Link Alleles with Their Genetic and Physical Interactions
(query-info "Connect alleles to their genetic interactions and the physical interactions they are involved in."
    (allele-genetic-and-physical-interactions $AlleleSymbol $GeneticInteraction $PhysicalInteraction)
   (, (allele_genetic_interactions! $AlleleSymbol $_ $GeneticInteraction $_)
      (physical_interactions_mitab! $_ $_ $_ $_ $_ $PhysicalInteraction))
)

;; Query: Find cDNA Clones Related to Specific Diseases
(query-info "Identify cDNA clones linked to genes associated with certain diseases."
    (cDNA-clones-and-diseases $CloneName $Disease)
   (, (cDNA_clone! $_ $_ $CloneName $_)
      (disease_model_annotations! $_ $_ $_ $_ $Disease $_)
)
)

;; Query: Determine Disease Models Based on Specific Gene Sequences
(query-info "Ascertain disease models associated with particular gene sequences."
    (gene-sequences-and-disease-models $Sequence $DiseaseModel)
   (, (dmel_gene_sequence_ontology_annotations! $_ $_ $_ $Sequence)
      (disease_model_annotations! $_ $_ $_ $_ $DiseaseModel $_))
)

;; Query: Associate Gene Symbols with Known Allele Phenotypes and Interactions
(query-info "Connect gene symbols with the phenotypes and interactions of their alleles."
    (gene-alleles-phenotypes-and-interactions $GeneSymbol $AllelePhenotype $Interaction)
   (, (fbal_to_fbgn! $_ $_ $_ $GeneSymbol)
      (allele_phenotypic! $_ $_ $AllelePhenotype $_)
      (allele_genetic_interactions! $_ $AllelePhenotype $Interaction $_))
)

;; Query: Link Gene Symbols with Their Enzyme Functions and Synonyms
(query-info "Relate gene symbols to their enzyme functions and any known synonyms."
    (gene-enzyme-functions-and-synonyms $GeneSymbol $EnzymeFunction $Synonym)
   (, (Dmel_enzyme! $GeneSymbol $_ $_ $EnzymeFunction $_)
      (fb_synonym! $_ $_ $GeneSymbol $_ $Synonym)
)
)

;; Query: Associate Gene Expression with Specific Cell Types
(query-info "Correlate gene expression levels with specific cell types."
    (gene-expression-and-cell-type $GeneSymbol $CellType $ExpressionLevel)
   (, (scRNA-Seq_gene_expression! $_ $_ $_ $_ $_ $_ $CellType $_ $ExpressionLevel)
      (dmel_gene_sequence_ontology_annotations! $GeneSymbol $_ $_))
)

;; Query: Identify Genes with Specific Protein Isoforms and Their Disease Associations
(query-info "Find genes that have specific protein isoforms and their links to diseases."
    (genes-protein-isoforms-and-diseases $GeneSymbol $ProteinIsoform $Disease)
   (, (dmel_unique_protein_isoforms! $_ $GeneSymbol $_ $ProteinIsoform)
      (disease_model_annotations! $_ $_ $_ $_ $_ $_ $_ $_ $_ $Disease))
)

;; Query: Relate Genomic Clones to Their Expression in Different Tissues
(query-info "Connect genomic clones to their expression profiles in various tissues."
    (clones-and-tissue-expression $CloneName $TissueType $ExpressionLevel)
   (, (genomic_clone! $_ $_ $CloneName $_)
      (gene_rpkm_report! $_ $_ $_ $_ $TissueType $_ $ExpressionLevel))
)

;; Query: Find Genes Involved in Specific Pathways and Their Genetic Interactions
(query-info "Identify genes that are part of certain pathways and their genetic interactions."
    (genes-in-pathways-and-interactions $Pathway $GeneSymbol $Interaction)
   (, (pathway_group! $_ $_ $Pathway $_)
      (gene_genetic_interactions! $_ $GeneSymbol $_ $Interaction $_))
)




;; Query: Determine the Expression of Specific Genes in Different Tissues and Stages
(query-info "Find the mean expression of specific genes across different tissues and developmental stages."
    (gene-expression-across-tissues-and-stages $GeneSymbol $Tissue $Stage $MeanExpression)
   (, (scRNA-Seq_gene_expression! $PubID $_ $_ $_ $Tissue $Stage $_ $ClusterID $_ $_ $_ $GeneID $GeneSymbol $MeanExpression $_)
      (fbgn_annotation_ID! $GeneSymbol $_ $GeneID $_ $_ $_ $_))
)

;; Query: Relate cDNA Clones to Their Corresponding Genes and Accessions
(query-info "Link cDNA clones to their corresponding genes and accessions."
    (cdna-clone-gene-link $CloneName $GeneSymbol $CDNAaccession)
   (, (cDNA_clone! $_ $_ $CloneName $_ $CDNAaccession $_)
      (fbgn_annotation_ID! $GeneSymbol $_ $_ $_ $_ $_))
)

;; Query: Associate Alleles with Genetic Interactions and References
(query-info "Find genetic interactions involving specific alleles and their references."
    (allele-genetic-interactions-and-references $AlleleSymbol $Interaction $Reference)
   (, (allele_genetic_interactions! $AlleleSymbol $_ $Interaction $_ $Reference)
      (fbrf_pmid_pmcid_doi! $Reference $_ $_ $_ $_ $_ $_))
)

;; Query: Explore Paralogous Genes and Their Genomic Locations
(query-info "Identify paralogous genes and their genomic locations."
    (gene-paralogs-and-locations $GeneSymbol $ParalogGeneSymbol $Location)
   (, (dmel_paralogs! $GeneID $GeneSymbol $_ $Location $_ $_ $Paralog_FBgn $Paralog_GeneSymbol $_ $_ $_ $_)
      (fbgn_annotation_ID! $GeneSymbol $_ $GeneID $_ $_ $_))
)


;; Query: Trace Genes to Their Synonyms and Organism Abbreviations
(query-info "Trace genes to their synonyms and organism abbreviations."
    (gene-synonyms-and-organisms $GeneSymbol $Synonym $OrganismAbbreviation)
   (, (fb_synonym! $_ $OrganismAbbreviation $GeneSymbol $_ $Synonym $_ $_)
      (gene_map_table! $OrganismAbbreviation $GeneSymbol $_ $_ $_ $_ $_))
)

;; Query: Link Gene Annotations with Corresponding Protein Accessions
(query-info "Connect gene annotations with their respective protein accessions."
    (gene-annotations-and-protein-accessions $GeneSymbol $AnnotationID $ProteinAccession)
   (, (fbgn_annotation_ID! $GeneSymbol $_ $_ $AnnotationID $_ $_)
      (fbgn_NAseq_Uniprot! $GeneSymbol $_ $_ $_ $_ $ProteinAccession $_ $_ $_))
)

;; Query: Correlate Gene Expression with Specific RNA and Protein Data
(query-info "Correlate gene expression levels with RNA and protein data."
    (gene-expression-rna-protein-correlation $GeneSymbol $ExpressionValue $RNAaccession $ProteinAccession)
   (, (gene_rpkm_matrix! $_ $GeneSymbol $_ $_ $ExpressionValue $_)
      (fbgn_NAseq_Uniprot! $GeneSymbol $_ $_ $RNAaccession $_ $ProteinAccession $_ $_ $_))
)

;; Query: Associate Genes with Disease Models and Relevant Alleles
(query-info "Associate genes with disease models and the alleles used in these models."
    (genes-disease-models-and-alleles $GeneSymbol $Disease $AlleleSymbol)
   (, (disease_model_annotations! $GeneID $GeneSymbol $_ $_ $Disease $_ $_ $_ $AlleleSymbol $_ $_ $_ $_)
      (fbal_to_fbgn! $GeneID $AlleleSymbol $_ $GeneSymbol))
)


;; Query: Match Genes with Their Corresponding Publication References
(query-info "Match genes with their corresponding publication references."
    (gene-publication-matches $GeneSymbol $Publication)
   (, (fbgn_annotation_ID! $GeneSymbol $_ $_ $_ $_ $_)
      (entity_publication! $_ $GeneSymbol $Publication $_))
)

;; Query: Find Alleles and Their Interactions with Genes
(query-info "Find alleles and their interactions with specific genes."
    (alleles-and-gene-interactions $AlleleSymbol $GeneSymbol $Interaction)
   (, (allele_genetic_interactions! $AlleleSymbol $_ $Interaction $_ $_)
      (fbal_to_fbgn! $_ $AlleleSymbol $_ $GeneSymbol))
)







;; Query: Associate Genes with Pathways, Disease Models, and Human Orthologs
(query-info "Link genes to their associated pathways, disease models, and human orthologs."
    (genes-pathways-diseases-orthologs $GeneSymbol $Pathway $Disease $HumanOrtholog)
   (, (gene_groups_HGNC! $GeneSymbol $_ $_ $Pathway)
      (disease_model_annotations! $GeneSymbol $_ $_ $_ $Disease)
      (dmel_human_orthologs_disease! $GeneSymbol $_ $_ $HumanOrtholog))
)

;; Query: Link Alleles to Phenotypes, Genetic Interactions, and Publications
(query-info "Connect alleles to their phenotypes, genetic interactions, and related publications."
    (alleles-phenotypes-interactions-publications $AlleleSymbol $Phenotype $Interaction $Publication)
   (, (allele_phenotypic! $AlleleSymbol $_ $Phenotype)
      (allele_genetic_interactions! $AlleleSymbol $_ $Interaction)
      (fbrf_pmid_pmcid_doi! $Interaction $Publication $_ $_ $_))
)

;; Query: Correlate Gene Expressions with cDNA Clones and Protein Accessions
(query-info "Correlate gene expressions with corresponding cDNA clones and protein accessions."
    (gene-expression-cdna-protein $GeneSymbol $ExpressionValue $CloneName $ProteinAccession)
   (, (gene_rpkm_matrix! $GeneSymbol $_ $_ $ExpressionValue)
      (cDNA_clone! $_ $_ $CloneName $_)
      (fbgn_NAseq_Uniprot! $GeneSymbol $_ $_ $ProteinAccession))
)

;; Query: Determine Gene Functions and Map them to Genetic and Physical Interactions
(query-info "Determine gene functions and map them to genetic and physical interactions."
    (gene-functions-genetic-physical-interactions $GeneSymbol $Function $GeneticInteraction $PhysicalInteraction)
   (, (automated_gene_summaries! $GeneSymbol $Function)
      (gene_genetic_interactions! $GeneSymbol $_ $_ $GeneticInteraction)
      (physical_interactions_mitab! $_ $_ $GeneSymbol $PhysicalInteraction))
)

;; Query: Explore Gene Synonyms, Genetic Map Positions, and Disease Associations
(query-info "Explore gene synonyms, genetic map positions, and disease associations."
    (gene-synonyms-map-diseases $GeneSymbol $Synonym $MapPosition $Disease)
   (, (fb_synonym! $GeneSymbol $_ $_ $Synonym)
      (cyto-genetic-seq! $GeneSymbol $_ $_ $MapPosition)
      (disease_model_annotations! $GeneSymbol $_ $_ $_ $Disease))
)

;; Query: Associate Gene Expression with Cell Types and Developmental Stages
(query-info "Associate gene expression with specific cell types and developmental stages."
    (gene-expression-cell-types-stages $GeneSymbol $ExpressionValue $CellType $Stage)
   (, (gene_rpkm_matrix! $GeneSymbol $_ $_ $ExpressionValue)
      (scRNA-Seq_gene_expression! $_ $_ $_ $_ $Stage $_ $CellType))
)

;; Query: Link Genomic Clones to Genes, Proteins, and Synonyms
(query-info "Link genomic clones to their corresponding genes, proteins, and synonyms."
    (genomic-clone-gene-protein-synonym $CloneName $GeneSymbol $ProteinAccession $Synonym)
   (, (genomic_clone! $CloneName $_ $_ $_)
      (fbgn_NAseq_Uniprot! $GeneSymbol $_ $_ $ProteinAccession)
      (fb_synonym! $_ $_ $GeneSymbol $Synonym))
)

;; Query: Relate Gene Snapshots to Disease Models and Human Orthologs
(query-info "Relate gene snapshots to disease models and their human orthologs."
    (gene-snapshots-diseases-orthologs $GeneSymbol $SnapshotText $Disease $HumanOrtholog)
   (, (gene_snapshots! $GeneSymbol $_ $_ $SnapshotText)
      (disease_model_annotations! $GeneSymbol $_ $_ $_ $Disease)
      (dmel_human_orthologs_disease! $GeneSymbol $_ $_ $HumanOrtholog))
)

;; Query: Connect Genes to Their Enzymatic Functions and Disease Models
(query-info "Connect genes to their enzymatic functions and associated disease models."
    (genes-enzymes-diseases $GeneSymbol $EnzymeFunction $Disease)
   (, (Dmel_enzyme! $GeneSymbol $_ $_ $_ $EnzymeFunction)
      (disease_model_annotations! $GeneSymbol $_ $_ $_ $Disease))
)

;; Query: Associate Pathway Groups with Gene Functions and Physical Interactions
(query-info "Associate pathway groups with gene functions and physical interactions."
    (pathway-gene-function-physical-interaction $Pathway $GeneFunction $PhysicalInteraction)
   (, (pathway_group! $Pathway $_ $_ $_)
      (automated_gene_summaries! $_ $GeneFunction)
      (physical_interactions_mitab! $_ $_ $_ $PhysicalInteraction))
)


;; Query: Associate Disease Models with Gene Transcripts, Protein Isoforms, Gene Summaries, and Snapshots
(query-info "Link disease model annotations with corresponding gene transcripts, unique protein isoforms, the best gene summaries, and gene snapshots."
   (gene-disease-models-transcripts-proteins-summaries-snapshots $GeneID $GeneSymbol $ProteinID $GeneSummaryID $GeneSnapshotID)
   (, (disease_model_annotations! $GeneID $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_)
      (fbgn_fbtr_fbpp_expanded! $TranscriptType $TranscriptID $GeneID $GeneSymbol $GeneFullName $AnnotationID $_ $_ $_ $_ $_)
      (dmel_unique_protein_isoforms! $ProteinID $GeneSymbol $ProteinSymbol $_)
      (best_gene_summary! $GeneSummaryID $GeneSymbol $SummarySource $SummaryText)
      (gene_snapshots! $GeneSnapshotID $GeneSymbol $GeneName $Datestamp $SnapshotText))
)


;; Query: Link Paralogous Genes with Gene Mapping, Transcripts, Protein Isoforms, and Summaries
(query-info "Connect paralogous genes to their gene mapping, transcripts, protein isoforms, and gene summaries."
   (paralogs-gene-mapping-transcripts-proteins-summaries $GeneID $GeneSymbol $ParalogGeneID $MapID $ProteinID)
   (, (dmel_paralogs! $GeneID $GeneSymbol $Arm $Location $Strand $ParalogGeneID $ParalogGeneSymbol $_ $_ $_ $_)
      (gene_map_table! $MapID $OrganismAbbreviation $GeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (fbgn_fbtr_fbpp_expanded! $TranscriptType $TranscriptID $TranscriptSymbol $MapID $_ $_ $_ $_ $_ $_)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $_)
      (best_gene_summary! $GeneSummaryID $ProteinSymbol $SummarySource $SummaryText))
)


;; Query: Correlate Expanded Gene Transcripts with Protein Isoforms, Paralogs, Gene Mapping, and Synonyms
(query-info "Associate expanded gene transcripts with corresponding protein isoforms, paralogs, gene mapping data, and synonyms."
   (transcripts-proteins-paralogs-mapping-synonyms $GeneID $TranscriptID $ProteinID $ParalogGeneID $MapID)
   (, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $_ $_ $_ $_ $_)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $_)
      (dmel_paralogs! $ParalogGeneID $ProteinSymbol $_ $_ $_ $_ $_ $_ $_ $_ $_)
      (gene_map_table! $MapID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (synonym! $SynonymID $MapID $CurrentSymbol $CurrentFullName $_ $_))
)

!"
!(number-of
   (, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $_ $_ $_ $_ $_)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $_)
      (dmel_paralogs! $ParalogGeneID $ProteinSymbol $_ $_ $_ $_ $_ $_ $_ $_ $_)
      (gene_map_table! $MapID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (synonym! $SynonymID $MapID $CurrentSymbol $CurrentFullName $_ $_))
"

;; Query: Connect Gene Mapping with Expanded Transcripts, Protein Isoforms, Gene Summaries, and Basic Transcripts
(query-info "Link gene mapping data with expanded transcripts, unique protein isoforms, gene summaries, and basic transcript information."
   (mapping-transcripts-proteins-summaries $MapID $GeneSymbol $TranscriptID $ProteinID $GeneSummaryID)
   (, (gene_map_table! $MapID $OrganismAbbreviation $GeneSymbol $RecombinationLoc $CytogeneticLoc $SequenceLoc)
      (fbgn_fbtr_fbpp_expanded! $TranscriptType $TranscriptID $TranscriptSymbol $OrganismAbbreviation $_ $_ $_ $_ $_ $_)
      (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $_)
      (best_gene_summary! $GeneSummaryID $ProteinSymbol $SummarySource $SummaryText)
      (fbgn_fbtr_fbpp! $GeneSummaryID $_ $_))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query: Associate Gene Expression with Developmental Stages and Publications
(query-info "Link gene expression data to specific developmental stages and corresponding publications."
    (gene-expression-stages-publications $GeneID $ExpressionValue $Stage $Publication)
    (, (gene_rpkm_report! $GeneID $ExpressionValue $_ $_ $_ $Stage $_ $_ $_ $_ $_ $_ $_ $_)
       (fbrf_pmid_pmcid_doi! $_ $Publication $_ $_ $_ $_ $_ $_))
)

;; Query: Link Gene Functional Data with Enzymatic Activities and Disease Models
(query-info "Connect gene functional data with enzymatic activities and their relevance to disease models."
    (gene-function-enzyme-disease $GeneSymbol $Function $EnzymeActivity $Disease)
    (, (automated_gene_summaries! $GeneSymbol $Function)
       (Dmel_enzyme! $GeneSymbol $_ $_ $_ $_ $_ $_ $_ $_ $_ $_)
       (disease_model_annotations! $_ $GeneSymbol $_ $_ $_ $Disease $_ $_ $_ $_ $_ $_))
)

;; Query: Relate Genes to Their Allelic Variants and Phenotypic Effects
(query-info "Relate genes to their allelic variants and the resulting phenotypic effects."
    (genes-alleles-phenotypes $GeneSymbol $AlleleSymbol $Phenotype)
    (, (fbal_to_fbgn! $_ $AlleleSymbol $_ $GeneSymbol)
       (allele_phenotypic! $AlleleSymbol $_ $Phenotype $_))
)

;; Query: Correlate Genes with their Physical Interactions and Pathway Involvements
(query-info "Correlate genes with their physical interactions and pathway involvements."
    (genes-physical-interactions-pathways $GeneID $PhysicalInteraction $Pathway)
    (, (physical_interactions_mitab! $GeneID $_ $_ $PhysicalInteraction $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_)
       (pathway_group! $Pathway $_ $_ $_ $_ $_ $GeneID $_ $_))
)

;; Query: Connect Genes with Disease Annotations and Relevant Publication Data
(query-info "Connect genes with disease annotations and relevant publication data."
    (genes-disease-annotations-publications $GeneSymbol $DiseaseAnnotation $Publication)
    (, (disease_model_annotations! $_ $GeneSymbol $_ $_ $_ $DiseaseAnnotation $_ $_ $_ $_ $_ $_ $_ $_)
       (fbrf_pmid_pmcid_doi! $_ $Publication $_ $_ $_ $_ $_ $_))
)

;; Query: Associate Gene Sequences with Synonyms and Protein Isoforms
(query-info "Associate gene sequences with their synonyms and protein isoforms."
    (gene-sequences-synonyms-proteins $GeneID $Synonym $ProteinIsoform)
    (, (dmel_gene_sequence_ontology_annotations! $GeneID $_ $_ $_)
       (fb_synonym! $_ $_ $GeneID $_ $Synonym $_)
       (dmel_unique_protein_isoforms! $_ $_ $GeneID $ProteinIsoform $_))
)

;; Query: Link Genotype Data with Phenotypes and Corresponding References
(query-info "Link genotype data with phenotypes and their corresponding references."
    (genotype-phenotypes-references $GenotypeSymbols $Phenotype $Reference)
    (, (genotype_phenotype_data! $GenotypeSymbols $_ $Phenotype $_ $_ $_ $_ $Reference $_)
       (fbrf_pmid_pmcid_doi! $Reference $_ $_ $_ $_ $_ $_ $_))
)

;; Query: Correlate Gene Snapshots with Their Enzymatic Functions and Disease Models
(query-info "Correlate gene snapshots with their enzymatic functions and disease models."
    (gene-snapshots-enzymes-diseases $GeneSymbol $SnapshotText $EnzymeFunction $Disease)
    (, (gene_snapshots! $GeneSymbol $_ $_ $SnapshotText $_)
       (Dmel_enzyme! $GeneSymbol $_ $_ $_ $_ $_ $_ $_ $_)
       (disease_model_annotations! $_ $GeneSymbol $_ $_ $_ $Disease $_ $_ $_ $_ $_ $_))
)

;; Query: Associate Genes with Their cDNA Clones and Disease Models
(query-info "Associate genes with their cDNA clones and relevant disease models."
    (genes-cdna-disease-models $GeneSymbol $CloneName $Disease)
    (, (cDNA_clone! $_ $_ $CloneName $_ $_ $_)
       (gene_map_table! $_ $GeneSymbol $_ $_ $_ $_ $_)
       (disease_model_annotations! $_ $GeneSymbol $_ $_ $_ $Disease $_ $_ $_ $_ $_ $_))
)

;; Query: Connect Genes to Publications and Allelic Interactions
(query-info "Connect genes to relevant publications and their allelic interactions."
    (genes-publications-allelic-interactions $GeneSymbol $Publication $AlleleInteraction)
    (, (entity_publication! $_ $GeneSymbol $Publication $_)
       (allele_genetic_interactions! $_ $_ $AlleleInteraction $_ $GeneSymbol))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ORs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query: Identify Genes Either Involved in Physical Interactions or Associated with Specific Diseases
(query-info "Find genes that are either involved in physical interactions or associated with specific diseases."
    (genes-physical-or-disease $GeneSymbol $PhysicalInteraction $Disease)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (physical_interactions_mitab! $GeneSymbol $_ $_ $PhysicalInteraction $_ $_ $_)
           (disease_model_annotations! $GeneSymbol $_ $_ $_ $_ $_ $_ $Disease $_ $_ $_)))
)

;; Query: Retrieve Gene Snapshots or Enzymatic Functions for Specific Genes
(query-info "Get gene snapshots or enzymatic functions for specific genes."
    (gene-snapshots-or-enzymes $GeneSymbol $SnapshotText $EnzymeFunction)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (gene_snapshots! $GeneSymbol $_ $_ $SnapshotText $_)
           (Dmel_enzyme! $GeneSymbol $_ $_ $_ $EnzymeFunction $_ $_ $_)))
)

;; Query: Find Genes with Either cDNA Clones or Related to Specific Pathways
(query-info "Identify genes that have cDNA clones or are related to specific pathways."
    (genes-cdna-or-pathways $GeneSymbol $CloneName $Pathway)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (cDNA_clone! $CloneName $_ $GeneSymbol $_ $_)
           (pathway_group! $GeneSymbol $_ $_ $_ $Pathway $_ $_)))
)

;; Query: Associate Genes with Synonyms or Protein Isoforms
(query-info "Associate genes with either their synonyms or protein isoforms."
    (genes-synonyms-or-proteins $GeneSymbol $Synonym $ProteinIsoform)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (fb_synonym! $GeneSymbol $_ $_ $Synonym $_)
           (dmel_unique_protein_isoforms! $GeneSymbol $_ $_ $ProteinIsoform $_)))
)

;; Query: Link Alleles to Phenotypes or Genetic Interactions
(query-info "Link alleles to their phenotypes or to their genetic interactions."
    (alleles-phenotypes-or-interactions $AlleleSymbol $Phenotype $Interaction)
    (, (fbal_to_fbgn! $AlleleSymbol $_ $_ $_)
       (or (allele_phenotypic! $AlleleSymbol $_ $Phenotype $_)
           (allele_genetic_interactions! $AlleleSymbol $_ $Interaction $_)))
)

;; Query: Find Genes Involved in RNA Processing or Gene Regulation
(query-info "Find genes involved in RNA processing or gene regulation."
    (genes-rna-or-regulation $GeneSymbol $RNAProcess $GeneRegulation)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (gene_rpkm_matrix! $GeneSymbol $_ $_ $RNAProcess $_)
           (gene_functional_complementation! $GeneSymbol $_ $_ $GeneRegulation $_)))
)

;; Query: Retrieve Publications Related to Genes or Their Allelic Variants
(query-info "Retrieve publications related to genes or their allelic variants."
    (publications-genes-or-alleles $GeneSymbol $Publication $AlleleSymbol)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (entity_publication! $Publication $_ $GeneSymbol $_)
           (allele_genetic_interactions! $AlleleSymbol $_ $_ $_ $GeneSymbol)))
)

;; Query: Correlate Genes with Their Expression in Tissues or During Specific Stages
(query-info "Correlate genes with their expression in tissues or during specific developmental stages."
    (genes-expression-tissues-or-stages $GeneSymbol $Tissue $Stage)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (or (scRNA-Seq_gene_expression! $_ $_ $_ $_ $Tissue $Stage $_ $_)
           (gene_rpkm_report! $GeneSymbol $_ $_ $_ $_ $_ $_ $Stage $_)))
)


;; Query: Find Genes Involved in RNA Processing but Not in Gene Regulation
(query-info "Identify genes involved in RNA processing but not in gene regulation."
    (genes-rna-not-regulation $GeneSymbol $RNAProcess)
    (, (gene_rpkm_matrix! $GeneSymbol $_ $_ $RNAProcess $_)
       (not (gene_functional_complementation! $GeneSymbol $_ $_ $_ $_ $_)))
)

;; Query: Retrieve Publications Related to Specific Genes but Not Their Allelic Variants
(query-info "Retrieve publications related to specific genes but not their allelic variants."
    (publications-genes-not-alleles $GeneSymbol $Publication)
    (, (entity_publication! $Publication $_ $GeneSymbol $_)
       (not (allele_genetic_interactions! $_ $_ $_ $GeneSymbol)))
)

;; Query: Identify Genes with cDNA Clones but Not Involved in Specific Pathways
(query-info "Identify genes that have cDNA clones but are not involved in specific pathways."
    (genes-cdna-not-pathways $GeneSymbol $CloneName)
    (, (cDNA_clone! $CloneName $_ $GeneSymbol $_ $_ $_)
       (not (pathway_group! $GeneSymbol $_ $_ $_ $_ $_ $_)))
)

;; Query: Associate Genes with Synonyms but Exclude Certain Protein Isoforms
(query-info "Associate genes with their synonyms but exclude certain protein isoforms."
    (genes-synonyms-not-proteins $GeneSymbol $Synonym)
    (, (fb_synonym! $GeneSymbol $_ $_ $Synonym $_)
       (not (dmel_unique_protein_isoforms! $GeneSymbol $_ $_ $_)))
)

;; Query: Link Alleles to Phenotypes but Exclude Specific Genetic Interactions
(query-info "Link alleles to their phenotypes but exclude specific genetic interactions."
    (alleles-phenotypes-not-interactions $AlleleSymbol $Phenotype)
    (, (allele_phenotypic! $AlleleSymbol $_ $Phenotype $_)
       (not (allele_genetic_interactions! $AlleleSymbol $_ $_ $_)))
)

;; Query: Find Genes Involved in Physical Interactions but Not Associated with Diseases
(query-info "Find genes involved in physical interactions but not associated with specific diseases."
    (genes-physical-not-disease $GeneSymbol $PhysicalInteraction)
    (, (physical_interactions_mitab! $GeneSymbol $_ $_ $PhysicalInteraction $_ $_ $_ $_ $_)
       (not (disease_model_annotations! $GeneSymbol $_ $_ $_ $_ $_ $_ $_ $_ $_)))
)

;; Query: Retrieve Gene Snapshots but Exclude Those Related to Enzymatic Functions
(query-info "Retrieve gene snapshots but exclude those related to enzymatic functions."
    (gene-snapshots-not-enzymes $GeneSymbol $SnapshotText)
    (, (gene_snapshots! $GeneSymbol $_ $_ $SnapshotText $_)
       (not (Dmel_enzyme! $GeneSymbol $_ $_ $_ $_ $_ $_ $_)))
)

;; Query: Correlate Genes with Expression in Tissues but Not During Specific Stages
(query-info "Correlate genes with their expression in tissues but not during specific developmental stages."
    (genes-expression-tissues-not-stages $GeneSymbol $Tissue)
    (, (scRNA-Seq_gene_expression! $_ $_ $_ $_ $Tissue $_ $_ $_ $GeneSymbol $_ $_)
       (not (gene_rpkm_report! $GeneSymbol $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_)))
)


;; Query: Find Genes with Specific Transcripts or Protein Isoforms but Not Involved in Certain Pathways
(query-info "Identify genes that have specific transcripts or protein isoforms but are not involved in certain pathways."
    (genes-transcripts-proteins-not-pathways $GeneSymbol $TranscriptID $ProteinIsoform $ExcludedPathway)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (, (or (fbgn_fbtr_fbpp! $GeneSymbol $TranscriptID $_ $_ $_)
                 (dmel_unique_protein_isoforms! $GeneSymbol $_ $ProteinIsoform $_))
            (not (pathway_group! $GeneSymbol $_ $_ $_ $ExcludedPathway $_ $_))))
)

;; Query: Retrieve Gene Snapshots or Allelic Variants but Exclude Genes with Certain Phenotypes
(query-info "Retrieve gene snapshots or allelic variants but exclude genes associated with certain phenotypes."
    (gene-snapshots-alleles-not-phenotypes $GeneSymbol $SnapshotText $AlleleSymbol $ExcludedPhenotype)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (, (or (gene_snapshots! $GeneSymbol $_ $SnapshotText $_)
                 (allele_genetic_interactions! $AlleleSymbol $_ $_ $_ $GeneSymbol))
            (not (allele_phenotypic! $AlleleSymbol $_ $ExcludedPhenotype $_))))
)

;; Query: Link Genes to Physical Interactions or Publications but Not to Specific Disease Models
(query-info "Link genes to physical interactions or relevant publications but not to specific disease models."
    (genes-interactions-publications-not-disease $GeneSymbol $PhysicalInteraction $Publication $ExcludedDisease)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (, (or (physical_interactions_mitab! $GeneSymbol $_ $_ $PhysicalInteraction $_ $_)
                 (entity_publication! $Publication $_ $GeneSymbol $_))
            (not (disease_model_annotations! $GeneSymbol $_ $_ $_ $_ $_ $_ $ExcludedDisease))))
)

;; Query: Associate Genes with cDNA Clones or Enzymatic Functions but Exclude Certain Synonyms
(query-info "Associate genes with cDNA clones or enzymatic functions but exclude certain synonyms."
    (genes-cdna-enzymes-not-synonyms $GeneSymbol $CloneName $EnzymeFunction $ExcludedSynonym)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (, (or (cDNA_clone! $CloneName $_ $GeneSymbol $_)
                 (Dmel_enzyme! $GeneSymbol $_ $_ $EnzymeFunction $_))
            (not (fb_synonym! $GeneSymbol $_ $_ $ExcludedSynonym $_))))
)

;; Query: Find Genes Involved in RNA Processing or Gene Regulation but Not Linked to Specific Protein Accessions
(query-info "Find genes involved in RNA processing or gene regulation but not linked to specific protein accessions."
    (genes-rna-regulation-not-proteins $GeneSymbol $RNAProcess $GeneRegulation $ExcludedProteinAccession)
    (, (gene_map_table! $GeneSymbol $_ $_ $_ $_ $_)
       (, (or (gene_rpkm_matrix! $GeneSymbol $_ $_ $RNAProcess $_)
                 (gene_functional_complementation! $GeneSymbol $_ $_ $GeneRegulation $_))
            (not (fbgn_NAseq_Uniprot! $GeneSymbol $_ $_ $ExcludedProteinAccession $_ $_ $_)))))


!(fb-info!)

(: number-of (-> Countable Number))
(= (number-of $query $length)
    (length! (match &self $query t) $length))

;; call-nth! is one of our builtins
(: nth-pose! (-> Number Atom))
(= (nth-pose! $length $eval)
   (call-nth! $eval $nth)
   (=< $nth $length))




(= (do-77-queries)
  (let $todo
   (match &self (query-info $English $template   $query) (demo-query $English $query $template))
   (superpose $todo)))


;; print no more than the first 5 answers
(= (demo-query $English $query $template)
   (println!  ($English  "=" $query))
   (println!  (limit! 5 (match &flybase $query $template)))
   (println! ("time was = " (time! (number-of $query $count))))
   (println! ("total of query = " $count)))


;; Rust only:
!(mettalog::vspace-main)

;; inside flybase we only see 74 answers
!(match &flybase (gene_map_table $Org $Sym $G 3-59 $D $E) ($Sym $G $D $E))

;; should return no results in &self
!(match &self (gene_map_table $Org $Sym $G 3-59 $D $E) ($Sym $G $D $E))

!(get-atoms &flybase (gene_map_table $Org $Sym $G 3-59 $D $E))

;; our special hack to see into other spaces
!(number-of (gene_map_table! $Org $Sym $G 3-59 $D $E)) ;; 74

;;     flybase space answer should be 0
!(number-of (gene_map_table $Org $Sym $G 3-59 $D $E)) ;; 0

;; use the host system limiter of 5
!(number-of  (limit! 5 (gene_map_table! $Org $Sym $G 3-59 $D $E)))  ;; 5


; (gene_map_table! MapID OrganismAbbreviation ParalogGeneID RecombinationLoc CytogeneticLoc SequenceLoc)

!(assertEqual (number-of (gene_map_table! $MapID $OrganismAbbreviation $ParalogGeneID 3-59 $CytogeneticLoc $SequenceLoc)) 74)

;; What recombination locations have 74 Paralog Genes?
;; !(,  (gene_map_table! $_ $_ $_ $RecombinationLoc $_ $_) (== 74 (number-of (gene_map_table! $_ $_ $ParalogGeneID $RecombinationLoc $_ $_))))



!(do-77-queries)

