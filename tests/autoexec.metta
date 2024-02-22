
(Recipe Morning-shot (With-ingredient Ginger))
(Recipe Morning-shot (With-ingredient Orange))

(In-store Apple)
(In-store Banana)
(In-store Orange)

(Hate Apple)
(Hate Grape)
(Love Banana)
(Love Orange)
(Love Strawberry)

!(transform (Love $fruit) (transform (In-store $fruit)
    (join
        (Buy $fruit)
        (transform (Recipe $recipe (With-ingredient $fruit))
            (transform (Recipe $recipe (With-ingredient $ingredient))
                (Buy $ingredient)
            )
        )
    )
))


!(print_pred_as_metta)



 (=  (pp-sex  (ValueAtom 2) $O) (sequential  (write             ) (pp-sex  $O)))


 (=  (into-sequential  $O $P) (sequential  (missing  (is-list  $O)) (conjuncts-to-list  $O $Q) (into-sequential  $Q $P)))
 (=  (into-sequential  $O $P) (sequential  (length  $O $Q) (>  $Q (ValueAtom 1)) (equals-ListMappingFn  $P ([|]  sequential $O)) no-more))
 (=  (into-sequential  $O $O) (:: no-more))


 (=  ah (sequential  (add-history1  fb-stats) (add-history1  mine-overlaps) (add-history1  load-flybase)))
 (=  ah (sequential  (add-history  fb-stats) (add-history  mine-overlaps) (add-history  try-overlaps) (add-history  load-flybase)))


 (=  list-column-names (:: (missing  (and  (column-names  $O $P) (and  (->  (and  (length  $P $Q) (and  (>=  $Q (ValueAtom 2)) (fb-pred  $O $Q))) True) (missing  (and  (print  (column-names  $O $P)) nl)))))))

 (=  (guess-rest  $O $P $Q $R) (sequential  (table-n-type  $O $P $Q $R) (var  $R) (fb-pred  $O $S) (functor  $T $O $S) (arg  $P $T $R) (->  (call  $T) True)))


 (=  (flybase-tables  (:: analysis analysisfeature analysisgrp analysisgrpmember analysisprop audit-chado cell-line cell-line-loaderm cell-line-loadermprop cell-line-dbxref cell-line-feature cell-line-library cell-line-libraryprop cell-line-pub cell-line-relationship cell-line-strain cell-line-strainprop cell-line-synonym cell-lineprop cell-lineprop-pub contact cv loaderm loaderm-dbxref loaderm-relationship loadermpath loadermprop loadermsynonym db dbxref dbxrefprop eimage environment environment-loaderm expression expression-loaderm expression-loadermprop expression-image expression-pub expressionprop feature feature-loaderm feature-loaderm-dbxref feature-loadermprop feature-dbxref feature-expression feature-expressionprop feature-genotype feature-grpmember feature-grpmember-pub feature-humanhealth-dbxref feature-interaction feature-interaction-pub feature-interactionprop feature-phenotype feature-pub feature-pubprop feature-relationship feature-relationship-pub feature-relationshipprop feature-relationshipprop-pub feature-synonym featureloc featureloc-pub featuremap featuremap-pub featurepos featureprop featureprop-pub featurerange genotype genotype-loaderm genotype-loadermprop genotype-dbxref genotype-pub genotype-synonym genotypeprop genotypeprop-pub grp grp-loaderm grp-dbxref grp-pub grp-pubprop grp-relationship grp-relationship-pub grp-relationshipprop grp-synonym grpmember grpmember-loaderm grpmember-pub grpmemberprop grpmemberprop-pub grpprop grpprop-pub humanhealth humanhealth-loaderm humanhealth-loadermprop humanhealth-dbxref humanhealth-dbxrefprop humanhealth-dbxrefprop-pub humanhealth-feature humanhealth-featureprop humanhealth-phenotype humanhealth-phenotypeprop humanhealth-pub humanhealth-pubprop humanhealth-relationship humanhealth-relationship-pub humanhealth-synonym humanhealthprop humanhealthprop-pub interaction interaction-cell-line interaction-loaderm interaction-loadermprop interaction-expression interaction-expressionprop interaction-group interaction-group-feature-interaction interaction-pub interactionprop interactionprop-pub library library-loaderm library-loadermprop library-dbxref library-dbxrefprop library-expression library-expressionprop library-feature library-featureprop library-grpmember library-humanhealth library-humanhealthprop library-interaction library-pub library-relationship library-relationship-pub library-strain library-strainprop library-synonym libraryprop libraryprop-pub lock organism organism-loaderm organism-loadermprop organism-dbxref organism-grpmember organism-library organism-pub organismprop organismprop-pub phendesc phenotype phenotype-comparison phenotype-comparison-loaderm phenotype-loaderm phenstatement project pub pub-dbxref pub-relationship pubauthor pubprop sql-features sql-implementation-info sql-parts sql-sizing stock stock-loaderm stock-dbxref stock-genotype stock-pub stock-relationship stock-relationship-pub stockcollection stockcollection-stock stockcollectionprop stockprop stockprop-pub strain strain-loaderm strain-loadermprop strain-dbxref strain-feature strain-featureprop strain-phenotype strain-phenotypeprop strain-pub strain-relationship strain-relationship-pub strain-synonym strainprop strainprop-pub synonym tableinfo update-track)) True)


 (=  (eigther-contains  $O $P) (sequential  (=  $O $P) no-more))
 (=  (eigther-contains  $O $P) (sequential  (atom-contains  $O $P) no-more))
 (=  (eigther-contains  $O $P) (sequential  (atom-contains  $P $O) no-more))


 (=  (table-columns-tt  $O $P) (:: (column-names  $O $P)))
 (=  (table-columns-tt  $O $P) (:: (column-names  $O $P)))
 (=  (table-columns-tt  $O $P) (:: (flybase-cols  $O $P)))
 (=  (table-columns-tt  $O $P) (:: (t-h-n  $O $A10 $P)))


 (=  (table-columns  $O $P) (sequential  (table-columns-tt  $Q $P) (eigther-contains  $O $Q) no-more))


 (=  setup-flybase-cols (:: (missing  (and  (flybase-cols  $O $P) (missing  (use-flybase-cols  $O $P))))))


 (=  (clip-id  $O $P) (sequential  (if-then-else  (atom-concat  $P -id $O) True (=  $O $P)) no-more))


 (=  (make-arity-2-name  $O $P $Q) (sequential  (clip-id  $P $R) (if-then-else  (atom-concat  $O $A10 $P) (=  $Q $P) (atomic-list-concat  (:: $O $R) - $Q))))


 (=  (do-arity-2-names-dc1  $O $P $Q $R) (:: (must-det-ll-r  (and  (arg  (ValueAtom 1) $P $S) (and  (arg  $Q $P $T) (and  (make-arity-2-name  $O $R $U) (and  (equals-ListMappingFn  $V (:: $O $S)) (and  (clip-id  $R $W) (and  (if-then-else  (==  $R $W) (=  $X $T) (equals-ListMappingFn  $X (:: $W $T))) (and  (equals-ListMappingFn  $Y (:: $U $V $X)) (and  (fbug  (:-  $Y $P)) (fb-assert  (:-  $Y $P)))))))))))))


 (=  (do-arity-2-names-dc  $O $P $Q ([|]  $R $S)) (sequential  (do-arity-2-names-dc1  $O $P $Q $R) no-more (is  $T (+  $Q (ValueAtom 1))) (do-arity-2-names-dc  $O $P $T $S)))
 (=  (do-arity-2-names-dc  $A10 $B10 $C10 ()) True)


 (=  (do-arity-2-names  $O ([|]  $P $Q)) (:: (must-det-ll-r  (and  (atom-concat  data- $O $R) (and  (length  ([|]  $P $Q) $S) (and  (length  $T $S) (and  (equals-ListMappingFn  $U ([|]  $R $T)) (do-arity-2-names-dc  $O $U (ValueAtom 2) $Q))))))))


 (=  (use-flybase-cols  $O $P) (:: (must-det-ll-r  (and  (maplist  (fix-header-names  $P $O) $P $Q) (and  (assert  (flybase-col-names  $O $P $Q)) (do-arity-2-names  $O $Q))))))


 (=  pmt (sequential  (flybase-tables  $O) (missing  (and  (member  $P $O) (missing  (if-then-else  (missing  (flybase-cols  $P $A10)) (format  ~N~q.~n (:: (get-fbt  $P))) True))))))


 (=  (fix-header-names  $A10 $B10 $O $P) (sequential  (number  $O) no-more (=  $P $O)))
 (=  (fix-header-names  $O $P (ListValue $Q) (ListValue $R)) (sequential  (fix-header-names  $O $P $Q $R) no-more))
 (=  (fix-header-names  $O $P (ListValue $Q) (ListValue $S)) (sequential  (fix-header-names  $O $P $Q $S) no-more))
 (=  (fix-header-names  $O $P $Q $R) (sequential  (member  $S (:: #   -)) (atom-concat  $T $S $Q) no-more (fix-header-names  $O $P $T $R)))
 (=  (fix-header-names  $O $P $Q $R) (sequential  (member  $S (:: #   -)) (atom-concat  $S $T $Q) no-more (fix-header-names  $O $P $T $R)))
 (=  (fix-header-names  $O $P $Q $R) (sequential  (member  $S (:: --  )) (atomic-list-concat  $T $S $Q) (\=  $T (:: $A10)) (atomic-list-concat  $T - $U) no-more (fix-header-names  $O $P $U $R)))
 (=  (fix-header-names  $O $P $Q (ListValue $R)) (sequential  (member  $S (:: (es) (s) ids)) (atomic-list-concat  (:: $T $U) $S $Q) (atomic-list-concat  (:: $T $U) - $V) no-more (fix-header-names  $O $P $V $R) no-more))
 (=  (fix-header-names  $O $P $Q (ListValue $R)) (sequential  (member  (=  $S $T) (:: (=  IDs ID))) (atom-concat  $U $S $Q) (atom-concat  $U $T $V) (fix-header-names  $O $P $V $R) no-more))
 (=  (fix-header-names  $O $P $Q (ListValue $R)) (sequential  (member  (=  $S $T) (:: (=  -IDs -ID) (=  IDs ID))) (atomic-list-concat  (:: $U $V) $S $Q) (atomic-list-concat  (:: $U $V) $T $W) no-more (fix-header-names  $O $P $W $R) no-more))
 (=  (fix-header-names  $A10 $B10 $O $O) (sequential  (missing  (too-generic  $O)) no-more))
 (=  (fix-header-names  $A10 $B10 $O $O) (sequential  (atomic-list-concat  ([|]  $C10 ([|]  $D10 $E10)) - $O) no-more))
 (=  (fix-header-names  $A10 $B10 $O $O) True)


 (=  (fix-header-names  $O $P $Q) (sequential  (--aux-maplist/3-fix-header-names+2  $P $R $P $O) (include  (\=  ()) $R $Q)))


 (=  (--aux-maplist/3-fix-header-names+2  () () $A10 $B10) True)
 (=  (--aux-maplist/3-fix-header-names+2  ([|]  $O $P) ([|]  $Q $R) $S $T) (sequential  (fix-header-names  $S $T $O $Q) (--aux-maplist/3-fix-header-names+2  $P $R $S $T)))


 (=  (too-generic  $O) (sequential  (var  $O) no-more False))
 (=  (too-generic  pub-id) True)
 (=  (too-generic  $O) (:: (missing  (atomic-list-concat  ([|]  $A10 ([|]  $B10 ([|]  $C10 $D10))) - $O))))


 (=  (column-names  cyto-genetic-seq (:: Cytogenetic-map-position Genetic-map-position Sequence-coordinates-(release-6) R6-conversion-notes)) True)
 (=  (column-names  Dmel-enzyme (:: gene-group-id gene-group-name (ListValue gene-group-GO-id) (ListValue gene-group-GO-name) (ListValue gene-group-EC-number) (ListValue gene-group-EC-name) gene-id gene-symbol gene-name (ListValue gene-EC-number) (ListValue gene-EC-name))) True)
 (=  (column-names  scRNA-Seq-gene-expression (:: Pub-ID Pub-miniref Clustering-Analysis-ID Clustering-Analysis-Name Source-Tissue-Sex Source-Tissue-Stage Source-Tissue-Anatomy Cluster-ID Cluster-Name Cluster-Cell-Type-ID Cluster-Cell-Type-Name Gene-ID Gene-Symbol Mean-Expression Spread)) True)
 (=  (column-names  allele-genetic-interactions (:: allele-symbol allele-FBal interaction FBrf)) True)
 (=  (column-names  allele-phenotypic (:: allele-symbol allele-FBal phenotype FBrf)) True)
 (=  (column-names  fbal-to-fbgn (:: AlleleID AlleleSymbol GeneID GeneSymbol)) True)
 (=  (column-names  genotype-phenotype-data (:: (ListValue genotype-symbols) (ListValue genotype-FBids) phenotype-name phenotype-id (ListValue qualifier-names) (ListValue qualifier-ids) reference)) True)
 (=  (column-names  automated-gene-summaries (:: primary-FBgn summary-text)) True)
 (=  (column-names  best-gene-summary (:: FBgn Gene-Symbol Summary-Source Summary)) True)
 (=  (column-names  cDNA-clone (:: FBcl organism-abbreviation clone-name dataset-metadata-name (ListValue cDNA-accession) (ListValue EST-accession))) True)
 (=  (column-names  dataset-metadata (:: Dataset-Metadata-ID Dataset-Metadata-Name Item-ID Item-Name)) True)
 (=  (column-names  disease-model-annotations (:: FBgn Gene-symbol HGNC DO-qualifier DO DO-term Allele-used-in-model-(FBal) Allele-used-in-model-(symbol) Based-on-orthology-with-(HGNC-ID) Based-on-orthology-with-(symbol) Evidence/interacting-alleles Reference-(FBrf))) True)
 (=  (column-names  dmel-gene-sequence-ontology-annotations (:: gene-primary-id gene-symbol so-term-name so-term-id)) True)
 (=  (column-names  dmel-human-orthologs-disease (:: Dmel-gene Dmel-gene-symbol Human-gene-HGNC Human-gene-OMIM Human-gene-symbol DIOPT-score OMIM-Phenotype-IDs OMIM-Phenotype-IDs[name])) True)
 (=  (column-names  dmel-paralogs (:: FBgn GeneSymbol Arm/Scaffold Location Strand Paralog-FBgn Paralog-GeneSymbol Paralog-Arm/Scaffold Paralog-Location Paralog-Strand DIOPT-score)) True)
 (=  (column-names  dmel-unique-protein-isoforms (:: FBgn FB-gene-symbol representative-protein (ListValue identical-protein))) True)
 (=  (column-names  entity-publication (:: entity-id entity-name FlyBase-publication PubMed)) True)
 (=  (column-names  fb-synonym (:: primary-FBid organism-abbreviation current-symbol current-fullname (ListValue fullname-synonym) (ListValue symbol-synonym))) True)
 (=  (column-names  fbgn-annotation-ID (:: gene-symbol organism-abbreviation primary-FBgn (ListValue secondary-FBgn) annotation-ID (ListValue secondary-annotation-ID))) True)
 (=  (column-names  fbgn-exons2affy1-overlaps (:: FBgn (ListValue affy))) True)
 (=  (column-names  fbgn-exons2affy2-overlaps (:: FBgn (ListValue affy))) True)
 (=  (column-names  fbgn-fbtr-fbpp (:: FBgn FBtr FBpp)) True)
 (=  (column-names  fbgn-fbtr-fbpp-expanded (:: organism gene-type gene-ID gene-symbol gene-fullname annotation-ID transcript-type transcript-ID transcript-symbol polypeptide-ID polypeptide-symbol)) True)
 (=  (column-names  fbgn-gleanr (:: organism-abbreviation gene-symbol primary-FBgn GLEANR)) True)
 (=  (column-names  fbgn-NAseq-Uniprot (:: gene-symbol organism-abbreviation primary-FBgn nucleotide-accession na-based-protein-accession UniprotKB/Swiss-Prot/TrEMBL-accession EntrezGene RefSeq-transcripts RefSeq-proteins)) True)
 (=  (column-names  fbgn-uniprot (:: primary-FBgn gene-symbol CG UniprotKB/Swiss-Prot/TrEMBL-accession)) True)
 (=  (column-names  fbrf-pmid-pmcid-doi (:: FBrf PMID PMCID DOI pub-type miniref pmid-added)) True)
 (=  (column-names  gene-functional-complementation (:: Dmel-gene-symbol Dmel-gene-FBgn ortholog-gene-symbol ortholog-gene-FBgn-ID reference-FBrf)) True)
 (=  (column-names  gene-genetic-interactions (:: (ListValue Starting-gene-symbol) (ListValue Starting-gene-FBgn) (ListValue Interacting-gene-symbol) (ListValue Interacting-gene-FBgn) Interaction-type Publication-FBrf)) True)
 (=  (column-names  gene-group (:: FB-group FB-group-symbol FB-group-name Parent-FB-group Parent-FB-group-symbol Group-member-FB-gene Group-member-FB-gene-symbol)) True)
 (=  (column-names  gene-groups-HGNC (:: FB-group FB-group-symbol FB-group-name HGNC-family)) True)
 (=  (column-names  gene-map-table (:: organism-abbreviation current-symbol primary-FBid recombination-loc cytogenetic-loc sequence-loc)) True)
 (=  (column-names  gene-rpkm-matrix (:: gene-primary-id gene-symbol gene-fullname gene-type DATASAMPLE-NAME-(DATASET-ID))) True)
 (=  (column-names  gene-rpkm-report (:: Release-ID FBgn GeneSymbol Parent-library-FBlc Parent-library-name RNASource-FBlc RNASource-name RPKM-value Bin-value Unique-exon-base-count Total-exon-base-count Count-used)) True)
 (=  (column-names  gene-snapshots (:: FBgn GeneSymbol GeneName datestamp gene-snapshot-text)) True)
 (=  (column-names  genomic-clone (:: FBcl organism-abbreviation clone-name accession)) True)
 (=  (column-names  insertion-mapping (:: insertion-symbol FBti genomic-location range orientation estimated-cytogenetic-location observed-cytogenetic-location)) True)
 (=  (column-names  organism-list (:: genus species abbreviation common-name NCBI-taxon drosophilid?)) True)
 (=  (column-names  pathway-group (:: FB-group FB-group-symbol FB-group-name Parent-FB-group Parent-FB-group-symbol Group-member-FB-gene Group-member-FB-gene-symbol)) True)
 (=  (column-names  physical-interactions-mitab (:: (ListValue ID-Interactor-A) (ListValue ID-Interactor-B) (ListValue Alt-ID-Interactor-A) (ListValue Alt-ID-Interactor-B) (ListValue Alias-Interactor-A) (ListValue Alias-Interactor-B) (ListValue Interaction-Detection-Method) (ListValue Publication-1st-Author) (ListValue Publication) Taxid-Interactor-A Taxid-Interactor-B (ListValue Interaction-Type) (ListValue Source-Database) (ListValue Interaction-Identifier) (ListValue Confidence-Value) (ListValue Expansion-Method) (ListValue Biological-Role-Interactor-A) (ListValue Biological-Role-Interactor-B) (ListValue Experimental-Role-Interactor-A) (ListValue Experimental-Role-Interactor-B) (ListValue Type-Interactor-A) (ListValue Type-Interactor-B) (ListValue Xref-Interactor-A) (ListValue Xref-Interactor-B) (ListValue Interaction-Xref) (ListValue Annotation-Interactor-A) (ListValue Annotation-Interactor-B) (ListValue Interaction-Annotation) (ListValue Host-Organism) Interaction-Parameters Creation-Date Update-Date Checksum-Interactor-A Checksum-Interactor-B Interaction-Checksum Negative (ListValue Feature-Interactor-A) (ListValue Feature-Interactor-B) Stoichiometry-Interactor-A Stoichiometry-Interactor-B (ListValue Identification-Method-Participant-A) (ListValue Identification-Method-Participant-B))) True)
 (=  (column-names  pmid-fbgn-uniprot (:: FBrf PMID FBgn UniProt-database UniProt-id)) True)
 (=  (column-names  synonym (:: primary-FBid organism-abbreviation current-symbol current-fullname (ListValue fullname-synonym) (ListValue symbol-synonym))) True)


 (=  (column-description  allele-FBal (StringValue "Current FlyBase identifier (FBal) of allele.") identifier Allele Identifier) True)
 (=  (column-description  allele-symbol (StringValue "Current FlyBase allele symbol.") symbol Allele Symbol) True)
 (=  (column-description  Bin-value (StringValue "The expression bin classification of this gene in this RNA-Seq experiment, based on RPKM value.") numeric Expression Bin) True)
 (=  (column-description  Cluster-Cell-Type-ID (StringValue "The FlyBase FBbt ID for the cell type represented by the cell cluster.") identifier Cell Type) True)
 (=  (column-description  Cluster-Cell-Type-Name (StringValue "The FlyBase name for the cell type represented by the cell cluster.") name (StringValue "Cell Type Name")) True)
 (=  (column-description  Cluster-ID (StringValue "The FlyBase FBlc ID for the dataset representing the cell cluster.") identifier Cell Cluster) True)
 (=  (column-description  Cluster-Name (StringValue "The FlyBase name for the dataset representing the cell cluster.") name (StringValue "Cell Cluster Name")) True)
 (=  (column-description  Clustering-Analysis-ID (StringValue "The FlyBase FBlc ID for the dataset representing the clustering analysis.") identifier Dataset) True)
 (=  (column-description  Clustering-Analysis-Name (StringValue "The FlyBase name for the dataset representing the clustering analysis.") name Dataset Name) True)
 (=  (column-description  Count-used (StringValue "Indicates if the RPKM expression value was calculated using only the exonic regions unique to the gene and not overlapping exons of other genes (Unique), or, if the RPKM expression value was calculated based on all exons of the gene regardless of overlap with other genes (Total).") category Count Type) True)
 (=  (column-description  current-fullname (StringValue "Current full name used in FlyBase for the object.") name Name) True)
 (=  (column-description  current-symbol (StringValue "Current symbol used in FlyBase for the object.") symbol Symbol) True)
 (=  (column-description  DATASAMPLE-NAME-(DATASET-ID) (StringValue "Each subsequent column reports the gene RPKM values for the sample listed in the header.") matrix Expression Matrix) True)
 (=  (column-description  FBgn (StringValue "The unique FlyBase gene ID for this gene.") identifier Gene) True)
 (=  (column-description  FBgn-id (StringValue "Unique FlyBase gene ID.") identifier Gene) True)
 (=  (column-description  FBrf (StringValue "Current FlyBase identifer (FBrf) of publication from which data came.") identifier Publication Identifier) True)
 (=  (column-description  FBrf-id (StringValue "FlyBase reference ID for the publication.") identifier Reference) True)
 (=  (column-description  gene-fullname (StringValue "The official full name for this gene.") name Gene Name) True)
 (=  (column-description  Gene-ID (StringValue "The FlyBase FBgn ID for the expressed gene.") identifier Gene) True)
 (=  (column-description  gene-primary-id (StringValue "The unique FlyBase gene ID for this gene.") identifier Gene) True)
 (=  (column-description  Gene-Symbol (StringValue "The FlyBase symbol for the expressed gene.") symbol Gene Symbol) True)
 (=  (column-description  gene-symbol (StringValue "The official FlyBase symbol for this gene.") symbol Gene Symbol) True)
 (=  (column-description  gene-type (StringValue "The type of gene.") category Gene Type) True)
 (=  (column-description  GeneSymbol (StringValue "The official FlyBase symbol for this gene.") symbol Gene Symbol) True)
 (=  (column-description  interaction (StringValue "Interaction information associated with allele.") text Interaction Info) True)
 (=  (column-description  Interaction-type (StringValue "Type of interaction observed, either 'suppressible' or 'enhanceable'.") category Interaction Type) True)
 (=  (column-description  Mean-Expression (StringValue "The average level of expression of the gene across all cells of the cluster.") numeric Expression Level) True)
 (=  (column-description  organism-abbreviation (StringValue "Abbreviation indicating the species of origin.") abbreviation Organism) True)
 (=  (column-description  Parent-library-FBlc (StringValue "The unique FlyBase ID for the dataset project to which the RNA-Seq experiment belongs.") identifier Dataset Project) True)
 (=  (column-description  Parent-library-name (StringValue "The official FlyBase symbol for the dataset project to which the RNA-Seq experiment belongs.") name (StringValue "Dataset Project Name")) True)
 (=  (column-description  phenotype-id (StringValue "Phenotypic identifier associated with the genotype.") identifier Phenotype Identifier) True)
 (=  (column-description  phenotype-name (StringValue "Phenotypic name associated with the genotype.") name Phenotype Name) True)
 (=  (column-description  PMID (StringValue "PubMed ID for the publication.") identifier Publication) True)
 (=  (column-description  primary-FBid (StringValue "Primary FlyBase identifier for the object.") identifier Object) True)
 (=  (column-description  Pub-ID (StringValue "The FlyBase FBrf ID for the reference in which the expression was reported.") identifier Publication) True)
 (=  (column-description  Pub-miniref (StringValue "The FlyBase citation for the publication in which the expression was reported.") citation Publication Citation) True)
 (=  (column-description  Publication-FBrf (StringValue "Current FlyBase identifier (FBrf) of publication from which the data came.") identifier Publication Reference) True)
 (=  (column-description  reference (StringValue "Current FlyBase identifer (FBrf) of publication from which data came.") identifier Publication Identifier) True)
 (=  (column-description  Release-ID (StringValue "The D. melanogaster annotation set version from which the gene model used in the analysis derives.") version Annotation Version) True)
 (=  (column-description  RNASource-FBlc (StringValue "The unique FlyBase ID for the RNA-Seq experiment used for RPKM expression calculation.") identifier RNA-Seq Experiment) True)
 (=  (column-description  RNASource-name (StringValue "The official FlyBase symbol for the RNA-Seq experiment used for RPKM expression calculation.") name (StringValue "RNA-Seq Experiment Name")) True)
 (=  (column-description  RPKM-value (StringValue "The RPKM expression value for the gene in the specified RNA-Seq experiment.") numeric Expression Value) True)
 (=  (column-description  Source-Tissue-Anatomy (StringValue "The anatomical region of the source tissue used for the experiment.") category Tissue Anatomy) True)
 (=  (column-description  Source-Tissue-Sex (StringValue "The sex of the source tissue used for the experiment.") category Tissue Sex) True)
 (=  (column-description  Source-Tissue-Stage (StringValue "The life stage of the source tissue used for the experiment.") category Tissue Stage) True)
 (=  (column-description  Spread (StringValue "The proportion of cells in the cluster in which the gene is detected.") proportion Expression Spread) True)
 (=  (column-description  Total-exon-base-count (StringValue "The number of bases in all exons of this gene.") numeric (StringValue "Total Exonic Base Count")) True)
 (=  (column-description  UniProt-database (StringValue "Database in UniProt where the protein is listed (either UniProt/TrEMBL or UniProt/Swiss-Prot).") category Protein Database) True)
 (=  (column-description  UniProt-id (StringValue "Unique identifier for the protein in UniProt.") identifier Protein) True)
 (=  (column-description  Unique-exon-base-count (StringValue "The number of exonic bases unique to the gene (not overlapping exons of other genes).") numeric (StringValue "Exonic Base Count")) True)
 (=  (column-description  (ListValue fullname-synonym) (StringValue "Non-current full name(s) associated with the object.") list Name Synonyms) True)
 (=  (column-description  (ListValue genotype-FBids) (StringValue "Current FlyBase identifier(s) of the components that make up the genotype.") list Genotype Identifiers) True)
 (=  (column-description  (ListValue genotype-symbols) (StringValue "Current FlyBase symbol(s) of the components that make up the genotype.") list Genotype Symbols) True)
 (=  (column-description  (ListValue Interacting-gene-FBgn) (StringValue "Current FlyBase identifier (FBgn) of gene(s) involved in the interacting genotype.") list Gene Identifier) True)
 (=  (column-description  (ListValue Interacting-gene-symbol) (StringValue "Current FlyBase symbol of gene(s) involved in the interacting genotype.") list Gene Symbol) True)
 (=  (column-description  (ListValue qualifier-ids) (StringValue "Qualifier identifier(s) associated with phenotypic data for genotype.") list Qualifier Identifiers) True)
 (=  (column-description  (ListValue qualifier-names) (StringValue "Qualifier name(s) associated with phenotypic data for genotype.") list Qualifier Names) True)
 (=  (column-description  (ListValue Starting-gene-FBgn) (StringValue "Current FlyBase identifier (FBgn) of gene(s) involved in the starting genotype.") list Gene Identifier) True)
 (=  (column-description  (ListValue Starting-gene-symbol) (StringValue "Current FlyBase symbol of gene(s) involved in the starting genotype.") list Gene Symbol) True)
 (=  (column-description  (ListValue symbol-synonym) (StringValue "Non-current symbol(s) associated with the object.") list Symbol Synonyms) True)


 (=  (reprefix  (:: GO- GO-- BiologicalProcess:GO:) GO:) True)
 (=  (reprefix  (:: flybase: FLYBASE: comment:) ()) True)
 (=  (reprefix  (:: FBpp:) FBpp) True)
 (=  (reprefix  (:: FBgn:) FBgn) True)
 (=  (reprefix  (:: FB:FB) FB) True)


 (=  (fix-concept  $O $P) (sequential  (is-list  $O) (--aux-maplist/3-fix-concept+0  $O $P) no-more))
 (=  (fix-concept  $O $O) (sequential  (missing  (atom  $O)) (missing  (string  $O)) no-more))
 (=  (fix-concept  $O $P) (sequential  (number  $O) no-more (=  $O $P)))
 (=  (fix-concept  $O $P) (sequential  (reprefix  $Q $R) (member  $S $Q) (atom-concat  $S $T $O) (atom-concat  $R $T $P)))
 (=  (fix-concept  $O $P) (sequential  (atom-concat  FB $A10 $O) (atomic-list-concat  (:: $Q $R) : $O) no-more (atom-concat  $Q $R $P)))
 (=  (fix-concept  $O $P) (sequential  (atom  $O) no-more (if-then-else  (atom-number  $O $P) True (=  $P $O))))
 (=  (fix-concept  $O $P) (sequential  (number-string  $P $O) no-more))
 (=  (fix-concept  $O $O) (:: no-more))


 (=  (--aux-maplist/3-fix-concept+0  () ()) True)
 (=  (--aux-maplist/3-fix-concept+0  ([|]  $O $P) ([|]  $Q $R)) (sequential  (fix-concept  $O $Q) (--aux-maplist/3-fix-concept+0  $P $R)))


 (=  (as-list  $A10 $O $P) (sequential  (as-list  $O $P) no-more))
 (=  (as-list  $O $P $Q) (sequential  (member  $R $O) (catch-ignore  (atomic-list-concat  $S $R $P)) (\=  $S (:: $A10)) no-more (--aux-maplist/3-fix-concept+0  $S $Q)))
 (=  (as-list  $A10 $O $P) (sequential  (member  $Q (:: | and or)) (catch-ignore  (atomic-list-concat  $R $Q $O)) (\=  $R (:: $B10)) no-more (--aux-maplist/3-fix-concept+0  $R $P)))
 (=  (as-list  $A10 $O (:: $P)) (:: (fix-concept  $O $P)))


 (=  (as-list  $O $P) (sequential  (is-list  $O) no-more (=  $P $O)))
 (=  (as-list  $O $P) (sequential  (var  $O) no-more (=  $P ())))
 (=  (as-list  - ()) True)
 (=  (as-list  (StringValue "-") ()) True)
 (=  (as-list  () ()) True)
 (=  (as-list  $O (:: $O)) (sequential  (number  $O) no-more))
 (=  (as-list  (StringValue "") ()) True)
 (=  (as-list    ()) True)
 (=  (as-list  (StringValue " ") ()) True)


 (=  (assert-new1  $O) (sequential  (missing  (missing  (call  $O))) no-more))
 (=  (assert-new1  $O) (:: (assert  $O)))


 (=  (retract1  $O) (sequential  (missing  (call  $O)) no-more))
 (=  (retract1  $O) (:: (if-then-else  (missing  (retract  $O)) True True)))


 (=  (assert-new  $O) (sequential  (call  $O) no-more (assert-new1  (repeats  $O))))
 (=  (assert-new  $O) (sequential  (assert  $O) (flag  assert-new $P (+  $P (ValueAtom 1))) (assert-new1  (not-repeats  $O)) no-more))








 (=  save-value-atom-cols (sequential  (missing  (and  (is-valueatom  $O $P $Q) (missing  (assert-new  (numeric-value-p-n  $O $P $Q))))) (listing  (/  numeric-value-p-n (ValueAtom 3)))))


 (=  (fis-valueatom  $O $P) (sequential  (findall  (-  $Q $R) (is-valueatom  $Q $R $A10) $O) (length  $O $P)))


 (=  (numeric-value-p-n  dmel-human-orthologs-disease (ValueAtom 6) DIOPT-score) True)
 (=  (numeric-value-p-n  dmel-human-orthologs-disease (ValueAtom 7) OMIM-Phenotype-IDs) True)
 (=  (numeric-value-p-n  fbrf-pmid-pmcid-doi (ValueAtom 2) PMID) True)
 (=  (numeric-value-p-n  pmid-fbgn-uniprot (ValueAtom 2) gene-symbol) True)
 (=  (numeric-value-p-n  fbgn-NAseq-Uniprot (ValueAtom 7) EntrezGene) True)
 (=  (numeric-value-p-n  gene-groups-HGNC (ValueAtom 4) Parent-FB-group) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 5) BCM-1-E2-4hr-(FBlc0000061)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 6) BCM-1-E14-16hr-(FBlc0000062)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 7) BCM-1-E2-16hr-(FBlc0000063)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 8) BCM-1-E2-16hr100-(FBlc0000064)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 9) BCM-1-L3i-(FBlc0000065)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 10) BCM-1-L3i100-(FBlc0000066)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 11) BCM-1-P3d-(FBlc0000067)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 12) BCM-1-FA3d-(FBlc0000068)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 13) BCM-1-MA3d-(FBlc0000069)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 14) BCM-1-P-(FBlc0000070)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 15) BCM-1-L-(FBlc0000071)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 16) BCM-1-A17d-(FBlc0000072)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 17) mE-mRNA-em0-2hr-(FBlc0000086)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 18) mE-mRNA-em2-4hr-(FBlc0000087)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 19) mE-mRNA-em4-6hr-(FBlc0000088)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 20) mE-mRNA-em6-8hr-(FBlc0000089)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 21) mE-mRNA-em8-10hr-(FBlc0000090)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 22) mE-mRNA-em10-12hr-(FBlc0000091)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 23) mE-mRNA-em12-14hr-(FBlc0000092)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 24) mE-mRNA-em14-16hr-(FBlc0000093)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 25) mE-mRNA-em16-18hr-(FBlc0000094)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 26) mE-mRNA-em18-20hr-(FBlc0000095)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 27) mE-mRNA-em20-22hr-(FBlc0000096)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 28) mE-mRNA-em22-24hr-(FBlc0000097)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 29) mE-mRNA-L1-(FBlc0000098)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 30) mE-mRNA-L2-(FBlc0000099)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 31) mE-mRNA-L3-12hr-(FBlc0000100)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 32) mE-mRNA-L3-PS1-2-(FBlc0000101)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 33) mE-mRNA-L3-PS3-6-(FBlc0000102)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 34) mE-mRNA-L3-PS7-9-(FBlc0000103)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 35) mE-mRNA-WPP-(FBlc0000104)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 36) mE-mRNA-P5-(FBlc0000105)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 37) mE-mRNA-P6-(FBlc0000106)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 38) mE-mRNA-P8-(FBlc0000107)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 39) mE-mRNA-P9-10-(FBlc0000108)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 40) mE-mRNA-P15-(FBlc0000109)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 41) mE-mRNA-AdF-Ecl-1days-(FBlc0000110)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 42) mE-mRNA-AdF-Ecl-5days-(FBlc0000111)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 43) mE-mRNA-AdF-Ecl-30days-(FBlc0000112)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 44) mE-mRNA-AdM-Ecl-1days-(FBlc0000113)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 45) mE-mRNA-AdM-Ecl-5days-(FBlc0000114)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 46) mE-mRNA-AdM-Ecl-30days-(FBlc0000115)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 47) mE-mRNA-A-MateF-1d-head-(FBlc0000207)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 48) mE-mRNA-A-MateF-4d-ovary-(FBlc0000208)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 49) mE-mRNA-A-MateM-1d-head-(FBlc0000209)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 50) mE-mRNA-A-VirF-1d-head-(FBlc0000210)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 51) mE-mRNA-A-VirF-4d-head-(FBlc0000211)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 52) mE-mRNA-A-MateF-20d-head-(FBlc0000212)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 53) mE-mRNA-A-MateF-4d-head-(FBlc0000213)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 54) mE-mRNA-A-MateM-20d-head-(FBlc0000214)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 55) mE-mRNA-A-MateM-4d-acc-gland-(FBlc0000215)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 56) mE-mRNA-A-MateM-4d-head-(FBlc0000216)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 57) mE-mRNA-A-MateM-4d-testis-(FBlc0000217)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 58) mE-mRNA-A-1d-carcass-(FBlc0000218)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 59) mE-mRNA-A-1d-dig-sys-(FBlc0000219)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 60) mE-mRNA-A-20d-carcass-(FBlc0000220)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 61) mE-mRNA-A-20d-dig-sys-(FBlc0000221)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 62) mE-mRNA-A-4d-carcass-(FBlc0000222)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 63) mE-mRNA-A-4d-dig-sys-(FBlc0000223)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 64) mE-mRNA-P8-CNS-(FBlc0000224)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 65) mE-mRNA-L3-CNS-(FBlc0000225)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 66) mE-mRNA-L3-Wand-carcass-(FBlc0000226)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 67) mE-mRNA-L3-Wand-dig-sys-(FBlc0000227)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 68) mE-mRNA-L3-Wand-fat-(FBlc0000228)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 69) mE-mRNA-L3-Wand-imag-disc-(FBlc0000229)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 70) mE-mRNA-L3-Wand-saliv-(FBlc0000230)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 71) mE-mRNA-A-VirF-20d-head-(FBlc0000231)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 72) mE-mRNA-A-VirF-4d-ovary-(FBlc0000232)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 73) mE-mRNA-WPP-fat-(FBlc0000233)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 74) mE-mRNA-WPP-saliv-(FBlc0000234)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 75) mE-mRNA-P8-fat-(FBlc0000235)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 76) mE-mRNA-A-4d-Cold1-(FBlc0000237)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 77) mE-mRNA-A-4d-Cold2-(FBlc0000238)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 78) mE-mRNA-L3-Cu-0.5mM-(FBlc0000239)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 79) mE-mRNA-L3-late-Zn-5mM-(FBlc0000240)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 80) mE-mRNA-A-4d-Cu-15mM-(FBlc0000241)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 81) mE-mRNA-A-4d-Zn-4.5mM-(FBlc0000242)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 82) mE-mRNA-A-4d-Caffeine-25mg/ml-(FBlc0000243)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 83) mE-mRNA-A-4d-Caffeine-2.5mg/ml-(FBlc0000244)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 84) mE-mRNA-L3-Caffeine-1.5mg/ml-(FBlc0000245)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 85) mE-mRNA-A-4d-Cd-0.1M-(FBlc0000246)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 86) mE-mRNA-A-4d-Cd-0.05M-(FBlc0000247)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 87) mE-mRNA-L3-Cd-12h-(FBlc0000248)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 88) mE-mRNA-L3-Cd-6hr-(FBlc0000249)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 89) mE-mRNA-A-4d-Paraquat-5mM-(FBlc0000250)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 90) mE-mRNA-A-4d-Paraquat-10mM-(FBlc0000251)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 91) mE-mRNA-L3-Rotenone-8ug-(FBlc0000252)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 92) mE-mRNA-L3-Rotenone-2ug-(FBlc0000253)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 93) mE-mRNA-L3-EtOH-10-(FBlc0000254)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 94) mE-mRNA-L3-EtOH-5-(FBlc0000255)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 95) mE-mRNA-L3-EtOH-2.5-(FBlc0000256)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 96) mE-mRNA-A-4d-Heatshock-(FBlc0000257)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 97) mE-mRNA-A-10d-Resveratrol-100uM-(FBlc0000672)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 98) mE-mRNA-A-10d-Rotenone-Starved-(FBlc0000673)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 99) mE-mRNA-F-Sindbis-virus-(FBlc0000674)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 100) mE-mRNA-L-Sindbis-virus-(FBlc0000675)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 101) mE-mRNA-M-Sindbis-virus-(FBlc0000676)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 102) mE-mRNA-P-Sindbis-virus-(FBlc0000677)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 103) mE-mRNA-CME-W2-cells-(FBlc0000261)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 104) mE-mRNA-GM2-cells-(FBlc0000262)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 105) mE-mRNA-mbn2-cells-(FBlc0000263)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 106) mE-mRNA-BG2-c2-cells-(FBlc0000264)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 107) mE-mRNA-D20-c5-cells-(FBlc0000265)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 108) mE-mRNA-S3-cells-(FBlc0000266)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 109) mE-mRNA-1182-4H-cells-(FBlc0000267)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 110) mE-mRNA-CME-L1-cells-(FBlc0000268)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 111) mE-mRNA-Kc167-cells-(FBlc0000269)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 112) mE-mRNA-BG1-c1-cells-(FBlc0000270)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 113) mE-mRNA-D11-cells-(FBlc0000271)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 114) mE-mRNA-D16-c3-cells-(FBlc0000272)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 115) mE-mRNA-D17-c3-cells-(FBlc0000273)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 116) mE-mRNA-D21-cells-(FBlc0000274)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 117) mE-mRNA-D32-cells-(FBlc0000275)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 118) mE-mRNA-D4-c1-cells-(FBlc0000276)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 119) mE-mRNA-D8-cells-(FBlc0000277)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 120) mE-mRNA-D9-cells-(FBlc0000278)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 121) mE-mRNA-S1-cells-(FBlc0000279)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 122) mE-mRNA-S2R+-cells-(FBlc0000280)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 123) mE-mRNA-Sg4-cells-(FBlc0000281)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 124) mE-mRNA-OSS-cells-(FBlc0000282)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 125) mE-mRNA-OSC-cells-(FBlc0000283)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 126) mE-mRNA-fGS-cells-(FBlc0000284)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 127) Knoblich-mRNA-L3-CNS-neuroblast-(FBlc0000505)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 128) Knoblich-mRNA-L3-CNS-neuron-(FBlc0000506)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 129) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Brain-(FBlc0003619)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 130) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Crop-(FBlc0003620)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 131) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Carcass-(FBlc0003621)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 132) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Eye-(FBlc0003622)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 133) RNA-Seq-Profile-FlyAtlas2-Adult-Female-FatBody-(FBlc0003623)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 134) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Head-(FBlc0003624)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 135) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Hindgut-(FBlc0003625)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 136) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Midgut-(FBlc0003626)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 137) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Ovary-(FBlc0003627)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 138) RNA-Seq-Profile-FlyAtlas2-Adult-Female-RectalPad-(FBlc0003628)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 139) RNA-Seq-Profile-FlyAtlas2-Adult-Female-SalivaryGland-(FBlc0003629)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 140) RNA-Seq-Profile-FlyAtlas2-Adult-Female-ThoracicoAbdominalGanglion-(FBlc0003630)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 141) RNA-Seq-Profile-FlyAtlas2-Adult-Female-MalpighianTubule-(FBlc0003631)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 142) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Mated-Spermathecum-(FBlc0003632)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 143) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Virgin-Spermathecum-(FBlc0003633)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 144) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Whole-(FBlc0003634)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 145) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Brain-(FBlc0003635)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 146) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Crop-(FBlc0003636)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 147) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Carcass-(FBlc0003637)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 148) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Eye-(FBlc0003638)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 149) RNA-Seq-Profile-FlyAtlas2-Adult-Male-FatBody-(FBlc0003639)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 150) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Head-(FBlc0003640)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 151) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Hindgut-(FBlc0003641)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 152) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Midgut-(FBlc0003642)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 153) RNA-Seq-Profile-FlyAtlas2-Adult-Male-RectalPad-(FBlc0003643)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 154) RNA-Seq-Profile-FlyAtlas2-Adult-Male-SalivaryGland-(FBlc0003644)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 155) RNA-Seq-Profile-FlyAtlas2-Adult-Male-ThoracicoAbdominalGanglion-(FBlc0003645)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 156) RNA-Seq-Profile-FlyAtlas2-Adult-Male-MalpighianTubule-(FBlc0003646)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 157) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Testis-(FBlc0003647)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 158) RNA-Seq-Profile-FlyAtlas2-Adult-Male-AccessoryGland-(FBlc0003648)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 159) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Whole-(FBlc0003649)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 160) RNA-Seq-Profile-FlyAtlas2-L3-CNS-(FBlc0003650)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 161) RNA-Seq-Profile-FlyAtlas2-L3-FatBody-(FBlc0003651)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 162) RNA-Seq-Profile-FlyAtlas2-L3-Hindgut-(FBlc0003652)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 163) RNA-Seq-Profile-FlyAtlas2-L3-MalpighianTubule-(FBlc0003653)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 164) RNA-Seq-Profile-FlyAtlas2-L3-Midgut-(FBlc0003654)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 165) RNA-Seq-Profile-FlyAtlas2-L3-SalivaryGland-(FBlc0003655)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 166) RNA-Seq-Profile-FlyAtlas2-L3-Trachea-(FBlc0003656)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 167) RNA-Seq-Profile-FlyAtlas2-L3-Carcass-(FBlc0003657)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 168) RNA-Seq-Profile-FlyAtlas2-L3-Whole-(FBlc0003658)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 169) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Heart-(FBlc0003724)) True)
 (=  (numeric-value-p-n  gene-rpkm-matrix (ValueAtom 170) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Heart-(FBlc0003725)) True)
 (=  (numeric-value-p-n  gene-rpkm-report (ValueAtom 8) RPKM-value) True)
 (=  (numeric-value-p-n  gene-rpkm-report (ValueAtom 9) Bin-value) True)
 (=  (numeric-value-p-n  gene-rpkm-report (ValueAtom 10) Unique-exon-base-count) True)
 (=  (numeric-value-p-n  gene-rpkm-report (ValueAtom 11) Total-exon-base-count) True)
 (=  (numeric-value-p-n  gene-snapshots (ValueAtom 4) datestamp) True)
 (=  (numeric-value-p-n  insertion-mapping (ValueAtom 5) orientation) True)
 (=  (numeric-value-p-n  dmel-paralogs (ValueAtom 5) Strand) True)
 (=  (numeric-value-p-n  dmel-paralogs (ValueAtom 10) Paralog-Strand) True)
 (=  (numeric-value-p-n  dmel-paralogs (ValueAtom 11) DIOPT-score) True)
 (=  (numeric-value-p-n  entity-publication (ValueAtom 4) PubMed) True)
 (=  (numeric-value-p-n  organism-list (ValueAtom 5) NCBI-taxon) True)


 (=  (is-valueatom  $O $P $Q) (sequential  (arg-table-n-type  $R $O $P $Q) (atom-number  $R $A10)))


 (=  (arg-table-n-type  $O $P $Q $R) (sequential  (table-n-type  $P $Q $R) (->  (and  (fb-pred  $P $S) (and  (functor  $T $P $S) (and  (arg  $Q $T $O) (and  (call  $T) (and  (missing  (is-list  $O)) (missing  (as-list  $O ()))))))) True)))


 (=  (is-concept-type  $O) (:: (fb-arg-type  $O)))


 (=  (is-concept  $O) (:: (fb-arg  $O)))


 (=  (add-table-n-types  $A10 $B10 $O) (sequential  (missing  (is-list  $O)) no-more))
 (=  (add-table-n-types  $O (ValueAtom 1) ([|]  $P $Q)) (sequential  (number  $P) no-more (add-table-n-types  $O (ValueAtom 1) $Q)))
 (=  (add-table-n-types  $O $P ([|]  $Q $R)) (sequential  no-more (sub-term  $S $Q) (atom  $S) no-more (assert-new  (fb-arg-type  $S)) (assert-new  (table-n-type  $O $P $S)) (is  $T (+  $P (ValueAtom 1))) (add-table-n-types  $O $T $R) no-more))
 (=  (add-table-n-types  $A10 $B10 ()) True)








 (=  (--aux-maplist/2-assert-type-of+4  () $A10 $B10 $C10 $D10) True)
 (=  (--aux-maplist/2-assert-type-of+4  ([|]  $O $P) $Q $R $S $T) (sequential  (assert-type-of  $Q $R $S $T $O) (--aux-maplist/2-assert-type-of+4  $P $Q $R $S $T)))


 (=  (assert-type-of  $A10 $B10 $C10 $D10 $E10) (sequential  (missing  should-sample) no-more))
 (=  (assert-type-of  $O $P $Q $R $S) (sequential  (is-list  $S) no-more (--aux-maplist/2-assert-type-of+4  $S $O $P $Q $R)))
 (=  (assert-type-of  $A10 $O $P $B10 $Q) (:: (must-det-ll-r  (and  (assert-new  (fb-arg  $Q)) (assert-new  (fb-arg-table-n  $Q $O $P))))))








 (=  (adjust-type  $O $P $Q (ListValue $R) $S $T) (sequential  (must-det-ll  (nonvar  $R)) (must-det-ll  (as-list  () $S $U)) (must-det-ll  (is-list  $U)) (md-maplist  must-det-ll (adjust-type  $O $P $Q $R) $U $T)))
 (=  (adjust-type  $O $P $Q (ListValue $R) $T $U) (sequential  (must-det-ll  (nonvar  $R)) (must-det-ll  (as-list  $S $T $V)) (must-det-ll  (is-list  $V)) (md-maplist  must-det-ll (adjust-type  $O $P $Q $R) $V $U)))
 (=  (adjust-type  $A10 $O $P $B10 $Q $R) (sequential  (number  $Q) (numeric-value-p-n  $O $P $C10) no-more (=  $R $Q)))
 (=  (adjust-type  $O $P $Q $R $S $T) (sequential  (numeric-value-p-n  $P $Q $A10) no-more (if-then-else  (must-not-error  (atom-number  $S $T)) True (must-det-ll  (=  $S $T))) (must-det-ll  (assert-type-of  $O $P $Q $R $T))))
 (=  (adjust-type  $O $P $Q $R $S $T) (sequential  (must-det-ll  (fix-concept  $S $T)) (must-det-ll  (assert-type-of  $O $P $Q $R $T))))
 (=  (adjust-type  $A10 $B10 $C10 $D10 $O $O) True)


 (=  (fix-elist-args  $O $P $Q ([|]  $R $S) $T $U) (sequential  (number  $R) no-more (fix-elist-args  $O $P $Q $S $T $U)))
 (=  (fix-elist-args  $O $P $Q ([|]  $R $S) ([|]  $T $U) ([|]  $V $W)) (sequential  no-more (must-det-ll  (adjust-type  $O $P $Q $R $T $V)) (must-det-ll  (is  $X (+  $Q (ValueAtom 1)))) (must-det-ll  (fix-elist-args  $O $P $X $S $U $W))))
 (=  (fix-elist-args  $A10 $B10 $C10 $D10 $O $O) True)


 (=  (primary-term  $A10 ([|]  $O $P) $B10 $C10 $P) (sequential  (number  $O) no-more))
 (=  (primary-term  $A10 ([|]  $O $P) $Q $R $P) (sequential  (number  $O) no-more (nth1  $O $Q $R)))
 (=  (primary-term  $A10 $O $B10 $C10 $O) (:: no-more))
 (=  (primary-term  $A10 $O $P $Q $R) (sequential  (append  $S ([|]  (primary  $T) $U) $O) (append  $S ([|]  $T $U) $R) (length  $S $V) (nth0  $V $P $Q)))
 (=  (primary-term  $O $P $Q $R $P) (sequential  (primary-column  $O $S) (nth1  $T $P $S) no-more (nth1  $T $Q $R) no-more))
 (=  (primary-term  $A10 $O ([|]  $P $B10) $P $O) (:: no-more))
 (=  (primary-term  $A10 $O $B10 $C10 $O) True)


 (=  (fix-list-args  $A10 $B10 $O $O) (sequential  (option-value  full-canon ()) (missing  should-sample) (missing  should-cache) no-more))
 (=  (fix-list-args  $O $P $Q $R) (:: (must-det-ll-r  (and  (primary-term  $O $P $Q $S $T) (and  (fix-elist-args  $S $O (ValueAtom 1) $T $Q $R) (extreme-debug  (ignore  (and  (\==  $Q $R) (fbug  $R)))))))))
 (=  (fix-list-args  $A10 $B10 $O $O) (:: no-more))


 (=  (make-assertion  $O $P $Q $R) (sequential  (must-det-ll  (option-value  fb-argtypes $S)) (must-det-ll  (into-datum  $O $P $R)) (must-det-ll  (equals-ListMappingFn  $R ([|]  $T $U))) (must-det-ll  (fix-list-args  $O $S $U $V)) (must-det-ll  (equals-ListMappingFn  $Q ([|]  $T $V)))))


 (=  (fb-argtypes  $A10 $O) (sequential  (option-value  fb-argtypes $O) (\==  $O ()) no-more))
 (=  (fb-argtypes  $O $P) (sequential  (table-columns  $O $P) no-more))


 (=  (into-datum  $O ([|]  $P $Q) $R) (:: (if-then-else  (option-value  use-va True) (equals-ListMappingFn  $R (:: $O $P $Q)) (equals-ListMappingFn  $R ([|]  $O ([|]  $P $Q))))))


 (=  (write-flybase-data  $A10 $B10 ()) (:: no-more))
 (=  (write-flybase-data  $A10 $B10 (:: ())) (:: no-more))
 (=  (write-flybase-data  $O $P $Q) (sequential  (make-assertion  $P $Q $R $S) (if-then-else  (and  heartbeat (and  (functor  $R $T $U) (and  (>=  $U (ValueAtom 2)) (and  (<  $U (ValueAtom 700)) (and  (catch  (decl-fb-pred  $T $U) $V (and  (pp-fb  (=  $V $Q)) trace)) (and  (flag  loaded-from-file $W (+  $W (ValueAtom 1))) (and  (if-then-else  (call  $R) True (and  (assert  $R) (flag  total-loaded-atoms $X (+  $X (ValueAtom 1))))) (if-then-else  (and  should-show-data (and  nl (and  nl (and  (if-then-else  (and  (\==  $S $Q) (pp-fb  (=  (oldData  $W) $S))) True True) (pp-fb  (=  (newData  $W) $R)))))) True True)))))))) True True) (catch-ignore  (ignore  (and  should-cache (and  (must-det-ll  (write-canonical  $O $R)) (must-det-ll  (writeln  $O .)))))) no-more))


 (=  (print-formatted-time  $O) (sequential  (format-time  $O $P) (writeln  $P)))


 (=  (format-time  $O $P) (sequential  (is  $Q (floor  $O)) (is  $R (div  $Q (ValueAtom 86400))) (is  $S (-  (mod  $Q (ValueAtom 86400)) (ValueAtom 57600))) (format-time  (atom  $T) %T $S) (format  (atom  $P) ~w:~w (:: $R $T))))


 (=  (format-bytes  $O $P) (sequential  (>=  $O (ValueAtom 1073741824)) (is  $Q (/  $O (ValueAtom 1073741824))) (format  (atom  $P) ~2fG (:: $Q))))
 (=  (format-bytes  $O $P) (sequential  (>=  $O (ValueAtom 104857600)) (<  $O (ValueAtom 1073741824)) no-more (is  $Q (/  $O (ValueAtom 1048576))) (is  $R (floor  $Q)) (format  (atom  $P) ~DM (:: $R))))
 (=  (format-bytes  $O $P) (:: (format  (atom  $P) ~D (:: $O))))


 (=  (format-value  $O) (sequential  (float  $O) no-more (format  (StringValue "~2f") (:: $O)) no-more))
 (=  (format-value  $O) (sequential  (integer  $O) (format-bytes  $O $P) (write  $P)))
 (=  (format-value  $O) (:: (format  (StringValue "~w") (:: $O))))


 (=  (pl-stats  $O ([|]  $P $A10)) (sequential  (nonvar  $P) no-more (pl-stats  $O $P)))
 (=  (pl-stats  $O $P) (sequential  (format  (StringValue "~N;          ~@: ~`.t ~@~100|") (:: (format-value  $O) (format-value  $P))) no-more))


 (=  (pl-stats  $O) (sequential  (statistics  $O $P) (pl-stats  $O $P)))


 (=  (fb-stats  $O $P $Q) (sequential  (functor  $R $O $P) (predicate-property  $R (number-of-clauses  $Q))))


 (=  (fb-stats  $O $P) (sequential  (fb-stats  $O $P $Q) (pl-stats  (/  $O $P) $Q)))


 (=  (fb-stats  $O) (:: (missing  (and  (fb-pred  $O $P) (missing  (fb-stats  $O $P))))))


 (=  fb-stats (sequential  gc-now (writeln





~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) (writeln  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) (full-atom-count  $O) (format  (StringValue "~N~n; Total              Atoms (Atomspace size): ~`.t ~D~108|~n") (:: $O)) (get-time  $P) (nb-setval  last-printed-time $P) (post-statistic  memory $Q) (post-statistic  atom-space $R) (post-statistic  cputime $S) (post-statistic  atoms $T) (flag  assert-new $U $U) (post-statistic  stack $V) (is  $W (+  $Q $V)) (is  $X (-  $Q $R)) (is  $Y (//  $X (+  $O (ValueAtom 1)))) (is  $Z (*  (ValueAtom 60) (floor  (/  $O (+  $S (ValueAtom 1)))))) (is  $A1 (//  $R (+  $T (ValueAtom 1)))) (pl-stats  ConceptNodes $T) (pl-stats  Random samples $U) (skip  (and  (pl-stats  (StringValue "Bytes Per Atom (Average)") $Y) (pl-stats  (StringValue "Bytes Per ConceptNode (Average)") $A1))) (skip  (and  (pl-stats  Relational Memory $X) (pl-stats  ConceptNode Memory $R))) (format-time  $S $B1) (skip  (pl-stats  (StringValue "Atoms per minute") $Z)) (pl-stats  (StringValue "Total Memory Used") $W) (pl-stats  Runtime (days:hh:mm:ss) $B1) nl nl no-more))


 (=  heartbeat (sequential  (get-time  $O) (if-then-else  (nb-current  last-printed-time $A10) True (nb-setval  last-printed-time $O)) (nb-getval  last-printed-time $P) (is  $Q (-  $O $P)) (if-then-else  (>=  $Q (ValueAtom 60)) fb-stats True)))


 (=  (full-atom-count  $O) (sequential  (flag  total-loaded-atoms $O $O) (>  $O (ValueAtom 1)) no-more))
 (=  (full-atom-count  $O) (sequential  (findall  $P (and  (fb-pred  $Q $R) (fb-stats  $Q $R $P)) $S) (sumlist  $S $O)))





 (=  (has-list  $O) (sequential  (is-list  $O) (member  (ListValue $A10) $O)))


 (=  (load-fb-data  $O $A10 $B10 $C10 $P $D10) (sequential  (or  (==  $P end-of-file) (done-reading  $O)) no-more))
 (=  (load-fb-data  $O $P $Q $R is-swipl $S) (sequential  no-more (if-then-else  (and  (option-value  max-per-file $T) (and  (number  $T) (number  $T))) True (=  $T inf)) (fbug  (load-fb-data  $O $T $Q $R)) (if-then-else  (and  (fb-argtypes  $Q $U) (pp-fb  (=  (fb-argtypes  $Q) $U))) True True) repeat (->  (read-csv-stream  $R $P $V) True) (if-then-else  (or  (==  $V end-of-file) reached-file-max) (assert  (done-reading  $O)) (and  (->  (write-flybase-data  $S $Q $V) True) False)) no-more))
 (=  (load-fb-data  $O $P $Q $R is-swipl $S) (sequential  no-more (name  $R (:: $T)) (csv-options  $U (:: (separator  $T))) (if-then-else  (and  (option-value  max-per-file $V) (number  $V)) True (=  $V inf)) (fbug  (load-fb-data  $O $V $Q $R)) (if-then-else  (and  (fb-argtypes  $Q $W) (pp-fb  (=  (fb-argtypes  $Q) $W))) True True) repeat (->  (csv-read-row  $P $X $U) True) (if-then-else  (or  (==  $X end-of-file) reached-file-max) (assert  (done-reading  $O)) (and  (equals-ListMappingFn  $X ([|]  $A10 $Y)) (and  (->  (write-flybase-data  $S $Q $Y) True) False))) no-more))
 (=  (load-fb-data  $O $P $Q $R is-swipl $S) (sequential  (name  $R (:: $T)) (csv-options  $U (:: (strip  True) (convert  True) (separator  $T))) (if-then-else  (and  (option-value  max-per-file $V) (number  $V)) True (=  $V inf)) (->  (csv-read-row  $P $W $U) True) (loaded-from-file  $X) (if-then-else  (or  (==  $W end-of-file) (>  $X $V)) (assert  (done-reading  $O)) (and  (equals-ListMappingFn  $W ([|]  $A10 $Y)) (and  (->  (write-flybase-data  $S $Q $Y) True) (load-fb-data  $O $P $Q $R is-swipl $S)))) no-more))


 (=  (load-flybase-chars  $O $P $Q $R $S $T) (sequential  (option-value  row-1-is-header True) no-more (must-det-ll  (attempt-header-row  $R $S $Q $U $V)) (must-det-ll  (fbug  (t-h-n  $Q $U $V))) (must-det-ll  (fb-assert  (t-h-n  $Q $U $V))) no-more (must-det-ll  (set-option-value  fb-argtypes $V)) no-more (must-det-ll  (length  $V $W)) (must-det-ll  (catch  (decl-fb-pred  $Q $W) $X (and  (pp-fb  (=  $X $V)) trace))) (must-det-ll  (load-fb-data  $O $P $Q $R is-swipl $T))))
 (=  (load-flybase-chars  $O $A10 $B10 $P $Q $C10) (sequential  (or  (missing  (member  $P $Q)) (=  $Q ([|]  # $D10))) (format  (StringValue "~n ; ~s") (:: $Q)) (if-then-else  (and  (loaded-from-file  $R) (and  (>  $R (ValueAtom 100)) (and  no-more (assert  (done-reading  $O))))) True True)))
 (=  (load-flybase-chars  $O $P $Q $R $S $T) (sequential  (member  $R $S) (=  $S ([|]  # $A10)) (format  (StringValue "~n ; Maybe Header: ~s") (:: $S)) (attempt-header-row  $R $S $Q $U $V) (is-really-header-row  $U $V) (fbug  (t-h-n  $Q $U $V)) (fb-assert  (t-h-n  $Q $U $V)) no-more (load-fb-data  $O $P $Q $R is-swipl $T)))
 (=  (load-flybase-chars  $O $P $Q $R $S $T) (sequential  is-swipl (attempt-header-row  $R $S $Q $U $A10) (write-flybase-data  $T $Q $U) no-more (load-fb-data  $O $P $Q $R is-swipl $T)))





 (=  (attempt-header-row  $O $P $Q $R $S) (sequential  (read-csv  $O $P $R) (fix-header-names  $Q $R $S) no-more))


 (=  (read-csv  $O $P $Q) (sequential  (atomic  $P) (is-stream  $P) no-more (read-csv-stream  $O $P $Q)))
 (=  (read-csv  $O $P $Q) (sequential  (missing  (option-value  full-canon ())) no-more (split-string  $P $O (StringValue "
") $Q)))
 (=  (read-csv  $O $P $Q) (sequential  (open-string  $P $R) (read-csv-stream  $O $R $Q)))


 (=  (read-csv-stream  $O $P $Q) (sequential  (read-line-to-string  $P $R) (if-then-else  (==  $R end-of-file) (=  $Q $R) (atomic-list-concat  $Q $O $R))))
 (=  (read-csv-stream  $O $P $Q) (sequential  (missing  (option-value  full-canon ())) no-more (read-line-to-string  $P $R) (if-then-else  (==  $R end-of-file) (=  $Q $R) (split-string  $R $O (StringValue "
") $Q))))
 (=  (read-csv-stream  $O $P $Q) (sequential  (name  $O (:: $R)) (csv-options  $S (:: (separator  $R))) (csv-read-row  $P $T $S) (equals-ListMappingFn  $T ([|]  $A10 $Q)) no-more))


 (=  (is-really-header-row  ([|]  $O $A10) $B10) (sequential  (atom-concat  () $C10 $O) no-more))


 (=  (load-flybase-sv  $O $P $Q $R $S) (sequential  (at-end-of-stream  $Q) no-more (->  (load-fb-data  $P $Q $S $O end-of-file $R) True)))
 (=  (load-flybase-sv  $O $P $Q $R $S) (sequential  (option-value  row-1-is-header True) no-more (must-det-ll  (attempt-header-row  $O $Q $S $T $U)) (must-det-ll  (fbug  (t-h-n  $S $T $U))) (must-det-ll  (fb-assert  (t-h-n  $S $T $U))) no-more (must-det-ll  (length  $U $V)) (must-det-ll  (decl-fb-pred  $S $V)) (must-det-ll  (set-option-value  fb-argtypes $U)) no-more (must-det-ll  (pp-fb  (=  (fb-argtypes  (/  $S $V)) $U))) (must-det-ll  (load-fb-data  $P $Q $S $O is-swipl $R))))
 (=  (load-flybase-sv  $O $P $Q $R $S) (:: (must-det-ll-r  (and  (ignore  (once  (or  (table-columns  $P $T) (table-columns  $S $T)))) (and  (fix-header-names  $S $T $U) (and  (forall  (and  (table-columns  $P $V) (\==  $U $V)) (pp-fb  (odd-table-columns  $P $V))) (and  (forall  (and  (table-columns  $S $V) (\==  $U $V)) (pp-fb  (odd-table-columns  $S $V))) (and  (if-t  (is-list  $U) (set-option-value  fb-argtypes $U)) (and  (time  (and  repeat (and  (read-line-to-chars  $Q $W) (and  (once  (load-flybase-chars  $P $Q $S $O $W $R)) (and  (once  (or  (done-reading  $P) (or  reached-file-max (at-end-of-stream  $Q)))) (and  no-more (once  (load-fb-data  $P $Q $S $O end-of-file $R)))))))) (and  (loaded-from-file  $X) (and  no-more (and  (fb-stats  $S) (pl-stats  $P $X)))))))))))))


 (=  (load-flybase  $O $P $Q $R $S) (:: (must-det-ll-r  (and  (ignore  (swi-only  (format  $R (StringValue ":- ~q.
") (:: (encoding  utf8))))) (and  (atomic-list-concat  (:: data $S) - $T) (and  (data-pred  $T $S) (load-flybase-sv  $O $P $Q $R $S)))))))





 (=  (fb-assert  $O) (sequential  (if-then-else  (=  $O (:-  $P $Q)) (copy-term  $Q $R) (and  (=  $P $O) (=  $R True))) (copy-term  $P $S) (if-then-else  (missing  (and  (clause  $S $R) (variant  $S $P))) (assertz  $O) True)))


 (=  (read-line-to-chars  $O $P) (sequential  (read-line-to-string  $O $Q) (string-chars  $Q $P)))


 (=  is-swipl (:: (missing  is-scryer)))


 (=  (file-to-sep  $O and) (sequential  (file-name-extension  $A10 csv $O) no-more))
 (=  (file-to-sep  $O   ) (sequential  (file-name-extension  $A10 tsv $O) no-more))


 (=  (not-trimmed-path  ([|]  $O $P) $Q) (sequential  (is-trimmed-path  $O) no-more (not-trimmed-path  $P $Q)))
 (=  (not-trimmed-path  ([|]  $O $P) ([|]  $O $Q)) (sequential  no-more (not-trimmed-path  $P $Q)))
 (=  (not-trimmed-path  () ()) True)


 (=  (is-trimmed-path  $O) (sequential  (atom-contains  $O (ValueAtom 0)) no-more))
 (=  (is-trimmed-path  fb) True)
 (=  (is-trimmed-path  public) True)
 (=  (is-trimmed-path  data) True)


 (=  (data-pred  $O $P) (sequential  (data-pred0  $O $P) (\==  $P ()) no-more))
 (=  (data-pred  $O $O) True)


 (=  (data-pred0  $O $P) (sequential  (atom-concat  public. $Q $O) no-more (data-pred0  $Q $P)))
 (=  (data-pred0  $O $P) (sequential  (atomic-list-concat  $Q / $O) (\==  $Q ()) (\=  $Q (:: $A10)) no-more (last  $Q $R) (data-pred0  $R $P)))
 (=  (data-pred0  $O $P) (sequential  (atomic-list-concat  $Q - $O) (->  (not-trimmed-path  $Q $R) True) (\==  $R ()) (\==  $R $Q) (atomic-list-concat  $R - $P) no-more))
 (=  (data-pred0  $O $P) (sequential  (atomic-list-concat  ([|]  $Q ([|]  $A10 $B10)) -fb- $O) no-more (data-pred0  $Q $P)))
 (=  (data-pred0  $O $O) True)


 (=  (load-fb-obo  $O $P $Q $R) (sequential  (fbug  (load-fb-obo  $O $P $Q $R)) (if-then-else  (current-predicate  (/  load-obo (ValueAtom 1))) (load-obo  $P) True)))


 (=  (load-fb-json  $O $P $Q $R) (sequential  (fbug  (load-fb-json  $O $P $Q $R)) (setup-call-cleanup  (open  $P read $S) (:  json (json-read  $S $T ())) (close  $S)) (time  (assert  (saved-fb-json  $O $P $T $R)))))


 (=  (load-flybase  $A10 $O $B10 $C10) (sequential  (load-state  $O $D10) no-more))
 (=  (load-flybase  $O $P $Q $R) (sequential  (file-to-sep  $P $S) no-more (assert  (load-state  $P loading)) (flag  loaded-from-file $A10 (ValueAtom 0)) (fbug  (load-flybase  $O $P $Q $R)) (setup-call-cleanup  (open  $P read $T) (setup-call-cleanup  (open  $Q write $U (:: (encoding  utf8))) (load-flybase  $S $P $T $U $R) (close  $U)) (close  $T)) no-more (retract  (load-state  $P loading)) (assert  (load-state  $P loaded)) fb-stats))
 (=  (load-flybase  $O $P $Q $R) (sequential  (==  $O json) no-more (load-fb-json  $O $P $Q $R)))
 (=  (load-flybase  $O $P $Q $R) (sequential  (==  $O obo) no-more (load-fb-obo  $O $P $Q $R)))
 (=  (load-flybase  $O $P $Q $R) (sequential  (fbug  (load-flybase  $O $P $Q $R)) no-more))





 (=  (load-flybase0  $O $A10) (sequential  (==  $O pl) no-more))
 (=  (load-flybase0  $O $P) (sequential  (file-name-extension  $Q $A10 $P) (atomic-list-concat  (:: $Q pl) . $R) (data-pred  $Q $S) (load-flybase  $O $P $R $S)))


 (=  (load-flybase  $O $P) (sequential  (with-wild-path  (load-flybase0  $P) $O) no-more))


 (=  (load-flybase  $O) (sequential  (if-then-else  (number  $O) True (==  $O inf)) no-more (set-option-value  max-per-file $O) no-more load-flybase))
 (=  (load-flybase  $O) (sequential  (file-name-extension  $A10 $P $O) no-more (load-flybase  $O $P)))


 (=  (load-fb-cache  $A10 $O $B10) (sequential  (exists-file  $O) no-more (ensure-loaded  $O) no-more))
 (=  (load-fb-cache  $O $A10 $B10) (:: (load-files  (:: $O) (:: (qcompile  large)))))


 (=  (load-fb-cache0  $O) (sequential  (file-name-extension  $P $A10 $O) (atomic-list-concat  (:: $Q $R) . $P) (atomic-list-concat  (:: $Q $R qlf) . $S) no-more (load-fb-cache  $O $S $R)))
 (=  (load-fb-cache0  $O) (sequential  (file-name-extension  $P $A10 $O) (atomic-list-concat  (:: $Q) . $P) (atomic-list-concat  (:: $Q qlf) . $R) (load-fb-cache  $O $R $Q)))


 (=  (load-fb-cache  $O) (:: (with-wild-path  load-fb-cache0 $O)))


 (=  (--aux-maplist/2-load-fb-cache+0  ()) True)
 (=  (--aux-maplist/2-load-fb-cache+0  ([|]  $O $P)) (sequential  (load-fb-cache  $O) (--aux-maplist/2-load-fb-cache+0  $P)))


 (=  (load-fb-mask  $O) (sequential  is-scryer (atom  $O) (name  $O $P) no-more (load-fb-mask  $P)))
 (=  (load-fb-mask  $O) (sequential  (expand-file-name  $O $P) (--aux-maplist/2-load-fb-cache+0  $P)))


 (=  (metta-iter-bind  $O $P $A10) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (call  (:  $O (for-metta  $O $P)))))


 (=  (metta-iter-bind  $O $P $Q $R) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (findall  $Q (:  $O (for-metta  $O $P)) $R)))


 (=  (metta-atoms  $O $P) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (findall  $Q (:  $O (for-metta  $O $Q)) $P)))


 (=  (metta-iter  $O $P) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (call  (:  $O (for-metta  $O $P)))))


 (=  (metta-count  $O $P) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (full-atom-count  $Q) (=  $R (:  $O (for-metta  $A10 $B10))) (predicate-property  $R (number-of-clauses  $S)) (predicate-property  $R (number-of-rules  $T)) (is  $P (-  (+  $Q $S) $T))))


 (=  (metta-replace  $O $P $Q) (sequential  (metta-del  $O $P) (metta-add  $O $Q)))


 (=  (metta-del  $O $P) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (=  $Q (:  $O (for-metta  $O $P))) (clause  $Q True $R) (clause  $S True $R) (=@=  $Q $S) no-more (erase  $R)))


 (=  (metta-rem  $O $P) (:: (if-then-else  (metta-del  $O $P) True True)))


 (=  (metta-add  $O $P) (sequential  (decl-m-fb-pred  $O for-metta (ValueAtom 2)) (=  $Q (:  $O (for-metta  $O $P))) (assert-new  $Q) (format  ~N~q.~n (:: $Q))))


 (=  (metta-ls  $O) (:: (listing  (:  $O (/  for-metta (ValueAtom 2))))))


 (=  (--aux-maplist/2-with-wild-path+1  () $A10) True)
 (=  (--aux-maplist/2-with-wild-path+1  ([|]  $O $P) $Q) (sequential  (with-wild-path  $Q $O) (--aux-maplist/2-with-wild-path+1  $P $Q)))


 (=  (with-wild-path-swi  $O $P) (sequential  (compound  $P) (absolute-file-name  $P $Q (:: (access  read) (file-errors  False) (file-type  directory))) (\=@=  $Q $P) no-more (with-wild-path  $O $Q)))
 (=  (with-wild-path-swi  $O $P) (sequential  (compound  $P) no-more (absolute-file-name  $P $Q (:: (access  read) (file-errors  False) (file-type  (:: csv tsv ())))) (\=@=  $Q $P) no-more (with-wild-path  $O $Q)))
 (=  (with-wild-path-swi  $O $P) (sequential  (atom-contains  $P *) (expand-file-name  $P $Q) no-more (--aux-maplist/2-with-wild-path+1  $Q $O)))
 (=  (with-wild-path-swi  $O $P) (sequential  (exists-directory  $P) (directory-file-path  $P *.*sv $Q) (expand-file-name  $Q $R) no-more (maplist  $O $R)))


 (=  (with-wild-path  $O $P) (sequential  (extreme-debug  (fbug  (with-wild-path  $O $P))) False))
 (=  (with-wild-path  $A10 ()) (:: no-more))
 (=  (with-wild-path  $O $P) (sequential  is-scryer (atom  $P) no-more (must-det-ll-r  (and  (atom-chars  $P $Q) (with-wild-path  $O $Q)))))
 (=  (with-wild-path  $O $P) (sequential  (missing  is-scryer) (missing  (atom  $P)) no-more (must-det-ll-r  (and  (name  $Q $P) (with-wild-path  $O $Q)))))
 (=  (with-wild-path  $O $P) (sequential  (exists-file  $P) no-more (must-det-ll-r  (call  $O $P))))
 (=  (with-wild-path  $O $P) (:: (with-wild-path-swi  $O $P)))
 (=  (with-wild-path  $O $P) (sequential  (exists-directory  $P) no-more (must-det-ll-r  (and  (directory-files  $P $Q) (and  (maplist  (directory-file-path  $P $Q) $R) (and  (maplist  atom-chars $R $S) (maplist  (with-wild-path  $O) $S))))) no-more))
 (=  (with-wild-path  $O $P) (sequential  (is-list  $P) no-more (must-det-ll-r  (maplist  (with-wild-path  $O) $P))))
 (=  (with-wild-path  $O $P) (:: (must-det-ll-r  (call  $O $P))))


 (=  load-fb-cache (sequential  (load-fb-mask  "precomputed-files/*/*pl") (load-fb-mask
   (StringValue "flybase-data/public.*.pl")) (load-fb-mask  "flybase-data/*fb-2023-01.pl")))


 (=  (est-size  (ValueAtom 248392754) feature-relationship) True)
 (=  (est-size  (ValueAtom 141933327) dbxrefprop) True)
 (=  (est-size  (ValueAtom 98464502) featureloc) True)
 (=  (est-size  (ValueAtom 92616770) feature) True)
 (=  (est-size  (ValueAtom 78909675) analysisfeature) True)
 (=  (est-size  (ValueAtom 61025742) feature-dbxref) True)
 (=  (est-size  (ValueAtom 53031863) library-featureprop) True)
 (=  (est-size  (ValueAtom 39950320) dbxref) True)
 (=  (est-size  (ValueAtom 27923222) library-feature) True)
 (=  (est-size  (ValueAtom 23805222) feature-relationshipprop) True)
 (=  (est-size  (ValueAtom 21280000) featureprop) True)
 (=  (est-size  (ValueAtom 7474186) feature-synonym) True)
 (=  (est-size  (ValueAtom 6554428) synonym) True)
 (=  (est-size  (ValueAtom 5578281) feature-pub) True)
 (=  (est-size  (ValueAtom 5341101) featureprop-pub) True)
 (=  (est-size  (ValueAtom 4865119) feature-relationship-pub) True)
 (=  (est-size  (ValueAtom 2813406) feature-interactionprop) True)
 (=  (est-size  (ValueAtom 2464356) feature-cvterm) True)
 (=  (est-size  (ValueAtom 1950808) feature-cvtermprop) True)
 (=  (est-size  (ValueAtom 1377259) feature-interaction) True)
 (=  (est-size  (ValueAtom 1116491) feature-genotype) True)
 (=  (est-size  (ValueAtom 888211) pubprop) True)
 (=  (est-size  (ValueAtom 734871) featureloc-pub) True)
 (=  (est-size  (ValueAtom 688735) pubauthor) True)
 (=  (est-size  (ValueAtom 518570) genotype-synonym) True)
 (=  (est-size  (ValueAtom 495849) genotype) True)
 (=  (est-size  (ValueAtom 491539) feature-pubprop) True)
 (=  (est-size  (ValueAtom 466210) phenstatement) True)
 (=  (est-size  (ValueAtom 413339) pub-dbxref) True)
 (=  (est-size  (ValueAtom 382055) genotype-dbxref) True)
 (=  (est-size  (ValueAtom 351943) phendesc) True)
 (=  (est-size  (ValueAtom 277993) phenotype-comparison-cvterm) True)
 (=  (est-size  (ValueAtom 254299) feature-expressionprop) True)
 (=  (est-size  (ValueAtom 252545) phenotype-comparison) True)
 (=  (est-size  (ValueAtom 251929) pub) True)
 (=  (est-size  (ValueAtom 242345) pub-relationship) True)
 (=  (est-size  (ValueAtom 227407) feature-expression) True)
 (=  (est-size  (ValueAtom 213361) cvterm-relationship) True)
 (=  (est-size  (ValueAtom 212143) cvterm-dbxref) True)
 (=  (est-size  (ValueAtom 209165) interaction-cvterm) True)
 (=  (est-size  (ValueAtom 195001) cvtermsynonym) True)
 (=  (est-size  (ValueAtom 180312) expression-cvterm) True)
 (=  (est-size  (ValueAtom 167583) update-track) True)
 (=  (est-size  (ValueAtom 150402) feature-relationshipprop-pub) True)
 (=  (est-size  (ValueAtom 149856) stockcollection-stock) True)
 (=  (est-size  (ValueAtom 149856) stock) True)
 (=  (est-size  (ValueAtom 149836) stock-genotype) True)
 (=  (est-size  (ValueAtom 146847) interactionprop) True)
 (=  (est-size  (ValueAtom 122005) interaction-group) True)
 (=  (est-size  (ValueAtom 119612) feature-interaction-pub) True)
 (=  (est-size  (ValueAtom 112785) interaction-pub) True)
 (=  (est-size  (ValueAtom 112782) interaction) True)
 (=  (est-size  (ValueAtom 101688) interaction-group-feature-interaction) True)
 (=  (est-size  (ValueAtom 96406) feature-grpmember-pub) True)
 (=  (est-size  (ValueAtom 94766) cvterm) True)
 (=  (est-size  (ValueAtom 79467) expression-cvtermprop) True)
 (=  (est-size  (ValueAtom 74874) interactionprop-pub) True)
 (=  (est-size  (ValueAtom 73829) library-interaction) True)
 (=  (est-size  (ValueAtom 57145) organism) True)
 (=  (est-size  (ValueAtom 48731) humanhealthprop) True)
 (=  (est-size  (ValueAtom 41076) feature-grpmember) True)
 (=  (est-size  (ValueAtom 36961) expression) True)
 (=  (est-size  (ValueAtom 23566) library-cvterm) True)
 (=  (est-size  (ValueAtom 23484) library-cvtermprop) True)
 (=  (est-size  (ValueAtom 21252) cvtermprop) True)
 (=  (est-size  (ValueAtom 19798) libraryprop) True)
 (=  (est-size  (ValueAtom 18397) phenotype) True)
 (=  (est-size  (ValueAtom 17872) phenotype-cvterm) True)
 (=  (est-size  (ValueAtom 16618) humanhealth-dbxrefprop) True)
 (=  (est-size  (ValueAtom 16530) interaction-expressionprop) True)
 (=  (est-size  (ValueAtom 16319) humanhealth-pub) True)
 (=  (est-size  (ValueAtom 15401) library-synonym) True)
 (=  (est-size  (ValueAtom 15356) humanhealth-dbxref) True)
 (=  (est-size  (ValueAtom 15143) cell-line-feature) True)
 (=  (est-size  (ValueAtom 14973) libraryprop-pub) True)
 (=  (est-size  (ValueAtom 13695) interaction-expression) True)
 (=  (est-size  (ValueAtom 13219) interaction-cell-line) True)
 (=  (est-size  (ValueAtom 10721) library-pub) True)
 (=  (est-size  (ValueAtom 9871) library-relationship) True)
 (=  (est-size  (ValueAtom 9852) humanhealthprop-pub) True)
 (=  (est-size  (ValueAtom 9559) library-dbxref) True)
 (=  (est-size  (ValueAtom 8340) library-relationship-pub) True)
 (=  (est-size  (ValueAtom 7096) grp-pub) True)
 (=  (est-size  (ValueAtom 6720) cell-line-pub) True)
 (=  (est-size  (ValueAtom 6658) grp-relationship) True)
 (=  (est-size  (ValueAtom 6606) strain-synonym) True)
 (=  (est-size  (ValueAtom 5991) grp-synonym) True)
 (=  (est-size  (ValueAtom 5948) humanhealth-synonym) True)
 (=  (est-size  (ValueAtom 5786) strainprop) True)
 (=  (est-size  (ValueAtom 5784) strainprop-pub) True)
 (=  (est-size  (ValueAtom 5770) library) True)
 (=  (est-size  (ValueAtom 5544) grp-cvterm) True)
 (=  (est-size  (ValueAtom 5445) cell-line-synonym) True)
 (=  (est-size  (ValueAtom 5278) library-expression) True)
 (=  (est-size  (ValueAtom 5188) grpprop) True)
 (=  (est-size  (ValueAtom 5160) grpmember) True)
 (=  (est-size  (ValueAtom 4470) humanhealth-dbxrefprop-pub) True)
 (=  (est-size  (ValueAtom 4451) library-expressionprop) True)
 (=  (est-size  (ValueAtom 4416) grpprop-pub) True)
 (=  (est-size  (ValueAtom 4320) stock-cvterm) True)
 (=  (est-size  (ValueAtom 3833) library-dbxrefprop) True)
 (=  (est-size  (ValueAtom 3830) grpmemberprop) True)
 (=  (est-size  (ValueAtom 3778) genotype-cvterm) True)
 (=  (est-size  (ValueAtom 3745) humanhealth-featureprop) True)
 (=  (est-size  (ValueAtom 3722) library-strainprop) True)
 (=  (est-size  (ValueAtom 3722) library-strain) True)
 (=  (est-size  (ValueAtom 3626) humanhealth-feature) True)
 (=  (est-size  (ValueAtom 2642) grp-dbxref) True)
 (=  (est-size  (ValueAtom 2264) humanhealth-relationship) True)
 (=  (est-size  (ValueAtom 2221) humanhealth-relationship-pub) True)
 (=  (est-size  (ValueAtom 2094) strain-pub) True)
 (=  (est-size  (ValueAtom 2011) grp-relationship-pub) True)
 (=  (est-size  (ValueAtom 1940) strain-cvtermprop) True)
 (=  (est-size  (ValueAtom 1940) strain-cvterm) True)
 (=  (est-size  (ValueAtom 1815) grp) True)
 (=  (est-size  (ValueAtom 1778) strain-dbxref) True)
 (=  (est-size  (ValueAtom 1777) strain) True)
 (=  (est-size  (ValueAtom 1740) organism-dbxref) True)
 (=  (est-size  (ValueAtom 1644) feature-humanhealth-dbxref) True)
 (=  (est-size  (ValueAtom 1541) humanhealth-cvtermprop) True)
 (=  (est-size  (ValueAtom 1541) humanhealth-cvterm) True)
 (=  (est-size  (ValueAtom 1516) humanhealth) True)
 (=  (est-size  (ValueAtom 1301) cell-lineprop-pub) True)
 (=  (est-size  (ValueAtom 1292) cell-lineprop) True)
 (=  (est-size  (ValueAtom 1216) cell-line-dbxref) True)
 (=  (est-size  (ValueAtom 1199) cell-line-libraryprop) True)
 (=  (est-size  (ValueAtom 1082) cell-line-library) True)
 (=  (est-size  (ValueAtom 1014) organism-pub) True)
 (=  (est-size  (ValueAtom 822) organismprop) True)
 (=  (est-size  (ValueAtom 732) organismprop-pub) True)
 (=  (est-size  (ValueAtom 715) cell-line-cvterm) True)
 (=  (est-size  (ValueAtom 519) db) True)
 (=  (est-size  (ValueAtom 436) strain-relationship-pub) True)
 (=  (est-size  (ValueAtom 436) strain-relationship) True)
 (=  (est-size  (ValueAtom 321) cell-line) True)
 (=  (est-size  (ValueAtom 309) analysis) True)
 (=  (est-size  (ValueAtom 239) stockprop) True)
 (=  (est-size  (ValueAtom 172) cell-line-relationship) True)
 (=  (est-size  (ValueAtom 140) strain-featureprop) True)
 (=  (est-size  (ValueAtom 140) strain-feature) True)
 (=  (est-size  (ValueAtom 108) strain-phenotypeprop) True)
 (=  (est-size  (ValueAtom 97) humanhealth-pubprop) True)
 (=  (est-size  (ValueAtom 74) cell-line-cvtermprop) True)
 (=  (est-size  (ValueAtom 72) cv) True)
 (=  (est-size  (ValueAtom 55) strain-phenotype) True)
 (=  (est-size  (ValueAtom 41) environment) True)
 (=  (est-size  (ValueAtom 28) stockcollectionprop) True)
 (=  (est-size  (ValueAtom 27) contact) True)
 (=  (est-size  (ValueAtom 19) environment-cvterm) True)
 (=  (est-size  (ValueAtom 12) organism-library) True)
 (=  (est-size  (ValueAtom 8) stockcollection) True)
 (=  (est-size  (ValueAtom 2) lock) True)
 (=  (est-size  (ValueAtom 1) analysisgrp) True)
 (=  (est-size  (ValueAtom 1) analysisgrpmember) True)
 (=  (est-size  (ValueAtom 1) analysisprop) True)
 (=  (est-size  (ValueAtom 1) audit-chado) True)
 (=  (est-size  (ValueAtom 1) cell-line-strain) True)
 (=  (est-size  (ValueAtom 1) cell-line-strainprop) True)
 (=  (est-size  (ValueAtom 1) cvtermpath) True)
 (=  (est-size  (ValueAtom 1) eimage) True)
 (=  (est-size  (ValueAtom 1) expression-image) True)
 (=  (est-size  (ValueAtom 1) expression-pub) True)
 (=  (est-size  (ValueAtom 1) expressionprop) True)
 (=  (est-size  (ValueAtom 1) feature-cvterm-dbxref) True)
 (=  (est-size  (ValueAtom 1) feature-phenotype) True)
 (=  (est-size  (ValueAtom 1) featuremap) True)
 (=  (est-size  (ValueAtom 1) featuremap-pub) True)
 (=  (est-size  (ValueAtom 1) featurepos) True)
 (=  (est-size  (ValueAtom 1) featurerange) True)
 (=  (est-size  (ValueAtom 1) genotype-cvtermprop) True)
 (=  (est-size  (ValueAtom 1) genotype-pub) True)
 (=  (est-size  (ValueAtom 1) genotypeprop) True)
 (=  (est-size  (ValueAtom 1) genotypeprop-pub) True)
 (=  (est-size  (ValueAtom 1) grp-pubprop) True)
 (=  (est-size  (ValueAtom 1) grp-relationshipprop) True)
 (=  (est-size  (ValueAtom 1) grpmember-cvterm) True)
 (=  (est-size  (ValueAtom 1) grpmember-pub) True)
 (=  (est-size  (ValueAtom 1) grpmemberprop-pub) True)
 (=  (est-size  (ValueAtom 1) humanhealth-phenotype) True)
 (=  (est-size  (ValueAtom 1) humanhealth-phenotypeprop) True)
 (=  (est-size  (ValueAtom 1) interaction-cvtermprop) True)
 (=  (est-size  (ValueAtom 1) library-grpmember) True)
 (=  (est-size  (ValueAtom 1) library-humanhealth) True)
 (=  (est-size  (ValueAtom 1) library-humanhealthprop) True)
 (=  (est-size  (ValueAtom 1) organism-cvterm) True)
 (=  (est-size  (ValueAtom 1) organism-cvtermprop) True)
 (=  (est-size  (ValueAtom 1) organism-grpmember) True)
 (=  (est-size  (ValueAtom 1) project) True)
 (=  (est-size  (ValueAtom 1) stock-dbxref) True)
 (=  (est-size  (ValueAtom 1) stock-pub) True)
 (=  (est-size  (ValueAtom 1) stock-relationship) True)
 (=  (est-size  (ValueAtom 1) stock-relationship-pub) True)
 (=  (est-size  (ValueAtom 1) stockprop-pub) True)
 (=  (est-size  (ValueAtom 1) tableinfo) True)


 (=  load-flybase-chado (:: (with-option  (:: (=  row-1-is-header True) (=  max-per-file (ValueAtom 100000))) (load-flybase  (StringValue "./data/tsv-exports/public/*.tsv")))))


 (=  load-flybase-files-ftp (sequential  load-flybase-das-11 (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/alleles/fbal-to-fbgn-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/clones/cDNA-clone-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/clones/genomic-clone-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/collaborators/fbgn-uniprot-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/collaborators/pmid-fbgn-uniprot-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/automated-gene-summaries.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/automated-gene-summaries-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/best-gene-summary-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/Dmel-enzyme-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/dmel-unique-protein-isoforms-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/fbgn-annotation-ID-fb-*.tsv")) (with-option  (:: (=  use-va True)) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/fbgn-exons2affy1-overlaps.tsv"))) (with-option  (:: (=  use-va True)) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/fbgn-exons2affy2-overlaps.tsv"))) (with-option  (:: (=  use-va false)) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/fbgn-fbtr-fbpp-fb-*.tsv"))) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/fbgn-gleanr-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/fbgn-NAseq-Uniprot-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/gene-functional-complementation-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/gene-group-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/gene-groups-HGNC-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/gene-rpkm-matrix-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/gene-rpkm-report-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/gene-snapshots-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/genes/pathway-group-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/insertions/fu-gal4-table-fb-*.json")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/insertions/insertion-mapping-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/map-conversion/cyto-genetic-seq.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/map-conversion/cytotable.txt") tsv) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/map-conversion/genome-cyto-seq.txt") tsv) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/metadata/dataset-metadata-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/orthologs/dmel-paralogs-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/references/entity-publication-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/species/organism-list-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/stocks/stocks-FB*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/synonyms/fb-synonym-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/transposons/transposon-sequence-set.gff") tsv) no-more))


 (=  load-flybase-das-11 (sequential  (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/fbgn-fbtr-fbpp-expanded-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/physical-interactions-mitab-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/dmel-gene-sequence-ontology-annotations-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/gene-map-table-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/ncRNA-genes-fb-*.json")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/gene-association-*.fb") tsv) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/gene-genetic-interactions-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/allele-genetic-interactions-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/alleles/genotype-phenotype-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/allele-phenotypic-data-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/disease-model-annotations-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/dmel-human-orthologs-disease-fb-*.tsv")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/fbrf-pmid-pmcid-doi-fb-*.tsv")) (format  (StringValue "~n=================================================================================================")) (format  (StringValue "~n=====================================Das Checkpoint==============================================")) (format  (StringValue "~n=================================================================================================")) fb-stats (format  (StringValue "~n=================================================================================================")) (format  (StringValue "~n=================================================================================================")) (format  (StringValue "~n=================================================================================================~n")) no-more))


 (=  load-flybase-obo-files (sequential  (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/ncRNA-genes-fb-*.json")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/chebi-fb-*.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/doid.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/fly-anatomy.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/fly-development.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/flybase-controlled-vocabulary.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/flybase-stock-vocabulary.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/gene-group-FB*.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/go-basic.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/image.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/psi-mi.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/slice.chebi.obo")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/ontologies/so-simple.obo")) no-more))


 (=  load-flybase-files (sequential  (ftp-data  $O) (with-cwd  $O load-flybase-files-ftp)))


 (=  load-flybase-dirs (sequential  (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/das-precomputed")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*")) (load-flybase  (StringValue "./data/ftp.flybase.net/releases/current/./*sv")) no-more))


 (=  load-flybase (sequential  is-scryer no-more load-flybase-files))
 (=  load-flybase (sequential  make recount-total-loaded-atoms no-more load-flybase-files no-more cleanup-arities no-more fb-stats))


 (=  recount-total-loaded-atoms (sequential  (flag  total-loaded-atoms $A10 (ValueAtom 0)) (full-atom-count  $O) (flag  total-loaded-atoms $B10 $O)))


 (=  is-scryer (:: (missing  (option-value  libswipl $A10))))


 (=  (swi-only  $A10) (sequential  is-scryer no-more False))
 (=  (swi-only  $O) (:: (call  $O)))


 (=  (fbdebug1  $O) (sequential  (flush-output  user-output) (flush-output  user-error) (catch  (portray-clause  user-error $O ()) $A10 (catch-ignore  (format  user-error (StringValue "~n/* ~q. */~n") (:: $O)))) (flush-output  user-error)))


 (=  (fbug  $O) (sequential  (format  (StringValue "~N")) (with-output-to  user-error (pp-fb  $O)) no-more))
 (=  (fbug  (=  $O $P)) (sequential  (nonvar  $O) no-more (fbdebug1  (:-  $O $P))))
 (=  (fbug  $O) (sequential  (compound  $O) (functor  $O $P $A10) no-more (fbdebug1  (:-  $P $O))))
 (=  (fbug  $O) (:: (fbdebug1  (:-  debug $O))))


 (=  (pp-fb1  $O) (sequential  (pp-as  $O) no-more))
 (=  (pp-fb1  $O) (sequential  (print  $O) no-more nl))
 (=  (pp-fb1  $O) (sequential  (fbdebug1  $O) no-more nl))


 (=  (pp-fb  $O) (sequential  (format  (StringValue "~N ")) (missing  (missing  (and  (numbervars-w-singles  $O) (pp-fb1  $O))))))


 (=  (numbervars-w-singles  $O) (sequential  (term-singletons  $O $P) (numbervars  $P (ValueAtom 260) $A10 (:: (attvar  bind) (singletons  false))) (numbervars  $O (ValueAtom 14) $B10 (:: (attvar  bind) (singletons  True)))))


 (=  (extreme-debug  $A10) True)


 (=  gc-now (sequential  (set-option-value  gc True) garbage-collect garbage-collect-atoms garbage-collect-clauses))


 (=  (call-sexpr  $O) (:: (writeln  (=  call $O))))


 (=  (print-list-as-sexpression  ()) True)
 (=  (print-list-as-sexpression  ([|]  $O $P)) (sequential  (write   ) (pp-sex  $O) (print-list-as-sexpression  $P)))


 (=  (write-args-as-sexpression  ()) True)
 (=  (write-args-as-sexpression  ([|]  $O $P)) (sequential  (write   ) (pp-sex  $O) (write-args-as-sexpression  $P)))


 (=  (has-type  $O $P) (sequential  (sub-atom  $O (ValueAtom 0) (ValueAtom 4) $Q $R) (flybase-identifier  $R $P) no-more (>  $Q (ValueAtom 0))))


 (=  (is-an-arg-type  $O $P) (sequential  (flybase-identifier  $O $P) no-more))


 (=  (pp-sax  $O) (sequential  (is-englishy  $O) no-more (format  (StringValue "~w") (:: $O))))
 (=  (pp-sax  $O) (sequential  (missing  allow-concepts) no-more (write  $O)))
 (=  (pp-sax  $O) (sequential  (atom-length  $O (ValueAtom 1)) (atom-string  $O $P) no-more (format  (StringValue "(StringValue ~q)") (:: $P))))
 (=  (pp-sax  $O) (sequential  (is-an-arg-type  $O $P) no-more (format  (TypeNode "~w") (:: $P))))
 (=  (pp-sax  $O) (sequential  (has-type  $O $P) no-more (format  (~wValueNode "~w") (:: $P $O))))
 (=  (pp-sax  $O) (sequential  (sub-atom  $O (ValueAtom 0) (ValueAtom 4) $P $Q) (flybase-identifier  $Q $R) no-more (if-then-else  (>  $P (ValueAtom 0)) (format  (~wValueNode "~w") (:: $R $O)) (format  (TypeNode "~w") (:: $R)))))
 (=  (pp-sax  $O) (:: (format  (ConceptNode "~w") (:: $O))))


 (=  (--aux-maplist/3-always-dash-functor+0  () ()) True)
 (=  (--aux-maplist/3-always-dash-functor+0  ([|]  $O $P) ([|]  $Q $R)) (sequential  (always-dash-functor  $O $Q) (--aux-maplist/3-always-dash-functor+0  $P $R)))


 (=  (dash-functor  $O $P) (sequential  (missing  (atom  $O)) no-more (=  $P $O)))
 (=  (dash-functor  $O $P) (sequential  (direct-mapping  $O $Q) (\==  $O $Q) no-more (always-dash-functor  $Q $P)))
 (=  (dash-functor  $O $P) (sequential  (atom  $O) (atomic-list-concat  $Q - $O) (\=  $Q (:: $A10)) (--aux-maplist/3-always-dash-functor+0  $Q $R) (atomic-list-concat  $R - $P)))
 (=  (dash-functor  $O $P) (sequential  (atom  $O) (atomic-list-concat  $Q - $O) (\=  $Q (:: $A10)) (--aux-maplist/3-always-dash-functor+0  $Q $R) (atomic-list-concat  $R - $P)))


 (=  (always-dash-functor  $O $P) (sequential  (->  (dash-functor  $O $P) True) (\=@=  $O $P) no-more))
 (=  (always-dash-functor  $O $O) True)


 (=  (pp-sex  $O) (sequential  (var  $O) no-more (format  $~p (:: $O))))
 (=  (pp-sex  $O) (sequential  (direct-mapping  $O $P) (\==  $O $P) no-more (pp-sex  $P)))
 (=  (pp-sex  ()) (:: (format  () ())))
 (=  (pp-sex  ()) (sequential  no-more (write  ())))
 (=  (pp-sex  (=  $O $P)) (sequential  allow-concepts no-more (format  (StringValue "~N;; ~w == ~n") (:: $O)) no-more (pp-sex  $P)))
 (=  (pp-sex  $O) (sequential  (or  (number  $O) (or  (and  (atom  $O) (atom-number  $O $A10)) (is-dict  $O))) no-more (format  (ValueAtom ~w) (:: $O))))
 (=  (pp-sex  $O) (sequential  (atom  $O) (always-dash-functor  $O $P) (pp-sax  $P) no-more))
 (=  (pp-sex  $O) (sequential  (string  $O) no-more (format  (StringValue "~w") (:: $O))))
 (=  (pp-sex  $O) (sequential  (missing  (compound  $O)) no-more (format  ~p (:: $O))))
 (=  (pp-sex  $O) (sequential  (=  $O $'$VAR'(A10)) no-more (format  $~p (:: $O))))
 (=  (pp-sex  (ListValue $O)) (sequential  no-more (pp-sex  (ListValue $O))))
 (=  (pp-sex  (ListValue $O)) (sequential  no-more (format  (ListValue ~@) (:: (pp-sex  $O)))))
 (=  (pp-sex  !$O) (sequential  (write  no-more) (pp-sex  $O)))
 (=  (pp-sex  ([|]  $O $P)) (sequential  (is-list  $P) no-more (write  (:: ) (pp-sex  $O) (print-list-as-sexpression  $P) (write  ))))
 (=  (pp-sex  $O) (sequential  (equals-ListMappingFn  $O ([|]  $P $Q)) (always-dash-functor  $P $R) (format  (~w  (:: $R)) (write-args-as-sexpression  $Q) (write  )) no-more))
 (=  (pp-sex  $O) (sequential  allow-concepts (equals-ListMappingFn  $O ([|]  $P $Q)) (format  (StringValue "(EvaluationLink (PredicateNode "~w") (ListLink ") (:: $P)) (write-args-as-sexpression  $Q) (write  ))) no-more))
 (=  (pp-sex  $O) (sequential  (equals-ListMappingFn  $O ([|]  $P $Q)) (always-dash-functor  $P $R) (format  (~w  (:: $R)) (write-args-as-sexpression  $Q) (write  )) no-more))


 (=  (pp-as  $O) (:: (missing  (missing  (pp-sex  $O)))))


 (=  (print-hb-as-metta  $O) (:: (with-option  (=  concepts false) (pp-fb  $O))))


 (=  (print-hb-as-metta  $O $P) (sequential  (==  $P True) no-more (print-hb-as-metta  (=  $O True))))
 (=  (print-hb-as-metta  $O $P) (sequential  (==  $P false) no-more (print-hb-as-metta  (=  $O False))))
 (=  (print-hb-as-metta  $O $P) (sequential  (conjuncts-to-list  $P $Q) (into-sequential  $Q $R) no-more (print-hb-as-metta  (=  $O $R))))


 (=  (print-pred-as-metta  $O $P) (sequential  (functor  $Q $O $P) nl nl nl (missing  (and  (clause  $Q $R) (missing  (print-hb-as-metta  $Q $R))))))


 (=  (print-pred-as-metta  (/  $O $P)) (sequential  no-more (print-pred-as-metta  $O $P)))
 (=  (print-pred-as-metta  $O) (sequential  (functor  $O $P $Q) (print-pred-as-metta  $P $Q)))


 (=  print-pred-as-metta (sequential  mmake (missing  (and  (source-file  $O $P) (and  (atom-contains  $P flybase) (missing  (print-pred-as-metta  $O)))))))


 (=  (direct-mapping  no-more no-more) True)
 (=  (direct-mapping  False False) True)
 (=  (direct-mapping  True True) True)
 (=  (direct-mapping  meTTa meTTa) True)
 (=  (direct-mapping  or or) True)
 (=  (direct-mapping  (if-then-else  $O $P $Q) (if-then-else  $O $P $Q)) True)
 (=  (direct-mapping  and and) True)
 (=  (direct-mapping  missing missing) True)
 (=  (direct-mapping  equals-ListMappingFn equals-ListMappingFn) True)


 (=  allow-concepts (sequential  (option-else  concepts $O True) (missing  (=  $O false))))


 (=  (skipped-anotations  fbgn-exons2affy1-overlaps) True)
 (=  (skipped-anotations  gene-rpkm-matrix) True)
 (=  (skipped-anotations  dmel-gene-sequence-ontology-annotations) True)
 (=  (skipped-anotations  fbgn-annotation-ID) True)


 (=  (safe  $O) (:: (safe  $O (/  safe (ValueAtom 1)))))


 (=  (safe  $O $P) (:: (catch  $O $Q (and  (catch-ignore  (portray-clause  (:  exception (:  $Q (:-  $P $O))))) (throw  $Q)))))


 (=  (must-det-ll-r  (and  $O $P)) (sequential  (if-then-else  (call  $O) True True) no-more (must-det-ll-r  $P)))
 (=  (must-det-ll-r  $O) (sequential  (call  $O) no-more))


 (=  (primary-column  fb-synonym primary-FBid) True)
 (=  (primary-column  gene-genetic-interactions Starting-gene-FBgn) True)
 (=  (primary-column  gene-rpkm-matrix gene-primary-id) True)
 (=  (primary-column  gene-rpkm-report FBgn) True)
 (=  (primary-column  genotype-phenotype-data genotype-FBids) True)
 (=  (primary-column  pmid-fbgn-uniprot FBgn-id) True)
 (=  (primary-column  scRNA-Seq-gene-expression Gene-ID) True)
 (=  (primary-column  allele-genetic-interactions allele-FBal) True)
 (=  (primary-column  fbgn-exons2affy1-overlaps FBgn) True)
 (=  (primary-column  fbgn-exons2affy2-overlaps FBgn) True)
 (=  (primary-column  dataset-metadata Item-ID) True)
 (=  (primary-column  dmel-paralogs Paralog-FBgn) True)


 (=  (column-names-ext  pmid-fbgn-uniprot (:: FBrf-id PMID FBgn-id UniProt-database UniProt-id)) True)
 (=  (column-names-ext  gene-genetic-interactions (:: (ListValue Starting-gene-symbol) (ListValue Starting-gene-FBgn) (ListValue Interacting-gene-symbol) (ListValue Interacting-gene-FBgn) Interaction-type Publication-FBrf)) True)
 (=  (column-names-ext  gene-rpkm-matrix (:: gene-primary-id gene-symbol gene-fullname gene-type BCM-1-E2-4hr-(FBlc0000061) BCM-1-E14-16hr-(FBlc0000062) BCM-1-E2-16hr-(FBlc0000063) BCM-1-E2-16hr100-(FBlc0000064) BCM-1-L3i-(FBlc0000065) BCM-1-L3i100-(FBlc0000066) BCM-1-P3d-(FBlc0000067) BCM-1-FA3d-(FBlc0000068) BCM-1-MA3d-(FBlc0000069) BCM-1-P-(FBlc0000070) BCM-1-L-(FBlc0000071) BCM-1-A17d-(FBlc0000072) mE-mRNA-em0-2hr-(FBlc0000086) mE-mRNA-em2-4hr-(FBlc0000087) mE-mRNA-em4-6hr-(FBlc0000088) mE-mRNA-em6-8hr-(FBlc0000089) mE-mRNA-em8-10hr-(FBlc0000090) mE-mRNA-em10-12hr-(FBlc0000091) mE-mRNA-em12-14hr-(FBlc0000092) mE-mRNA-em14-16hr-(FBlc0000093) mE-mRNA-em16-18hr-(FBlc0000094) mE-mRNA-em18-20hr-(FBlc0000095) mE-mRNA-em20-22hr-(FBlc0000096) mE-mRNA-em22-24hr-(FBlc0000097) mE-mRNA-L1-(FBlc0000098) mE-mRNA-L2-(FBlc0000099) mE-mRNA-L3-12hr-(FBlc0000100) mE-mRNA-L3-PS1-2-(FBlc0000101) mE-mRNA-L3-PS3-6-(FBlc0000102) mE-mRNA-L3-PS7-9-(FBlc0000103) mE-mRNA-WPP-(FBlc0000104) mE-mRNA-P5-(FBlc0000105) mE-mRNA-P6-(FBlc0000106) mE-mRNA-P8-(FBlc0000107) mE-mRNA-P9-10-(FBlc0000108) mE-mRNA-P15-(FBlc0000109) mE-mRNA-AdF-Ecl-1days-(FBlc0000110) mE-mRNA-AdF-Ecl-5days-(FBlc0000111) mE-mRNA-AdF-Ecl-30days-(FBlc0000112) mE-mRNA-AdM-Ecl-1days-(FBlc0000113) mE-mRNA-AdM-Ecl-5days-(FBlc0000114) mE-mRNA-AdM-Ecl-30days-(FBlc0000115) mE-mRNA-A-MateF-1d-head-(FBlc0000207) mE-mRNA-A-MateF-4d-ovary-(FBlc0000208) mE-mRNA-A-MateM-1d-head-(FBlc0000209) mE-mRNA-A-VirF-1d-head-(FBlc0000210) mE-mRNA-A-VirF-4d-head-(FBlc0000211) mE-mRNA-A-MateF-20d-head-(FBlc0000212) mE-mRNA-A-MateF-4d-head-(FBlc0000213) mE-mRNA-A-MateM-20d-head-(FBlc0000214) mE-mRNA-A-MateM-4d-acc-gland-(FBlc0000215) mE-mRNA-A-MateM-4d-head-(FBlc0000216) mE-mRNA-A-MateM-4d-testis-(FBlc0000217) mE-mRNA-A-1d-carcass-(FBlc0000218) mE-mRNA-A-1d-dig-sys-(FBlc0000219) mE-mRNA-A-20d-carcass-(FBlc0000220) mE-mRNA-A-20d-dig-sys-(FBlc0000221) mE-mRNA-A-4d-carcass-(FBlc0000222) mE-mRNA-A-4d-dig-sys-(FBlc0000223) mE-mRNA-P8-CNS-(FBlc0000224) mE-mRNA-L3-CNS-(FBlc0000225) mE-mRNA-L3-Wand-carcass-(FBlc0000226) mE-mRNA-L3-Wand-dig-sys-(FBlc0000227) mE-mRNA-L3-Wand-fat-(FBlc0000228) mE-mRNA-L3-Wand-imag-disc-(FBlc0000229) mE-mRNA-L3-Wand-saliv-(FBlc0000230) mE-mRNA-A-VirF-20d-head-(FBlc0000231) mE-mRNA-A-VirF-4d-ovary-(FBlc0000232) mE-mRNA-WPP-fat-(FBlc0000233) mE-mRNA-WPP-saliv-(FBlc0000234) mE-mRNA-P8-fat-(FBlc0000235) mE-mRNA-A-4d-Cold1-(FBlc0000237) mE-mRNA-A-4d-Cold2-(FBlc0000238) mE-mRNA-L3-Cu-0.5mM-(FBlc0000239) mE-mRNA-L3-late-Zn-5mM-(FBlc0000240) mE-mRNA-A-4d-Cu-15mM-(FBlc0000241) mE-mRNA-A-4d-Zn-4.5mM-(FBlc0000242) mE-mRNA-A-4d-Caffeine-25mg/ml-(FBlc0000243) mE-mRNA-A-4d-Caffeine-2.5mg/ml-(FBlc0000244) mE-mRNA-L3-Caffeine-1.5mg/ml-(FBlc0000245) mE-mRNA-A-4d-Cd-0.1M-(FBlc0000246) mE-mRNA-A-4d-Cd-0.05M-(FBlc0000247) mE-mRNA-L3-Cd-12h-(FBlc0000248) mE-mRNA-L3-Cd-6hr-(FBlc0000249) mE-mRNA-A-4d-Paraquat-5mM-(FBlc0000250) mE-mRNA-A-4d-Paraquat-10mM-(FBlc0000251) mE-mRNA-L3-Rotenone-8ug-(FBlc0000252) mE-mRNA-L3-Rotenone-2ug-(FBlc0000253) mE-mRNA-L3-EtOH-10-(FBlc0000254) mE-mRNA-L3-EtOH-5-(FBlc0000255) mE-mRNA-L3-EtOH-2.5-(FBlc0000256) mE-mRNA-A-4d-Heatshock-(FBlc0000257) mE-mRNA-A-10d-Resveratrol-100uM-(FBlc0000672) mE-mRNA-A-10d-Rotenone-Starved-(FBlc0000673) mE-mRNA-F-Sindbis-virus-(FBlc0000674) mE-mRNA-L-Sindbis-virus-(FBlc0000675) mE-mRNA-M-Sindbis-virus-(FBlc0000676) mE-mRNA-P-Sindbis-virus-(FBlc0000677) mE-mRNA-CME-W2-cells-(FBlc0000261) mE-mRNA-GM2-cells-(FBlc0000262) mE-mRNA-mbn2-cells-(FBlc0000263) mE-mRNA-BG2-c2-cells-(FBlc0000264) mE-mRNA-D20-c5-cells-(FBlc0000265) mE-mRNA-S3-cells-(FBlc0000266) mE-mRNA-1182-4H-cells-(FBlc0000267) mE-mRNA-CME-L1-cells-(FBlc0000268) mE-mRNA-Kc167-cells-(FBlc0000269) mE-mRNA-BG1-c1-cells-(FBlc0000270) mE-mRNA-D11-cells-(FBlc0000271) mE-mRNA-D16-c3-cells-(FBlc0000272) mE-mRNA-D17-c3-cells-(FBlc0000273) mE-mRNA-D21-cells-(FBlc0000274) mE-mRNA-D32-cells-(FBlc0000275) mE-mRNA-D4-c1-cells-(FBlc0000276) mE-mRNA-D8-cells-(FBlc0000277) mE-mRNA-D9-cells-(FBlc0000278) mE-mRNA-S1-cells-(FBlc0000279) mE-mRNA-S2R+-cells-(FBlc0000280) mE-mRNA-Sg4-cells-(FBlc0000281) mE-mRNA-OSS-cells-(FBlc0000282) mE-mRNA-OSC-cells-(FBlc0000283) mE-mRNA-fGS-cells-(FBlc0000284) Knoblich-mRNA-L3-CNS-neuroblast-(FBlc0000505) Knoblich-mRNA-L3-CNS-neuron-(FBlc0000506) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Brain-(FBlc0003619) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Crop-(FBlc0003620) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Carcass-(FBlc0003621) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Eye-(FBlc0003622) RNA-Seq-Profile-FlyAtlas2-Adult-Female-FatBody-(FBlc0003623) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Head-(FBlc0003624) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Hindgut-(FBlc0003625) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Midgut-(FBlc0003626) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Ovary-(FBlc0003627) RNA-Seq-Profile-FlyAtlas2-Adult-Female-RectalPad-(FBlc0003628) RNA-Seq-Profile-FlyAtlas2-Adult-Female-SalivaryGland-(FBlc0003629) RNA-Seq-Profile-FlyAtlas2-Adult-Female-ThoracicoAbdominalGanglion-(FBlc0003630) RNA-Seq-Profile-FlyAtlas2-Adult-Female-MalpighianTubule-(FBlc0003631) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Mated-Spermathecum-(FBlc0003632) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Virgin-Spermathecum-(FBlc0003633) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Whole-(FBlc0003634) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Brain-(FBlc0003635) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Crop-(FBlc0003636) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Carcass-(FBlc0003637) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Eye-(FBlc0003638) RNA-Seq-Profile-FlyAtlas2-Adult-Male-FatBody-(FBlc0003639) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Head-(FBlc0003640) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Hindgut-(FBlc0003641) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Midgut-(FBlc0003642) RNA-Seq-Profile-FlyAtlas2-Adult-Male-RectalPad-(FBlc0003643) RNA-Seq-Profile-FlyAtlas2-Adult-Male-SalivaryGland-(FBlc0003644) RNA-Seq-Profile-FlyAtlas2-Adult-Male-ThoracicoAbdominalGanglion-(FBlc0003645) RNA-Seq-Profile-FlyAtlas2-Adult-Male-MalpighianTubule-(FBlc0003646) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Testis-(FBlc0003647) RNA-Seq-Profile-FlyAtlas2-Adult-Male-AccessoryGland-(FBlc0003648) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Whole-(FBlc0003649) RNA-Seq-Profile-FlyAtlas2-L3-CNS-(FBlc0003650) RNA-Seq-Profile-FlyAtlas2-L3-FatBody-(FBlc0003651) RNA-Seq-Profile-FlyAtlas2-L3-Hindgut-(FBlc0003652) RNA-Seq-Profile-FlyAtlas2-L3-MalpighianTubule-(FBlc0003653) RNA-Seq-Profile-FlyAtlas2-L3-Midgut-(FBlc0003654) RNA-Seq-Profile-FlyAtlas2-L3-SalivaryGland-(FBlc0003655) RNA-Seq-Profile-FlyAtlas2-L3-Trachea-(FBlc0003656) RNA-Seq-Profile-FlyAtlas2-L3-Carcass-(FBlc0003657) RNA-Seq-Profile-FlyAtlas2-L3-Whole-(FBlc0003658) RNA-Seq-Profile-FlyAtlas2-Adult-Female-Heart-(FBlc0003724) RNA-Seq-Profile-FlyAtlas2-Adult-Male-Heart-(FBlc0003725))) True)


 (=  (whs  ([|]  () $O) $P) (sequential  no-more (=  $Q $O) (whs  $Q $P)))
 (=  (whs  $O $P) (:: (=  $O $P)))


 (=  (named-like-string  ([|]  $O $P) ([|]  $O $Q) $R) (sequential  (missing  (member  $O (:: '\n'))) (=  $S $Q) (named-like-string  $P $S $R)))
 (=  (named-like-string  () $O $P) (:: (=  $O $P)))


 (=  (string-until-end-quote  () ([|]  '"' $O) $P) (sequential  no-more (=  $P $O)))
 (=  (string-until-end-quote  ([|]  $O $P) $Q $R) (sequential  (or  (=  $Q ([|]  \ ([|]  $O $S))) (=  $Q ([|]  $O $S))) no-more (=  $T $S) (string-until-end-quote  $P $T $R)))


 (=  (symbol-or-url  ([|]  $O $P) ([|]  $O $Q) $R) (sequential  (missing  (member  $O (:: and [ ] '"'  ))) (=  $S $Q) (symbol-or-url  $P $S $R)))
 (=  (symbol-or-url  () $O $P) (:: (=  $O $P)))


 (=  (id-like-string  ([|]  $O $P) ([|]  $O $Q) $R) (sequential  (missing  (member  $O (:: no-more
 and [ ] { } '"'))) (=  $S $Q) (id-like-string  $P $S $R)))
 (=  (id-like-string  () $O $P) (:: (=  $O $P)))


 (=  (key-like-string  ([|]  $O $P) ([|]  $O $Q) $R) (sequential  (missing  (member  $O (:: :
))) (=  $S $Q) (key-like-string  $P $S $R)))
 (=  (key-like-string  () $O $P) (:: (=  $O $P)))


 (=  (item  $O $P $Q) (sequential  (symbol-or-url  $R $P $S) (\==  $R ()) (atom-chars  $O $R) (=  $Q $S)))


 (=  (items  ([|]  $O $P) $Q $R) (sequential  (item  $O $Q $S) (whs  $S $T) (=  $T ([|]  and $U)) (whs  $U $V) (items  $P $V $R)))
 (=  (items  (:: $O) $P $Q) (sequential  (item  $O $P $R) no-more (=  $Q $R)))


 (=  (get-one-item  $O ([|]    $P) $Q) (sequential  no-more (=  $R $P) (get-one-item  $O $R $Q)))
 (=  (get-one-item  (quoted  $O) ([|]  x ([|]  s ([|]  d ([|]  : $P)))) $Q) (sequential  (symbol-or-url  $R $P $S) (atom-chars  $O ([|]  x ([|]  s ([|]  d ([|]  : $R))))) (=  $Q $S)))
 (=  (get-one-item  (quoted  $O) ([|]  h ([|]  t ([|]  t ([|]  p $P)))) $Q) (sequential  (symbol-or-url  $R $P $S) (string-chars  $O ([|]  h ([|]  t ([|]  t ([|]  p $R))))) (=  $Q $S)))
 (=  (get-one-item  (quoted  $O) ([|]  f ([|]  t ([|]  p $P))) $Q) (sequential  (symbol-or-url  $R $P $S) (string-chars  $O ([|]  f ([|]  t ([|]  p $R)))) (=  $Q $S)))
 (=  (get-one-item  (quoted  $O) ([|]  '"' $P) $Q) (sequential  (string-until-end-quote  $R $P $S) (string-chars  $O $R) (=  $Q $S)))
 (=  (get-one-item  (named  $O) ([|]  no-more $P) $Q) (sequential  (whs  $P $R) (named-like-string  $S $R $T) (atom-chars  $O $S) (=  $Q $T)))
 (=  (get-one-item  (bracketed  $O) ([|]  [ $P) $Q) (sequential  (whs  $P $R) (items  $O $R $S) (whs  $S $T) (=  $T ([|]  ] $Q))))
 (=  (get-one-item  (bracketed  $O) ([|]  { $P) $Q) (sequential  (whs  $P $R) (items  $O $R $S) (whs  $S $T) (=  $T ([|]  } $Q))))
 (=  (get-one-item  (keyword  $O) $P $Q) (sequential  (whs  $P $R) (id-like-string  $S $R $T) (\==  $S ()) (=  $U $T) (whs  $U $V) (atom-chars  $O $S) (=  $Q $V)))
 (=  (get-one-item  (text  $O) $P $Q) (sequential  (named-like-string  $R $P $S) (string-chars  $O $R) (=  $Q $S)))
 (=  (get-one-item  (text  $O) ([|]  $P $Q) ()) (sequential  (ground  ([|]  $P $Q)) (string-chars  $O ([|]  $P $Q)) no-more))


 (=  (get-some-items  $O ([|]    $P) $Q) (sequential  no-more (=  $R $P) (get-some-items  $O $R $Q)))
 (=  (get-some-items  $A10 () ()) (:: no-more))
 (=  (get-some-items  ([|]  $O $P) $Q $R) (sequential  (get-one-item  $O $Q $S) (get-some-items  $P $S $R)))
 (=  (get-some-items  () $O $P) (:: (=  $O $P)))


 (=  (get-key  $O $P $Q) (sequential  (key-like-string  $R $P $S) (=  $S ([|]  : $T)) (atom-chars  $O $R) (=  $U $T) no-more (=  $Q $U)))


 (=  (process-rest-line  $O $P id $Q $A10) (sequential  (get-some-items  (:: (item  $P)) $Q ()) no-more (assert-OBO  id-type $P $O) no-more))
 (=  (process-rest-line  $A10 $O $P $B10 $Q) (sequential  (member  $P (:: name comment)) (assert-OBO  $P $O $Q) no-more))
 (=  (process-rest-line  $O $P relationship $Q $A10) (sequential  no-more (must-det-ll  (key-like-string  $R $Q $S)) (must-det-ll  (atom-chars  $T $R)) (must-det-ll  (into-rest  $S $U $V)) (must-det-ll  (process-rest-line  $O $P $T $U $V))))
 (=  (process-rest-line  $A10 $O $P $Q $B10) (sequential  (missing  (and  (member  $R $Q) (member  $R (:: no-more "[ ")))) (if-then-else  (missing  (member    $Q)) (atom-chars  $S $Q) (string-chars  $S $Q)) (assert-OBO  $P $O $S) no-more))
 (=  (process-rest-line  $A10 $O is-a $P $Q) (sequential  (member  no-more $P) (atomic-list-concat  (:: $R $S) no-more $Q) (normalize-space  (atom  $T) $R) (normalize-space  (string  $U) $S) (assert-OBO  is-a $O $T) (assert-OBO  name $T $U) no-more))
 (=  (process-rest-line  $A10 $O $P $Q $B10) (sequential  (get-some-items  $R $Q ()) (--aux-maplist/3-arg+1  $R $S (ValueAtom 1)) (equals-ListMappingFn  $T ([|]  $P ([|]  $O $S))) (assert-OBO  $T) no-more))
 (=  (process-rest-line  $O $P $Q $R $S) (sequential  (pp-fb  (process-rest-line  $O $P $Q $R $S)) no-more))


 (=  (process-stream-chars  $A10 $B10 (:: e n d - o f - f i l e) $C10) (:: no-more))
 (=  (process-stream-chars  $O $A10 () $B10) (sequential  no-more (process-stream  $O $C10 $D10)))
 (=  (process-stream-chars  $O $A10 ([|]  [ $P) $B10) (sequential  no-more (must-det-ll  (append  $Q (:: ]) $P)) (must-det-ll  (atom-chars  $R $Q)) no-more (must-det-ll  (nb-setval  obo-type $R)) (must-det-ll  (flag  loaded-from-file $S (+  $S (ValueAtom 1)))) (must-det-ll  (nop  (process-stream  $O $R $C10)))))
 (=  (process-stream-chars  $O $P $Q $A10) (sequential  (get-key  $R $Q $S) (==  $R id) (into-rest  $S $T $B10) (atom-chars  $U $T) (assert-OBO  id-type $U $P) (nb-setval  obo-id $U) (nb-setval  obo-type $P) (nop  (process-stream  $O $P $U))))
 (=  (process-stream-chars  $O $P $Q $R) (sequential  (must-det-ll  (get-key  $S $Q $T)) (must-det-ll  (into-rest  $T $U $V)) (must-det-ll  (process-rest-line  $P $R $S $U $V)) (nop  (process-stream  $O $P $R))))


 (=  (into-rest  $O $P $Q) (sequential  (string-chars  $R $O) (normalize-space  (chars  $P) $R) (string-chars  $Q $P)))


 (=  (process-stream  $O $A10 $B10) (sequential  (or  (at-end-of-stream  $O) reached-file-max) no-more))
 (=  (process-stream  $O $P $Q) (sequential  (must-det-ll  (read-line-to-string  $O $R)) (must-det-ll  (normalize-space  (chars  $S) $R)) (must-det-ll  (process-stream-chars  $O $P $S $Q))))


 (=  (process-stream-repeat  $O) (sequential  repeat (nb-current  obo-type $P) (nb-current  obo-id $Q) (->  (and  (read-line-to-string  $O $R) (and  (if-then-else  should-show-data (writeln  $R) True) (normalize-space  (chars  $S) $R))) True) (\==  $S ()) (->  (process-stream-chars  $O $P $S $Q) True) (if-then-else  (or  (at-end-of-stream  $O) reached-file-max) no-more False)))


 (=  (--aux-maplist/2-load-obo+0  ()) True)
 (=  (--aux-maplist/2-load-obo+0  ([|]  $O $P)) (sequential  (load-obo  $O) (--aux-maplist/2-load-obo+0  $P)))


 (=  (load-obo  $O) (sequential  (missing  (atomic  $O)) (absolute-file-name  $O $P (:: (read  exists) (extension  (:: ())) (file-type  directory) (file-errors  False) (solutions  first))) no-more (load-obo  $P)))
 (=  (load-obo  $O) (sequential  (missing  (atomic  $O)) no-more (absolute-file-name  $O $P (:: (read  exists) (extension  (:: ())) (file-errors  False) (solutions  first))) no-more (load-obo  $P)))
 (=  (load-obo  $O) (sequential  (atomic  $O) (missing  (exists-file  $O)) (expand-file-name  $O $P) (\==  $P ()) (\==  $P (:: $O)) (--aux-maplist/2-load-obo+0  $P)))
 (=  (load-obo  $O) (sequential  (atomic  $O) (exists-directory  $O) (directory-file-path  $O (StringValue "*.obo") $P) (expand-file-name  $P $Q) no-more (--aux-maplist/2-load-obo+0  $Q)))
 (=  (load-obo  $O) (sequential  (must-det-ll  (directory-file-path  $P $Q $O)) (must-det-ll  (file-name-extension  $R $A10 $Q)) (must-det-ll  (=  $S OntologyFile)) (must-det-ll  (assert-OBO  id-type $R $S)) (must-det-ll  (nb-setval  obo-id $R)) (must-det-ll  (nb-setval  obo-type $S)) (must-det-ll  (assert-OBO  pathname $R $O)) no-more (must-det-ll  (assert-OBO  basename $R $Q)) no-more (must-det-ll  (assert-OBO  directory $R $P)) no-more (setup-call-cleanup  (must-det-ll  (open  $O read $T)) (must-det-ll  (process-stream-repeat  $T)) (must-det-ll  (close  $T))) fb-stats))


 (=  load-obo (sequential  make (load-obo  (StringValue "./reqs/obonet/tests/data/*.obo")) (load-obo  (StringValue "./data/ftp.flybase.net/releases/current/precomputed-files/*/*.obo"))))


 (=  (assert-OBO  $O) (sequential  heartbeat (functor  $O $P $Q) (>=  $Q (ValueAtom 2)) (<  $Q (ValueAtom 700)) (decl-fb-pred  $P $Q) (loaded-from-file  $R) (equals-ListMappingFn  $O ([|]  $P $S)) (make-assertion  $P $S $T $U) no-more (if-then-else  (call  $T) True (and  (assert  $T) (and  (flag  total-loaded-atoms $V (+  $V (ValueAtom 1))) (if-then-else  (and  should-show-data (and  nl (and  nl (and  (fbug  (=  (newData  $R) $T)) (if-then-else  (and  (\==  $U $S) (fbug  (=  (oldData  $R) $U))) True True))))) True True)))) no-more))


 (=  (assert-OBO  $O $P $Q) (:: (assert-OBO  (ontology-info  $O $P $Q))))


 (=  should-show-data (sequential  (loaded-from-file  $O) (->  (or  (and  (=<  $O (ValueAtom 13)) (>=  $O (ValueAtom 10))) (and  (>  $O (ValueAtom 0)) (is  (ValueAtom 0) (rem  $O (ValueAtom 1000000))))) True) (format  user-error ~N ()) (format  user-output ~N ())))


 (=  should-sample (sequential  (->  (or  (option-value  samples-per-million $O) (=  $O (ValueAtom 50))) True) (loaded-from-file  $P) (is  $Q (mod  $P (ValueAtom 1000000))) (>=  $Q (ValueAtom 0)) (=<  $Q $O)))


 (=  reached-file-max (sequential  (loaded-from-file  $O) (option-value  max-per-file $P) (>=  $O $P)))


 (=  should-cache (sequential  (loaded-from-file  $O) (option-else  max-disk-cache $P (ValueAtom 1000)) (=<  $O $P)))


 (=  (loaded-from-file  $O) (:: (flag  loaded-from-file $O $O)))








 (=  (decl-fb-pred  $O $P) (:: (if-then-else  (fb-pred  $O $P) True (and  (dynamic  (/  $O $P)) (assert  (fb-pred  $O $P))))))


 (=  (decl-m-fb-pred  $O $P $Q) (:: (if-then-else  (mod-f-a  $O $P $Q) True (and  (dynamic  (:  $O (/  $P $Q))) (assert  (mod-f-a  $O $P $Q))))))





 (=  (match  (:: $O)) (sequential  no-more (call  $O)))
 (=  (match  (::  $O $P)) (sequential  no-more (call  $O) (match  $P)))
 (=  (match  $O) (:: (call  $O)))


 (=  cleanup-arities (:: (missing  (and  (fb-pred  $O (ValueAtom 2)) (and  (fb-pred  $O $P) (and  (>  $P (ValueAtom 2)) (missing  (retract  (fb-pred  $O (ValueAtom 2))))))))))


 (=  (assert-progress  $O $P) (sequential  (equals-ListMappingFn  $P (:: $Q $R $S)) (@>  $R $S) no-more (equals-ListMappingFn  $T (:: $Q $S $R)) no-more (assert-progress  $O $T)))
 (=  (assert-progress  $O $P) (sequential  (call  $P) no-more (pp-fb  (=  (already  $O) $P))))
 (=  (assert-progress  $O $P) (sequential  (pp-fb  (=  (assert-progress  $O) $P)) (assert  $P)))


 (=  (into-len  $O (ValueAtom 0)) (sequential  (var  $O) no-more))
 (=  (into-len  $O $P) (sequential  (missing  (string  $O)) no-more (sformat  $Q (StringValue "~w") (:: $O)) (into-len  $Q $P)))
 (=  (into-len  $O (+  (ValueAtom 0) $O)) (:: (atom-contains  $O (StringValue " "))))
 (=  (into-len  $O (+  $P $O)) (sequential  (atom-length  $O $Q) (if-then-else  (==  $Q (ValueAtom 11)) (=  $P (ValueAtom 0)) (is  $P (-  $Q)))))


 (=  (longest-first  $O $P $Q) (sequential  (into-len  $P $R) (into-len  $Q $S) (compare  $O $S $R)))


 (=  (extract-concepts  $O $P) (sequential  (findall  $Q (and  (sub-term  $Q $O) (and  (atomic  $Q) (good-concept  $Q))) $R) (predsort  longest-first $R $P)))


 (=  (some-english  $O) (:: (if-then-else  (and  (fb-arg-table-n  $P $Q $R) (and  (missing  (no-english  $Q $R)) (and  (is-englishy  $P) (and  (make-atom  $Q $R $S $T) (and  (arg  $U $S $O) (and  (\==  $U $R) (and  (call  $S) (and  (\==  $T ()) (and  no-more (pp-fb  (=  $O $T))))))))))) True True)))


 (=  (is-englishy  $O) (sequential  (missing  (atom  $O)) (missing  (string  $O)) no-more False))
 (=  (is-englishy  $O) (:: (split-string  $O (StringValue ". ") (StringValue " ") ([|]  $A10 ([|]  $B10 ([|]  $C10 $D10))))))
 (=  (is-englishy  $O) (:: (atom-contains  $O (StringValue ". "))))


 (=  (--aux-maplist/2-some-english+0  ()) True)
 (=  (--aux-maplist/2-some-english+0  ([|]  $O $P)) (sequential  (some-english  $O) (--aux-maplist/2-some-english+0  $P)))


 (=  (maybe-english  $A10 $O) (sequential  (select  $P $O $Q) (is-englishy  $P) (member  $R $Q) (is-englishy  $R) no-more (pp-fb  (=  english (:: $P $R)))))
 (=  (maybe-english  $A10 $O) (sequential  (pp-fb  (=  concepts $O)) (--aux-maplist/2-some-english+0  $O)))


 (=  (maybe-english  $O) (sequential  (extract-concepts  $O $P) no-more (if-then-else  (maybe-english  $O $P) True True) no-more))


 (=  (no-english  fbrf-pmid-pmcid-doi $A10) True)
 (=  (no-english  physical-interactions-mitab (ValueAtom 8)) True)


 (=  (try-overlaps  $O) (sequential  (synth-query  $O $P) (missing  (missing  (and  (match  $P) (and  (pp-fb  (=  grounded $P)) (if-then-else  (maybe-english  $P) True True))))) nl nl (pp-fb  (=  ungrounded !$P)) nl nl nl))


 (=  try-overlaps (:: (try-overlaps  (ValueAtom 5))))


 (=  (ok-if-dupped  best-gene-summary) True)


 (=  (make-atom  $O) (sequential  (fb-pred-g  $P $Q) (functor  $O $P $Q)))


 (=  (all-dif-functors  $O) (:: (missing  (and  (select  $P $O $Q) (and  (member  $R $Q) (and  (functor  $P $S $A10) (and  (functor  $R $T $B10) (and  (==  $S $T) (missing  (ok-if-dupped  $S))))))))))


 (=  (synth-query  $A10 (ValueAtom 1) (:: $O)) (sequential  no-more (make-atom  $O)))
 (=  (synth-query  $O $P ([|]  $Q ([|]  $R $S))) (sequential  (is  $T (+  $P (ValueAtom -1))) (synth-conj  $O $Q $R) (if-then-else  (>  $T (ValueAtom 1)) (dif  $O $U) True) (synth-query  $U $T ([|]  $R $S)) (all-dif-functors  ([|]  $Q ([|]  $R $S)))))


 (=  (synth-query  $O $P) (:: (synth-query  $A10 $O $P)))


 (=  (synth-conj  $O $P $Q) (sequential  (maybe-corisponds  (ConceptMapFn  $R $S $T) (ConceptMapFn  $U $V $W)) (make-atom  $T $S $P $X) (make-atom  $W $V $Q $Y) (\=@=  $T $W) (skip  $R) (skip  $U) (=  $X $Y) (=  $O $X)))


 (=  (make-atom  $O $P $Q $R) (sequential  (fb-pred-g  $O $S) (functor  $Q $O $S) (arg  $P $Q $R)))


 (=  (table-column-type  $O $P $Q) (sequential  (table-n-type  $O $P $R $S) (if-then-else  (nonvar  $S) (=  $Q $S) (=  $Q $R))))


 (=  (fb-two-preds  $O $P $Q $R $S $T) (sequential  (fb-pred-g  $O $Q) (fb-pred-g  $R $T) (@>=  $O $R)
      (between  (ValueAtom 1) $Q $P) (between  (ValueAtom 1) $T $S) (if-then-else  (==  $O $R) (>  $P $S) True)))
 (=  (fb-two-preds  $O $P $Q $R $S $T) (sequential  (fb-pred-g  $O $Q) (fb-pred-g  $R $T) (@>  $O $R) (mine-typelevel-overlaps  $A10 (ConceptMapFn  $B10 $P $O) (ConceptMapFn  $C10 $S $R))))


 (=  mine-atomspace-overlaps (sequential  (fb-two-preds  $O $P $Q $R $S $T)
     (->  (and  (functor  $U $O $Q) (and  (functor  $V $R $T) (and  (call  $U) (and  (arg  $P $U $W) (and  (good-concept  $W) (and  (arg  $S $V $W) (call  $V))))))) True) (->  (and  (table-column-type  $O $P $X) (and  (nonvar  $X) (and  (table-column-type  $R $S $Y) (nonvar  $X)))) True) (assert-progress  $W (maybe-corisponds  (ConceptMapFn  $X $P $O) (ConceptMapFn  $Y $S $R)))))


 (=  (mine-typelevel-overlaps  $O (ConceptMapFn  $P $Q $R) (ConceptMapFn  $S $T $U)) (sequential  (=  $P $O) (=  $S $O) (table-columns  $R $V) (table-columns  $U $W) (fb-pred-g  $R $X) (fb-pred-g  $U $Y) (@>  $R $U) (nth1  $Q $V $O) (good-concept  $O) (->  (and  (nth1  $T $W $O) (and  (length  $V $X) (length  $W $Y))) True)))


 (=  mine-typelevel-overlaps (:: (missing  (and  (mine-typelevel-overlaps  $O $P $Q) (missing  (assert-progress  (mine-typelevel-overlaps  $O) (maybe-corisponds  $P $Q)))))))


 (=  mine-overlaps2-slow (:: (missing  (and  mine-atomspace-overlaps (missing  True)))))


 (=  mine-overlaps1 (:: (missing  (and  (mine-corisponds  $O $P) (missing  (assert-progress  (mine-overlaps1  $O) $P))))))


 (=  mine-overlaps (sequential  (retractall  (maybe-corisponds  $A10 $B10)) (time
    (->  mine-overlaps1 True)) (skip  mine-overlaps2)))


 (=  (mine-corisponds  $O $P)
      (sequential  (fb-arg-table-n  $O $Q $R) (is-good-atom-name  $O)
                  (fb-arg-table-n  $O $S $T) (>  (+  $Q $R) (+  $S $T)) (->  (and  (table-column-type  $Q $R $U)
		   (and  (nonvar  $U) (and  (table-column-type  $S $T $V) (nonvar  $V)))) True) (=  $P (maybe-corisponds  (ConceptMapFn  $U $R $Q) (ConceptMapFn  $V $T $S)))))


 (=  (fb-pred-g  $O $P) (sequential  (fb-pred  $O $P) (missing  (skipped-anotations  $O))))


 (=  (is-good-atom-name  $O) (sequential  (atom  $O) (atom-length  $O $P) (>=  $P (ValueAtom 2)) (missing  (atom-number  $O $A10))))





 (=  (good-concept  $O) (sequential  (var  $O) no-more (table-columns  $P $Q) (nth1  $R $Q $S) (or  (=  $O $S) (=  $O (nth  $R $P)))))
 (=  (good-concept  $O) (sequential  (atom  $O) no-more (is-good-atom-name  $O)))
 (=  (good-concept  $O) (sequential  (number  $O) no-more (>  $O (ValueAtom 300))))
 (=  (good-concept  (ListValue $O)) (sequential  (good-concept  $O) (atom  $O)))
 (=  (good-concept  (ListValue $O)) (sequential  (good-concept  $O) (atom  $O)))


 (=  (concept-type  $O $P) (sequential  (fb-arg  $O) (fb-arg-table-n  $O $Q $R) (table-n-type  $Q $R $P)))


 (=  (atom-prefix  $O flybase $P) (:: (flybase-identifier  $O $P)))
 (=  (atom-prefix  GO obo Gene Ontology) True)
 (=  (atom-prefix  PO obo Plant Ontology) True)
 (=  (atom-prefix  DOID obo Disease Ontology) True)
 (=  (atom-prefix  UBERON obo Uber-anatomy ontology) True)
 (=  (atom-prefix  CHEBI obo (StringValue "Chemical Entities of Biological Interest")) True)


 (=  (flybase-identifier  FBab Aberration) True)
 (=  (flybase-identifier  FBal Allele) True)
 (=  (flybase-identifier  FBba Balancer) True)
 (=  (flybase-identifier  FBbt AnatomyTerm) True)
 (=  (flybase-identifier  FBch ChromosomeArm) True)
 (=  (flybase-identifier  FBcl Clone) True)
 (=  (flybase-identifier  FBcv ControlledVocabulary) True)
 (=  (flybase-identifier  FBdv DevelopmentalStageTerm) True)
 (=  (flybase-identifier  FBgg GeneGroup) True)
 (=  (flybase-identifier  FBgn Gene) True)
 (=  (flybase-identifier  FBhh HumanDisease) True)
 (=  (flybase-identifier  FBig GeneInteraction) True)
 (=  (flybase-identifier  FBim Image) True)
 (=  (flybase-identifier  FBlc LargeDatasetMetadata) True)
 (=  (flybase-identifier  FBmc MolecularConstruct) True)
 (=  (flybase-identifier  FBms MolecularSegment) True)
 (=  (flybase-identifier  FBpl Probe) True)
 (=  (flybase-identifier  FBpp Polypeptide) True)
 (=  (flybase-identifier  FBrf Reference) True)
 (=  (flybase-identifier  FBsf SequenceFeature) True)
 (=  (flybase-identifier  FBsn GeneStrain) True)
 (=  (flybase-identifier  FBst GeneStock) True)
 (=  (flybase-identifier  FBtc CellLine) True)
 (=  (flybase-identifier  FBti TransposableIlementInsertion) True)
 (=  (flybase-identifier  FBto ExperimentalTool) True)
 (=  (flybase-identifier  FBte TransgenicElement) True)
 (=  (flybase-identifier  FBtp Transposon) True)
 (=  (flybase-identifier  FBtr Transcript) True)


 (=  (term-number  $O $P) (sequential  (sub-term  $P $O) (number  $P)))


 (=  (post-statistic  $O $P) (sequential  (statistics  $O $Q) (term-number  $Q $R) (pre-statistic  $O $S) (is  $T (-  $R $S)) (if-then-else  (<  $T (ValueAtom 0)) (=  $P (ValueAtom 0)) (=  $T $P))))


 (=  (pre-statistic  $O $P) (:: (if-then-else  (is-pre-statistic  $O $P) True (=  $P (ValueAtom 0)))))


 (=  (save-pre-statistic  $O) (:: (if-then-else  (is-pre-statistic  $O $A10) True (and  (statistics  $O $P) (and  (term-number  $P $Q) (assert  (is-pre-statistic  $O $Q)))))))


 (=  (is-pre-statistic  memory (ValueAtom 290515568)) True)
 (=  (is-pre-statistic  atoms (ValueAtom 32862)) True)
 (=  (is-pre-statistic  atom-space (ValueAtom 3959268)) True)


 (=  (catch-i  $O) (:: (if-then-else  (catch  $O $A10 True) True True)))


 (=  (catch-ignore  $O) (:: (if-then-else  (catch  $O $P (catch-i  (and  nl (and  (writeq  (=  $O $P)) nl)))) True True)))


 (=  kaggle-arc (sequential  (missing  (exists-directory  /opt/logicmoo-workspace/packs-sys/logicmoo-agi/prolog/kaggle-arc/)) no-more))
 (=  kaggle-arc (:: (with-option  argv (:: --libonly) (with-cwd  /opt/logicmoo-workspace/packs-sys/logicmoo-agi/prolog/kaggle-arc/ (ensure-loaded  kaggle-arc)))))


 (=  (set-option-value  $O $P) (sequential  (catch  (nb-setval  $O $P) $A10 True) (catch  (create-meTTa-flag  $O $P (:: (keep  false) (access  read-write) (type  term))) $B10 True) (catch  (set-meTTa-flag  $O $P) $C10 True)))


 (=  (option-value  $O $P) (:: (option-else  $O $P ())))


 (=  (option-else  $O $P $A10) (sequential  (was-option-value  $O $Q) no-more (=  $Q $P)))
 (=  (option-else  $A10 $O $P) (sequential  no-more (=  $O $P)))


 (=  (was-option-value  $O $P) (sequential  (nb-current  $O $Q) no-more (=  $P $Q)))
 (=  (was-option-value  $O $P) (sequential  (current-meTTa-flag  $O $Q) no-more (=  $P $Q)))
 (=  (was-option-value  $O $P) (sequential  (meTTa-load-context  $O $Q) no-more (=  $P $Q)))


 (=  (with-option  $O $P $Q) (sequential  (option-value  $O $R) (setup-call-cleanup  (set-option-value  $O $P) $Q (set-option-value  $O $R))))


 (=  (with-option  () $O) (sequential  no-more (call  $O)))
 (=  (with-option  ([|]  $O $P) $Q) (sequential  no-more (with-option  $O (with-option  $P $Q))))
 (=  (with-option  (=  $O $P) $Q) (sequential  no-more (with-option  $O $P $Q)))
 (=  (with-option  $O $P) (sequential  (compound  $O) (equals-ListMappingFn  $O (:: $Q $R)) no-more (with-option  $Q $R $P)))
 (=  (with-option  $O $P) (:: (with-option  $O True $P)))


 (=  (with-cwd  $O $P) (:: (setup-call-cleanup  (working-directory  $Q $O) $P (working-directory  $A10 $Q))))


 (=  (skip  $A10) True)
