/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%*********************************************************************************************
% PROGRAM FUNCTION: converts FlyBase biological data from various file formats (TSV, JSON, OBO, etc.)
% into Prolog facts and MeTTa/datalog representations while handling data type conversions and
% maintaining statistics about loaded content.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* mined
; Total         Atoms (Atomspace size): .................................................. 56,354,849
;               ConceptNodes: ............................................................. 9,472,616
;               Bytes Per Atom (Average): ....................................................... 140
;               Bytes Per ConceptNode (Average): ................................................ 120
;               Relational Memory: ............................................................ 7.39G
;               ConceptNode Memory: ........................................................... 1.07G
;               Atoms per minute: ......................................................... 3,491,880
;               Total Physical Memory Used: ................................................... 9.08G
;               Runtime (days:hh:mm:ss): ................................................. 0:00:16:08


; Total         Atoms (Atomspace size): .................................................. 38,812,356
;               ConceptNodes: ............................................................. 9,380,821
;               Total Memory Used: ............................................................ 8.26G
;               Runtime (days:hh:mm:ss): ................................................. 0:00:19:15


; Total         Atoms (Atomspace size): .................................................. 38,822,366
;               ConceptNodes: ............................................................. 9,824,355
;               Random samples: ................................................................. 805
;               Total Memory Used: ............................................................ 8.18G
;               Runtime (days:hh:mm:ss): ................................................. 0:00:08:28

*/

%!  recount_total_loaded_symbols is det.
%
%   Resets and updates the counter for the total number of loaded symbols
%   in the system. This predicate uses a flag-based mechanism to manage the count.
%
%   The `total_loaded_symbols` flag is first reset to `0`. The current count of
%   fully loaded atoms is retrieved using `full_atom_count/1`, and this value is
%   stored back in the `total_loaded_symbols` flag.
%
%   @details
%   This predicate ensures that the system maintains an accurate record of the
%   loaded symbols by fetching the current state from `full_atom_count/1`.
%
%   @example
%     % Recount and update the total number of loaded symbols:
%     ?- recount_total_loaded_symbols.
%
%     % Check the updated count of total loaded symbols:
%     ?- flag(total_loaded_symbols, Current, Current).
%     Current = <updated_count>.
%
recount_total_loaded_symbols :-
    % Reset the `total_loaded_symbols` flag to zero.
    flag(total_loaded_symbols, _, 0),
    % Retrieve the current count of fully loaded atoms.
    full_atom_count(Was),
    % Update the `total_loaded_symbols` flag with the retrieved count.
    flag(total_loaded_symbols, _, Was).

%!  load_flybase is det.
%
%   Loads FlyBase data and converts it from CSV to Prolog format.
%
%   Depending on the Prolog environment (e.g., Scryer Prolog), the predicate
%   invokes the appropriate sequence of operations to load the data and prepare it
%   for use. This includes recounting total loaded symbols, cleaning up arities,
%   and generating statistics on the FlyBase data.
%
%   @details
%   - If the system is identified as Scryer Prolog (using `is_scryer/0`),
%     only the `load_flybase_files/0` operation is performed.
%   - In other environments, it processes FlyBase data by enabling the
%     `mettafiles` option, recompiling necessary components (`make`), recounting
%     symbols, loading files, cleaning up arities, and finally gathering statistics.
%
%   @example
%     % Load FlyBase data:
%     ?- load_flybase.
%
%     % Check if data has been properly loaded and processed.
%
load_flybase :-
    % Check if the system is Scryer Prolog.
    is_scryer, !,
    % Load FlyBase files directly in Scryer.
    load_flybase_files.
load_flybase :-
    % Enable `mettafiles` option as false and perform FlyBase data processing.
    with_option(mettafiles, false,
        ( make,
          recount_total_loaded_symbols,
          !,
          load_flybase_files,
          !,
          cleanup_arities,
          !,
          fb_stats)).

%!  load_flybase_dirs is det.
%
%   Loads FlyBase data from specific directories.
%
%   This predicate specifies multiple directories where FlyBase data may be located
%   and loads the data from these paths. It uses wildcards to match multiple files.
%
%   @details
%   The directories include:
%   - Current release DAS precomputed files (`./data/ftp.flybase.net/releases/current/das_precomputed/*`).
%   - Other precomputed files (`./precomputed_files/*`).
%   - Specific files containing "sv" in their paths (`./data/ftp.flybase.net/releases/current/./*sv`).
%
%   @example
%     % Load FlyBase data from specified directories:
%     ?- load_flybase_dirs.
%
load_flybase_dirs :-
    % Load FlyBase data from current DAS precomputed directory.
    load_flybase('./data/ftp.flybase.net/releases/current/das_precomputed/*'),
    % Load FlyBase data from precomputed files directory.
    load_flybase('./precomputed_files/*'),
    % Load FlyBase data from directories containing "sv".
    load_flybase('./data/ftp.flybase.net/releases/current/./*sv'), !.

file_search_path(fb_current,'/wam/data/FB_current/').
/*

find . -type f -exec wc -l {} \; | awk -F'/' '{
    dir=".";
    size=$1;
    file=$NF;
    for(i=2; i<NF; i++) dir=(dir "/" $i);
    print size, dir, file
}' | sort -k3,3 -k1,1n | awk '{
    number = sprintf("%\047d", $1);
    gsub(/,/, "_", number);
    printf "   load_data(%16s, fb_current(%s%s/%s%s)),\n", number, "\"", $3, $4, "\""
}' | sed -e "s/\"/'/g" -e 's|20.....|*|g'


*/

% Informs the system that the clauses of column_names/2 might not be together in the source file.
:- discontiguous column_names/2.

%!  load_data_fb_data is det.
%
%   Loads FlyBase data from multiple specified sources and formats.
%
%   This predicate sequentially invokes the `load_data/2` predicate for various
%   FlyBase data files. These files are categorized by type (e.g., `chado-xml`,
%   `dmel_r6.56`, `precomputed_files`) and format (e.g., `.dtd`, `.xml`, `.tsv`,
%   `.fasta`, `.obo`, `.json`). Each `load_data/2` invocation is parameterized
%   with a numeric identifier and the file path, indicating the estimated size
%   or order of loading.
%
%   @details
%   - The predicate is comprehensive, covering a wide range of data, including
%     annotations, ontologies, genetic interactions, gene expression data, and
%     more.
%   - It uses a uniform structure for loading, ensuring consistent handling of
%     all file types and sources.
%   - The final `!` ensures the loading process concludes deterministically.
%
%   @example
%     % Load all FlyBase data:
%     ?- load_data_fb_data.
%
%     % Verify that the data has been loaded successfully and is accessible
%     % in the Prolog environment.
%
load_data_fb_data:-
   load_data(              64, fb_current('./chado-xml/chado_FBim.dtd')),
   load_data(              76, fb_current('./chado-xml/chado_FBst.dtd')),
   load_data(              84, fb_current('./chado-xml/chado_FBch.dtd')),
   load_data(              94, fb_current('./chado-xml/chado_FBba.dtd')),
   load_data(              98, fb_current('./chado-xml/chado_FBco.dtd')),
   load_data(             102, fb_current('./chado-xml/chado_FBtc.dtd')),
   load_data(             104, fb_current('./chado-xml/chado_FBte.dtd')),
   load_data(             104, fb_current('./chado-xml/chado_FBto.dtd')),
   load_data(             116, fb_current('./chado-xml/chado_FBsn.dtd')),
   load_data(             118, fb_current('./chado-xml/chado_FBtr.dtd')),
   load_data(             128, fb_current('./chado-xml/chado_FBig.dtd')),
   load_data(             132, fb_current('./chado-xml/chado_FBgg.dtd')),
   load_data(             136, fb_current('./chado-xml/chado_FBab.dtd')),
   load_data(             136, fb_current('./chado-xml/chado_FBpp.dtd')),
   load_data(             144, fb_current('./chado-xml/chado_FBcl.dtd')),
   load_data(             154, fb_current('./chado-xml/chado_FBtp.dtd')),
   load_data(             156, fb_current('./chado-xml/chado_FBlc.dtd')),
   load_data(             156, fb_current('./chado-xml/chado_FBti.dtd')),
   load_data(             158, fb_current('./chado-xml/chado_FBsf.dtd')),
   load_data(             160, fb_current('./chado-xml/chado_FBal.dtd')),
   load_data(             162, fb_current('./chado-xml/chado_FBrf.dtd')),
   load_data(             172, fb_current('./chado-xml/chado_FBhh.dtd')),
   load_data(             234, fb_current('./chado-xml/chado_FBgn.dtd')),
   load_data(         466_642, fb_current('./chado-xml/chado_FBim.xml')),
   load_data(         504_174, fb_current('./chado-xml/chado_FBba.xml')),
   load_data(         664_653, fb_current('./chado-xml/chado_FBsn.xml')),
   load_data(         673_011, fb_current('./chado-xml/chado_FBtc.xml')),
   load_data(         703_891, fb_current('./chado-xml/chado_FBco.xml')),
   load_data(       2_609_997, fb_current('./chado-xml/chado_FBch.xml')),
   load_data(       7_339_713, fb_current('./chado-xml/chado_FBte.xml')),
   load_data(      14_830_637, fb_current('./chado-xml/chado_FBgg.xml')),
   load_data(      20_919_950, fb_current('./chado-xml/chado_FBst.xml')),
   load_data(      26_767_347, fb_current('./chado-xml/chado_FBig.xml')),
   load_data(      26_818_296, fb_current('./chado-xml/chado_FBab.xml')),
   load_data(      37_216_220, fb_current('./chado-xml/chado_FBpp.xml')),
   load_data(      55_429_842, fb_current('./chado-xml/chado_FBhh.xml')),
   load_data(      82_559_962, fb_current('./chado-xml/chado_FBto.xml')),
   load_data(      95_417_605, fb_current('./chado-xml/chado_FBtr.xml')),
   load_data(     139_237_689, fb_current('./chado-xml/chado_FBti.xml')),
   load_data(     168_912_941, fb_current('./chado-xml/chado_FBrf.xml')),
   load_data(     259_648_210, fb_current('./chado-xml/chado_FBal.xml')),
   load_data(     274_962_987, fb_current('./chado-xml/chado_FBtp.xml')),
   load_data(     392_245_861, fb_current('./chado-xml/chado_FBsf.xml')),
   load_data(     588_536_952, fb_current('./chado-xml/chado_FBcl.xml')),
   load_data(   1_287_499_618, fb_current('./chado-xml/chado_FBlc.xml')),
   load_data(   5_155_405_685, fb_current('./chado-xml/chado_FBog.xml')),
   load_data(   5_354_257_324, fb_current('./chado-xml/chado_FBgn.xml')),
   load_data(         132_884, fb_current('./dmel_r6.56/chado-xml/chado_dmel_scaffolds.xml')),
   load_data(         568_549, fb_current('./dmel_r6.56/chado-xml/chado_dmel_intergenic.xml')),
   load_data(         586_821, fb_current('./dmel_r6.56/chado-xml/chado_dmel_repeat_regions.xml')),
   load_data(       2_893_700, fb_current('./dmel_r6.56/chado-xml/chado_dmel_syntenic.xml')),
   load_data(      30_716_865, fb_current('./dmel_r6.56/chado-xml/chado_dmel_misc_features.xml')),
   load_data(     108_816_559, fb_current('./dmel_r6.56/chado-xml/chado_dmel_predicted.xml')),
   load_data(     522_030_598, fb_current('./dmel_r6.56/chado-xml/chado_dmel_gene_models.xml')),
   load_data(   1_335_239_246, fb_current('./dmel_r6.56/chado-xml/chado_dmel_aligned.xml')),
   load_data(           1_870, fb_current('./dmel_r6.56/dna/dmel-raw_scaffolds-r6.56.tar')),
   load_data(             667, fb_current('./dmel_r6.56/fasta/dmel-all-tRNA-r6.56.fasta')),
   load_data(           1_676, fb_current('./dmel_r6.56/fasta/dmel-all-miRNA-r6.56.fasta')),
   load_data(           1_709, fb_current('./dmel_r6.56/fasta/dmel-all-miscRNA-r6.56.fasta')),
   load_data(           4_137, fb_current('./dmel_r6.56/fasta/dmel-all-pseudogene-r6.56.fasta')),
   load_data(          46_464, fb_current('./dmel_r6.56/fasta/dmel-all-ncRNA-r6.56.fasta')),
   load_data(         112_847, fb_current('./dmel_r6.56/fasta/dmel-all-transposon-r6.56.fasta')),
   load_data(         165_310, fb_current('./dmel_r6.56/fasta/dmel-all-five_prime_UTR-r6.56.fasta')),
   load_data(         273_148, fb_current('./dmel_r6.56/fasta/dmel-all-three_prime_UTR-r6.56.fasta')),
   load_data(         300_869, fb_current('./dmel_r6.56/fasta/dmel-all-translation-r6.56.fasta')),
   load_data(         563_767, fb_current('./dmel_r6.56/fasta/dmel-all-intergenic-r6.56.fasta')),
   load_data(         709_312, fb_current('./dmel_r6.56/fasta/dmel-all-exon-r6.56.fasta')),
   load_data(         811_702, fb_current('./dmel_r6.56/fasta/dmel-all-CDS-r6.56.fasta')),
   load_data(       1_158_900, fb_current('./dmel_r6.56/fasta/dmel-all-transcript-r6.56.fasta')),
   load_data(       1_190_673, fb_current('./dmel_r6.56/fasta/dmel-all-synteny-r6.56.fasta')),
   load_data(       1_312_171, fb_current('./dmel_r6.56/fasta/dmel-all-gene-r6.56.fasta')),
   load_data(       1_586_746, fb_current('./dmel_r6.56/fasta/dmel-all-intron-r6.56.fasta')),
   load_data(       1_799_389, fb_current('./dmel_r6.56/fasta/dmel-all-chromosome-r6.56.fasta')),
   load_data(       2_205_176, fb_current('./dmel_r6.56/fasta/dmel-all-gene_extended2000-r6.56.fasta')),
   load_data(       6_577_953, fb_current('./dmel_r6.56/fasta/dmel-all-predicted-r6.56.fasta')),
   load_data(       7_128_567, fb_current('./dmel_r6.56/fasta/dmel-all-clones-r6.56.fasta')),
   load_data(      10_205_467, fb_current('./dmel_r6.56/fasta/dmel-all-sequence_features-r6.56.fasta')),
   load_data(      75_894_094, fb_current('./dmel_r6.56/fasta/dmel-all-aligned-r6.56.fasta')),
   load_data(      33_369_054, fb_current('./dmel_r6.56/gff/dmel-all-r6.56.gff')),
   load_data(         549_131, fb_current('./dmel_r6.56/gtf/dmel-all-r6.56.gtf')),
   load_data(         293_958, fb_current('./precomputed_files/alleles/fbal_to_fbgn_fb_*.tsv')),
   load_data(         367_019, fb_current('./precomputed_files/alleles/allele_genetic_interactions_fb_*.tsv')),
   load_data(         384_970, fb_current('./precomputed_files/alleles/genotype_phenotype_data_fb_*.tsv')),
   load_data(          50_407, fb_current('./precomputed_files/clones/genomic_clone_data_fb_*.tsv')),
   load_data(         722_577, fb_current('./precomputed_files/clones/cDNA_clone_data_fb_*.tsv')),
   load_data(          18_864, fb_current('./precomputed_files/collaborators/gp_information.fb')),
   load_data(          28_237, fb_current('./precomputed_files/collaborators/fbgn_uniprot_fb_*.tsv')),
   load_data(       2_291_091, fb_current('./precomputed_files/collaborators/pmid_fbgn_uniprot_fb_*.tsv')),
   load_data(             177, fb_current('./precomputed_files/genes/fbgn_gleanr_fb_*.tsv')),
   load_data(             477, fb_current('./precomputed_files/genes/gene_functional_complementation_fb_*.tsv')),
   load_data(           1_022, fb_current('./precomputed_files/genes/pathway_group_data_fb_*.tsv')),
   load_data(           1_804, fb_current('./precomputed_files/genes/gene_groups_HGNC_fb_*.tsv')),
   load_data(           3_971, fb_current('./precomputed_files/genes/Dmel_enzyme_data_fb_*.tsv')),
   load_data(          11_307, fb_current('./precomputed_files/genes/gene_group_data_fb_*.tsv')),
   load_data(          12_497, fb_current('./precomputed_files/genes/fbgn_exons2affy1_overlaps.tsv')),
   load_data(          13_746, fb_current('./precomputed_files/genes/fbgn_exons2affy2_overlaps.tsv')),
   load_data(          13_750, fb_current('./precomputed_files/genes/best_gene_summary_fb_*.tsv')),
   load_data(          13_994, fb_current('./precomputed_files/genes/gene_snapshots_fb_*.tsv')),
   load_data(          14_303, fb_current('./precomputed_files/genes/FlyCellAtlas_slimmed_gene_expression_fb_*.tsv')),
   load_data(          17_770, fb_current('./precomputed_files/genes/gene_rpkm_matrix_fb_*.tsv')),
   load_data(          17_877, fb_current('./precomputed_files/genes/fbgn_annotation_ID_fb_*.tsv')),
   load_data(          17_877, fb_current('./precomputed_files/genes/fbgn_annotation_ID.tsv')),
   load_data(          20_469, fb_current('./precomputed_files/genes/gene_genetic_interactions_fb_*.tsv')),
   load_data(          22_461, fb_current('./precomputed_files/genes/dmel_unique_protein_isoforms_fb_*.tsv')),
   load_data(          34_826, fb_current('./precomputed_files/genes/gene_map_table_fb_*.tsv')),
   load_data(          35_708, fb_current('./precomputed_files/genes/fbgn_fbtr_fbpp_expanded_fb_*.tsv')),
   load_data(          35_708, fb_current('./precomputed_files/genes/fbgn_fbtr_fbpp_fb_*.tsv')),
   load_data(          38_524, fb_current('./precomputed_files/genes/dmel_gene_sequence_ontology_annotations_fb_*.tsv')),
   load_data(          40_044, fb_current('./precomputed_files/genes/automated_gene_summaries_fb_*.tsv')),
   load_data(          40_044, fb_current('./precomputed_files/genes/automated_gene_summaries.tsv')),
   load_data(          52_694, fb_current('./precomputed_files/genes/physical_interactions_mitab_fb_*.tsv')),
   load_data(         211_391, fb_current('./precomputed_files/genes/ncRNA_genes_fb_*.json')),
   load_data(         946_090, fb_current('./precomputed_files/genes/fbgn_NAseq_Uniprot_fb_*.tsv')),
   load_data(       2_188_277, fb_current('./precomputed_files/genes/gene_rpkm_report_fb_*.tsv')),
   load_data(       2_966_086, fb_current('./precomputed_files/genes/high-throughput_gene_expression_fb_*.tsv')),
   load_data(      14_765_500, fb_current('./precomputed_files/genes/scRNA-Seq_gene_expression_fb_*.tsv')),
   load_data(         134_598, fb_current('./precomputed_files/go/gene_association.fb')),
   load_data(          27_686, fb_current('./precomputed_files/human_disease/disease_model_annotations_fb_*.tsv')),
   load_data(           2_632, fb_current('./precomputed_files/insertions/construct_maps.zip')),
   load_data(          46_116, fb_current('./precomputed_files/insertions/fu_gal4_table_fb_*.json')),
   load_data(         218_236, fb_current('./precomputed_files/insertions/insertion_mapping_fb_*.tsv')),
   load_data(             602, fb_current('./precomputed_files/map_conversion/cyto-genetic-seq.tsv')),
   load_data(             607, fb_current('./precomputed_files/map_conversion/cytotable.txt')),
   load_data(           5_040, fb_current('./precomputed_files/map_conversion/genome-cyto-seq.txt')),
   load_data(      23_576_029, fb_current('./precomputed_files/metadata/dataset_metadata_fb_*.tsv')),
   load_data(             607, fb_current('./precomputed_files/ontologies/flybase_stock_vocabulary.obo')),
   load_data(           1_351, fb_current('./precomputed_files/ontologies/image.obo')),
   load_data(           2_038, fb_current('./precomputed_files/ontologies/fly_development.obo')),
   load_data(          10_348, fb_current('./precomputed_files/ontologies/flybase_controlled_vocabulary.obo')),
   load_data(          12_691, fb_current('./precomputed_files/ontologies/psi-mi.obo')),
   load_data(          14_673, fb_current('./precomputed_files/ontologies/gene_group_FB*.obo')),
   load_data(          24_773, fb_current('./precomputed_files/ontologies/so-simple.obo')),
   load_data(          24_940, fb_current('./precomputed_files/ontologies/slice.chebi.obo')),
   load_data(         157_356, fb_current('./precomputed_files/ontologies/doid.obo')),
   load_data(         263_738, fb_current('./precomputed_files/ontologies/fly_anatomy.obo')),
   load_data(         547_441, fb_current('./precomputed_files/ontologies/go-basic.obo')),
   load_data(       3_468_781, fb_current('./precomputed_files/ontologies/chebi_fb_*.obo')),
   load_data(          35_114, fb_current('./precomputed_files/orthologs/dmel_human_orthologs_disease_fb_*.tsv')),
   load_data(         223_061, fb_current('./precomputed_files/orthologs/dmel_paralogs_fb_*.tsv')),
   load_data(         100_685, fb_current('./precomputed_files/references/fbrf_pmid_pmcid_doi_fb_*.tsv')),
   load_data(       5_075_301, fb_current('./precomputed_files/references/entity_publication_fb_*.tsv')),
   load_data(          59_754, fb_current('./precomputed_files/species/organism_list_fb_*.tsv')),
   load_data(         178_104, fb_current('./precomputed_files/stocks/stocks_FB*.tsv')),
   load_data(         807_717, fb_current('./precomputed_files/synonyms/fb_synonym_fb_*.tsv')),
   load_data(             924, fb_current('./precomputed_files/transposons/transposon_sequence_set.gff')),
   load_data(          13_574, fb_current('./precomputed_files/transposons/transposon_sequence_set.fa')),
    !.


%:- ensure_loaded(obo_loader).
%

%!  create_flybase_qlf is det.
%
%   Creates a compiled FlyBase file in QLF format using SWI-Prolog.
%
%   This predicate invokes the `swipl` shell command with the goal of
%   compiling the `whole_flybase` file into QLF format. This allows for
%   efficient loading and querying in SWI-Prolog.
%
%   @example
%     % Compile FlyBase into QLF format:
%     ?- create_flybase_qlf.
%
create_flybase_qlf :-
    shell('swipl -g "qcompile(whole_flybase)').

%!  create_flybase_pl(+FtpDir) is det.
%
%   Creates a FlyBase Prolog database file.
%
%   If the argument `FtpDir` is provided, it is ignored, and the base predicate
%   `create_flybase_pl/0` is invoked. This is a placeholder to maintain API
%   consistency but does not currently use the argument.
%
%   @arg FtpDir The directory containing FlyBase data (currently unused).
%
%   @example
%     % Create a FlyBase Prolog database:
%     ?- create_flybase_pl(_FtpDir).
%
create_flybase_pl(_FtpDir) :-
    create_flybase_pl.

%!  create_flybase_pl is det.
%
%   Processes and saves FlyBase data into a Prolog database file.
%
%   This predicate executes the full sequence of FlyBase data processing,
%   including:
%   - Preparing the environment with `all_data_once/0`.
%   - Loading the full FlyBase dataset with `load_flybase_full/0`.
%   - Completing the process with `all_data_done/0`.
%
%   This workflow ensures that the entire FlyBase dataset is processed and saved.
%
%   @example
%     % Process and save FlyBase data into a Prolog file:
%     ?- create_flybase_pl.
%
create_flybase_pl :-
    all_data_once,
    % all_metta_once,  % Uncomment if metta processing is required.
    load_flybase_full,
    all_data_done.
    % all_metta_done,  % Uncomment if metta processing is required.
    % shell('mv whole_metta.pl whole_flybase.pl'). % Uncomment if renaming is needed.

%!  create_flybase_pl_tiny is det.
%
%   Processes and saves a reduced FlyBase dataset into a Prolog database file.
%
%   This predicate executes a similar workflow to `create_flybase_pl/0`, but it
%   processes a smaller, reduced FlyBase dataset using `load_flybase_tiny/0`. The
%   output is saved as `tiny_flybase.pl`.
%
%   @example
%     % Process and save a tiny FlyBase dataset into a Prolog file:
%     ?- create_flybase_pl_tiny.
%
create_flybase_pl_tiny :-
    all_data_once,
    % all_metta_once,  % Uncomment if metta processing is required.
    load_flybase_tiny,
    all_data_done,
    % all_metta_done,  % Uncomment if metta processing is required.
    shell('mv whole_metta.pl tiny_flybase.pl').

%!  save_to_pl is det.
%
%   Saves the FlyBase data to a Prolog file.
%
%   This predicate writes all FlyBase predicates and their definitions to a file
%   named `flybase_metta.pl`. It iterates over all predicates matching `fb_pred(F, A)`
%   and uses `listing/1` to output their definitions.
%
%   @example
%     % Save FlyBase data to a Prolog file:
%     ?- save_to_pl.
%
save_to_pl :-
    % Open the file for writing.
    tell('flybase_metta.pl'),
    % List all predicates matching `fb_pred(F, A)` and write them to the file.
    forall(fb_pred(F, A), listing(F/A)),
    % Close the file.
    told.

%!  real_assert(+OBO) is det.
%
%   Asserts or processes the given object (`OBO`) in the appropriate context.
%
%   This predicate handles assertion in different scenarios:
%   - If the system is in conversion mode (`is_converting/0`) and not currently
%     loading a file, it throws an exception to avoid direct assertion.
%   - Otherwise, it attempts to process the object through `real_assert1/1`
%     and `real_assert2/1` for handling output or internal storage.
%
%   @arg OBO The object to be asserted or processed.
%
%   @example
%     % Assert or process an object:
%     ?- real_assert(my_object).
%
real_assert(OBO) :-
    % If in conversion mode but not loading a file, throw an exception.
    is_converting,\+ is_loading_file(_File), !,throw(real_assert(OBO)).
real_assert(OBO) :-
    % Process the object using `real_assert1/1` and `real_assert2/1`.
    ignore(real_assert1(OBO)),real_assert2(OBO).

% commented code for debugging or alternate behaviors:
% real_assert(OBO):- is_converting, !, print_src(OBO).

%!  real_assert1(+OBO) is det.
%
%   Writes the source representation of the object (`OBO`) to the `all_metta_to/1` stream.
%
%   @arg OBO The object whose source is to be written.
%
real_assert1(OBO) :- all_metta_to(Out),!,with_output_to(Out, print_src(OBO)).

%!  real_assert2(+OBO) is det.
%
%   Handles the secondary assertion of the object (`OBO`) for various contexts.
%   - Writes the canonical representation to the `all_data_to/1` stream.
%   - Throws an exception if in conversion mode and unhandled.
%   - Invokes `pfcAdd_Now/1` to add the object.
%
%   @arg OBO The object to process.
%
real_assert2(OBO) :-
    % Write the canonical representation to the `all_data_to/1` stream.
    all_data_to(Out),!,write_canonical(Out, OBO),!,writeln(Out, '.').
real_assert2(OBO) :-
    % Throw an exception if in conversion mode and unhandled.
    is_converting,!,throw(real_assert2(OBO)).
% commented code for alternative behaviors:
% real_assert2(OBO):- call(OBO),!.
real_assert2(OBO) :-
    % Add the object using `pfcAdd_Now/1`.
    pfcAdd_Now(OBO).

%!  print_src(+OBO) is det.
%
%   Prints the source representation of the given object (`OBO`).
%   - Decomposes the object into its compound representation and prints each part.
%
%   @arg OBO The object to be printed in its source form.
%
print_src(OBO) :- format('~N'),uncompound(OBO, Src),!,write_srcH(Src).

%!  write_srcH(+Src) is det.
%
%   Writes the compound representation of the source (`Src`) in a structured format.
%
%   @arg Src The source to be written.
%
write_srcH([F | Args]) :- write('( '),write_src(F),maplist(write_srcE, Args),writeln(' )').

%!  write_srcE(+Arg) is det.
%
%   Writes a single argument (`Arg`) in the source representation.
%
%   @arg Arg The argument to be written.
%
write_srcE(Arg) :-
    write(' '),
    write_src(Arg).

%!  is_loading_file(-File) is nondet.
%
%   Checks if a file is currently being loaded or saved.
%
%   @arg File The name of the file currently being processed.
%
is_loading_file(File) :- nb_current(loading_file, File),File \== [],!.
is_loading_file(File) :- nb_current(saving_file, File),File \== [].

%!  all_data_once is det.
%
%   Initializes and prepares the environment for data operations if not already loading a file.
%   - Opens the `whole_metta` file for saving.
%   - Sets up the output streams with UTF-8 encoding and disables discontiguous style checks.
%   - Lists all data predicates using `all_data_preds/0`.
%
all_data_once :-
    % If already loading a file, do nothing.
    is_loading_file(_File),
    !.
all_data_once :-
    % Set the saving file as `whole_metta`.
    nb_setval(saving_file, 'whole_metta'),
    % Prepare the output stream and write initial configurations.
    all_data_to(Out),
    writeln(Out, ':- encoding(utf8).'),
    writeln(Out, ':- style_check(-discontiguous).'),
    flush_output(Out),
    !,
    all_data_preds.

%!  all_data_preds is det.
%
%   Lists all data-related predicates to the current output stream.
%
all_data_preds :- all_data_to(Out),with_output_to(Out, all_data_preds0),!.
all_data_preds.

%!  all_data_preds0 is det.
%
%   Outputs specific predicate listings for data management and tracking.
%
all_data_preds0 :-
    listing_c(table_n_type/3),
    listing_c(load_state/2),
    listing_c(is_loaded_from_file_count/2),
    listing_c(fb_pred/2),
    listing_c(fb_arg_type/1),
    listing_c(fb_arg_table_n/3),
    listing_c(fb_arg/1),
    listing_c(done_reading/1),
    !.

% declare that is_all_data_to/2 may change during execution
:- dynamic(is_all_data_to/2).

%!  all_data_to(-Out) is det.
%
%   Determines or prepares the output stream (`Out`) for writing data.
%
%   This predicate ensures that data is directed to the appropriate output stream,
%   depending on the current state of the program. It handles cases such as switching
%   between files, closing previous streams, and creating new ones. The stream is
%   managed based on the association between the `is_all_data_to/2` and `is_loading_file/1`
%   predicates, which track the current output state.
%
%   @arg Out The output stream to be used. It can be an existing stream or a new
%            stream created based on the current file being loaded.
all_data_to(Out) :-
    % Case 1: Use an existing output stream if the current loading file matches the file
    % associated with the output stream.
    once((is_all_data_to(File1, Out1), is_loading_file(File2))),
    File1 == File2, !,
    Out = Out1.
all_data_to(Out) :-
    % Case 2: Switch to a new output file if the loading file has changed.
    is_all_data_to(File1, Out1),
    is_loading_file(File2), !,
    % Close the old output stream.
    close(Out1),
    % Construct the new temporary file name and open it for writing.
    atom_concat(File2, '.metta.datalog.tmp', File2Data),
    open(File2Data, write, Out2, [alias(all_data), encoding(utf8), lock(write)]),
    % Update the `is_all_data_to/2` fact to reflect the new file and stream.
    retract(is_all_data_to(File1, Out1)),
    assert(is_all_data_to(File2, Out2)),
    % Log the switch between files for debugging purposes.
    fbug(all_data_to_switch(File1, File2)), !,
    Out = Out2.
all_data_to(Out) :-
    % Case 3: Reuse an existing output stream if one is available.
    is_all_data_to(_File1, Out), !.
all_data_to(Out) :-
    % Case 4: Create a new output stream if no current stream exists and the system
    % is in a converting state.
    is_converting,
    is_loading_file(File2), !,
    % Construct the new temporary file name and open it for writing.
    atom_concat(File2, '.metta.datalog.tmp', File2Data),
    open(File2Data, write, Out2),
    % Store the new output stream for future reference.
    assert(is_all_data_to(File2, Out2)),
    % Log the creation of the new stream for debugging purposes.
    fbug(all_data_to(File2)), !,
    Out = Out2.

%!  all_data_done is det.
%
%   Finalizes all pending data operations by completing output stream management,
%   deleting temporary states, and ensuring clean termination of the process.
%
%   This predicate handles the final steps in data processing by:
%   - Running all predicates required to finalize the output data (`all_data_preds/0`).
%   - Deleting the temporary state associated with `saving_file` using `nb_delete/1`.
%   - Completing any remaining tasks in the "Metta" workflow with `all_metta_done/0`.
%   - Closing and cleaning up all active output streams that are tracked by
%     `is_all_data_to/2`.
%
%   This ensures that all resources are properly closed, leaving no dangling streams
%   or incomplete states.
all_data_done :-
    % Execute all predicates required to finalize the output data.
    all_data_preds,
    % Delete the non-backtrackable state associated with 'saving_file'.
    nb_delete(saving_file),
    % Finalize any additional tasks related to the "Metta" workflow.
    all_metta_done,
    % Close all output streams tracked by `is_all_data_to/2`.
    forall(
        retract(is_all_data_to(_, Out)),
        % Use catch_ignore/1 to suppress any exceptions that might occur during close/1.
        catch_ignore(close(Out))
    ).

% This group of directives ensures that the `all_data_done/0` predicate is executed
% when the system halts, but only if the condition `is_converting/0` holds true.
%
:- if(is_converting).
:- at_halt(all_data_done).
:- endif.

%!  listing_c(+Input) is det.
%
%   Outputs a Prolog listing for a predicate specified by its functor and arity.
%
%   This predicate generates a listing of all clauses for the predicate specified
%   by its functor and arity (`Input`) in a format suitable for reloading or debugging.
%   It also handles multifile and dynamic declarations and logs any errors that
%   occur during the listing process.
%
%   @arg Input The predicate specification in the form of `Functor/Arity`. `Functor`
%              is the predicate's name, and `Arity` is the number of arguments.
%
%   Behavior:
%   - Outputs the `:- multifile/1` declaration for the predicate to indicate that
%     it may be spread across multiple files.
%   - Outputs the `:- dynamic/1` declaration to indicate that the predicate can
%     be modified at runtime.
%   - Lists all the clauses of the predicate by iterating through its solutions
%     and formatting them as Prolog terms.
%   - If an error occurs during clause listing, it logs the issue with `fbug/1`.

listing_c(F/A) :-
    % Print the `:- multifile/1` declaration for the predicate.
    format('~N~q.~n', [:- multifile(F/A)]),
    % Print the `:- dynamic/1` declaration for the predicate.
    format('~q.~n', [:- dynamic(F/A)]),
    % Create a functor P with the specified functor (F) and arity (A).
    functor(P, F, A),
    % Attempt to list all clauses of the predicate P. If an error occurs,
    % log it using `fbug/1`.
    catch(
        % Iterate through all clauses of P and format them for output.
        forall(P, format('~q.~n', [P])),
        % Capture any exception that occurs during listing.
        E,
        % Log the error using `fbug/1`, specifying the problematic predicate.
        fbug(caused(F/A, E))
    ).

% 'dynamic' is used to indicate the predicate may be modified at runtime.
:- dynamic(is_all_metta_to/2).

%!  all_metta_to(-Out) is det.
%
%   Handles the output stream management for processing metta data files.
%   Ensures the correct output stream (`Out`) is used or switched as necessary
%   based on the current loading file.
%
%   This predicate manages temporary `.metta.tmp` files for output, ensuring they
%   are properly closed and reopened when switching between files. It also handles
%   asserting and retracting dynamic facts to keep track of active streams.
%
%   @arg Out The output stream that will be used for metta data.
%
%   @example
%   % Use the output stream associated with the current file being processed:
%   ?- all_metta_to(Out).
%   Out = <stream_reference>.
%
all_metta_to(Out) :-
    %   If the currently loaded file (`File2`) matches the file associated with the tracked
    %   metta output stream (`File1`), reuse the existing output stream (`Out1`) by unifying it
    %   with `Out`. This avoids opening a new stream unnecessarily.
    once((is_all_metta_to(File1, Out1),is_loading_file(File2))),File1 == File2, !,Out = Out1.
all_metta_to(Out) :-
    is_all_metta_to(File1, Out1),
    is_loading_file(File2), !,
    close(Out1),  % Close the previous stream.
    atom_concat(File2, '.metta.tmp', File2Data),
    open(File2Data, write, Out2, [alias(all_metta), encoding(utf8), lock(write)]),
    % Update dynamic facts to track the new stream.
    retract(is_all_metta_to(File1, Out1)),
    assert(is_all_metta_to(File2, Out2)),
    % Log the switch for debugging purposes.
    fbug(all_metta_to_switch(File1, File2)), !,
    Out = Out2.
all_metta_to(Out) :- is_all_metta_to(_File1, Out), !.
all_metta_to(Out) :-
    is_loading_file(File2), !,
    atom_concat(File2, '.metta.tmp', File2Data),
    open(File2Data, write, Out2),
    % Track the newly created stream.
    assert(is_all_metta_to(File2, Out2)),
    % Log the new stream for debugging purposes.
    fbug(all_metta_to(File2)), !,
    Out = Out2.

%!  all_metta_done is det.
%
%   Closes all open output streams associated with metta processing and cleans up
%   related dynamic facts.
%
%   This predicate ensures that any output stream being tracked via the `is_all_metta_to/2`
%   dynamic fact is safely closed. The `catch_ignore/1` wrapper is used to handle any
%   exceptions that might occur during the `close/1` operation without interrupting the process.
%
%   This is typically called as part of a cleanup routine to release resources.
all_metta_done:-
    %   Iterates over all tracked metta output streams (`is_all_metta_to/2` facts),
    %   retracts each fact, and safely closes the associated output stream (`Out`).
    %   The `catch_ignore/1` is used to handle any exceptions during the `close/1` operation
    %   to ensure the process continues uninterrupted, even if an error occurs.
    forall(retract(is_all_metta_to(_,Out)), catch_ignore(close(Out))).

%!  loaded_from_file_count(-Count) is det.
%
%   Retrieves the current count of files loaded.
%   The `flag/3` predicate is used to read the value of the `loaded_from_file_count` flag
%   without modifying it.
%
%   @arg Count The current count of files loaded.
%
%   @example
%   % Get the current count of loaded files:
%   ?- loaded_from_file_count(Count).
%   Count = 42.
loaded_from_file_count(X) :-
    flag(loaded_from_file_count, X, X).

%!  incr_file_count(-NewCount) is det.
%
%   Increments the count of files loaded and updates the count of total loaded symbols
%   and atoms. This ensures all relevant counters are updated simultaneously.
%
%   The `flag/3` predicate is used to atomically increment the counters:
%   - `loaded_from_file_count`: Number of files loaded.
%   - `total_loaded_symbols`: Total number of loaded symbols.
%   - `total_loaded_atoms`: Total number of loaded atoms.
%
%   @arg NewCount The updated count of files loaded.
%
%   @example
%   % Increment the file count and retrieve the new count:
%   ?- incr_file_count(NewCount).
%   NewCount = 43.
incr_file_count(X) :-
    flag(loaded_from_file_count, X, X + 1),
    flag(total_loaded_symbols, TA, TA + 1),
    flag(total_loaded_atoms, TA, TA + 1).

%!  should_cache is nondet.
%
%   Determines if caching should occur based on the current count of loaded files.
%   This predicate fails unless the loaded file count is less than or equal to the
%   maximum disk cache size (`max_disk_cache`).
%
%   The maximum cache size defaults to 1000 if not explicitly provided.
%
%   @example
%   % Check if caching should happen:
%   ?- should_cache.
%   false.  % Always fails because of the initial `fail/0`.
should_cache :-
    % Ensure this predicate always fails.
    fail,
    % Retrieve the current count of loaded files.
    loaded_from_file_count(X),
    % Check the maximum allowed disk cache size.
    option_else(max_disk_cache, Num, 1000),
    X =< Num.

%!  reached_file_max is nondet.
%
%   Determines if the maximum allowed file count (`max_per_file`) has been reached.
%   This predicate fails unless `loaded_from_file_count` exceeds the value specified
%   in the `max_per_file` option, provided it is neither infinite (`inf`) nor zero.
%
%   @example
%   % Check if the maximum file count is reached:
%   ?- reached_file_max.
%   false.
reached_file_max :-
    % Retrieve the maximum allowed file count.
    option_value(max_per_file, Y),
    % Ensure it is finite and non-zero.
    Y \== inf, Y \== 0,
    % Compare the current file count.
    loaded_from_file_count(X),
    X >= Y.

%!  should_fix_args is nondet.
%
%   Determines if arguments should be fixed based on the current sampling condition.
%   This predicate always fails unless explicitly altered.
%
%   @example
%   % Check if arguments need fixing:
%   ?- should_fix_args.
%   false.
should_fix_args :-
    % Ensure the predicate fails.
    fail,
    % Check if sampling should not occur.
    \+ should_sample.

%!  should_sample is nondet.
%
%   Determines if sampling should be performed based on various conditions.
%   The predicate can succeed if specific options or conditions for sampling are met.
%
%   @example
%   % Check if sampling is allowed:
%   ?- should_sample.
%   false.  % Due to the first cut-fail combination.
should_sample :-
    !,
    % Ensure the predicate fails.
    fail.
should_sample :-
    % Check if data should be displayed.
    should_show_data(_),
    !.
should_sample :-
    % Retrieve the sampling rate or use a default.
    once(option_value(samples_per_million, Fifty); Fifty = 50),
    % Retrieve the current count of loaded files.
    loaded_from_file_count(X),
    % Calculate the remainder and check conditions.
    Y is X mod 1_000_000, !, Y >= 0, Y =< Fifty, !.

%!  should_show_data(+X) is nondet.
%
%   Determines if data should be displayed based on the current loaded file count (`X`).
%   The predicate succeeds if specific conditions on `X` are met, such as being within
%   a certain range or divisible by a specified value.
%
%   @arg X The count of loaded files.
%
%   @example
%   % Check if data should be displayed for a given count:
%   ?- should_show_data(12).
%   true.
should_show_data(X) :-
    % Retrieve the current file count.
    loaded_from_file_count(X),
    % Ensure this clause succeeds.
    !,
    % Check if the file count falls within the desired range.
    once((X =< 13, X >= 10); (X > 0, (0 is X rem 1_000_000))),
    % Log an empty line to the error stream.
    format(user_error, '~N', []),
    % Log an empty line to the output stream.
    format(user_output, '~N', []),
    % Ensure this clause succeeds.
    !,heartbeat.
should_show_data(X) :-
    % Retrieve the current loading file.
    nb_current(loading_file, F),
    % Ensure the loading file is not empty.
    F \== [],
    % Check if the file has an `.obo` extension.
    symbol_concat(_, '.obo', F),
    % Retrieve the current file count.
    loaded_from_file_count(X),
    % Calculate the remainder for modulo operation.
    Y is X mod 100_000,
    % Check if the remainder falls within the desired range.
    Y =< 15, Y >= 10.

%:- current_prolog_flag(libswipl,_)->use_module(library(logicmoo_utils)); true.

/*

declare -a StringArray=(\
"fbgn_fbtr_fbpp_expanded_*.tsv.gz" \
"physical_interactions_mitab*.tsv.gz" \
"dmel_gene_sequence_ontology_annotations*.tsv.gz" \
"gene_map_table_*.tsv.gz" \
"ncRNA_genes_fb_*.json.gz" \
"gene_association.fb.gz" \
"gene_genetic_interactions_*.tsv.gz" \
"allele_genetic_interactions_*.tsv.gz" \
"allele_phenotypic_data_*.tsv.gz" \
"disease_model_annotations*.tsv.gz" \
"dmel_human_orthologs_disease*.tsv.gz" \
"fbrf_pmid_pmcid_doi*.tsv.gz")
*/

%!  load_flybase_files is det.
%
%   Loads FlyBase files from a specified FTP directory.
%
%   This predicate retrieves the directory path using `ftp_data/1` and then
%   changes the working directory to this path using `with_cwd/2` to load the files
%   through `load_flybase_files_ftp/0`.
%
%   @example
%   % Load FlyBase files:
%   ?- load_flybase_files.
%   true.
load_flybase_files :-
    % Retrieve the FTP directory path.
    ftp_data(Dir),
    % Change the working directory and load the files.
    with_cwd(Dir, load_flybase_files_ftp).

%!  load_flybase_das_11 is det.
%
%   Loads and processes the 11 main data sources (DAS) from FlyBase, including
%   JSON and TSV files, which contain various types of biological and genomic data.
%
%   This predicate handles data related to ncRNA genes, expanded gene mappings,
%   physical interactions, sequence ontology annotations, gene maps, genetic
%   and phenotypic interactions, disease models, and human orthologs.
%
%   It supports both newer (`genotype_phenotype_data*.tsv`) and older
%   (`allele_phenotypic_data*.tsv`) file formats for phenotypic data.
%
%   After loading the data, it outputs formatted checkpoints and calls `fb_stats/0`
%   to generate FlyBase statistics.
%
%   @example
%   % Load all FlyBase DAS files and generate statistics:
%   ?- load_flybase_das_11.
%   =================================================================================================
%   =====================================Das Checkpoint==============================================
%   =================================================================================================
%   true.
load_flybase_das_11:-
  % DAS''s 11 tsv and 1 json file
  load_flybase('./precomputed_files/*/ncRNA_genes_fb_*.json'),
  load_flybase('./precomputed_files/*/fbgn_fbtr_fbpp_expanded*.tsv'),
  load_flybase('./precomputed_files/*/physical_interactions_mitab*.tsv'),
  load_flybase('./precomputed_files/*/dmel_gene_sequence_ontology_annotations*.tsv'),
  load_flybase('./precomputed_files/*/gene_map_table*.tsv'),
  load_flybase('./precomputed_files/*/gene_association_*.fb',tsv),
  load_flybase('./precomputed_files/*/gene_genetic_interactions*.tsv'),
  load_flybase('./precomputed_files/*/allele_genetic_interactions*.tsv'),
  % Note: this file replaces 'allele_phenotypic_data_*.tsv' from FB2023_01 onward.
  load_flybase('./precomputed_files/alleles/genotype_phenotype_data*.tsv'),
  load_flybase('./precomputed_files/*/allele_phenotypic_data*.tsv'),


  load_flybase('./precomputed_files/*/disease_model_annotations*.tsv'),
  load_flybase('./precomputed_files/*/dmel_human_orthologs_disease*.tsv'),
  load_flybase('./precomputed_files/*/fbrf_pmid_pmcid_doi*.tsv'),
  format("~n================================================================================================="),
  format("~n=====================================Das Checkpoint=============================================="),
  format("~n================================================================================================="),
              fb_stats,
  format("~n================================================================================================="),
  format("~n================================================================================================="),
  format("~n=================================================================================================~n"),
  !.

%!  load_flybase_files_ftp is det.
%
%   Loads FlyBase files from an FTP directory, processing a comprehensive set of data files.
%
%   This predicate uses `maplist/2` to iterate through a list of file-loading operations,
%   including precomputed files for collaborators, alleles, clones, genes, references, and more.
%   The predicate also calls `load_flybase_das_11/0` for the main DAS data and performs
%   additional operations to ensure all required files are loaded.
%
%   Special options are applied to certain file-loading operations, such as
%   filtering or format changes. After loading the data, formatted checkpoints are
%   output, and `fb_stats/0` is called to generate FlyBase statistics.
%
%   @example
%   % Load all FlyBase files from the FTP directory:
%   ?- load_flybase_files_ftp.
%   =================================================================================================
%   ==========================Should be 18 minute Checkpoint=========================================
%   =================================================================================================
%   true.
%
load_flybase_files_ftp:-
 maplist(must_det_ll,[
  load_flybase('./precomputed_files/collaborators/pmid_fbgn_uniprot*.tsv'),

 %% load_flybase_obo_files,
  load_flybase_das_11,
  % 36 more that DAS doesnt load

  load_flybase_obo_files,

  load_flybase('./precomputed_files/alleles/fbal_to_fbgn*.tsv'),

  load_flybase('./precomputed_files/clones/cDNA_clone_data*.tsv'),
  load_flybase('./precomputed_files/clones/genomic_clone_data*.tsv'),
  load_flybase('./precomputed_files/collaborators/fbgn_uniprot*.tsv'),
  load_flybase('./precomputed_files/collaborators/gp_information*.fb'),
  load_flybase('./precomputed_files/genes/automated_gene_summaries*.tsv'),
  load_flybase('./precomputed_files/genes/best_gene_summary*.tsv'),
  load_flybase('./precomputed_files/genes/Dmel_enzyme_data*.tsv'),
  load_flybase('./precomputed_files/genes/dmel_unique_protein_isoforms*.tsv'),
  load_flybase('./precomputed_files/genes/fbgn_annotation_ID*.tsv'),
  with_option([pred_va='True'],load_flybase('./precomputed_files/genes/fbgn_exons2affy1_overlaps*.tsv')),
  with_option([pred_va='True'],load_flybase('./precomputed_files/genes/fbgn_exons2affy2_overlaps*.tsv')),
  with_option([pred_va=false],load_flybase('./precomputed_files/genes/fbgn_fbtr_fbpp*.tsv')),
  load_flybase('./precomputed_files/genes/fbgn_gleanr*.tsv'),
  load_flybase('./precomputed_files/genes/fbgn_NAseq_Uniprot*.tsv'),
  load_flybase('./precomputed_files/genes/gene_functional_complementation*.tsv'),
  load_flybase('./precomputed_files/genes/gene_group_data*.tsv'),
  load_flybase('./precomputed_files/genes/gene_groups_HGNC*.tsv'),
  load_flybase('./precomputed_files/genes/gene_rpkm_matrix*.tsv'),
  load_flybase('./precomputed_files/genes/gene_rpkm_report*.tsv'),
  load_flybase('./precomputed_files/genes/gene_snapshots*.tsv'),
  load_flybase('./precomputed_files/genes/pathway_group_data*.tsv'),
  %load_flybase('./precomputed_files/insertions/construct_maps.zip'),
  load_flybase('./precomputed_files/insertions/fu_gal4_table_fb_*.json'),
  load_flybase('./precomputed_files/insertions/insertion_mapping*.tsv'),
  load_flybase('./precomputed_files/map_conversion/cyto-genetic-seq*.tsv'),
  load_flybase('./precomputed_files/metadata/dataset_metadata*.tsv'),
  load_flybase('./precomputed_files/orthologs/dmel_paralogs*.tsv'),
  load_flybase('./precomputed_files/references/entity_publication*.tsv'),
  load_flybase('./precomputed_files/species/organism_list*.tsv'),
  load_flybase('./precomputed_files/stocks/stocks_FB*.tsv'),
  load_flybase('./precomputed_files/synonyms/fb_synonym*.tsv'),
  format("~n================================================================================================="),
  format("~n==========================Should be 18 minute Checkpoint========================================="),
  format("~n================================================================================================="),
  fb_stats,
  format("~n================================================================================================="),
  format("~n================================================================================================="),
  format("~n=================================================================================================~n"),
  load_flybase('./precomputed_files/map_conversion/cytotable.txt',tsv),
  load_flybase('./precomputed_files/map_conversion/genome-cyto-seq.txt',tsv),
  load_fbase_after_17]),
  !.

 %!  gene_sequences is det.
%
%   Loads Drosophila melanogaster gene sequence data, including GFF, FASTA, and GTF files.
%   This predicate handles loading of the annotation and sequence data from version 6.55.
%
%   @example
%   % Load gene sequence data:
%   ?- gene_sequences.
%   true.
gene_sequences :-
    load_flybase('./dmel_r6.55/gff/dmel-all-r6.55.gff'),
    load_flybase('./dmel_r6.55/fasta/*.fasta'),
    load_flybase('./dmel_r6.55/gtf/*.gft'),
    !.

%!  load_fbase_after_17 is det.
%
%   Loads additional FlyBase data that is processed after the initial DAS and other main files.
%   This includes transposon sequences, ncRNA gene data, and ontology files.
%
%   Some parts of this predicate are commented out for future use or as placeholders.
%
%   @example
%   % Load additional FlyBase data:
%   ?- load_fbase_after_17.
%   true.
load_fbase_after_17 :-
    % load_flybase('./precomputed_files/genes/scRNA-Seq_gene_expression*.tsv'),
    must_det_ll(load_flybase('./precomputed_files/transposons/transposon_sequence_set.gff*')),
    load_flybase('./precomputed_files/transposons/transposon_sequence_set.fa'),
    load_flybase('./precomputed_files/*/ncRNA_genes_fb_*.json'),
    load_obo_files,
    % load_flybase_chado,
    !.

%!  load_flybase_obo_files is det.
%
%   Loads FlyBase ontology files in OBO format, including various biological and
%   controlled vocabularies. This predicate also loads SCM files and additional
%   precomputed ontology files.
%
%   The ontology files include:
%   - Disease Ontology (doid.obo)
%   - Fly anatomy and development vocabularies
%   - FlyBase-controlled and stock vocabularies
%   - Gene group ontologies
%   - Gene Ontology (go-basic.obo)
%   - Image, PSI-MI, and CHEBI slices
%   - Sequence Ontology (so-simple.obo)
%
%   @example
%   % Load all ontology files:
%   ?- load_flybase_obo_files.
%   true.
%
load_flybase_obo_files:-
 load_flybase('./data/*/*/*.scm'),
  load_flybase('./precomputed_files/ontologies/doid.obo'),
  load_flybase('./precomputed_files/ontologies/fly_anatomy.obo'),
  load_flybase('./precomputed_files/ontologies/fly_development.obo'),
  load_flybase('./precomputed_files/ontologies/flybase_controlled_vocabulary.obo'),
  load_flybase('./precomputed_files/ontologies/flybase_stock_vocabulary.obo'),
  load_flybase('./precomputed_files/ontologies/gene_group_FB*.obo'),
  load_flybase('./precomputed_files/ontologies/go-basic.obo'),
  load_flybase('./precomputed_files/ontologies/image.obo'),
  load_flybase('./precomputed_files/ontologies/psi-mi.obo'),
  load_flybase('./precomputed_files/ontologies/slice.chebi.obo'),
  load_flybase('./precomputed_files/ontologies/so-simple.obo'),
  load_flybase('./precomputed_files/ontologies/chebi_fb_*.obo'),
  !.

/*
:- ensure_loaded('./reqs/obo_core/prolog/obo_core/goslim.pl').
:- ensure_loaded('./reqs/obo_metadata/prolog/obo_metadata.pl').
:- ensure_loaded('./reqs/obo_metadata/prolog/obo_metadata/iao_metadata.pl').
:- ensure_loaded('./reqs/obo_metadata/prolog/obo_metadata/oio.pl').
:- ensure_loaded('./reqs/obo_ro/prolog/obo_ro/ro.pl').

:- attach_packs('./reqs',[]).
:- ensure_loaded(library(obo_metadata)).
:- goslim:ensure_loaded(library(obo_core/goslim)).
:- ensure_loaded(library(obo_ro/ro)).

937_381_148
*/

:- ensure_loaded(ext_loader_tsv).

/*
(load_fb_obo "data/ontologies/so.obo")

; Total         Atoms (Atomspace size): ...................................................... 19,967
;               ConceptNodes: ................................................................. 4,258
;               Random samples: ................................................................. 158
;               Total Memory Used: ........................................................ 1,089,408
;               Runtime (days:hh:mm:ss): ................................................. 0:00:00:29



(load_fb_obo "./precomputed_files/ontologies/so-simple.obo" )

; Total         Atoms (Atomspace size): ...................................................... 19,484
;               ConceptNodes: ................................................................. 4,194
;               Random samples: ................................................................. 160
;               Total Memory Used: ........................................................ 1,089,408
;               Runtime (days:hh:mm:ss): ................................................. 0:00:00:29


*/

/*
?- xinfo('SO:0000797').
ontology_info(id_type,'SO:0000797','Term').
def('SO:0000797',"TE that exists (or existed) in nature.",['FB:mc']).
has_quality('SO:0000797','SO:0000782',' natural').
intersection_of('SO:0000797','SO:0000101',' transposable_element').
intersection_of('SO:0000797',has_quality,'SO:0000782',' natural').
ontology_info(is_a,'SO:0000797','SO:0000101').
ontology_info(is_a,'SO:0000797','SO:0001038').
ontology_info(name,'SO:0000797',"natural_transposable_element").
synonym('SO:0000797',"natural transposable element",'EXACT',[]).



xinfo('SO:0000797').
ontology_info(id_type,'SO:0000797','Term').
def('SO:0000797',"TE that exists (or existed) in nature.",['FB:mc']).
has_quality('SO:0000797','SO:0000782',' natural').
intersection_of('SO:0000797','SO:0000101',' transposable_element').
intersection_of('SO:0000797',has_quality,'SO:0000782',' natural').
ontology_info(is_a,'SO:0000797','SO:0000101').
ontology_info(is_a,'SO:0000797','SO:0001038').
ontology_info(name,'SO:0000797',"natural_transposable_element").
synonym('SO:0000797',"natural transposable element",'EXACT',[]).

*/

%!  load_obo_files is det.
%
%   Loads a variety of ontology files in OBO format, including standard ontologies,
%   subsets, and legacy cross-product files. The predicate also computes and logs
%   statistics for loaded files, such as memory usage, runtime, and atom space.
%
%   This predicate supports the following types of OBO files:
%   - Sequence Ontology (SO) files and their subsets
%   - Legacy cross-products and their derivatives
%   - Precomputed ontology files from various FlyBase sources
%   - CHEBI, GO, PSI-MI, and other domain-specific ontologies
%
%   The predicate includes commented-out legacy or alternative loading paths for
%   flexibility and future reference. It outputs the loaded file statistics for
%   further analysis and logs detailed runtime metrics.
%
%   @example
%   % Load all ontology files and print statistics:
%   ?- load_obo_files.
%   true.
%
load_obo_files:-
  %load_obo('./reqs/obonet/tests/data/?*.obo'),

  load_flybase('./precomputed_files/*/so*.obo'),
  fb_stats,
  load_flybase('./data/SO-Ontologies/Ontology_Files/*.obo'),
  % Total         Atoms (Atomspace size): ...................................................... 20,069
  %               ConceptNodes: ................................................................. 4,200
  %               Random samples: ................................................................. 159
  %               Total Memory Used: ........................................................ 1,089,408
  %               Runtime (days:hh:mm:ss): ................................................. 0:00:00:29

  load_flybase('./data/SO-Ontologies/Ontology_Files/subsets/*.obo'),
  % Total         Atoms (Atomspace size): ...................................................... 20,551
  %               ConceptNodes: ................................................................. 4,270
  %               Random samples: ............................................................... 2,928
  %               Total Memory Used: ........................................................ 1,154,944
  %               Runtime (days:hh:mm:ss): ................................................. 0:00:00:40

  load_flybase('./data/Legacy/Cross_Products/*.obo'),
  %load_flybase('./data/*/*/*.obo'),

  % Total         Atoms (Atomspace size): ...................................................... 20,968
  %               ConceptNodes: ................................................................. 4,306
  %               Random samples: .............................................................. 14,418
  %               Total Memory Used: ........................................................ 9,828,592
  %               Runtime (days:hh:mm:ss): ................................................. 0:00:01:14
  %print_loaded_from_files,
     %loaded_from_file(         19_515, './data/SO-Ontologies/Ontology_Files/so-simple.obo').
     %         only reflects new entries ... thus full OBO adds 481 entries to the simple one
     %loaded_from_file(            481, './data/SO-Ontologies/Ontology_Files/so.obo').

     %loaded_from_file(            336, './data/SO-Ontologies/Legacy/Cross_Products/so-xp-dec.obo').
     %loaded_from_file(            310, './data/SO-Ontologies/Ontology_Files/subsets/SOFA.obo').
     %loaded_from_file(            141, './data/SO-Ontologies/Ontology_Files/subsets/biosapiens.obo').
     %loaded_from_file(             73, './data/SO-Ontologies/Ontology_Files/subsets/Alliance_of_Genome_Resources.obo').
     %loaded_from_file(             35, './data/SO-Ontologies/Legacy/Cross_Products/so-xp-non-classified.obo').
     %loaded_from_file(             31, './data/SO-Ontologies/Ontology_Files/subsets/DBVAR.obo').
     %loaded_from_file(             23, './data/SO-Ontologies/Legacy/Cross_Products/so-xp.obo').
     %loaded_from_file(             23, './data/SO-Ontologies/Legacy/Cross_Products/so-xp-simple.obo').

  load_flybase('./precomputed_files/*/*.obo'),
  % Total         Atoms (Atomspace size): ................................................... 3,489,211
  %               ConceptNodes: ............................................................... 688,541
  %               Random samples: .............................................................. 26,006
  %               Total Memory Used: ............................................................ 1.19G
  %               Runtime (days:hh:mm:ss): ................................................. 0:00:34:35
  print_loaded_from_files,
%loaded_from_file(2_637_502, './precomputed_files/ontologies/chebi_fb_2023_04.obo').
%loaded_from_file(  451_168, './precomputed_files/ontologies/go-basic.obo').
%loaded_from_file(  221_705, './precomputed_files/ontologies/fly_anatomy.obo').
%loaded_from_file(  128_798, './precomputed_files/ontologies/doid.obo').
%loaded_from_file(   19_515, './data/SO-Ontologies/Ontology_Files/so-simple.obo').
%loaded_from_file(    9_852, './precomputed_files/ontologies/psi-mi.obo').
%loaded_from_file(    8_644, './precomputed_files/ontologies/gene_group_FB2023_04.obo').
%loaded_from_file(    7_605, './precomputed_files/ontologies/flybase_controlled_vocabulary.obo').
%loaded_from_file(    1_598, './precomputed_files/ontologies/fly_development.obo').
%loaded_from_file(      834, './precomputed_files/ontologies/image.obo').
%loaded_from_file(      491, './precomputed_files/ontologies/flybase_stock_vocabulary.obo').
%loaded_from_file(      481, './data/SO-Ontologies/Ontology_Files/so.obo').
%loaded_from_file(      336, './data/SO-Ontologies/Legacy/Cross_Products/so-xp-dec.obo').
%loaded_from_file(      310, './data/SO-Ontologies/Ontology_Files/subsets/SOFA.obo').
%loaded_from_file(      141, './data/SO-Ontologies/Ontology_Files/subsets/biosapiens.obo').
%loaded_from_file(       73, './data/SO-Ontologies/Ontology_Files/subsets/Alliance_of_Genome_Resources.obo').
%loaded_from_file(       35, './data/SO-Ontologies/Legacy/Cross_Products/so-xp-non-classified.obo').
%loaded_from_file(       31, './data/SO-Ontologies/Ontology_Files/subsets/DBVAR.obo').
%loaded_from_file(       25, './precomputed_files/ontologies/so-simple.obo').
%loaded_from_file(       23, './data/SO-Ontologies/Legacy/Cross_Products/so-xp.obo').
%loaded_from_file(       23, './data/SO-Ontologies/Legacy/Cross_Products/so-xp-simple.obo').
%loaded_from_file(       21, './precomputed_files/ontologies/slice.chebi.obo').
  !.

%!  load_flybase_chado is det.
%
%   Loads data from FlyBase Chado exports, which consist of 359 tables containing
%   over 937 million rows. The data is loaded from TSV files located in the
%   `./data/tsv_exports/public/` directory.
%
%   This predicate uses options to ensure proper handling of headers and limits
%   the maximum number of rows per file to 100,000 for efficient processing.
%
%   @example
%   % Load FlyBase Chado data:
%   ?- load_flybase_chado.
%   true.
%
load_flybase_chado:-  % 359 tables with 937,381,148 rows

  with_option([row_1_is_header=true,max_per_file=100_000],load_flybase('./data/tsv_exports/public/*.tsv')).


est_size(     22_220_267, dataset_metadata).
est_size(      5_580_889, entity_publication).
est_size(      2_928_712, gene_rpkm_report).
est_size(      2_262_949, pmid_fbgn_uniprot).
est_size(      1_611_349, ontology_info).
est_size(      1_316_132, fbgn_NAseq_Uniprot).
est_size(      1_045_549, property_value).
est_size(      1_001_254, synonym).
est_size(        722_570, cDNA_clone_data).
est_size(        384_206, genotype_phenotype).
est_size(        363_453, allele_genetic_interactions).
est_size(        288_469, fbal_to_fbgn).
est_size(        245_829, gene_map_table).
est_size(        223_056, dmel_paralogs).
est_size(        212_010, insertion_mapping).
est_size(        174_894, fbgn_gleanr).
est_size(        147_155, stocks).
est_size(         99_294, fbrf_pmid_pmcid_doi).
est_size(         57_986, organism_list).
est_size(         51_248, physical_interactions_mitab).
est_size(         50_401, genomic_clone).
est_size(         38_591, dmel_gene_sequence_ontology_annotations).
est_size(         35_732, fbgn_fbtr_fbpp_expanded).
est_size(         35_732, fbgn_fbtr_fbpp).
est_size(         35_107, dmel_human_orthologs_disease).
est_size(         32_530, fbgn_uniprot).
est_size(         32_143, best_gene_summary).
est_size(         27_827, receives_synaptic_input_in_region).
est_size(         27_370, automated_gene_summaries).
est_size(         26_750, disease_model_annotations).
est_size(         22_453, dmel_unique_protein_isoforms).
est_size(         20_392, sends_synaptic_output_to_region).
est_size(         20_322, gene_genetic_interactions).
est_size(         17_901, fbgn_annotation_ID).
est_size(         17_748, gene_rpkm_matrix).
est_size(         14_139, part_of).
est_size(         13_986, gene_snapshots).
est_size(         13_394, seq).
est_size(         12_497, fbgn_exons2affy1_overlaps).
est_size(         13_746, fbgn_exons2affy2_overlaps).
est_size(         10_958, gene_group).
est_size(          5_680, has_synaptic_IO_in).
est_size(          3_969, 'Dmel_enzyme').
est_size(          3_120, regulates).
est_size(          2_704, negatively_regulates).
est_size(          2_692, positively_regulates).
est_size(          2_126, has_soma_location).
est_size(          1_941, overlaps).
est_size(          1_744, develops_from).
est_size(          1_711, gene_groups_HGNC).
est_size(          1_149, sends_synaptic_output_to_cell).
est_size(            993, pathway_group).
est_size(            738, receives_synaptic_input_from_neuron).
est_size(            706, fasciculates_with).
est_size(            601, 'cyto-genetic-seq').
est_size(            572, has_sensory_dendrite_in).
est_size(            526, continuous_with).
est_size(            464, gene_functional_complementation).
est_size(            311, synapsed_via_type_Ib_bouton_to).
est_size(            287, innervates).
est_size(            239, immediately_preceded_by).
est_size(            229, has_fasciculating_neuron_projection).
est_size(            200, synapsed_via_type_II_bouton_to).
est_size(            186, receives_synaptic_input_throughout).
est_size(            185, substage_of).
est_size(            165, has_quality).
est_size(            156, has_part).
est_size(            151, def).
est_size(            148, synapsed_via_type_Is_bouton_to).
est_size(            126, attached_to).
est_size(            100, subsetdef).
est_size(             89, receives_input_from).
est_size(             88, disjoint_from).
est_size(             74, sends_synaptic_output_throughout).
est_size(             68, develops_into).
est_size(             64, derives_from).
est_size(             30, has_origin).
est_size(             30, electrically_synapsed_to).
est_size(             23, develops_directly_from).
est_size(             20, attached_to_part_of).
est_size(             18, synonymtypedef).
est_size(             17, innervated_by).
est_size(             14, synapsed_via_type_III_bouton_to).
est_size(             11, has_synaptic_IO_throughout).
est_size(              9, transitive_over).
est_size(              8, composed_primarily_of).
est_size(              7, transcribed_to).
est_size(              7, inverse_of).
est_size(              6, conditionality).
est_size(              6, adjacent_to).
est_size(              4, non_functional_homolog_of).
est_size(              3, xref).
est_size(              3, variant_of).
est_size(              3, member_of).
est_size(              3, develops_from_part_of).
est_size(              3, contains).
est_size(              3, 'RO').
est_size(              2, connected_to).
est_size(              1, transcribed_from).
est_size(              1, guided_by).
% SQL
sql_est_size(    248_392_753,feature_relationship).
sql_est_size(    141_933_326,dbxrefprop).
sql_est_size(     98_464_501,featureloc).
sql_est_size(     92_616_769,feature).
sql_est_size(     78_909_674,analysisfeature).
sql_est_size(     61_025_741,feature_dbxref).
sql_est_size(     53_031_862,library_featureprop).
sql_est_size(     39_950_319,dbxref).
sql_est_size(     27_923_221,library_feature).
sql_est_size(     23_805_221,feature_relationshipprop).
sql_est_size(     21_279_999,featureprop).
sql_est_size(      7_474_185,feature_synonym).
sql_est_size(      6_554_427,synonym).
sql_est_size(      5_578_280,feature_pub).
sql_est_size(      5_341_100,featureprop_pub).
sql_est_size(      4_865_118,feature_relationship_pub).
sql_est_size(      2_813_405,feature_interactionprop).
sql_est_size(      2_464_355,feature_cvterm).
sql_est_size(      1_950_807,feature_cvtermprop).
sql_est_size(      1_377_258,feature_interaction).
sql_est_size(      1_116_490,feature_genotype).
sql_est_size(        888_210,pubprop).
sql_est_size(        734_870,featureloc_pub).
sql_est_size(        688_734,pubauthor).
sql_est_size(        518_569,genotype_synonym).
sql_est_size(        495_848,genotype).
sql_est_size(        491_538,feature_pubprop).
sql_est_size(        466_209,phenstatement).
sql_est_size(        413_338,pub_dbxref).
sql_est_size(        382_054,genotype_dbxref).
sql_est_size(        351_942,phendesc).
sql_est_size(        277_992,phenotype_comparison_cvterm).
sql_est_size(        254_298,feature_expressionprop).
sql_est_size(        252_544,phenotype_comparison).
sql_est_size(        251_928,pub).
sql_est_size(        242_344,pub_relationship).
sql_est_size(        227_406,feature_expression).
sql_est_size(        213_360,cvterm_relationship).
sql_est_size(        212_142,cvterm_dbxref).
sql_est_size(        209_164,interaction_cvterm).
sql_est_size(        195_000,cvtermsynonym).
sql_est_size(        180_311,expression_cvterm).
sql_est_size(        167_582,update_track).
sql_est_size(        150_401,feature_relationshipprop_pub).
sql_est_size(        149_855,stockcollection_stock).
sql_est_size(        149_855,stock).
sql_est_size(        149_835,stock_genotype).
sql_est_size(        146_846,interactionprop).
sql_est_size(        122_004,interaction_group).
sql_est_size(        119_611,feature_interaction_pub).
sql_est_size(        112_784,interaction_pub).
sql_est_size(        112_781,interaction).
sql_est_size(        101_687,interaction_group_feature_interaction).
sql_est_size(         96_405,feature_grpmember_pub).
sql_est_size(         94_765,cvterm).
sql_est_size(         79_466,expression_cvtermprop).
sql_est_size(         74_873,interactionprop_pub).
sql_est_size(         73_828,library_interaction).
sql_est_size(         57_144,organism).
sql_est_size(         48_730,humanhealthprop).
sql_est_size(         41_075,feature_grpmember).
sql_est_size(         36_960,expression).
sql_est_size(         23_565,library_cvterm).
sql_est_size(         23_483,library_cvtermprop).
sql_est_size(         21_251,cvtermprop).
sql_est_size(         19_797,libraryprop).
sql_est_size(         18_396,phenotype).
sql_est_size(         17_871,phenotype_cvterm).
sql_est_size(         16_617,humanhealth_dbxrefprop).
sql_est_size(         16_529,interaction_expressionprop).
sql_est_size(         16_318,humanhealth_pub).
sql_est_size(         15_400,library_synonym).
sql_est_size(         15_355,humanhealth_dbxref).
sql_est_size(         15_142,cell_line_feature).
sql_est_size(         14_972,libraryprop_pub).
sql_est_size(         13_694,interaction_expression).
sql_est_size(         13_218,interaction_cell_line).
sql_est_size(         10_720,library_pub).
sql_est_size(          9_870,library_relationship).
sql_est_size(          9_851,humanhealthprop_pub).
sql_est_size(          9_558,library_dbxref).
sql_est_size(          8_339,library_relationship_pub).
sql_est_size(          7_095,grp_pub).
sql_est_size(          6_719,cell_line_pub).
sql_est_size(          6_657,grp_relationship).
sql_est_size(          6_605,strain_synonym).
sql_est_size(          5_990,grp_synonym).
sql_est_size(          5_947,humanhealth_synonym).
sql_est_size(          5_785,strainprop).
sql_est_size(          5_783,strainprop_pub).
sql_est_size(          5_769,library).
sql_est_size(          5_543,grp_cvterm).
sql_est_size(          5_444,cell_line_synonym).
sql_est_size(          5_277,library_expression).
sql_est_size(          5_187,grpprop).
sql_est_size(          5_159,grpmember).
sql_est_size(          4_469,humanhealth_dbxrefprop_pub).
sql_est_size(          4_450,library_expressionprop).
sql_est_size(          4_415,grpprop_pub).
sql_est_size(          4_319,stock_cvterm).
sql_est_size(          3_832,library_dbxrefprop).
sql_est_size(          3_829,grpmemberprop).
sql_est_size(          3_777,genotype_cvterm).
sql_est_size(          3_744,humanhealth_featureprop).
sql_est_size(          3_721,library_strainprop).
sql_est_size(          3_721,library_strain).
sql_est_size(          3_625,humanhealth_feature).
sql_est_size(          2_641,grp_dbxref).
sql_est_size(          2_263,humanhealth_relationship).
sql_est_size(          2_220,humanhealth_relationship_pub).
sql_est_size(          2_093,strain_pub).
sql_est_size(          2_010,grp_relationship_pub).
sql_est_size(          1_939,strain_cvtermprop).
sql_est_size(          1_939,strain_cvterm).
sql_est_size(          1_814,grp).
sql_est_size(          1_777,strain_dbxref).
sql_est_size(          1_776,strain).
sql_est_size(          1_739,organism_dbxref).
sql_est_size(          1_643,feature_humanhealth_dbxref).
sql_est_size(          1_540,humanhealth_cvtermprop).
sql_est_size(          1_540,humanhealth_cvterm).
sql_est_size(          1_515,humanhealth).
sql_est_size(          1_300,cell_lineprop_pub).
sql_est_size(          1_291,cell_lineprop).
sql_est_size(          1_215,cell_line_dbxref).
sql_est_size(          1_198,cell_line_libraryprop).
sql_est_size(          1_081,cell_line_library).
sql_est_size(          1_013,organism_pub).
sql_est_size(            821,organismprop).
sql_est_size(            731,organismprop_pub).
sql_est_size(            714,cell_line_cvterm).
sql_est_size(            518,db).
sql_est_size(            435,strain_relationship_pub).
sql_est_size(            435,strain_relationship).
sql_est_size(            320,cell_line).
sql_est_size(            308,analysis).
sql_est_size(            238,stockprop).
sql_est_size(            171,cell_line_relationship).
sql_est_size(            139,strain_featureprop).
sql_est_size(            139,strain_feature).
sql_est_size(            107,strain_phenotypeprop).
sql_est_size(             96,humanhealth_pubprop).
sql_est_size(             73,cell_line_cvtermprop).
sql_est_size(             71,cv).
sql_est_size(             54,strain_phenotype).
sql_est_size(             40,environment).
sql_est_size(             27,stockcollectionprop).
sql_est_size(             26,contact).
sql_est_size(             18,environment_cvterm).
sql_est_size(             11,organism_library).
sql_est_size(              7,stockcollection).
sql_est_size(              1,lock).
sql_est_size(              0,analysisgrp).
sql_est_size(              0,analysisgrpmember).
sql_est_size(              0,analysisprop).
sql_est_size(              0,audit_chado).
sql_est_size(              0,cell_line_strain).
sql_est_size(              0,cell_line_strainprop).
sql_est_size(              0,cvtermpath).
sql_est_size(              0,eimage).
sql_est_size(              0,expression_image).
sql_est_size(              0,expression_pub).
sql_est_size(              0,expressionprop).
sql_est_size(              0,feature_cvterm_dbxref).
sql_est_size(              0,feature_phenotype).
sql_est_size(              0,featuremap).
sql_est_size(              0,featuremap_pub).
sql_est_size(              0,featurepos).
sql_est_size(              0,featurerange).
sql_est_size(              0,genotype_cvtermprop).
sql_est_size(              0,genotype_pub).
sql_est_size(              0,genotypeprop).
sql_est_size(              0,genotypeprop_pub).
sql_est_size(              0,grp_pubprop).
sql_est_size(              0,grp_relationshipprop).
sql_est_size(              0,grpmember_cvterm).
sql_est_size(              0,grpmember_pub).
sql_est_size(              0,grpmemberprop_pub).
sql_est_size(              0,humanhealth_phenotype).
sql_est_size(              0,humanhealth_phenotypeprop).
sql_est_size(              0,interaction_cvtermprop).
sql_est_size(              0,library_grpmember).
sql_est_size(              0,library_humanhealth).
sql_est_size(              0,library_humanhealthprop).
sql_est_size(              0,organism_cvterm).
sql_est_size(              0,organism_cvtermprop).
sql_est_size(              0,organism_grpmember).
sql_est_size(              0,project).
sql_est_size(              0,stock_dbxref).
sql_est_size(              0,stock_pub).
sql_est_size(              0,stock_relationship).
sql_est_size(              0,stock_relationship_pub).
sql_est_size(              0,stockprop_pub).
sql_est_size(              0,tableinfo).

%!  est_size_loaded(-N, +FunctorArity) is det.
%
%   Estimates the size of a loaded predicate in terms of the number of facts or clauses.
%
%   This predicate calculates the size of a loaded predicate identified by its functor
%   `F` and arity `A`. The size is retrieved by querying the internal `metta_stats/3` predicate,
%   which provides statistical information about the predicate. Only predicates marked
%   as "major" (via `fb_pred_major/2`) are considered.
%
%   @arg N   The estimated size (number of facts or clauses) of the predicate.
%   @arg FunctorArity   The functor and arity of the predicate, represented as `F/A` where `F` is the
%            functor (atom) and `A` is the arity (integer).
%
%   @example
%       % Estimate the size of a major predicate with functor 'example' and arity 2:
%       ?- est_size_loaded(N, example/2).
%       N = 42.
%
%   @see fb_pred_major/2 for identifying major predicates.
%   @see metta_stats/3 for retrieving predicate statistics.
est_size_loaded(N, F/A) :-
    % Ensure that the predicate is identified as "major".
    fb_pred_major(F, A),
    % Retrieve the size (number of clauses or facts) using metta_stats/3.
    metta_stats(F, A, N).

%!  fb_pred_major(+F, +A) is nondet.
%
%   Determines if a predicate is "major" based on its functor and arity.
%
%   A predicate is classified as "major" if it meets one of the following conditions:
%   1. It is explicitly marked as major using `fb_pred_m/2`.
%   2. It is not marked as major but satisfies specific conditions:
%      - It is listed by `fb_pred_nr/2`.
%      - No predicate of the same functor exists with a higher arity.
%
%   @arg F The functor of the predicate (atom).
%   @arg A The arity of the predicate (integer).
%
%   @example
%       % Check if a predicate is major:
%       ?- fb_pred_major(fbgn_exons2affy1_overlaps, 2).
%       true.
%
%       % Check for a non-major predicate:
%       ?- fb_pred_major(fbgn_non_major_pred, 3).
%       false.
fb_pred_major(F, A) :-
    % Check if the predicate is explicitly marked as major.
    fb_pred_m(F, A).
fb_pred_major(F, A) :-
    % Otherwise, verify conditions for a non-marked "major" predicate.
    fb_pred_nr(F, A),
    \+ fb_pred_m(F, A),
    \+ (fb_pred(F, A2), A2 > A).

%!  fb_pred_m(+F, +A) is nondet.
%
%   Identifies predicates explicitly marked as "major".
%
%   This predicate lists functor/arity combinations that are predefined as major predicates.
%
%   @arg F The functor of the predicate (atom).
%   @arg A The arity of the predicate (integer).
fb_pred_m(fbgn_exons2affy1_overlaps, 2).
fb_pred_m(fbgn_exons2affy2_overlaps, 2).

%!  print_loaded_from_files is det.
%
%   Prints statistics of predicates loaded from files.
%
%   This predicate collects and sorts statistics about predicates loaded from files
%   and prints them in descending order. It uses the helper predicate
%   `is_loaded_from_file_count/2` to gather data and displays the results using
%   `print_est_size/3`.
%
%   @example
%       % Print loaded predicate statistics:
%       ?- print_loaded_from_files.
%       ...
print_loaded_from_files :-
    % Gather all predicates loaded from files with their counts.
    findall(print_est_size(loaded_from_file, N, F),
            is_loaded_from_file_count(F, N), L),
    % Sort the results for consistency.
    sort(L, S),
    % Reverse to ensure descending order of counts.
    reverse(S, R),
    % Execute the collected print instructions.
    maplist(call, R),
    % Print additional predicate size estimates.
    print_est_sizes.

%!  fb_info is det.
%
%   Displays general information about the system's loaded predicates and statistics.
%
%   This predicate prints the loaded predicates (via `print_loaded_from_files/0`)
%   and overall statistics (via `fb_stats/0`).
%
%   @example
%       % Display system information:
%       ?- fb_info.
%       ...
fb_info :-
    % Print statistics for predicates loaded from files.
    print_loaded_from_files,
    % Display additional system-wide statistics.
    fb_stats.

%!  fb_show is det.
%
%   Alias for `fb_info/0`. Displays loaded predicate statistics and system information.
%
%   This predicate is functionally identical to `fb_info/0`.
%
%   @example
%       % Show system information:
%       ?- fb_show.
%       ...
fb_show :-
    % Print statistics for predicates loaded from files.
    print_loaded_from_files,
    % Display additional system-wide statistics.
    fb_stats.

%!  print_est_sizes is det.
%
%   Prints estimated sizes of loaded predicates.
%
%   This predicate retrieves and displays the estimated size of predicates using
%   `est_size_loaded/2`. The results are sorted, reversed, and printed in a
%   readable format using `print_est_size/3`.
%
%   @example
%       % Print predicate size estimates:
%       ?- print_est_sizes.
%       ...
print_est_sizes :-
    % Gather size estimates for all relevant predicates.
    findall(print_est_size(est_size_loaded, N, F),
            est_size_loaded(N, F), L),
    % Sort the results alphabetically or by size.
    sort(L, S),
    % Reverse to ensure largest sizes are printed first.
    reverse(S, R),
    % Print each predicate's size estimate.
    maplist(call, R).

%!  print_est_size(+F, +N1, +S) is det.
%
%   Prints the size and statistics of a predicate in a formatted way.
%
%   This predicate formats and displays the statistics of a predicate using
%   `format/2`. If the size is numeric and other conditions are met, it ensures
%   proper formatting for readability.
%
%   @arg F   The label or category (e.g., `loaded_from_file`).
%   @arg N1  The numeric size or count (if applicable).
%   @arg S   The subject of the predicate (e.g., functor/arity combination).
%
%   @example
%       % Print a predicate's size:
%       ?- print_est_size(loaded_from_file, 42, example/2).
%       ...
print_est_size(F, N1, S) :-
    % Ensure consistent formatting for numeric predicates.
    number(S),
    \+ number(N1),
    !,
    % Swap arguments for proper display order.
    print_est_size(F, S, N1).
print_est_size(F, N1, S) :-
    % Format and print the size and statistics.
    format('~N (~q ~@ ~q) ', [F, pad_number(N1, 15), S]), !.

%!  pad_number(+Number, +N) is det.
%
%   Pads a given number with spaces to occupy a total width of N characters,
%   while also formatting the number to include underscores as the thousand separator.
%
%   This predicate formats a number by:
%   1. Creating a string representation with spaces for padding.
%   2. Replacing commas (used as thousand separators) with underscores.
%   3. Writing the result to the output.
%
%   @arg Number The number to be padded and formatted (integer).
%   @arg N      The total width of the padded output (integer).
%
%   @example
%       % Pad and format a number:
%       ?- pad_number(1234567, 15).
%           1_234_567
%
%       % The number is right-aligned in a field of 15 characters.
pad_number(Number, N) :-
    % Format the number into a string, padded to occupy N characters.
    sformat(S, "~t~D~*|", [Number, N]),
    % Split the string at commas to create a list of components.
    symbolic_list_concat(L, ',', S),
    % Rejoin the components using underscores as separators.
    symbolic_list_concat(L, '_', SS),
    % Output the formatted string.
    write(SS).

% dynamic is used to allow for alteration at runtime.
:- dynamic(fix_columns_nth/2).

%!  needs_fixed(+X, -Y) is nondet.
%
%   Determines whether an argument `X` needs to be fixed and computes the fixed version `Y`.
%
%   This predicate first checks if `X` is unbound (a variable) and attempts to bind it
%   using `fb_arg/1`. Then, it tries to fix the concept `X` by finding an alternative
%   value `L`. If `L` differs from `X`, it is assigned to `Y`.
%
%   @arg X The original argument to be checked or fixed.
%   @arg Y The fixed version of `X`, if applicable.
%
%   @example
%       % Determine if an argument needs fixing:
%       ?- needs_fixed(original_value, FixedValue).
%       ...
needs_fixed(X, Y) :-
    % If X is unbound, try to bind it using fb_arg/1.
    (var(X) -> fb_arg(X) ; true),
    % Attempt to find a fixed concept L for X.
    fix_concept(X, L),
    % Ensure L is not structurally or semantically the same as X.
    (L \=@=[X], L \=@= X),
    % If L is a valid single value, assign it to Y; otherwise, set Y to L.
    (L = [Y] -> true ; Y = L).

%!  mine_args_that_need_reduced is det.
%
%   Processes all arguments that need to be fixed, updating columns and listings.
%
%   This predicate identifies all arguments requiring reduction or fixing using
%   `needs_fixed/2`, logs the transformations, and updates columns via
%   `fix_columns_with_arg/1`. Finally, it lists all `fix_columns_nth/2` updates.
%
%   @example
%       % Process and update arguments:
%       ?- mine_args_that_need_reduced.
%       ...
mine_args_that_need_reduced :-
    % Print separator for clarity in logs.
    writeln('\n\n\n=====\n\n\n'),
    % For every argument that needs fixing, log and process it.
    forall(needs_fixed(X, Y), (
        % Log the transformation X -> Y.
        pp_as(needs_fixed(X -> Y)),
        % Fix columns with the problematic argument.
        fix_columns_with_arg(X)
    )),
    % Display the updated columns.
    listing(fix_columns_nth).

%!  fix_columns_with_arg(+Arg) is det.
%
%   Fixes all columns in the argument table associated with a given argument.
%
%   For a specified argument, this predicate iterates through all matching
%   function and column number combinations in `fb_arg_table_n/3` and applies
%   `fix_columns_n/2` to them.
%
%   @arg Arg The argument whose columns need fixing.
%
%   @example
%       % Fix columns for a specific argument:
%       ?- fix_columns_with_arg(problematic_arg).
%       ...
fix_columns_with_arg(Arg) :-
    % For each matching function and column number, apply fixes.
    forall(fb_arg_table_n(Arg, Fn, N),
        fix_columns_n(Fn, N)).

%!  fix_columns_n(+Fn, +N) is det.
%
%   Applies fixes to a specific function and column combination.
%
%   This predicate adds an assertion that the column number `N` of the function
%   `Fn` needs fixing, represented as `fix_columns_nth/2`.
%
%   @arg Fn The function whose column is being fixed.
%   @arg N  The column number being fixed.
fix_columns_n(Fn, N) :-
    % Add an assertion for the column that needs fixing.
    pfcAdd_Now(fix_columns_nth(Fn, N)).

%!  load_fb_mask(+Filename) is det.
%
%   Loads a file-based mask configuration.
%
%   This predicate handles file loading with adjustments based on the environment.
%   If running on Scryer Prolog, it processes symbols and character lists; otherwise,
%   it expands filenames and applies `load_fb_cache/1` to them.
%
%   @arg Filename The file or pattern to be loaded.
%
%   @example
%       % Load a mask file:
%       ?- load_fb_mask('mask_file.mask').
%       ...
load_fb_mask(Filename) :-
    % Handle Scryer Prolog-specific file loading.
    is_scryer,
    symbol(Filename),
    name(Filename, Chars),
    !,
    load_fb_mask(Chars).
load_fb_mask(Filename) :-
    % Expand and process filenames for other environments.
    expand_file_name(Filename, Files1),
    maplist(load_fb_cache, Files1).

%!  load_fb_cache(+File) is det.
%
%   Processes a single file and its associated cache.
%
%   This predicate uses `load_fb_cache0/1` to load files, optionally processing
%   wild paths for dynamic configurations.
%
%   @arg File The file to be processed.
load_fb_cache(File) :-
    % Process the file, handling paths dynamically.
    with_wild_path(load_fb_cache0, File).

%!  load_fb_cache0(+RFile) is det.
%
%   Loads a single cache file, creating output paths as needed.
%
%   This predicate determines the proper naming conventions for cache files
%   based on input filenames and processes them accordingly.
%
%   @arg RFile The raw file path to be processed.
load_fb_cache0(RFile) :-
    % Resolve the absolute file path.
    absolute_file_name(RFile, File),
    % Extract the base name and extension of the file.
    file_name_extension(Name, _E, File),
    % Split the name into publisher and table.
    symbolic_list_concat([Pub, Table], '.', Name),
    % Construct the output file path.
    symbolic_list_concat([Pub, Table, qlf], '.', OutputFile),
    !,
    % Load the file with the derived output.
    load_fb_cache(File, OutputFile, Table).
load_fb_cache0(File) :-
    % Handle cases without a publisher in the name.
    file_name_extension(Name, _E, File),
    % Extract just the table name.
    symbolic_list_concat([Table], '.', Name),
    % Construct the output file path.
    symbolic_list_concat([Table, qlf], '.', OutputFile),
    % Load the file with the derived output.
    load_fb_cache(File, OutputFile, Table).

% ============================================================================
%  LOAD FB Files
% ============================================================================

%!  track_load_into_file(+RelFilename, :Goal) is det.
%
%   Tracks the loading of a file and executes a specified goal.
%
%   This predicate resolves a relative file path to its absolute path, then ensures
%   that the specified goal is executed in a tracked context. If the file is already
%   being processed, the goal is executed immediately. Otherwise, the process is
%   tracked, including recording the loading count and saving metadata.
%
%   @arg RelFilename The relative filename of the file to be loaded.
%   @arg Goal        The goal to execute during the file's loading process.
%
%   @example
%       % Track and load a file with a specific goal:
%       ?- track_load_into_file('example_file.pl', some_goal).
%       ...
track_load_into_file(RelFilename, Goal) :-
    % Resolve the relative file name to an absolute path.
    must_det_ll(absolute_file_name(RelFilename, Filename)),
    % Proceed to track and load the file with the specified goal.
    must_det_ll(track_load_into_file0(Filename, Goal)),
    !.

%!  track_load_into_file0(+Filename, :Goal) is det.
%
%   Performs the file loading process with tracking.
%
%   If the file is already being tracked, the goal is executed immediately.
%   Otherwise, the predicate initializes tracking, records metadata, executes
%   the goal, and finalizes the process (e.g., by renaming temporary files).
%
%   @arg Filename The absolute filename of the file being loaded.
%   @arg Goal     The goal to execute during the file's loading process.
track_load_into_file0(Filename, Goal) :-
    % Check if the file is already being tracked.
    nb_current(tracking_file, FilenameW),
    Filename == FilenameW,
    % If yes, execute the goal immediately.
    !,
    call(Goal).

track_load_into_file0(Filename, Goal) :-
    % Start the tracking process for the file.
    must_det_ll((
        % Mark the file as the current tracking file.
        nb_setval(tracking_file, Filename),
        % Begin recording HTML metadata for the file.
        start_html_of(Filename),
        % Log the start of the loading process.
        fbug(track_load_into_file(Filename)),
        % Initialize the loading counter.
        flag(loaded_from_file_count, Was, 0)
    )),
    % Execute the goal within the context of the tracked file.
    must_det_ll(with_option(loading_file, Filename, time(must_det_ll(Goal)))),
    % Finalize the tracking process.
    must_det_ll((
        % Update the loading counter.
        flag(loaded_from_file_count, New, Was),
        % Record the loading count if necessary.
        ((New > 0 ; \+ is_loaded_from_file_count(Filename, _)) ->
            assert(is_loaded_from_file_count(Filename, New)) ; true),
        % Log the final count.
        fbug(Filename = New),
        % Rename temporary files associated with the file.
        rename_tmp_files(Filename),
        % Save the HTML metadata.
        save_html_of(Filename)
    )),
    !.

%!  rename_tmp_files(+Filename) is det.
%
%   Renames temporary files associated with a loaded file.
%
%   This predicate ensures that temporary files are renamed to their
%   final filenames once the loading process is complete. It operates
%   only if the system is in a converting state.
%
%   @arg Filename The base filename of the file being processed.
rename_tmp_files(_Filename) :-
    % Skip renaming if not in converting mode.
    \+ is_converting,
    !.
rename_tmp_files(Filename) :-
    % Rename specific temporary files associated with the base filename.
    rename_tmp_files(Filename, '.metta'),
    rename_tmp_files(Filename, '.metta.datalog').

%!  rename_tmp_files(+Filename, +NonTmp) is det.
%
%   Renames a specific temporary file associated with a filename.
%
%   This predicate constructs the temporary file path and its intended
%   final path, then attempts to rename the file if it exists.
%
%   @arg Filename The base filename.
%   @arg NonTmp   The non-temporary file extension to rename to.
rename_tmp_files(Filename, NonTmp) :-
    % Construct the source (temporary) file path.
    symbolic_list_concat([Filename, NonTmp, '.tmp'], From),
    % Construct the target (final) file path.
    symbolic_list_concat([Filename, NonTmp], To),
    % Log the renaming process.
    fbug(rename_file(From, To)),
    % Attempt to rename the file if it exists.
    ignore((
        exists_file(From),
        rename_file(From, To)
    )).

:- dynamic(is_loaded_from_file_count/2).

:- use_module(library(http/json)).
%:- ensure_loaded(json_loader).

%!  load_fb_json(+Fn, +File) is det.
%
%   Loads FlyBase JSON data into the system.
%
%   This predicate attempts to load JSON data from a file using either a custom
%   `load_flybase_json/2` predicate, if it exists, or directly processes the JSON
%   data. The loading process is tracked to ensure proper recording and metadata handling.
%
%   @arg Fn   A functor used to associate the loaded JSON data.
%   @arg File The filename of the JSON file to be loaded.
%
%   @example
%       % Load a FlyBase JSON file:
%       ?- load_fb_json(flybase_data, 'flybase.json').
%       ...
load_fb_json(Fn, File) :-
    % Log the start of the JSON loading process.
    fbug(load_fb_json(Fn, File)),
    % Check if a custom `load_flybase_json/2` predicate is available.
    current_predicate(load_flybase_json/2),
    % Resolve the absolute path of the file.
    absolute_file_name(File, Filename),
    % Track the loading of the file while invoking the custom loader.
    track_load_into_file(Filename, load_flybase_json(Fn, File)).

% Alternate method for loading JSON if no custom predicate exists.
load_fb_json(Fn, File) :-
    % Log the start of the JSON loading process.
    fbug(load_fb_json(Fn, File)),
    % Open the file with UTF-8 encoding and read its contents as a JSON term.
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        json:json_read(In, Term, []),
        close(In)
    ),
    % Record the loaded JSON term with an associated functor.
    time(assert(saved_fb_json(File, Term, Fn))).

%!  maybe_sample(+Fn, +Args) is det.
%
%   Optionally samples arguments for further processing.
%
%   This predicate samples arguments if the sampling condition is met, calling
%   `assert_arg_samples/3` to record them. Sampling is skipped if `should_sample/0` fails.
%
%   @arg Fn   The functor associated with the sampling process.
%   @arg Args The arguments to be sampled.
%
%   @example
%       % Sample arguments for a specific functor:
%       ?- maybe_sample(my_functor, [arg1, arg2, arg3]).
%       ...
maybe_sample(_Fn, _Args) :-
    % Skip sampling if the sampling condition is not met.
    \+ should_sample,
    !.
maybe_sample(Fn, Args) :-
    % Proceed with sampling by asserting argument samples.
    assert_arg_samples(Fn, 1, Args).

:-dynamic(fb_arg/1).
:-dynamic(fb_arg_table_n/3).

%!  assert_arg_table_n(+A, +Fn, +N) is det.
%
%   Asserts that an argument belongs to a specific function and column.
%
%   This predicate records an argument in the `fb_arg/1` and `fb_arg_table_n/3`
%   databases, linking it to a specific functor and column number.
%
%   @arg A   The argument to be recorded.
%   @arg Fn  The functor associated with the argument.
%   @arg N   The column number of the argument.
assert_arg_table_n(A, Fn, N) :-
    % Assert the argument in the global argument database.
    pfcAdd_Now(fb_arg(A)),
    % Assert the argument's association with the functor and column.
    pfcAdd_Now(fb_arg_table_n(A, Fn, N)).

%!  assert_arg_samples(+Fn, +N, +Args) is det.
%
%   Asserts samples of arguments for a specific functor.
%
%   This predicate iterates through a list of arguments, associating each with
%   a functor and column number. Arguments marked as "non-sampleable" are skipped.
%
%   @arg Fn   The functor associated with the arguments.
%   @arg N    The starting column number for sampling.
%   @arg Args The list of arguments to sample.
%
%   @example
%       % Assert samples for a functor:
%       ?- assert_arg_samples(my_functor, 1, [arg1, arg2, arg3]).
%       ...
assert_arg_samples(Fn, N, [A | Args]) :-
    % Skip arguments that should not be sampled.
    (dont_sample(A) -> true ; assert_arg_table_n(A, Fn, N)),
    % Increment the column number and process the remaining arguments.
    N2 is N + 1,
    assert_arg_samples(Fn, N2, Args).
assert_arg_samples(_, _, _).

%!  dont_sample(+N) is nondet.
%
%   Determines if an argument should not be sampled.
%
%   This predicate identifies arguments that are not suitable for sampling,
%   such as symbols or certain placeholder values.
%
%   @arg N The argument to check for sampling eligibility.
%
%   @example
%       % Check if an argument is non-sampleable:
%       ?- dont_sample('-').
%       true.
%
%       ?- dont_sample(42).
%       false.
dont_sample(N) :-
    % Skip sampling for non-symbol arguments.
    \+ symbol(N).
dont_sample('').
dont_sample('-').

%!  data_pred0(+X, -Y) is det.
%
%   Processes a symbolic name or path `X` and simplifies it into a canonical form `Y`.
%
%   This predicate recursively applies various transformations to simplify `X` into `Y`.
%   The transformations handle specific patterns such as file extensions, path separators,
%   and FlyBase-specific naming conventions.
%
%   @arg X The input symbolic name or path.
%   @arg Y The simplified or canonicalized version of `X`.
%
%   @example
%       % Simplify a symbolic name:
%       ?- data_pred0('public.flybase_data.tsv', Y).
%       Y = flybase_data.
%
%       % Handle FlyBase naming conventions:
%       ?- data_pred0('example_fb_3', Y).
%       Y = example_fb.
data_pred0(X,Y):- symbolic_list_concat(List,'/',X),List\==[],List\=[_],!,last(List,L),data_pred0(L,Y).
data_pred0(X,Y):- symbol_concat(YY,'.tsv',X),!,data_pred0(YY,Y).
data_pred0(X,Y):- symbol_concat(YY,'.fb',X),!,data_pred0(YY,Y).
data_pred0(X,Y):- symbol_concat(YY,'_',X),!,data_pred0(YY,Y).
data_pred0(X,Y):- symbol_concat(YY,'_fb',X),!,data_pred0(YY,Y).
data_pred0(X,Y):- symbol_concat('public.',YY,X),!,data_pred0(YY,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'.',X),L=[_,_|_],symbolic_list_concat(L,'_',XL),!,data_pred0(XL,Y).
%data_pred0(X,Y):- symbolic_list_concat([L,_|_],'_fb_2',X),!,data_pred0(L,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_0',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_1',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_2',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_3',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_4',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_5',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_6',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_7',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_8',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb_9',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
data_pred0(X,Y):- symbolic_list_concat(L,'_fb__',X),L=[_,_|_],symbolic_list_concat(L,'_fb_',XL),!,data_pred0(XL,Y).
%data_pred0(X,Y):- symbolic_list_concat(List,'_',X),once(not_trimmed_path(List,NewList)),
%  NewList\==[],NewList\==List,symbolic_list_concat(NewList,'_',Y),!.
data_pred0(X,X).
data_pred(X,Y):- data_pred0(X,Y), Y\=='',!.
data_pred(X,X).

%!  is_swipl is nondet.
%
%   Succeeds if the Prolog environment is SWI-Prolog.
%
%   This predicate checks if the current Prolog environment is SWI-Prolog.
%   It is defined as the negation of `is_scryer/0`, assuming that `is_scryer/0`
%   identifies the Scryer Prolog environment.
%
%   @example
%       % Check if running in SWI-Prolog:
%       ?- is_swipl.
%       true.
is_swipl :-
    % Negate the result of `is_scryer/0`.
    \+ is_scryer.

% Define `read_line_to_chars/2` differently depending on the Prolog environment.
:- if(is_scryer).

%!  read_line_to_chars(+Stream, -Chars) is det.
%
%   Reads a line from a stream as a list of characters (Scryer Prolog version).
%
%   In Scryer Prolog, this implementation uses the built-in `get_line_to_chars/3`
%   to read a line from the stream and convert it into a list of characters.
%
%   @arg Stream The input stream from which the line is read.
%   @arg Chars  The list of characters read from the stream.
read_line_to_chars(S, L) :-
    % Ensure this clause is used only in Scryer Prolog.
    is_scryer,
    !,
    % Use Scryer-specific predicate to read the line.
    get_line_to_chars(S, L, []).
:- endif.

%!  read_line_to_chars(+Stream, -Chars) is det.
%
%   Reads a line from a stream as a list of characters (generic version).
%
%   For non-Scryer environments, this predicate reads a line from the stream
%   using `read_line_to_string/2` and converts the resulting string into a
%   list of characters.
%
%   @arg Stream The input stream from which the line is read.
%   @arg Chars  The list of characters read from the stream.
%
%   @example
%       % Read a line from the current input:
%       ?- current_input(S), read_line_to_chars(S, Chars).
%       Chars = ['H', 'e', 'l', 'l', 'o'].
read_line_to_chars(S, L) :-
    % Read a line from the stream as a string.
    read_line_to_string(S, Str),
    % Convert the string to a list of characters.
    string_chars(Str, L).

%!  fb_assert(+Term) is det.
%
%   Asserts a given term into the database if no variant of it already exists.
%
%   This predicate checks whether a variant of the given term (`Term`) already
%   exists in the database. If no such variant exists, it asserts the term. The
%   term can be either a rule (`Head :- Body`) or a fact (`Head`).
%
%   @arg Term The term to assert. Can be a rule or a fact.
%
%   @example
%       % Assert a fact if it doesn't already exist:
%       ?- fb_assert(my_fact(a)).
%
%       % Assert a rule if it doesn't already exist:
%       ?- fb_assert(my_rule(X) :- member(X, [1, 2, 3])).
%
fb_assert(Term) :-
    % Check if Term is a rule (Head :- Body) or a fact (just Head).
    ( Term = (Head :- Body)
    -> copy_term(Body, CopiedBody)
    ; (Head = Term, CopiedBody = true)
    ),
    % Copy the Head to generate a new term with fresh variables.
    copy_term(Head, CopiedHead),
    % If no variant of CopiedHead exists in the database with the same body,
    % assert Term; otherwise, succeed without asserting Term.
    ( \+ (clause(CopiedHead, CopiedBody), variant(CopiedHead, Head))
    -> assertz(Term)
    ; true
    ).

:- dynamic(done_reading/1).

%!  use_metta_x is nondet.
%
%   Placeholder predicate that always fails.
%
%   This predicate is a placeholder, possibly intended for future logic or configuration.
%   It currently always fails due to the combination of `fail` and `fail_flag`.
%
%   @example
%       % Check the status of `use_metta_x`:
%       ?- use_metta_x.
%       false.
use_metta_x :-
    fail,
    fail_flag.

%!  load_fb_cache(+File, +OutputFile, +Fn) is det.
%
%   Loads a FlyBase cache file, either by ensuring the output file is loaded
%   if it exists or by compiling the input file.
%
%   This predicate first checks if the output file already exists. If so, it
%   ensures the file is loaded. Otherwise, it compiles the input file using
%   `load_files/2` with `qcompile(large)`.
%
%   @arg File       The input file to be compiled or loaded.
%   @arg OutputFile The output file to check or load.
%   @arg Fn         A functor associated with the file loading process.
load_fb_cache(_File, OutputFile, _Fn) :-
    % If the output file exists, load it directly.
    exists_file(OutputFile), !,
    ensure_loaded(OutputFile),!.
load_fb_cache(File, _OutputFile, _Fn) :-
    % Otherwise, compile the input file with large file handling.
    load_files([File], [qcompile(large)]).

%!  'load_flybase_tiny' is det.
%
%   Loads a tiny subset of the FlyBase data.
%
%   This predicate is a shortcut for calling `load_flybase/1` with a small
%   limit of 20,000 entries.
%
%   @example
%       % Load a tiny FlyBase dataset:
%       ?- 'load_flybase_tiny'.
%       ...
'load_flybase_tiny' :-
    load_flybase(20_000).

%!  'load_flybase_full' is det.
%
%   Loads the full FlyBase dataset.
%
%   This predicate calls `load_flybase/1` with an extremely high limit,
%   effectively attempting to load all available data.
%
%   @example
%       % Load the full FlyBase dataset:
%       ?- 'load_flybase_full'.
%       ...
'load_flybase_full' :-
    load_flybase(1_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000).

%!  'save_flybase_full' is det.
%
%   Loads the full FlyBase dataset and saves it as a program.
%
%   This predicate combines `load_flybase_full/0` and `qsave_program/0`
%   to load all data and save it as a compiled program.
%
%   @example
%       % Load and save the full FlyBase dataset:
%       ?- 'save_flybase_full'.
%       ...
'save_flybase_full' :-
    load_flybase_full,
    qsave_program.

%!  load_flybase_with_size(+N) is det.
%
%   Loads FlyBase data up to a specified limit.
%
%   If the limit `N` is a number or `inf` (infinity), this predicate proceeds
%   to load data with the specified maximum entries. It uses `with_option/2`
%   to configure the limit dynamically during the loading process.
%
%   @arg N The maximum number of entries to load (numeric or `inf`).
%
%   @example
%       % Load FlyBase data with a specific limit:
%       ?- load_flybase_with_size(10000).
%       ...
load_flybase_with_size(N) :-
    % Ensure `N` is a number or `inf` before proceeding.
    (number(N) -> true ; N == inf),
    !,
    % Dynamically configure and load data with the specified limit.
    with_option([max_per_file = N], (
        % Log the maximum entries being processed.
        option_value(max_per_file, Max),
        write(max_per_file = Max),
        load_flybase
    )).

%!  load_flybase(+File) is det.
%
%   Loads FlyBase data from a specified file.
%
%   This predicate determines the file extension and delegates the loading
%   process to `load_flybase/2`.
%
%   @arg File The input file containing FlyBase data.
%
%   @example
%       % Load FlyBase data from a file:
%       ?- load_flybase('flybase_data.csv').
%       ...
load_flybase(File) :-
    % Extract the file extension and delegate to `load_flybase/2`.
    file_name_extension(_, Ext, File),
    !,
    load_flybase(File, Ext).

% These directives import the load_flybase/1 predicate into the system module.
:- export(load_flybase/1).
:- system:import(load_flybase/1).

%!  load_flybase(+File, +Ext) is det.
%
%   Loads FlyBase data from a file with a specific extension.
%
%   This predicate dynamically applies `with_wild_path/2` to load files based
%   on their extensions.
%
%   @arg File The input file containing FlyBase data.
%   @arg Ext  The file extension.
load_flybase(File, Ext) :-
    % Delegate loading to the appropriate handler for the file type.
    with_wild_path(load_flybase0(Ext), File),
    !.

%!  exists_with_ext(+File, +Ext) is nondet.
%
%   Checks if a file with a specific extension exists.
%
%   This predicate appends the extension `Ext` to the base filename `File`
%   and verifies its existence.
%
%   @arg File The base filename.
%   @arg Ext  The file extension to check.
%
%   @example
%       % Check for a file with a `.csv` extension:
%       ?- exists_with_ext('flybase_data', '.csv').
%       true.
exists_with_ext(File, Ext) :-
    % Concatenate the base filename and extension.
    atom_concat(File, Ext, Res),
    % Check if the resulting file exists.
    exists_file(Res),
    !.

%!  load_flybase0(+Ext, +File) is det.
%
%   Loads a FlyBase file based on its extension and the current system state.
%
%   This predicate determines how to handle the file `File` based on its extension `Ext`.
%   It accounts for specific extensions (e.g., `.pl`, `.metta`, `.datalog`) and whether
%   the system is in a "converting" state. If no direct match is found, it delegates the
%   task to `load_flybase/3` using a predicate derived from the file name.
%
%   @arg Ext  The file extension (e.g., `'pl'`, `'metta'`, `'datalog'`).
%   @arg File The file to be loaded.
%
%   @example
%       % Load a `.pl` file (no operation performed for `.pl` files):
%       ?- load_flybase0('pl', 'example.pl').
%       true.
%
%       % Load a `.metta` file while converting:
%       ?- is_converting, load_flybase0('metta', 'example.metta').
%       true.
%
%       % Load a file with a dynamic predicate derived from its name:
%       ?- load_flybase0('', 'example.datalog').
%       ...
load_flybase0(Ext,File):- Ext=='',file_name_extension(_,Ext2,File),Ext2\=='',!,load_flybase0(Ext2,File).
load_flybase0(Ext,_File):-  Ext=='pl',!.
load_flybase0(Ext,_File):-  Ext=='metta', is_converting,!.
load_flybase0(Ext,_File):-  Ext=='datalog', is_converting,!.
load_flybase0(_Ext, File):-  is_converting,exists_with_ext(File,'.metta'),(exists_with_ext(File,'.datalog');exists_with_ext(File,'.metta.datalog')),!.
load_flybase0(Ext,File):-
  must_det_ll((file_name_extension(Name,_,File),
  data_pred(Name,Fn),load_flybase(Ext,File,Fn))).

:- dynamic(load_state/2).

% load_flybase(_Ext, _File, OutputFile, _Fn):-
%     exists_file(OutputFile),
%     size_file(OutputFile, N),
%     N > 100,
%     !.

%!  load_flybase(+Ext, +File, +Fn) is det.
%
%   Loads a FlyBase file, handling its state and invoking the appropriate loader.
%
%   This predicate manages the state of a FlyBase file (`File`) while loading it.
%   It ensures that the file's state transitions through `loading` to `loaded`
%   and invokes the appropriate loading process based on its extension (`Ext`)
%   and derived predicate (`Fn`).
%
%   A previously loaded state is checked to prevent redundant operations.
%
%   @arg Ext  The file extension (e.g., `'metta'`, `'datalog'`).
%   @arg File The FlyBase file to load.
%   @arg Fn   The functor associated with the file loading process.
%
%   @example
%       % Load a FlyBase file:
%       ?- load_flybase('metta', 'example.metta', example_fn).
%       ...
load_flybase(_Ext, File, _Fn) :-
    % If the file is already loaded, skip reloading it.
    load_state(File, _),
    !.
load_flybase(Ext, File, Fn) :-
    must_det_ll((
        % Assert that the file is currently in the loading state.
        assert(load_state(File, loading)),
        % Log the start of the file loading process.
        fbug(load_flybase(Ext, File, Fn)),
        % Delegate the actual loading process based on extension.
        load_flybase_ext(Ext, File, Fn),
        % Transition the file's state from loading to loaded.
        ignore(retract(load_state(File, loading))),
        assert(load_state(File, loaded)),
        % Collect and update FlyBase statistics.
        fb_stats
    )).

%!  lfbm is det.
%
%   Loads a specific FlyBase file for performance testing.
%
%   This predicate loads a FlyBase knowledge graph file located in the
%   `tests/performance/knowledge_graphs/graphml_csv/cml/` directory.
%   The file `ckg_neighbors_cml_edges_e21425.csv` is loaded with the
%   `csv` file extension, utilizing the `load_flybase/2` predicate.
%
%   @example
%       % Load the specified FlyBase knowledge graph:
%       ?- lfbm.
%       ...
lfbm :-
    % Load the specified CSV file using the FlyBase loader.
    load_flybase('tests/performance/knowledge_graphs/graphml_csv/cml/ckg_neighbors_cml_edges_e21425.csv', csv).

%!  load_flybase_ext(+Ext, +File, +Fn) is det.
%
%   Handles FlyBase file loading based on file extension and processing requirements.
%
%   This predicate processes FlyBase files using specific logic for their extensions
%   (`Ext`). It supports extensions such as `.metta_x`, `.metta`, `.obo`, `.json`,
%   `.gff`, `.fasta`, and others. Each extension is mapped to the appropriate loading
%   method, including support for custom loaders like `load_obo/1` or `load_metta/2`.
%
%   @arg Ext  The extension of the file (e.g., `'json'`, `'obo'`, `'metta'`).
%   @arg File The path to the file being loaded.
%   @arg Fn   The functor associated with the loading process.
%
%   @example
%       % Load a JSON FlyBase file:
%       ?- load_flybase_ext(json, 'data/example.json', example_fn).
%
%       % Attempt to load a non-supported extension:
%       ?- load_flybase_ext('unsupported', 'data/example.unsupported', example_fn).
%       false.
%
load_flybase_ext(_Ext,File,_Fn):- use_metta_x,  atom_concat(File,'.metta_x',MFile),
  exists_file(MFile), \+ is_converting, % Ext \== 'obo',
  \+ option_value(metta_x_files,false),!,
  process_metta_x_file(MFile).
load_flybase_ext(_Ext,File,_Fn):-  fail, atom_concat(File,'.metta',MFile),
  exists_file(MFile), \+ is_converting, % Ext \== 'obo',
  \+ option_value(mettafiles,false),!,
  load_flybase_metta(MFile).
load_flybase_ext(Ext,File,_Fn):-  Ext==obo,current_predicate(load_obo/1),!,load_obo(File).
load_flybase_ext(Ext,File,_Fn):-  Ext==scm,include_atomspace_1_0(File).
load_flybase_ext(Ext,File, Fn):-  Ext==json,!,load_fb_json(Fn,File),!.
load_flybase_ext(Ext,File, Fn):-  Ext==gff,!,load_fb_gff(Fn,File),!.
load_flybase_ext(Ext,File, Fn):-  Ext==gff3,!,load_fb_gff(Fn,File),!.
load_flybase_ext(Ext,File, Fn):-  Ext==fa,!,load_fb_fa(Fn,File),!.
load_flybase_ext(Ext,File, Fn):-  Ext==fasta,!,load_fb_fa(Fn,File),!.
load_flybase_ext(Ext,File,_Fn):-  Ext==metta,current_predicate(load_metta/2),!,load_flybase_metta(File).
load_flybase_ext(Ext,File, Fn):-  file_to_sep(Ext,Sep),!,
  track_load_into_file(File,
    setup_call_cleanup(open(File,read,Stream),
       must_det_ll(load_flybase_sv(Sep,File,Stream,Fn)),
        close(Stream))),!.
load_flybase_ext(Ext,File, Fn):-  fbug(missed_loading_flybase(Ext,File,Fn)),!,fail.

% load_flybase_metta(File):-
%     !,
%     load_metta('&flybase', File).

%!  load_flybase_metta(+File) is det.
%
%   Loads a FlyBase `.metta` file into the system.
%
%   This predicate wraps the loading process with specific options, such as
%   disabling trace-on-load and setting the `current_self` context to `&flybase`.
%   The actual file inclusion is handled by `include_atomspace_1_0/1`.
%
%   @arg File The `.metta` file to load.
%
%   @example
%       % Load a FlyBase metta file:
%       ?- load_flybase_metta('flybase.metta').
%       ...
load_flybase_metta(File) :-
    % Wrap the loading process with customized options.
    with_option('trace-on-load', false,
        with_option('current_self', '&flybase',
            % Include the file into the Atomspace context.
            include_atomspace_1_0(File)
        )
    ).

%!  fix_list_args(+Fn, +ArgTypes, +Args, -NewArgs) is det.
%
%   Adjusts or canonicalizes a list of arguments based on specific rules.
%
%   This predicate modifies a list of arguments (`Args`) associated with a given
%   functor (`Fn`) and argument types (`ArgTypes`). If the `early_canon` option
%   is set to an empty list and sampling is disabled, the arguments are returned
%   unchanged. Otherwise, it attempts to transform the arguments.
%
%   @arg Fn       The functor associated with the arguments.
%   @arg ArgTypes The types of the arguments being processed.
%   @arg Args     The original list of arguments.
%   @arg NewArgs  The transformed or canonicalized list of arguments.
%
%   @example
%       % Transform a list of arguments:
%       ?- fix_list_args(my_functor, [type1, type2], [arg1, arg2], NewArgs).
%       ...
%
%   @see primary_term/5, fix_elist_args/6.
fix_list_args(_, _, Y, Y) :-
    % If `early_canon` is empty and sampling is disabled, return arguments unchanged.
    option_value(early_canon, []),
    \+ should_sample,
    !.

% fix_list_args(_Fn, _ArgTypes, [X], [X]) :-
%     !.
fix_list_args(Fn, ArgTypes, Args, NewArgs) :-
    % Apply transformation logic when adjustments are needed.
    must_det_ll((
        % Generate a primary term based on the functor, argument types, and arguments.
        primary_term(Fn, ArgTypes, Args, Term, NewArgTypes),
        % Adjust or canonicalize the argument list.
        fix_elist_args(Term, Fn, 1, NewArgTypes, Args, NewArgs),
        % Log changes for debugging if the arguments were modified.
        extreme_debug(ignore(((Args \== NewArgs, fbug(NewArgs)))))
    )).

fix_list_args(_Fn, _ArgTypes, Args, Args) :-
    % Default case: return the arguments unchanged.
    !.

%!  primary_term(+Fn, +ArgTypes, +Args, -Term, -NewArgTypes) is det.
%
%   Determines the primary term and adjusted argument types for a given functor and argument list.
%
%   This predicate identifies a "primary term" from the list of arguments (`Args`) based on the
%   argument types (`ArgTypes`) and potentially modifies the argument types to reflect this
%   selection. It handles cases where the primary term is specified by a numeric position,
%   a primary column, or explicitly marked as `primary(Name)`.
%
%   @arg Fn          The functor associated with the arguments.
%   @arg ArgTypes    The list of argument types (e.g., `[type1, type2]`).
%   @arg Args        The list of arguments corresponding to the argument types.
%   @arg Term        The selected primary term.
%   @arg NewArgTypes The updated list of argument types after processing.
%
%   @example
%       % Select a primary term using argument types:
%       ?- primary_term(my_functor, [1, type2], [arg1, arg2], Term, NewArgTypes).
%       Term = arg1,
%       NewArgTypes = [type2].
%
%       % Use a primary column to determine the term:
%       ?- primary_term(my_functor, [type1, primary(primary_name), type2], [arg1, arg2, arg3], Term, NewArgTypes).
%       Term = arg2,
%       NewArgTypes = [type1, primary_name, type2].
primary_term(_Fn, [N | ArgTypes], _Args, _Term, ArgTypes) :-
    % Handle the case where the primary term is determined by a numeric position.
    number(N),
    !.
primary_term(_Fn, [N | ArgTypes], Args, Term, ArgTypes) :-
    % Extract the term at position `N` in the argument list.
    number(N),
    !,
    nth1(N, Args, Term).
primary_term(_Fn, ArgTypes, _Args, _Term, ArgTypes) :-
    % Base case: no specific logic applies, return the original argument types.
    !.
primary_term(_Fn, ArgTypes, Args, Term, NewArgTypes) :-
    % Handle explicitly marked primary terms.
    append(L, [primary(Name) | R], ArgTypes),
    append(L, [Name | R], NewArgTypes),
    length(L, N),
    nth0(N, Args, Term).
primary_term(Fn, ArgTypes, Args, Term, ArgTypes) :-
    % Use a primary column to select the term.
    primary_column(Fn, Name),
    nth1(N, ArgTypes, Name),
    !,
    nth1(N, Args, Term),
    !.
primary_term(_Fn, ArgTypes, [Term | _], Term, ArgTypes) :-
    % Fallback to the first argument if no other rule matches.
    !.
primary_term(_Fn, ArgTypes, _Args, _Term, ArgTypes).

%!  fix_elist_args(+Term, +Fn, +N, +ArgTypes, +Args, -NewArgs) is det.
%
%   Adjusts or canonicalizes an extended list of arguments.
%
%   This predicate iteratively processes arguments in `Args` based on their
%   corresponding types in `ArgTypes`. The result is a transformed list of
%   arguments (`NewArgs`) that complies with the expected types.
%
%   @arg Term      The primary term associated with the arguments.
%   @arg Fn        The functor associated with the arguments.
%   @arg N         The current argument position (1-based index).
%   @arg ArgTypes  The list of expected argument types.
%   @arg Args      The original list of arguments.
%   @arg NewArgs   The transformed list of arguments.
%
%   @see adjust_type/6.
fix_elist_args(Term, Fn, N, [Nth | ArgTypes], Args, NewArgs) :-
    % Skip processing for numeric argument types.
    number(Nth),
    !,
    fix_elist_args(Term, Fn, N, ArgTypes, Args, NewArgs).
fix_elist_args(Term, Fn, N, [Type | ArgTypes], [Concept | Args], [Arg | NewArgs]) :-
    % Adjust the current argument based on its type.
    !,
    must_det_ll((
        adjust_type(Term, Fn, N, Type, Concept, Arg),
        N2 is N + 1,
        fix_elist_args(Term, Fn, N2, ArgTypes, Args, NewArgs)
    )).
fix_elist_args(_Term, _Fn, _N, _, X, X).  % Base case: no more arguments to process.

%!  adjust_type(+Term, +Fn, +N, +Type, +Concept, -Arg) is det.
%
%   Adjusts an argument to comply with the specified type.
%
%   This predicate transforms a given argument (`Concept`) into its adjusted form
%   (`Arg`) based on the expected type (`Type`). It handles lists, numeric conversions,
%   and general type fixes.
%
%   @arg Term     The primary term associated with the arguments.
%   @arg Fn       The functor associated with the arguments.
%   @arg N        The current argument position (1-based index).
%   @arg Type     The expected type for the argument.
%   @arg Concept  The original argument to be adjusted.
%   @arg Arg      The transformed argument.
adjust_type(Term, Fn, N, listOf(Type), Arg, NewL) :-
    % Adjust a list of elements of a specific type.
    must_det_ll((
        nonvar(Type),
        as_list([], Arg, New),
        is_list(New),
        maplist(adjust_type(Term, Fn, N, Type), New, NewL)
    )).
adjust_type(Term, Fn, N, listOf(Type, Seps), Arg, NewL) :-
    % Adjust a list of elements with specific separators.
    must_det_ll((
        nonvar(Type),
        as_list(Seps, Arg, New),
        is_list(New),
        maplist(adjust_type(Term, Fn, N, Type), New, NewL)
    )).
adjust_type(Term, Fn, N, Type, Concept, Arg) :-
    % Handle numeric conversions.
    numeric_value_p_n(Fn, N, _),
    !,
    must_det_ll((
        (into_number(Concept, Arg) -> true ; (Concept = Arg)),
        assert_type_of(Term, Fn, N, Type, Arg)
    )).
adjust_type(Term, Fn, N, Type, Concept, Arg) :-
    % General adjustment for other types.
    must_det_ll((
        fix_concept(Concept, Arg),
        assert_type_of(Term, Fn, N, Type, Arg)
    )).
adjust_type(_Term, _Fn, _N, _, X, X).

%!  into_number(+Concept, -Arg) is nondet.
%
%   Converts a concept into a numeric value if possible.
%
%   This predicate attempts to convert `Concept` into `Arg`, ensuring that `Arg`
%   is a numeric value. If `Concept` is already numeric, it succeeds directly.
%
%   @arg Concept The original value to be converted.
%   @arg Arg     The numeric representation of `Concept`, if applicable.
into_number(Concept, Arg) :- number(Concept),!,Arg = Concept.
into_number(Concept, Arg) :- symbol_number(Concept, Arg),!.
into_number(Concept, Arg) :- Concept = Arg,!.

:- dynamic(fb_arg/1).
:- dynamic(fb_arg_table_n/3).

%!  assert_type_of(+Term, +Fn, +N, +Type, +Arg) is det.
%
%   Asserts the type of an argument into the knowledge base if sampling is enabled.
%
%   This predicate records type-related information for an argument (`Arg`) into the
%   database. It ensures that the argument is processed as a single value or as a list
%   of values. If `should_sample/0` fails, the predicate succeeds without performing
%   any operations.
%
%   @arg Term The primary term associated with the argument (not currently used).
%   @arg Fn   The functor to which the argument belongs.
%   @arg N    The position of the argument within the functor's argument list (1-based index).
%   @arg Type The expected type of the argument (not currently used).
%   @arg Arg  The argument or list of arguments to process.
%
%   @example
%       % Assert a single argument:
%       ?- assert_type_of(term_example, my_functor, 1, type_example, 'arg1').
%
%       % Assert a list of arguments:
%       ?- assert_type_of(term_example, my_functor, 1, type_example, ['arg1', 'arg2']).
%
%   @see should_sample/0, pfcAdd_Now/1.
assert_type_of(_Term, _Fn, _N, _Type, _Arg) :-
    % Skip processing if sampling is not enabled.
    \+ should_sample,
    !.
assert_type_of(Term, Fn, N, Type, Arg) :-
    % Handle the case where Arg is a list by recursively asserting each element.
    is_list(Arg),
    !,
    maplist(assert_type_of(Term, Fn, N, Type), Arg).
assert_type_of(_Term, Fn, N, _Type, Arg) :-
    % Assert type-related information for a single argument.
    must_det_ll((
        % Add the argument to the `fb_arg/1` database.
        pfcAdd_Now(fb_arg(Arg)),
        % Record the argument's association with the functor and its position.
        pfcAdd_Now(fb_arg_table_n(Arg, Fn, N))
    )).

:- dynamic(fb_arg_type/1).
:- dynamic(table_n_type/3).

%!  add_table_n_types(+Fn, +N, +ArgTypes) is det.
%
%   Records argument types for a given functor in the knowledge base.
%
%   This predicate processes a list of argument types (`ArgTypes`) for a functor (`Fn`)
%   and associates each type with its corresponding argument position (`N`). If a type
%   contains a symbolic sub-term, it is recorded in the `fb_arg_type/1` and
%   `table_n_type/3` databases.
%
%   @arg Fn       The functor associated with the argument types.
%   @arg N        The starting position for arguments (1-based index).
%   @arg ArgTypes The list of argument types to process.
%
%   @example
%       % Record argument types for a functor:
%       ?- add_table_n_types(my_functor, 1, [type1, type2]).
%
%       % Skip processing for invalid (non-list) argument types:
%       ?- add_table_n_types(my_functor, 1, invalid).
%       true.
%
%   @see fb_arg_type/1, table_n_type/3.
add_table_n_types(_Fn, _, ArgTypes) :-
    % Skip processing if ArgTypes is not a list.
    \+ is_list(ArgTypes),
    !.
add_table_n_types(Fn, 1, [N | ArgTypes]) :-
    % Skip numeric entries in the argument types list.
    number(N),
    !,
    add_table_n_types(Fn, 1, ArgTypes).
add_table_n_types(Fn, N, [Type | ArgTypes]) :-
    % Process symbolic sub-terms in argument types.
    !,
    sub_term(Sub, Type),
    symbol(Sub),
    !,
    % Add the symbolic sub-term as an argument type.
    pfcAdd_Now(fb_arg_type(Sub)),
    % Record the type's position and associated functor.
    pfcAdd_Now(table_n_type(Fn, N, Sub)),
    % Increment the argument position and continue processing.
    N2 is N + 1,
    add_table_n_types(Fn, N2, ArgTypes),
    !.
add_table_n_types(_Fn, _, []). % Base case: empty argument types list.

%!  is_concept(+Arg) is nondet.
%
%   Succeeds if `Arg` is recorded as a concept in the database.
%
%   @arg Arg The argument to check.
is_concept(Arg) :-
    fb_arg(Arg).

%!  is_concept_type(+Type) is nondet.
%
%   Succeeds if `Type` is recorded as an argument type in the database.
%
%   @arg Type The argument type to check.
is_concept_type(Type) :-
    fb_arg_type(Type).

%!  arg_table_n_type(+Arg, +Fn, +N, -Type) is nondet.
%
%   Retrieves the type of an argument in a specific position for a functor.
%
%   This predicate checks the argument type (`Type`) of a functor (`Fn`) at
%   a given position (`N`) and ensures the argument is not a list.
%
%   @arg Arg  The argument to retrieve the type for.
%   @arg Fn   The functor associated with the argument.
%   @arg N    The position of the argument within the functor's arguments.
%   @arg Type The type of the argument.
arg_table_n_type(Arg, Fn, N, Type) :-
    % Retrieve the type of the argument at position N.
    table_n_type(Fn, N, Type),
    % Ensure the argument matches the functor's signature.
    once((
        fb_pred(Fn, A),
        functor(G, Fn, A),
        arg(N, G, Arg),
        call(G),
        \+ is_list(Arg),
        \+ as_list(Arg, [])
    )).

%!  is_valuesymbol(+Fn, +N, -Type) is nondet.
%
%   Succeeds if an argument at position `N` for functor `Fn` is a value symbol.
%
%   A value symbol is a symbolic argument that can be converted into a numeric value.
%
%   @arg Fn   The functor associated with the argument.
%   @arg N    The position of the argument.
%   @arg Type The type of the argument.
is_valuesymbol(Fn, N, Type) :-
    arg_table_n_type(Arg, Fn, N, Type),
    symbol_number(Arg, _).

:-dynamic(numeric_value_p_n/3).

%!  fis_valuesymbol(-PNList, -Len) is det.
%
%   Finds all value symbols and returns their functor-position pairs and count.
%
%   This predicate collects all value symbols, represented as pairs of functor (`P`)
%   and position (`N`), and returns the list along with its length.
%
%   @arg PNList The list of functor-position pairs representing value symbols.
%   @arg Len    The length of the list.
fis_valuesymbol(PNList, Len) :-
    % Find all value symbols as functor-position pairs.
    findall(P-N, is_valuesymbol(P, N, _Type), PNList),
    % Calculate the length of the list.
    length(PNList, Len).

%!  save_value_symbol_cols is det.
%
%   Records all value symbol columns into the `numeric_value_p_n/3` database.
%
%   This predicate iterates over all value symbols, asserting their functor,
%   position, and type into the database. It then lists all recorded entries.
%
%   @example
%       % Save and display value symbol columns:
%       ?- save_value_symbol_cols.
%       ...
save_value_symbol_cols :-
    % Record each value symbol into the database.
    for_all(is_valuesymbol(Fn, N, Type),
        pfcAdd_Now(numeric_value_p_n(Fn, N, Type))
    ),
    % Display all recorded value symbol columns.
    listing(numeric_value_p_n/3).


/*
load_flybase_chars(Sep,File,Stream,Chars,OutputStream,Fn):-
  flag(loaded_from_file_count,_,0),
  ignore(once((table_columns(File,Header);table_columns(Fn,Header)))),
  fix_header_names(Fn,Header,ArgTypes),
  for_all((table_columns(File,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(File,ColInfo))),
  for_all((table_columns(Fn,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(Fn,ColInfo))),
  if_t(is_list(ArgTypes),set_option_value(fb_argtypes,ArgTypes)),
  time((repeat,
  read_line_to_chars(Stream, Chars),
    once(load_flybase_chars(Sep,File,Stream,Chars,OutputStream,Fn)),
  once(done_reading(File);reached_file_max;at_end_of_stream(Stream)),!,
    once(end_fb_file_data(File,Stream,Fn,OutputStream)),
  loaded_from_file_count(X),!,
  fb_stats(Fn),
  pl_stats(File,X))).

write_flybase_data(_OutputStream,_Fn,[]):-!.
write_flybase_data(_OutputStream,_Fn,['']):-!.
write_flybase_data(_OutputStream,Fn,DataL):- assert_MeTTa(Fn,DataL).

FBgn: FlyBase gene number - Represents a gene.
FBal: FlyBase allele number - Represents an allele.
FBst: FlyBase stock number - Represents a stock.
FBtp: FlyBase transposon number - Represents a transposon.
FBab: FlyBase aberration number - Represents a chromosomal aberration.
FBba: FlyBase balancer number - Represents a balancer.
FBcl: FlyBase clone number - Represents a DNA or RNA clone.
FBim: FlyBase image number - Represents an image.
FBpp: FlyBase polypeptide number - Represents a protein.
FBtr: FlyBase transcript number - Represents a transcript.
FBte: FlyBase transgenic element number - Represents a transgenic element.
*/

%!  process_metta_x_file(+MXFile) is det.
%
%   Processes a `.metta_x` file and asserts its data into the knowledge base.
%
%   This predicate reads a `.metta_x` file line by line, parses each row into
%   tab-separated values, and asserts the resulting data into the knowledge base
%   under a derived functor (`Fn`). The file is read in UTF-8 encoding.
%
%   @arg MXFile The path to the `.metta_x` file to process.
%
%   @example
%       % Process a `.metta_x` file:
%       ?- process_metta_x_file('example.metta_x').
%       ...
process_metta_x_file(MXFile) :-
    % Derive the functor for the data from the file name.
    data_pred(MXFile, Fn),
    % Open the file for reading in UTF-8 encoding.
    setup_call_cleanup(
        open(MXFile, read, In, [encoding(utf8)]),
        % Process each line of the file.
        ((
            repeat,
            % Read a line as a string.
            read_line_to_string(In, Chars),
            % Stop processing at the end of the file.
            (In == end_of_file -> ! ;
            % Parse the line and assert the resulting data.
            once((
                % Split the line into tab-separated values.
                symbolic_list_concat(Row0, '\t', Chars),
                % Process each column in the row.
                maplist(fast_column, Row0, Row),
                % Assert the processed row into the knowledge base.
                assert_MeTTa([Fn | Row]))
            )))
        ),
        % Ensure the file is closed after processing.
        close(In)
    ).

%!  fast_column(+X, -Y) is det.
%
%   Processes a single column in a `.metta_x` file row.
%
%   This predicate attempts to convert a column value (`X`) into a FlyBase term (`Y`).
%   If conversion fails, it defaults to returning the original value (`X`).
%
%   @arg X The original column value.
%   @arg Y The processed column value or the original value if no conversion is applicable.
%
%   @example
%       % Process a column value:
%       ?- fast_column('123', Y).
%       Y = 123.
%
%       % Handle a non-convertible value:
%       ?- fast_column('unknown', Y).
%       Y = 'unknown'.
fast_column(X, X) :-
    % Return the original value if no further processing is needed.
    !.
fast_column(X, Y) :-
    % Attempt to convert the column value into a FlyBase term.
    into_fb_term(X, Y),
    !.
fast_column(X, X).

%!  if_m2(:Goal) is det.
%
%   Executes a goal deterministically.
%
%   This predicate ensures that the given goal (`Goal`) is executed at most once.
%   It is effectively a shorthand for wrapping a goal with `once/1`, guaranteeing
%   that the goal succeeds deterministically (i.e., no backtracking occurs).
%
%   @arg Goal The goal to execute deterministically.
%
%   @example
%       % Run a goal deterministically:
%       ?- if_m2(member(X, [1, 2, 3])).
%       X = 1.
%
%       % The goal does not backtrack to find other solutions:
%       ?- if_m2(member(X, [1, 2, 3])), fail.
%       false.
if_m2(G) :-
    once(G).

%!  datalog_to_termlog(+File) is det.
%
%   Converts a Datalog file to a Termlog format.
%
%   This predicate processes a Datalog file, converts its terms to a Termlog format,
%   and writes the results to an output file with a `.metta` extension (if applicable).
%   It supports wildcard file patterns and handles multiple files by expanding the pattern.
%
%   @arg File The Datalog file or pattern to process.
%
%   @example
%       % Convert a single Datalog file to Termlog:
%       ?- datalog_to_termlog('example.dl').
%
%       % Process multiple files matching a wildcard pattern:
%       ?- datalog_to_termlog('*.dl').
%
%   @see process_datalog/3.
datalog_to_termlog(File) :-
    % Handle wildcard patterns by expanding to a list of files.
    atom_contains(File, '*'),
    expand_file_name(File, List),
    !,
    % Process each file in the list.
    maplist(datalog_to_termlog, List).

datalog_to_termlog(File) :-
    % Set source indentation handling to 'False'.
    nb_setval(src_indents, 'False'),
    % Construct the output file name by appending '2'.
    atom_concat(File, '2', File2),
    % Log the conversion process.
    fbug(datalog_to_termlog(File)),
    % Attempt to construct a `.metta` file name if applicable.
    if_m2(atom_concat(File, '.metta', M)),
    % Open input and output files with UTF-8 encoding.
    setup_call_cleanup(
        (open(File, read, In, [encoding(utf8)]),
         open(File2, write, Out, [encoding(utf8)]),
         if_m2(open(M, write, OutM, [encoding(utf8)]))),
        % Process each term in the Datalog file.
        (repeat,
         read_term(In, Term, []),
         (Term == end_of_file
            -> ! ;
            (process_datalog(Out, OutM, Term), fail))),
        % Ensure all files are closed after processing.
        ((close(In), close(Out), if_m2(close(OutM)), listing(fb_pred/2)))
    ).

%!  process_datalog(+Out, +OutM, +Term) is det.
%
%   Processes a single Datalog term and writes it to the output streams.
%
%   This predicate converts a Datalog term (`Term`) into a canonical format and
%   writes it to the output file (`Out`). If a `.metta` file output stream is provided
%   (`OutM`), it writes the source representation to that stream.
%
%   @arg Out  The output stream for the Termlog file.
%   @arg OutM The optional output stream for the `.metta` file.
%   @arg Term The Datalog term to process.
%
%   @see process_datalog/4.
process_datalog(Out, OutM, Term) :-
    % Decompose the term into its functor and arguments.
    Term =.. [F | Args],
    % Delegate processing to the arity-specific predicate.
    process_datalog(Out, OutM, F, Args).

%!  process_datalog(+Out, +OutM, +Functor, +Args) is det.
%
%   Processes a Datalog term by its functor and arguments.
%
%   This predicate converts a term represented by its functor (`Functor`) and
%   arguments (`Args`) into a canonical form, asserts its predicate information
%   into the knowledge base, and writes the term to the output streams.
%
%   @arg Out     The output stream for the Termlog file.
%   @arg OutM    The optional output stream for the `.metta` file.
%   @arg Functor The functor of the term.
%   @arg Args    The arguments of the term.
%
%   @see better_arg/2, fb_pred/2.
process_datalog(Out, OutM, F, Args) :-
    must_det_ll((
        % Process the arguments for canonicalization.
        maplist(better_arg, Args, ArgsL),
        % Construct the canonical term.
        Src = [F | ArgsL],
        OBO =.. Src,
        % Determine the arity of the functor.
        length(ArgsL, N),
        % Assert the predicate information if it's new.
        assert_if_new(fb_pred(F, N)),
        % Write the canonical term to the Termlog output stream.
        write_canonical(Out, OBO),
        !,
        writeln(Out, '.'),
        % Optionally write the source representation to the `.metta` output stream.
        if_m2((with_output_to(OutM, write_srcH(Src))))
    )).

%!  split_by_delimiter(+Input, +Delimiter, -Parts) is nondet.
%
%   Splits a string or atom into a list of parts based on a specified delimiter.
%
%   This predicate breaks the input (`Input`) into parts using the specified
%   delimiter (`Delimiter`). It ensures that the resulting list of parts contains
%   at least two elements.
%
%   @arg Input     The string or atom to split.
%   @arg Delimiter The delimiter used to split the input.
%   @arg Parts     The resulting list of parts after splitting.
%
%   @example
%       % Split a string by a comma:
%       ?- split_by_delimiter('a,b,c', ',', Parts).
%       Parts = ['a', 'b', 'c'].
%
%       % Fail if no delimiter is present:
%       ?- split_by_delimiter('abc', ',', Parts).
%       false.
split_by_delimiter(Input, Delimiter, Parts) :-
    % Split the input using the specified delimiter.
    symbolic_list_concat(Parts, Delimiter, Input),
    % Ensure that the resulting list has at least two parts.
    Parts = [_, _ | _].

%!  always_delistify(+A, -B) is det.
%
%   Simplifies compound or list terms by "delistifying" nested structures.
%
%   This predicate attempts to reduce nested or singleton list structures into a simpler form:
%   - If `A` is not compound or not a list, it is returned unchanged.
%   - If `A` is a structure `s/3`, the middle component is recursively delistified.
%   - If `A` is a singleton list, the single element is extracted.
%   - Otherwise, `A` is returned unchanged.
%
%   @arg A The input term to simplify.
%   @arg B The simplified term.
%
%   @example
%       % Simplify a singleton list:
%       ?- always_delistify([42], B).
%       B = 42.
%
%       % Simplify a compound term:
%       ?- always_delistify(s(a, [b], c), B).
%       B = s(a, b, c).
%
%       % Return an atom unchanged:
%       ?- always_delistify(hello, B).
%       B = hello.
always_delistify(A, A) :-
    % If A is not compound, return it unchanged.
    \+ compound(A),!.
always_delistify(s(A, M, E), s(A, MM, E)) :-
    % If A is a structure `s/3`, delistify the middle component.
    !,
    always_delistify(M, MM).
always_delistify(A, A) :-
    % If A is not a list, return it unchanged.
    \+ is_list(A),!.
always_delistify([A], A) :-
    % If A is a singleton list, extract the single element.
    !.
always_delistify(A, A).

%!  better_arg(+S, -A) is det.
%
%   Processes an argument, converting or simplifying it if necessary.
%
%   This predicate converts a string argument (`S`) into an atom (`A`) or simplifies
%   complex terms. It uses `better_arg1/2` for more advanced processing if applicable.
%
%   @arg S The input argument to process.
%   @arg A The processed or simplified argument.
%
%   @example
%       % Convert a string to an atom:
%       ?- better_arg("example", A).
%       A = example.
%
%       % Return a non-string argument unchanged:
%       ?- better_arg(42, A).
%       A = 42.
better_arg(S, A) :-
    % If S is a string, convert it to an atom.
    string(S),
    string_to_syms, % Placeholder for a potentially active transformation.
    atom_string(A, S),
    !.
% better_arg1(A, B) :- fix_concept(A, B), !.
better_arg(A, A) :-
    % Return non-string arguments unchanged.
    !.
better_arg(A, B) :-
    % Apply further processing and delistify results.
    better_arg1(A, AA),
    always_delistify(AA, B).

%!  better_arg1(+Input, -Output) is det.
%
%   Performs advanced argument transformations.
%
%   Attempts to break the input into components and construct a structured term
%   or simplifies it into another form if applicable.
%
%   @arg Input  The argument to transform.
%   @arg Output The transformed or structured argument.
%
%   @see to_case_breaks/2, cb_better_args/2.
better_arg1(Input, s(A, Mid, E)) :-
    % Attempt advanced parsing of input into structured terms.
    fail, % Current placeholder, left inactive.
    (string(Input); atom(Input)),
    once(to_case_breaks(Input, CB)),
    CB = [_, _, _ | _],
    once(cb_better_args(CB, [A | ABCParts])),
    ABCParts = [_, _ | _],
    append(Mid, [E], ABCParts),
    !.
better_arg1(S, A) :-
    % Convert strings to atoms via symbolic processing.
    string(S),
    string_to_syms, % Placeholder for active transformation logic.
    tom_string(A, S),
    !.
% better_arg1(A, B) :- fix_concept(A, B), !.
better_arg1(A, A).

%!  is_FB_input(+List) is nondet.
%
%   Checks if a list matches the FlyBase input format.
%
%   This predicate succeeds if the list represents a FlyBase input pattern,
%   such as "FB" followed by alphanumeric elements.
%
%   @arg List The list to check.
is_FB_input([xti(_, upper), xti(":", punct), xti(_, digit)]) :- !.
is_FB_input([xti("FB", upper), xti(_, lower), xti(_, digit)]) :- !.

%!  cb_better_args(+CaseBreaks, -Parts) is nondet.
%
%   Processes case-breaks into meaningful parts unless the input matches FlyBase format.
%
%   This predicate transforms a list of case-breaks (`CaseBreaks`) into `Parts`,
%   skipping processing for recognized FlyBase input patterns.
%
%   @arg CaseBreaks The input list of case-breaks.
%   @arg Parts      The resulting list of processed parts.
%
%   @see cb_better_args_ni/2, is_FB_input/1.
cb_better_args([_], _) :-
    % Fail if there's only one element.
    !, fail.
cb_better_args(X, _) :-
    % Skip if the input matches FlyBase format.
    is_FB_input(X),
    !, fail.
cb_better_args(CB, Parts) :-
    % Process non-FlyBase input patterns.
    cb_better_args_ni(CB, Parts),
    !.

%!  cb_better_args_ni(+CaseBreaks, -Parts) is det.
%
%   Processes non-FlyBase case-breaks into meaningful parts.
%
%   This predicate processes the case-breaks (`CaseBreaks`) and constructs
%   a list of meaningful parts (`Parts`).
%
%   @arg CaseBreaks The input list of case-breaks.
%   @arg Parts      The resulting list of processed parts.
cb_better_args_ni([A, B, C | L], [I | Parts]) :-
    % Combine FlyBase input patterns into a single identifier.
    is_FB_input([A, B, C]),
    maplist(arg(1), [A, B, C], ABC),
    symbolic_list_concat(ABC, I),
    cb_better_args_ni(L, Parts).
cb_better_args_ni([XTI | L], [I | Parts]) :-
    % Convert other case-breaks into atoms.
    arg(1, XTI, S),
    string_to_syms, % Placeholder for symbolic processing logic.
    !,
    atom_string(I, S),
    cb_better_args_ni(L, Parts).
cb_better_args_ni([], []) :-
    % Base case: no more elements to process.
    !.

%!  datalog_to_termlog is det.
%
%   Converts all Datalog files in specified directories to Termlog format.
%
%   This predicate processes `.datalog` files across multiple nested directories
%   using the `datalog_to_termlog/1` predicate.
%
%   @see datalog_to_termlog/1.
datalog_to_termlog :-
    datalog_to_termlog('./data/*/*.datalog'),
    datalog_to_termlog('./data/*/*/*.datalog'),
    datalog_to_termlog('./data/*/*/*/*.datalog'),
    datalog_to_termlog('./data/*/*/*/*/*.datalog'),
    datalog_to_termlog('./data/*/*/*/*/*/*.datalog'),
    datalog_to_termlog('./data/*/*/*/*/*/*/*.datalog').

%datalog_to_termlog:- datalog_to_termlog('whole_flybase.datalog').