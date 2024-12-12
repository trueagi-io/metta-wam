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
% PROGRAM FUNCTION:  This is the main flybase program.
%*********************************************************************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set the character encoding for this Prolog file to ISO Latin-1.
:- encoding(iso_latin_1).

% Ensure that all buffered output is flushed immediately.
:- flush_output.

% Set the 'RUST_BACKTRACE' environment variable to 'full' for detailed Rust error traces.
:- setenv('RUST_BACKTRACE', full).

% Declare fb_pred/2 as a multifile predicate, allowing its definition to be spread across multiple files.
:- multifile(fb_pred/2).

% Allow fb_pred/2 clauses to be non-contiguous in the source code.
:- discontiguous(fb_pred/2).

% Define fb_pred/2 as a dynamic predicate, allowing its clauses to be added or removed at runtime.
:- dynamic(fb_pred/2).

%!  fb_stats is det.
%
%   Collects and reports FlyBase statistics by invoking the `metta_stats/0` predicate.
%   This predicate succeeds deterministically after executing `metta_stats/0`.
%
%   @example Usage:
%       ?- fb_stats.
%       % Executes metta_stats and completes successfully.
%
fb_stats :- metta_stats, !.

%!  '&flybase':for_metta(+Entity, -Predicate) is det.
%
%   Dynamically resolves FlyBase predicates based on the given entity ('&flybase') and 
%   constructs a callable predicate term.
%
%   This predicate checks if a predicate with the required arity exists, creates a 
%   list of arguments of the correct length, and combines them with the predicate name 
%   into a single term. The predicate is then applied with the constructed arguments.
%
%   @arg Entity The FlyBase entity being handled (always '&flybase').
%   @arg Predicate Unifies with the resolved predicate term `[F|L]`, where F is the 
%        predicate name, and L is a list of arguments.
%
%   @example Usage:
%       % Dynamically resolves and applies a FlyBase predicate.
%       ?- '&flybase':for_metta('&flybase', P).
%       P = [some_predicate_name, Arg1, Arg2, ...].
%
'&flybase' : for_metta('&flybase', P) :-
    % Retrieve the name (F) and arity (A) of a FlyBase predicate.
    fb_pred_nr(F, A),
    % Check if a predicate with the name F and arity A is currently defined.
    current_predicate(F/A),
    % Create a list L of length A, representing the predicate arguments.
    length(L, A),
    % Combine the predicate name F and the arguments list L into the term P.
    P = [F | L],
    % Apply the predicate F with the arguments L.
    apply(F, L).

% ==============
% OBO LOADER
% ==============
% Set the encoding option to UTF-8, ensuring that text is processed using the UTF-8 character encoding.
:- set_option_value(encoding, utf8).
%:- ensure_loaded('./obo_loader').
%:- ensure_loaded(json_loader).

% Ensure that the library `metta_interp` is loaded, providing access to its predicates and functionality.
:- ensure_loaded(library(metta_interp)).

% ==============
% VSPACE LOADER
% ==============
%:- set_option_value(max_per_file,10_000_000).
%:- set_option_value(max_per_file,1_000).
%:- set_option_value(max_per_file,300).
%:- set_option_value(max_per_file,inf+1).
:- set_option_value(max_per_file,0).
%:- set_option_value(max_per_file,20_000).
%:- set_option_value(max_per_file,20_000_000_000_000_000_000_000_000_000_000_000).
% load_flybase('./precomputed_files/insertions/fu_gal4_table_fb_*.json').
:- set_option_value(max_disk_cache,1000).
:- set_option_value(samples_per_million,30).
:- set_option_value(full_canon,true).

%!  flybase_identifier(+Prefix, +EntityType) is det.
%
%   Maps FlyBase identifier prefixes to their corresponding biological or data entity types.
%   Each prefix uniquely identifies an entity type, enabling classification and interpretation
%   of FlyBase data. These mappings are essential for working with FlyBase datasets and 
%   understanding the context of each entity.
%
%   @arg Prefix The FlyBase identifier prefix (e.g., 'FBgn', 'FBal').
%   @arg EntityType The type of entity represented by the prefix (e.g., 'Gene', 'Allele').
%
%   @example
%       % Mapping for genes and alleles:
%       flybase_identifier('FBgn', 'Gene').
%       flybase_identifier('FBal', 'Allele').
%
%       % Mapping for anatomy terms and developmental stages:
%       flybase_identifier('FBbt', 'AnatomyTerm').
%       flybase_identifier('FBdv', 'DevelopmentalStageTerm').
%
flybase_identifier('FBab', 'Aberration').
flybase_identifier('FBal', 'Allele').
flybase_identifier('FBba', 'Balancer').
flybase_identifier('FBbt', 'AnatomyTerm').
flybase_identifier('FBch', 'ChromosomeArm').
flybase_identifier('FBcl', 'Clone').
flybase_identifier('FBcv', 'ControlledVocabulary').
flybase_identifier('FBdv', 'DevelopmentalStageTerm').
flybase_identifier('FBgg', 'GeneGroup').
flybase_identifier('FBgn', 'Gene').
flybase_identifier('FBhh', 'HumanDisease').
flybase_identifier('FBig', 'GeneInteraction').
flybase_identifier('FBim', 'Image').
flybase_identifier('FBlc', 'LargeDatasetMetadata').
flybase_identifier('FBmc', 'MolecularConstruct').
flybase_identifier('FBms', 'MolecularSegment').
flybase_identifier('FBpl', 'Probe').
flybase_identifier('FBpp', 'Polypeptide').
flybase_identifier('FBrf', 'Reference').
flybase_identifier('FBsf', 'SequenceFeature').
flybase_identifier('FBsn', 'GeneStrain').
flybase_identifier('FBst', 'GeneStock').
flybase_identifier('FBtc', 'CellLine').
flybase_identifier('FBti', 'TransposableIlementInsertion').
flybase_identifier('FBto', 'ExperimentalTool').
flybase_identifier('FBte', 'TransgenicElement').
flybase_identifier('FBtp', 'Transposon'). %flybase_identifier('FBtp', 'transgenic construct or natural transposon').
flybase_identifier('FBtr', 'Transcript').

%!  symbol_prefix(+Prefix, +Source, +Description) is det.
%
%   Maps symbol prefixes to their respective data sources and descriptions. 
%   This predicate associates a prefix with a source and provides a textual 
%   description, enabling classification and interpretation of biological or 
%   ontological entities in FlyBase and related datasets.
%
%   FlyBase-specific prefixes are dynamically derived using `flybase_identifier/2`, 
%   while other common prefixes are manually specified for OBO (Open Biomedical Ontology) 
%   data sources.
%
%   @arg Prefix The prefix used in identifiers or entities (e.g., 'GO', 'DOID').
%   @arg Source The source or database associated with the prefix (e.g., 'flybase', 'obo').
%   @arg Description A textual description of the prefix, indicating its meaning or context 
%        (e.g., 'Gene Ontology', 'Disease Ontology').
%
%   @example
%       % FlyBase-specific prefixes:
%       symbol_prefix(Prefix, flybase, Desc) :-
%           flybase_identifier(Prefix, Desc).
%
%       % Common OBO prefixes:
%       symbol_prefix('GO', obo, 'Gene Ontology').
%       symbol_prefix('DOID', obo, 'Disease Ontology').
%       symbol_prefix('CHEBI', obo, 'Chemical Entities of Biological Interest').
symbol_prefix(Prefix, flybase, Desc):- flybase_identifier(Prefix, Desc).
% Some common OBO prefixes (Note: these are more generalized and not specific to FlyBase)
symbol_prefix('GO', obo, 'Gene Ontology').
symbol_prefix('PO', obo, 'Plant Ontology').
symbol_prefix('DOID', obo, 'Disease Ontology').
symbol_prefix('UBERON', obo, 'Uber-anatomy ontology').
symbol_prefix('CHEBI', obo, 'Chemical Entities of Biological Interest').


%:- abolish(gp_information/0).

% Remove all fb_pred/2 predicates with arity 0 from the database and abolish their definitions.
:- forall(retract(fb_pred(F, 0)), abolish(F/0)).

% Ensure the `flybase_learn` module is loaded, providing its predicates and functionality.
:- ensure_loaded(flybase_learn).

% Dynamically load all files matching the '*_loader.pl' pattern in the current directory.
:- prolog_load_context(directory, Here),           % Retrieve the current directory.
   atom_concat(Here, '/*_loader.pl', Mask),        % Create a file search pattern.
   expand_file_name(Mask, Files),                  % Find all matching files.
   maplist([File] >> load_files(File), Files).     % Load each file using load_files/1.

%fbd(X,P):- fb_pred(F,A),functor(P,F,A),arg(_,P,X), no_repeats(P,call(P)).

%!  fbdead is det.
%
%   Identifies and logs "dead" FlyBase predicates by attempting to construct and call 
%   them with an invalid argument. A "dead" predicate is one that fails or is not 
%   defined properly when invoked with test data.
%
%   This predicate iterates through all known FlyBase predicates, constructs a term with 
%   an invalid argument, and checks if it can be successfully called. If a predicate is 
%   identified as "dead," it is logged to the console. The predicate succeeds deterministically 
%   after all possible predicates have been checked.
%
%   @example Usage:
%       % Check for and log "dead" predicates.
%       ?- fbdead.
%       fbdead = some_predicate_name/arity.
%
fbdead :-
    % Retrieve the name (F) and arity (A) of each FlyBase predicate.
    fb_pred_nr(F, A),
    % Construct a predicate term (P) with the given name and arity.
    functor(P, F, A),
    % Assign an invalid value ('xxxxxxxxxxxxxxxxx') to one of the predicate arguments.
    arg(_, P, xxxxxxxxxxxxxxxxx),
    % Ensure there are no duplicate checks or calls for the predicate.
    no_repeats(P, call(P)),
    % Log the "dead" predicate to the console.
    writeln(fbdead = P),
    % Force backtracking to continue checking other predicates.
    fail.
% Ensure fbdead/0 succeeds deterministically after all backtracking is complete.
fbdead.

%!  column_description(+ColumnName, +Description, +DataType, +Category) is det.
%
%   Provides metadata and attributes for specific FlyBase data columns. Each column
%   is characterized by its name, a textual description, its data type, and a category
%   or label for classification purposes. These mappings are crucial for interpreting
%   and analyzing FlyBase datasets effectively.
%
%   @arg ColumnName The name of the column in the dataset (e.g., 'allele_FBal#', 'phenotype_name').
%   @arg Description A detailed description of the column's purpose or content.
%   @arg DataType The type of data the column holds (e.g., identifier, numeric, symbol, text, list, etc.).
%   @arg Category A short label or classification for the column (e.g., 'Allele Identifier').
%
%   @example
%       % Example for allele-related data columns:
%       column_description('allele_FBal#', "Current FlyBase identifier (FBal#) of allele.", identifier, 'Allele Identifier').
%       column_description('allele_symbol', "Current FlyBase allele symbol.", symbol, 'Allele Symbol').
%
column_description('allele_FBal#', "Current FlyBase identifier (FBal#) of allele.", identifier, 'Allele Identifier').
column_description('allele_symbol', "Current FlyBase allele symbol.", symbol, 'Allele Symbol').
column_description('Bin_value', "The expression bin classification of this gene in this RNA-Seq experiment, based on RPKM value.", numeric, 'Expression Bin').
column_description('Cluster_Cell_Type_ID', "The FlyBase FBbt ID for the cell type represented by the cell cluster.", identifier, 'Cell Type').
column_description('Cluster_Cell_Type_Name', "The FlyBase name for the cell type represented by the cell cluster.", name, 'Cell Type Name').
column_description('Cluster_ID', "The FlyBase FBlc ID for the dataset representing the cell cluster.", identifier, 'Cell Cluster').
column_description('Cluster_Name', "The FlyBase name for the dataset representing the cell cluster.", name, 'Cell Cluster Name').
column_description('Clustering_Analysis_ID', "The FlyBase FBlc ID for the dataset representing the clustering analysis.", identifier, 'Dataset').
column_description('Clustering_Analysis_Name', "The FlyBase name for the dataset representing the clustering analysis.", name, 'Dataset Name').
column_description('Count_used', "Indicates if the RPKM expression value was calculated using only the exonic regions unique to the gene and not overlapping exons of other genes (Unique), or, if the RPKM expression value was calculated based on all exons of the gene regardless of overlap with other genes (Total).", category, 'Count Type').
column_description('DATASAMPLE_NAME_(DATASET_ID)', "Each subsequent column reports the gene RPKM values for the sample listed in the header.", matrix, 'Expression Matrix').
column_description('FBgn', "The unique FlyBase gene ID for this gene.", identifier, 'Gene').
column_description('FBgn_id', "Unique FlyBase gene ID.", identifier, 'Gene').
column_description('FBrf#', "Current FlyBase identifer (FBrf#) of publication from which data came.", identifier, 'Publication Identifier').
column_description('FBrf', "Current FlyBase identifer (FBrf) of publication from which data came.", identifier, 'Publication Identifier').
column_description('FBrf_id', "FlyBase reference ID for the publication.", identifier, 'Reference').
column_description('Gene_ID', "The FlyBase FBgn ID for the expressed gene.", identifier, 'Gene').
column_description('Gene_Symbol', "The FlyBase symbol for the expressed gene.", symbol, 'Gene Symbol').
column_description('GeneSymbol', "The official FlyBase symbol for this gene.", symbol, 'Gene Symbol').
column_description('interaction', "Interaction information associated with allele.", text, 'Interaction Info').
column_description('Interaction_type', "Type of interaction observed, either 'suppressible' or 'enhanceable'.", category, 'Interaction Type').
column_description('Mean_Expression', "The average level of expression of the gene across all cells of the cluster.", numeric, 'Expression Level').
column_description('Parent_library_FBlc', "The unique FlyBase ID for the dataset project to which the RNA-Seq experiment belongs.", identifier, 'Dataset Project').
column_description('Parent_library_name', "The official FlyBase symbol for the dataset project to which the RNA-Seq experiment belongs.", name, 'Dataset Project Name').
column_description('phenotype_id', "Phenotypic identifier associated with the genotype.", identifier, 'Phenotype Identifier').
column_description('phenotype_name', "Phenotypic name associated with the genotype.", name, 'Phenotype Name').
column_description('PMID', "PubMed ID for the publication.", identifier, 'Publication').
column_description('Pub_ID', "The FlyBase FBrf ID for the reference in which the expression was reported.", identifier, 'Publication').
column_description('Pub_miniref', "The FlyBase citation for the publication in which the expression was reported.", citation, 'Publication Citation').
column_description('Publication_FBrf', "Current FlyBase identifier (FBrf) of publication from which the data came.", identifier, 'Publication Reference').
column_description('reference', "Current FlyBase identifer (FBrf#) of publication from which data came.", identifier, 'Publication Identifier').
column_description('Release_ID', "The D. melanogaster annotation set version from which the gene model used in the analysis derives.", version, 'Annotation Version').
column_description('RNASource_FBlc', "The unique FlyBase ID for the RNA-Seq experiment used for RPKM expression calculation.", identifier, 'RNA-Seq Experiment').
column_description('RNASource_name', "The official FlyBase symbol for the RNA-Seq experiment used for RPKM expression calculation.", name, 'RNA-Seq Experiment Name').
column_description('RPKM_value', "The RPKM expression value for the gene in the specified RNA-Seq experiment.", numeric, 'Expression Value').
column_description('Source_Tissue_Anatomy', "The ansymbolical region of the source tissue used for the experiment.", category, 'Tissue Anatomy').
column_description('Source_Tissue_Sex', "The sex of the source tissue used for the experiment.", category, 'Tissue Sex').
column_description('Source_Tissue_Stage', "The life stage of the source tissue used for the experiment.", category, 'Tissue Stage').
column_description('Spread', "The proportion of cells in the cluster in which the gene is detected.", proportion, 'Expression Spread').
column_description('Total_exon_base_count', "The number of bases in all exons of this gene.", numeric, 'Total Exonic Base Count').
column_description('UniProt_database', "Database in UniProt where the protein is listed (either UniProt/TrEMBL or UniProt/Swiss-Prot).", category, 'Protein Database').
column_description('UniProt_id', "Unique identifier for the protein in UniProt.", identifier, 'Protein').
column_description('Unique_exon_base_count', "The number of exonic bases unique to the gene (not overlapping exons of other genes).", numeric, 'Exonic Base Count').
column_description(allele_FBal, "Current FlyBase identifier (FBal) of allele.", identifier, 'Allele Identifier').
column_description(allele_symbol, "Current FlyBase allele symbol.", symbol, 'Allele Symbol').
column_description(current_fullname, "Current full name used in FlyBase for the object.", name, 'Name').
column_description(current_symbol, "Current symbol used in FlyBase for the object.", symbol, 'Symbol').
column_description(gene_fullname, "The official full name for this gene.", name, 'Gene Name').
column_description(gene_primary_id, "The unique FlyBase gene ID for this gene.", identifier, 'Gene').
column_description(gene_symbol, "The official FlyBase symbol for this gene.", symbol, 'Gene Symbol').
column_description(gene_type, "The type of gene.", category, 'Gene Type').
column_description(interaction, "Interaction information associated with allele.", text, 'Interaction Info').
column_description(listOf('genotype_FBids', ['/', ' ']), "Current FlyBase identifier(s) of the components that make up the genotype.", list, 'Genotype Identifiers').
column_description(listOf('genotype_symbols', ['/', ' ']), "Current FlyBase symbol(s) of the components that make up the genotype.", list, 'Genotype Symbols').
column_description(listOf('Interacting_gene_FBgn', ['|']), "Current FlyBase identifier (FBgn) of gene(s) involved in the interacting genotype.", list, 'Gene Identifier').
column_description(listOf('Interacting_gene_symbol', ['|']), "Current FlyBase symbol of gene(s) involved in the interacting genotype.", list, 'Gene Symbol').
column_description(listOf('qualifier_ids', ['|']), "Qualifier identifier(s) associated with phenotypic data for genotype.", list, 'Qualifier Identifiers').
column_description(listOf('qualifier_names', ['|']), "Qualifier name(s) associated with phenotypic data for genotype.", list, 'Qualifier Names').
column_description(listOf('Starting_gene_FBgn', ['|']), "Current FlyBase identifier (FBgn) of gene(s) involved in the starting genotype.", list, 'Gene Identifier').
column_description(listOf('Starting_gene_symbol', ['|']), "Current FlyBase symbol of gene(s) involved in the starting genotype.", list, 'Gene Symbol').
column_description(listOf(fullname_synonym, ['|']), "Non-current full name(s) associated with the object.", list, 'Name Synonyms').
column_description(listOf(genotype_FBids, [/, ' ']), "Current FlyBase identifier(s) of the components that make up the genotype.", list, 'Genotype Identifiers').
column_description(listOf(genotype_symbols, [/, ' ']), "Current FlyBase symbol(s) of the components that make up the genotype.", list, 'Genotype Symbols').
column_description(listOf(qualifier_ids, ['|']), "Qualifier identifier(s) associated with phenotypic data for genotype.", list, 'Qualifier Identifiers').
column_description(listOf(qualifier_names, ['|']), "Qualifier name(s) associated with phenotypic data for genotype.", list, 'Qualifier Names').
column_description(listOf(symbol_synonym, ['|']), "Non-current symbol(s) associated with the object.", list, 'Symbol Synonyms').
column_description(organism_abbreviation, "Abbreviation indicating the species of origin.", abbreviation, 'Organism').
column_description(phenotype_id, "Phenotypic identifier associated with the genotype.", identifier, 'Phenotype Identifier').
column_description(phenotype_name, "Phenotypic name associated with the genotype.", name, 'Phenotype Name').
column_description(primary_FBid, "Primary FlyBase identifier for the object.", identifier, 'Object').
column_description(reference, "Current FlyBase identifer (FBrf) of publication from which data came.", identifier, 'Publication Identifier').

%!  column_names(+Alias, +Columns) is det.
%
%   Specifies the column names associated with each dataset in FlyBase. 
%   The predicate associates an alias with a list of column names, which define the structure
%   and attributes of the dataset represented by the alias.
%
%   Each column can be represented as a simple string or as a `listOf/2` term to indicate a 
%   compound field with delimiters.
%
%   @arg Alias A symbolic name identifying the dataset (e.g., 'allele_genetic_interactions').
%   @arg Columns A list of column names or compound fields defining the structure of the dataset.
%
%   @example
%       % Columns for allele genetic interactions data.
%       column_names('allele_genetic_interactions', ['allele_symbol', 'allele_FBal#', 'interaction', 'FBrf#']).
%
column_names('allele_genetic_interactions', ['allele_symbol', 'allele_FBal#', 'interaction', 'FBrf#']).
column_names('fb_synonym', ['primary_FBid', 'organism_abbreviation', 'current_symbol', 'current_fullname', listOf(fullname_synonym, ['|']), listOf(symbol_synonym, ['|'])]).
column_names('gene_genetic_interactions', [listOf('Starting_gene_symbol', ['|']), listOf('Starting_gene_FBgn', ['|']), listOf('Interacting_gene_symbol', ['|']), listOf('Interacting_gene_FBgn', ['|']), 'Interaction_type', 'Publication_FBrf']).
column_names('gene_rpkm_matrix', ['gene_primary_id', 'gene_symbol', 'gene_fullname', 'gene_type', 'DATASAMPLE_NAME_(DATASET_ID)']).
column_names('gene_rpkm_report', ['Release_ID', 'FBgn#', 'GeneSymbol', 'Parent_library_FBlc#', 'Parent_library_name', 'RNASource_FBlc#', 'RNASource_name', 'RPKM_value', 'Bin_value', 'Unique_exon_base_count', 'Total_exon_base_count', 'Count_used']).
column_names('genotype_phenotype_data', [listOf('genotype_symbols', ['/', ' ']), listOf('genotype_FBids', ['/', ' ']), 'phenotype_name', 'phenotype_id', listOf('qualifier_names', ['|']), listOf('qualifier_ids', ['|']), 'reference']).
column_names('pmid_fbgn_uniprot', ['FBrf_id', 'PMID', 'FBgn_id', 'UniProt_database', 'UniProt_id']).
column_names('scRNA-Seq_gene_expression', ['Pub_ID', 'Pub_miniref', 'Clustering_Analysis_ID', 'Clustering_Analysis_Name', 'Source_Tissue_Sex', 'Source_Tissue_Stage', 'Source_Tissue_Anatomy', 'Cluster_ID', 'Cluster_Name', 'Cluster_Cell_Type_ID', 'Cluster_Cell_Type_Name', 'Gene_ID', 'Gene_Symbol', 'Mean_Expression', 'Spread']).

%!  file_location(+Alias, +PathPattern) is det.
%
%   Defines the location of specific data files associated with FlyBase data processing.
%   Each file location is represented by an alias and a corresponding path pattern
%   that may include wildcards to reference multiple related files.
%
%   @arg Alias A symbolic name representing the data file category or purpose.
%        Example: 'allele_genetic_interactions', 'genotype_phenotype_data'.
%   @arg PathPattern A file path pattern (including wildcards) indicating the location
%        or naming convention of the data files.
%
%   @example
%       % File location for allele genetic interactions data.
%       file_location('allele_genetic_interactions', "path_to_file/allele_genetic_interactions_*.tsv").
%
%       % File location for genotype-phenotype data.
%       file_location('genotype_phenotype_data', "path_to_file/genotype_phenotype_data_*.tsv").
%
file_location('allele_genetic_interactions', "path_to_file/allele_genetic_interactions_*.tsv").
file_location('genotype_phenotype_data', "path_to_file/genotype_phenotype_data_*.tsv").

%!  primary_column(+DatasetAlias, +PrimaryColumn) is det.
%
%   Associates each dataset alias with its primary column. The primary column serves as the
%   unique identifier or main reference point for each dataset, facilitating indexing and
%   lookup operations in FlyBase. These mappings are essential for efficient data handling
%   and querying in analyses.
%
%   @arg DatasetAlias The alias representing the dataset (e.g., 'allele_genetic_interactions').
%   @arg PrimaryColumn The primary column for the dataset, typically an identifier or key
%        (e.g., 'allele_FBal#').
%
%   @example
%       % Primary columns for datasets related to alleles and genes:
%       primary_column('allele_genetic_interactions', 'allele_FBal#').
%       primary_column('gene_rpkm_matrix', 'gene_primary_id').
%
%       % Primary columns for expression and interaction datasets:
%       primary_column('gene_genetic_interactions', 'Starting_gene_FBgn').
%       primary_column('scRNA-Seq_gene_expression', 'Gene_ID').
%
primary_column('allele_genetic_interactions', 'allele_FBal#').
primary_column('fb_synonym', 'primary_FBid').
primary_column('gene_genetic_interactions', 'Starting_gene_FBgn').
primary_column('gene_rpkm_matrix', 'gene_primary_id').
primary_column('gene_rpkm_report', 'FBgn#').
primary_column('genotype_phenotype_data', 'genotype_FBids').
primary_column('pmid_fbgn_uniprot', 'FBgn_id').
primary_column('scRNA-Seq_gene_expression', 'Gene_ID').
primary_column(allele_genetic_interactions, allele_FBal).
primary_column(dataset_metadata, 'Item_ID').
primary_column(dmel_paralogs, 'Paralog_FBgn').
primary_column(fb_synonym, primary_FBid).
primary_column(fbgn_exons2affy1_overlaps, 'FBgn').
primary_column(fbgn_exons2affy2_overlaps, 'FBgn').
primary_column(gene_genetic_interactions, 'Starting_gene_FBgn').
primary_column(gene_rpkm_matrix, gene_primary_id).
primary_column(gene_rpkm_report, 'FBgn').
primary_column(genotype_phenotype_data, genotype_FBids).
primary_column(pmid_fbgn_uniprot, 'FBgn_id').

%!  too_generic(+Value) is nondet.
%
%   Checks if a given value is considered too generic to be useful in certain contexts.
%   This predicate is used to filter out overly generic identifiers or terms that may not
%   provide meaningful distinctions in analyses or datasets.
%
%   The predicate uses the following rules to determine if a value is too generic:
%   1. Variables (`Var`) are always excluded (treated as generic).
%   2. Certain predefined values (e.g., 'pub_id') are explicitly marked as too generic.
%   3. A value is deemed too generic if it does not match a specific structural pattern
%      defined by `symbolic_list_concat/3`.
%
%   @arg Value The value being evaluated (e.g., an identifier or term).
%
%   @example
%       % Example where the value is a variable (fails):
%       ?- too_generic(Var).
%       false.
%
%       % Example where the value is predefined as too generic:
%       ?- too_generic(pub_id).
%       true.
%
%       % Example with a value failing the structural pattern check:
%       ?- too_generic('some_value').
%       false.
%
%   @note The `symbolic_list_concat/3` predicate is used to check for values with at least
%         three underscore-separated parts. Adjust this pattern as needed for specific use cases.
%
%   Implementation:
too_generic(Var) :-
    % Exclude variables as too generic.
    var(Var), !, fail.
% Explicitly mark 'pub_id' as too generic.
too_generic(pub_id).
too_generic(X) :-
    % Check if X fails the pattern of having at least three parts separated by underscores.
    \+ symbolic_list_concat([_, _, _ | _], '_', X).

%!  flybase_cols(+TableName, +ColumnList) is det.
%
%   Maps FlyBase table names to their respective column lists. Each table is represented
%   by its name and a list of column names defining its structure. This mapping is essential
%   for understanding the schema of FlyBase data tables and facilitates data querying and processing.
%
%   @arg TableName The name of the FlyBase table (e.g., 'allele_genetic_interactions', 'analysis').
%   @arg ColumnList A list of column names defining the schema of the table.
%
%   @example
%       % Example table with its column schema:
%       flybase_cols(allele_genetic_interactions, ['##allele_symbol', 'allele_FBal#', interaction, 'FBrf#']).
%       flybase_cols(analysis, [analysis_id, name, description, program, programversion, algorithm, sourcename]).
%
flybase_cols(allele_genetic_interactions,['##allele_symbol','allele_FBal#',interaction,'FBrf#']).
flybase_cols(analysis,[ analysis_id,name,description,program,programversion,algorithm,sourcename,sourceversion,sourceuri,timeexecuted]).
flybase_cols(analysisfeature,[ analysisfeature_id,feature_id,analysis_id,rawscore,normscore,significance,identity]).
flybase_cols(analysisgrp,[ analysisgrp_id,rawscore,normscore,significance,identity,analysis_id,grp_id]).
flybase_cols(analysisgrpmember,[ analysisgrpmember_id,rawscore,normscore,significance,identity,analysis_id,grpmember_id]).
flybase_cols(analysisprop,[ analysisprop_id,analysis_id,type_id,value]).
flybase_cols(audit_chado,[ audit_transaction,transaction_timestamp,userid,audited_table,record_pkey,record_ukey_cols,record_ukey_vals,audited_cols,audited_vals]).
flybase_cols(cell_line,[ cell_line_id,name,uniquename,organism_id,timeaccessioned,timelastmodified]).
flybase_cols(cell_line_loaderm,[ cell_line_loaderm_id,cell_line_id,loaderm_id,pub_id,rank]).
flybase_cols(cell_line_loadermprop,[ cell_line_loadermprop_id,cell_line_loaderm_id,type_id,value,rank]).
flybase_cols(cell_line_dbxref,[ cell_line_dbxref_id,cell_line_id,dbxref_id,is_current]).
flybase_cols(cell_line_feature,[ cell_line_feature_id,cell_line_id,feature_id,pub_id]).
flybase_cols(cell_line_library,[ cell_line_library_id,cell_line_id,library_id,pub_id]).
flybase_cols(cell_line_libraryprop,[ cell_line_libraryprop_id,cell_line_library_id,type_id,value,rank]).
flybase_cols(cell_line_relationship,[ cell_line_relationship_id,subject_id,object_id,type_id]).
flybase_cols(cell_line_strain,[ cell_line_strain_id,strain_id,cell_line_id,pub_id]).
flybase_cols(cell_line_strainprop,[ cell_line_strainprop_id,cell_line_strain_id,type_id,value,rank]).
flybase_cols(cell_line_synonym,[ cell_line_synonym_id,cell_line_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(cell_lineprop,[ cell_lineprop_id,cell_line_id,type_id,value,rank]).
flybase_cols(cell_lineprop_pub,[ cell_lineprop_pub_id,cell_lineprop_id,pub_id]).
flybase_cols(cell_line_pub,[ cell_line_pub_id,cell_line_id,pub_id]).
flybase_cols(contact,[ contact_id,description,name]).
flybase_cols(cv,[ cv_id,name,definition]).
flybase_cols(loaderm,[ loaderm_id,cv_id,definition,dbxref_id,is_obsolete,is_relationshiptype,name]).
flybase_cols(loaderm_dbxref,[ loaderm_dbxref_id,loaderm_id,dbxref_id,is_for_definition]).
flybase_cols(loaderm_relationship,[ loaderm_relationship_id,type_id,subject_id,object_id]).
flybase_cols(loadermpath,[ loadermpath_id,type_id,subject_id,object_id,cv_id,pathdistance]).
flybase_cols(loadermprop,[ loadermprop_id,loaderm_id,type_id,value,rank]).
flybase_cols(loadermsynonym,[ loadermsynonym_id,loaderm_id,name,type_id]).
flybase_cols(db,[ db_id,name,contact_id,description,urlprefix,url]).
flybase_cols(dbxref,[ dbxref_id,db_id,accession,version,description,url]).
flybase_cols(dbxrefprop,[ dbxrefprop_id,dbxref_id,type_id,value,rank]).
flybase_cols(eimage,[ eimage_id,eimage_data,eimage_type,image_uri]).
flybase_cols(environment,[ environment_id,uniquename,description]).
flybase_cols(environment_loaderm,[ environment_loaderm_id,environment_id,loaderm_id]).
flybase_cols(expression,[ expression_id,uniquename,md5checksum,description]).
flybase_cols(expression_loaderm,[ expression_loaderm_id,expression_id,loaderm_id,rank,loaderm_type_id]).
flybase_cols(expression_loadermprop,[ expression_loadermprop_id,expression_loaderm_id,type_id,value,rank]).
flybase_cols(expression_image,[ expression_image_id,expression_id,eimage_id]).
flybase_cols(expressionprop,[ expressionprop_id,expression_id,type_id,value,rank]).
flybase_cols(expression_pub,[ expression_pub_id,expression_id,pub_id]).
flybase_cols(feature,[ feature_id,dbxref_id,organism_id,name,uniquename,residues,seqlen,md5checksum,type_id,is_analysis,timeaccessioned,timelastmodified,is_obsolete]).
flybase_cols(feature_loaderm,[ feature_loaderm_id,feature_id,loaderm_id,pub_id,is_not]).
flybase_cols(feature_loaderm_dbxref,[ feature_loaderm_dbxref_id,feature_loaderm_id,dbxref_id]).
flybase_cols(feature_loadermprop,[ feature_loadermprop_id,feature_loaderm_id,type_id,value,rank]).
flybase_cols(feature_dbxref,[ feature_dbxref_id,feature_id,dbxref_id,is_current]).
flybase_cols(feature_expression,[ feature_expression_id,expression_id,feature_id,pub_id]).
flybase_cols(feature_expressionprop,[ feature_expressionprop_id,feature_expression_id,type_id,value,rank]).
flybase_cols(feature_genotype,[ feature_genotype_id,feature_id,genotype_id,chromosome_id,rank,cgroup,loaderm_id]).
flybase_cols(feature_grpmember,[ feature_grpmember_id,grpmember_id,feature_id]).
flybase_cols(feature_grpmember_pub,[ feature_grpmember_pub_id,pub_id,feature_grpmember_id]).
flybase_cols(feature_humanhealth_dbxref,[ feature_humanhealth_dbxref_id,humanhealth_dbxref_id,feature_id,pub_id]).
flybase_cols(feature_interaction,[ feature_interaction_id,feature_id,interaction_id,role_id,rank]).
flybase_cols(feature_interactionprop,[ feature_interactionprop_id,feature_interaction_id,type_id,value,rank]).
flybase_cols(feature_interaction_pub,[ feature_interaction_pub_id,feature_interaction_id,pub_id]).
flybase_cols(feature_phenotype,[ feature_phenotype_id,feature_id,phenotype_id]).
flybase_cols(feature_pubprop,[ feature_pubprop_id,feature_pub_id,type_id,value,rank]).
flybase_cols(feature_relationship,[ feature_relationship_id,subject_id,object_id,type_id,rank,value]).
flybase_cols(feature_relationshipprop,[ feature_relationshipprop_id,feature_relationship_id,type_id,value,rank]).
flybase_cols(feature_relationshipprop_pub,[ feature_relationshipprop_pub_id,feature_relationshipprop_id,pub_id]).
flybase_cols(feature_relationship_pub,[ feature_relationship_pub_id,feature_relationship_id,pub_id]).
flybase_cols(feature_synonym,[ feature_synonym_id,synonym_id,feature_id,pub_id,is_current,is_internal]).
flybase_cols(featureloc,[ featureloc_id,feature_id,srcfeature_id,fmin,is_fmin_partial,fmax,is_fmax_partial,strand,phase,residue_info,locgroup,rank]).
flybase_cols(featureloc_pub,[ featureloc_pub_id,featureloc_id,pub_id]).
flybase_cols(featuremap,[ featuremap_id,name,description,unittype_id]).
flybase_cols(featuremap_pub,[ featuremap_pub_id,featuremap_id,pub_id]).
flybase_cols(featurepos,[ featurepos_id,featuremap_id,feature_id,map_feature_id,mappos]).
flybase_cols(featureprop,[ featureprop_id,feature_id,type_id,value,rank]).
flybase_cols(featureprop_pub,[ featureprop_pub_id,featureprop_id,pub_id]).
flybase_cols(feature_pub,[ feature_pub_id,feature_id,pub_id]).
flybase_cols(featurerange,[ featurerange_id,featuremap_id,feature_id,leftstartf_id,leftendf_id,rightstartf_id,rightendf_id,rangestr]).
flybase_cols(genotype,[ genotype_id,uniquename,description,name,is_obsolete]).
flybase_cols(genotype_loaderm,[ genotype_loaderm_id,genotype_id,loaderm_id,pub_id,is_not,rank]).
flybase_cols(genotype_loadermprop,[ genotype_loadermprop_id,genotype_loaderm_id,type_id,value,rank]).
flybase_cols(genotype_dbxref,[ genotype_dbxref_id,genotype_id,dbxref_id,is_current]).
flybase_cols(genotype_synonym,[ genotype_synonym_id,genotype_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(genotypeprop,[ genotypeprop_id,genotype_id,type_id,value,rank,cvalue_id]).
flybase_cols(genotypeprop_pub,[ genotypeprop_pub_id,genotypeprop_id,pub_id]).
flybase_cols(genotype_pub,[ genotype_pub_id,genotype_id,pub_id]).
flybase_cols(grp,[ grp_id,name,uniquename,type_id,is_analysis,is_obsolete]).
flybase_cols(grp_loaderm,[ grp_loaderm_id,is_not,loaderm_id,grp_id,pub_id]).
flybase_cols(grp_dbxref,[ grp_dbxref_id,is_current,dbxref_id,grp_id]).
flybase_cols(grp_pubprop,[ grp_pubprop_id,value,rank,type_id,grp_pub_id]).
flybase_cols(grp_relationship,[ grp_relationship_id,value,rank,type_id,subject_id,object_id]).
flybase_cols(grp_relationshipprop,[ grp_relationshipprop_id,value,rank,type_id,grp_relationship_id]).
flybase_cols(grp_relationship_pub,[ grp_relationship_pub_id,pub_id,grp_relationship_id]).
flybase_cols(grp_synonym,[ grp_synonym_id,synonym_id,grp_id,pub_id,is_current,is_internal]).
flybase_cols(grpmember,[ grpmember_id,rank,type_id,grp_id]).
flybase_cols(grpmember_loaderm,[ grpmember_loaderm_id,is_not,loaderm_id,grpmember_id,pub_id]).
flybase_cols(grpmemberprop,[ grpmemberprop_id,value,rank,type_id,grpmember_id]).
flybase_cols(grpmemberprop_pub,[ grpmemberprop_pub_id,pub_id,grpmemberprop_id]).
flybase_cols(grpmember_pub,[ grpmember_pub_id,pub_id,grpmember_id]).
flybase_cols(grpprop,[ grpprop_id,value,rank,type_id,grp_id]).
flybase_cols(grpprop_pub,[ grpprop_pub_id,pub_id,grpprop_id]).
flybase_cols(grp_pub,[ grp_pub_id,pub_id,grp_id]).
flybase_cols(humanhealth,[ humanhealth_id,name,uniquename,organism_id,dbxref_id,is_obsolete]).
flybase_cols(humanhealth_loaderm,[ humanhealth_loaderm_id,humanhealth_id,loaderm_id,pub_id]).
flybase_cols(humanhealth_loadermprop,[ humanhealth_loadermprop_id,humanhealth_loaderm_id,type_id,value,rank]).
flybase_cols(humanhealth_dbxref,[ humanhealth_dbxref_id,humanhealth_id,dbxref_id,is_current]).
flybase_cols(humanhealth_dbxrefprop,[ humanhealth_dbxrefprop_id,humanhealth_dbxref_id,type_id,value,rank]).
flybase_cols(humanhealth_dbxrefprop_pub,[ humanhealth_dbxrefprop_pub_id,humanhealth_dbxrefprop_id,pub_id]).
flybase_cols(humanhealth_feature,[ humanhealth_feature_id,humanhealth_id,feature_id,pub_id]).
flybase_cols(humanhealth_featureprop,[ humanhealth_featureprop_id,humanhealth_feature_id,type_id,value,rank]).
flybase_cols(humanhealth_phenotype,[ humanhealth_phenotype_id,humanhealth_id,phenotype_id,pub_id]).
flybase_cols(humanhealth_phenotypeprop,[ humanhealth_phenotypeprop_id,humanhealth_phenotype_id,type_id,value,rank]).
flybase_cols(humanhealth_pubprop,[ humanhealth_pubprop_id,value,rank,type_id,humanhealth_pub_id]).
flybase_cols(humanhealth_relationship,[ humanhealth_relationship_id,subject_id,object_id,type_id,value,rank]).
flybase_cols(humanhealth_relationship_pub,[ humanhealth_relationship_pub_id,humanhealth_relationship_id,pub_id]).
flybase_cols(humanhealth_synonym,[ humanhealth_synonym_id,humanhealth_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(humanhealthprop,[ humanhealthprop_id,humanhealth_id,type_id,value,rank]).
flybase_cols(humanhealthprop_pub,[ humanhealthprop_pub_id,humanhealthprop_id,pub_id]).
flybase_cols(humanhealth_pub,[ humanhealth_pub_id,humanhealth_id,pub_id]).
flybase_cols(interaction,[ interaction_id,uniquename,type_id,description,is_obsolete]).
flybase_cols(interaction_cell_line,[ interaction_cell_line_id,cell_line_id,interaction_id,pub_id]).
flybase_cols(interaction_loaderm,[ interaction_loaderm_id,interaction_id,loaderm_id]).
flybase_cols(interaction_loadermprop,[ interaction_loadermprop_id,interaction_loaderm_id,type_id,value,rank]).
flybase_cols(interaction_expression,[ interaction_expression_id,expression_id,interaction_id,pub_id]).
flybase_cols(interaction_expressionprop,[ interaction_expressionprop_id,interaction_expression_id,type_id,value,rank]).
flybase_cols(interaction_group,[ interaction_group_id,uniquename,is_obsolete,description]).
flybase_cols(interaction_group_feature_interaction,[ interaction_group_feature_interaction_id,interaction_group_id,feature_interaction_id,rank,ftype]).
flybase_cols(interactionprop,[ interactionprop_id,interaction_id,type_id,value,rank]).
flybase_cols(interactionprop_pub,[ interactionprop_pub_id,interactionprop_id,pub_id]).
flybase_cols(interaction_pub,[ interaction_pub_id,interaction_id,pub_id]).
flybase_cols(library,[ library_id,organism_id,name,uniquename,type_id,is_obsolete,timeaccessioned,timelastmodified]).
flybase_cols(library_loaderm,[ library_loaderm_id,library_id,loaderm_id,pub_id]).
flybase_cols(library_loadermprop,[ library_loadermprop_id,library_loaderm_id,type_id,value,rank]).
flybase_cols(library_dbxref,[ library_dbxref_id,library_id,dbxref_id,is_current]).
flybase_cols(library_dbxrefprop,[ library_dbxrefprop_id,library_dbxref_id,type_id,value,rank]).
flybase_cols(library_expression,[ library_expression_id,expression_id,library_id,pub_id]).
flybase_cols(library_expressionprop,[ library_expressionprop_id,library_expression_id,type_id,value,rank]).
flybase_cols(library_feature,[ library_feature_id,library_id,feature_id]).
flybase_cols(library_featureprop,[ library_featureprop_id,library_feature_id,type_id,value,rank]).
flybase_cols(library_grpmember,[ library_grpmember_id,grpmember_id,library_id]).
flybase_cols(library_humanhealth,[ library_humanhealth_id,humanhealth_id,library_id,pub_id]).
flybase_cols(library_humanhealthprop,[ library_humanhealthprop_id,library_humanhealth_id,type_id,value,rank]).
flybase_cols(library_interaction,[ library_interaction_id,interaction_id,library_id,pub_id]).
flybase_cols(library_relationship,[ library_relationship_id,subject_id,object_id,type_id]).
flybase_cols(library_relationship_pub,[ library_relationship_pub_id,library_relationship_id,pub_id]).
flybase_cols(library_strain,[ library_strain_id,strain_id,library_id,pub_id]).
flybase_cols(library_strainprop,[ library_strainprop_id,library_strain_id,type_id,value,rank]).
flybase_cols(library_synonym,[ library_synonym_id,synonym_id,library_id,pub_id,is_current,is_internal]).
flybase_cols(libraryprop,[ libraryprop_id,library_id,type_id,value,rank]).
flybase_cols(libraryprop_pub,[ libraryprop_pub_id,libraryprop_id,pub_id]).
flybase_cols(library_pub,[ library_pub_id,library_id,pub_id]).
flybase_cols(lock,[ lock_id,username,locktype,lockname,lockrank,lockstatus,timeaccessioend,timelastmodified,chadoxmlfile,comment,task]).
flybase_cols(organism,[ organism_id,abbreviation,genus,species,common_name,comment]).
flybase_cols(organism_loaderm,[ organism_loaderm_id,organism_id,loaderm_id,rank,pub_id]).
flybase_cols(organism_loadermprop,[ organism_loadermprop_id,organism_loaderm_id,type_id,value,rank]).
flybase_cols(organism_dbxref,[ organism_dbxref_id,organism_id,dbxref_id,is_current]).
flybase_cols(organism_grpmember,[ organism_grpmember_id,grpmember_id,organism_id]).
flybase_cols(organism_library,[ organism_library_id,organism_id,library_id]).
flybase_cols(organismprop,[ organismprop_id,organism_id,type_id,value,rank]).
flybase_cols(organismprop_pub,[ organismprop_pub_id,organismprop_id,pub_id]).
flybase_cols(organism_pub,[ organism_pub_id,organism_id,pub_id]).
flybase_cols(phendesc,[ phendesc_id,genotype_id,environment_id,description,type_id,pub_id]).
flybase_cols(phenotype,[ phenotype_id,uniquename,observable_id,attr_id,value,cvalue_id,assay_id]).
flybase_cols(phenotype_comparison,[ phenotype_comparison_id,genotype1_id,environment1_id,genotype2_id,environment2_id,phenotype1_id,phenotype2_id,pub_id,organism_id]).
flybase_cols(phenotype_comparison_loaderm,[ phenotype_comparison_loaderm_id,phenotype_comparison_id,loaderm_id,rank]).
flybase_cols(phenotype_loaderm,[ phenotype_loaderm_id,phenotype_id,loaderm_id,rank]).
flybase_cols(phenstatement,[ phenstatement_id,genotype_id,environment_id,phenotype_id,type_id,pub_id]).
flybase_cols(project,[ project_id,name,description]).
flybase_cols(pub,[ pub_id,title,volumetitle,volume,series_name,issue,pyear,pages,miniref,type_id,is_obsolete,publisher,pubplace,uniquename]).
flybase_cols(pub_dbxref,[ pub_dbxref_id,pub_id,dbxref_id,is_current]).
flybase_cols(pub_relationship,[ pub_relationship_id,type_id,subject_id,object_id]).
flybase_cols(pubauthor,[ pubauthor_id,pub_id,rank,editor,surname,givennames,suffix]).
flybase_cols(pubprop,[ pubprop_id,pub_id,type_id,value,rank]).
flybase_cols(stock,[ stock_id,dbxref_id,organism_id,name,uniquename,description,type_id,is_obsolete]).
flybase_cols(stock_loaderm,[ stock_loaderm_id,stock_id,loaderm_id,pub_id]).
flybase_cols(stock_dbxref,[ stock_dbxref_id,stock_id,dbxref_id,is_current]).
flybase_cols(stock_genotype,[ stock_genotype_id,stock_id,genotype_id]).
flybase_cols(stock_relationship,[ stock_relationship_id,subject_id,object_id,type_id,value,rank]).
flybase_cols(stock_relationship_pub,[ stock_relationship_pub_id,stock_relationship_id,pub_id]).
flybase_cols(stockcollection,[ stockcollection_id,type_id,contact_id,name,uniquename]).
flybase_cols(stockcollection_stock,[ stockcollection_stock_id,stockcollection_id,stock_id]).
flybase_cols(stockcollectionprop,[ stockcollectionprop_id,stockcollection_id,type_id,value,rank]).
flybase_cols(stockprop,[ stockprop_id,stock_id,type_id,value,rank]).
flybase_cols(stockprop_pub,[ stockprop_pub_id,stockprop_id,pub_id]).
flybase_cols(stock_pub,[ stock_pub_id,stock_id,pub_id]).
flybase_cols(strain,[ strain_id,name,uniquename,organism_id,dbxref_id,is_obsolete]).
flybase_cols(strain_loaderm,[ strain_loaderm_id,strain_id,loaderm_id,pub_id]).
flybase_cols(strain_loadermprop,[ strain_loadermprop_id,strain_loaderm_id,type_id,value,rank]).
flybase_cols(strain_dbxref,[ strain_dbxref_id,strain_id,dbxref_id,is_current]).
flybase_cols(strain_feature,[ strain_feature_id,strain_id,feature_id,pub_id]).
flybase_cols(strain_featureprop,[ strain_featureprop_id,strain_feature_id,type_id,value,rank]).
flybase_cols(strain_phenotype,[ strain_phenotype_id,strain_id,phenotype_id,pub_id]).
flybase_cols(strain_phenotypeprop,[ strain_phenotypeprop_id,strain_phenotype_id,type_id,value,rank]).
flybase_cols(strain_relationship,[ strain_relationship_id,subject_id,object_id,type_id,value,rank]).
flybase_cols(strain_relationship_pub,[ strain_relationship_pub_id,strain_relationship_id,pub_id]).
flybase_cols(strain_synonym,[ strain_synonym_id,strain_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(strainprop,[ strainprop_id,strain_id,type_id,value,rank]).
flybase_cols(strainprop_pub,[ strainprop_pub_id,strainprop_id,pub_id]).
flybase_cols(strain_pub,[ strain_pub_id,strain_id,pub_id]).
flybase_cols(synonym,[ synonym_id,name,type_id,synonym_sgml]).
flybase_cols(tableinfo,[ tableinfo_id,name,primary_key_column,is_view,view_on_table_id,superclass_table_id,is_updateable,modification_date]).
flybase_cols(update_track,[ update_track_id,release,fbid,time_update,author,statement,comment,annotation_id]).

%!  table_columns(+TableName, -ColumnList) is det.
%
%   Retrieves the list of columns associated with a given table name. The columns can
%   be obtained from multiple sources, such as `column_names/2`, `flybase_cols/2`, or `t_h_n/3`.
%   This predicate unifies the table name with a known table and provides its corresponding schema.
%
%   @arg TableName The name of the table whose column list is to be retrieved.
%   @arg ColumnList The list of column names associated with the specified table.
%
%   @example
%       % Retrieve columns for a table using various sources:
%       ?- table_columns('allele_genetic_interactions', Columns).
%       Columns = ['##allele_symbol', 'allele_FBal#', interaction, 'FBrf#'].
%
%   Implementation:
table_columns(TableName, ColumnList) :-
    % Check if the table name is mapped to columns via `table_columns_tt/2`.
    table_columns_tt(MatchingTable, ColumnList),
    % Ensure that the provided table name matches the mapped table name.
    eigther_contains(TableName, MatchingTable),
    !.

%!  table_columns_tt(+MappedTable, -ColumnList) is det.
%
%   Determines the column list for a table using various data sources. It tries
%   the following sources in order of priority:
%   1. `column_names/2`
%   2. `flybase_cols/2`
%   3. `t_h_n/3`
%
%   @arg MappedTable The table name or alias matched to a column list.
%   @arg ColumnList The list of column names for the matched table.
%
table_columns_tt(TableName, ColumnList) :-
    % Attempt to retrieve columns using `column_names/2`.
    column_names(TableName, ColumnList).
table_columns_tt(TableName, ColumnList) :-
    % Attempt to retrieve columns using `flybase_cols/2`.
    flybase_cols(TableName, ColumnList).
table_columns_tt(TableName, ColumnList) :-
    % Attempt to retrieve columns using `t_h_n/3`.
    t_h_n(TableName, _, ColumnList).

%!  eigther_contains(+Name1, +Name2) is nondet.
%
%   Determines if two names (e.g., table names or aliases) are considered equivalent or 
%   if one name symbolically contains the other. This predicate supports flexible matching
%   for aliases or variations in naming conventions.
%
%   @arg Name1 The first name to compare.
%   @arg Name2 The second name to compare.
%
%   @example
%       % Direct equality:
%       ?- eigther_contains('allele_genetic_interactions', 'allele_genetic_interactions').
%       true.
%
%       % Symbolic containment:
%       ?- eigther_contains('allele_genetic_interactions', 'genetic').
%       true.
%
%       % No match:
%       ?- eigther_contains('allele_genetic_interactions', 'nonexistent').
%       false.
%
%   Implementation:
eigther_contains(Name1, Name2) :-
    % Check if the names are directly equal.
    Name1 = Name2,!.
eigther_contains(Name2, Name1) :-
    % Check if Name2 symbolically contains Name1.
    symbol_contains(Name2, Name1), !.
eigther_contains(Name1, Name2) :-
    % Check if Name1 symbolically contains Name2.
    symbol_contains(Name1, Name2), !.

%!  column_names(+TableName, +ColumnList) is det.
%
%   Maps table names to their respective column lists. Each table is defined by a name
%   and a list of columns that describe its schema. This predicate is used to retrieve
%   column names for FlyBase and other datasets, enabling schema exploration and validation.
%
%   @arg TableName The name of the table (e.g., 'allele_genetic_interactions').
%   @arg ColumnList The list of column names defining the table's schema.
%
%   @example
%       % Example: Retrieve column names for a specific table:
%       column_names('allele_genetic_interactions', ColumnList).
%       ColumnList = ['allele_symbol', 'allele_FBal', interaction, 'FBrf'].
%
column_names('cyto-genetic-seq', ['Cytogenetic_map_position', 'Genetic_map_position', 'Sequence_coordinates_(release_6)', 'R6_conversion_notes']).
column_names('Dmel_enzyme', [gene_group_id, gene_group_name, listOf(gene_group_GO_id), listOf(gene_group_GO_name), listOf(gene_group_EC_number), listOf(gene_group_EC_name), gene_id, gene_symbol, gene_name, listOf(gene_EC_number), listOf(gene_EC_name)]).
column_names('scRNA-Seq_gene_expression', ['Pub_ID', 'Pub_miniref', 'Clustering_Analysis_ID', 'Clustering_Analysis_Name', 'Source_Tissue_Sex', 'Source_Tissue_Stage', 'Source_Tissue_Anatomy', 'Cluster_ID', 'Cluster_Name', 'Cluster_Cell_Type_ID', 'Cluster_Cell_Type_Name', 'Gene_ID', 'Gene_Symbol', 'Mean_Expression', 'Spread']).
column_names(allele_genetic_interactions, [allele_symbol, allele_FBal, interaction, 'FBrf']).
column_names(allele_phenotypic,           [allele_symbol, allele_FBal, phenotype, 'FBrf']).
column_names(fbal_to_fbgn,             ['AlleleID', 'AlleleSymbol', 'GeneID', 'GeneSymbol']).
column_names(genotype_phenotype_data, [listOf(genotype_symbols, [/, ' ']), listOf(genotype_FBids, [/, ' ']), phenotype_name, phenotype_id, listOf(qualifier_names, ['|']), listOf(qualifier_ids, ['|']), reference]).
%                                        #genotype_symbols              genotype_FBids  phenotype_name  phenotype_id    qualifier_names qualifier_ids   reference
column_names(automated_gene_summaries, [primary_FBgn, summary_text]).
column_names(best_gene_summary, ['FBgn', 'Gene_Symbol', 'Summary_Source', 'Summary']).
column_names(cDNA_clone_data, ['FBcl', organism_abbreviation, clone_name, dataset_metadata_name, listOf(cDNA_accession), listOf('EST_accession')]).
column_names(dataset_metadata, ['Dataset_Metadata_ID', 'Dataset_Metadata_Name', 'Item_ID', 'Item_Name']).
column_names(disease_model_annotations, ['FBgn', 'Gene_symbol', 'HGNC', 'DO_qualifier', 'DO', 'DO_term', 'Allele_used_in_model_(FBal)', 'Allele_used_in_model_(symbol)', 'Based_on_orthology_with_(HGNC_ID)', 'Based_on_orthology_with_(symbol)', 'Evidence/interacting_alleles', 'Reference_(FBrf)']).
column_names(dmel_gene_sequence_ontology_annotations, [gene_primary_id, gene_symbol, so_term_name, so_term_id]).
column_names(dmel_human_orthologs_disease, ['Dmel_gene', 'Dmel_gene_symbol', 'Human_gene_HGNC', 'Human_gene_OMIM', 'Human_gene_symbol', 'DIOPT_score', 'OMIM_Phenotype_IDs', 'OMIM_Phenotype_IDs[name]']).
column_names(dmel_paralogs, ['FBgn', 'GeneSymbol', 'Arm/Scaffold', 'Location', 'Strand', 'Paralog_FBgn', 'Paralog_GeneSymbol', 'Paralog_Arm/Scaffold', 'Paralog_Location', 'Paralog_Strand', 'DIOPT_score']).
column_names(dmel_unique_protein_isoforms, ['FBgn', 'FB_gene_symbol', representative_protein, listOf(identical_protein)]).
column_names(entity_publication, [entity_id, entity_name, 'FlyBase_publication', 'PubMed']).
column_names(fb_synonym, [primary_FBid, organism_abbreviation, current_symbol, current_fullname, listOf(fullname_synonym, ['|']), listOf(symbol_synonym, ['|'])]).
column_names(fbgn_annotation_ID, [gene_symbol, organism_abbreviation, primary_FBgn, listOf(secondary_FBgn), annotation_ID, listOf(secondary_annotation_ID)]).
column_names(fbgn_exons2affy1_overlaps, ['FBgn', listOf(affy)]).
column_names(fbgn_exons2affy2_overlaps, ['FBgn', listOf(affy)]).
column_names(fbgn_fbtr_fbpp, ['FBgn', 'FBtr', 'FBpp']).
column_names(fbgn_fbtr_fbpp_expanded, [organism, gene_type, gene_ID, gene_symbol, gene_fullname, annotation_ID, transcript_type, transcript_ID, transcript_symbol, polypeptide_ID, polypeptide_symbol]).
column_names(fbgn_gleanr, [organism_abbreviation, gene_symbol, primary_FBgn, 'GLEANR']).
column_names(fbgn_NAseq_Uniprot, [gene_symbol, organism_abbreviation, primary_FBgn, nucleotide_accession, na_based_protein_accession, 'UniprotKB/Swiss-Prot/TrEMBL_accession', 'EntrezGene', 'RefSeq_transcripts', 'RefSeq_proteins']).
column_names(fbgn_uniprot, [primary_FBgn, gene_symbol, 'CG', 'UniprotKB/Swiss-Prot/TrEMBL_accession']).
column_names(fbrf_pmid_pmcid_doi, ['FBrf', 'PMID', 'PMCID', 'DOI', pub_type, miniref, pmid_added]).
column_names(gene_functional_complementation, ['Dmel_gene_symbol', 'Dmel_gene_FBgn', ortholog_gene_symbol, ortholog_gene_FBgn_ID, reference_FBrf]).
column_names(gene_genetic_interactions, [listOf('Starting_gene_symbol'), listOf('Starting_gene_FBgn'), listOf('Interacting_gene_symbol'), listOf('Interacting_gene_FBgn'), 'Interaction_type', 'Publication_FBrf']).
column_names(gene_group, ['FB_group', 'FB_group_symbol', 'FB_group_name', 'Parent_FB_group', 'Parent_FB_group_symbol', 'Group_member_FB_gene', 'Group_member_FB_gene_symbol']).
column_names(gene_groups_HGNC, ['FB_group', 'FB_group_symbol', 'FB_group_name', 'HGNC_family']).
column_names(gene_map_table, [organism_abbreviation, current_symbol, primary_FBid, recombination_loc, cytogenetic_loc, sequence_loc]).
column_names(gene_rpkm_matrix, [gene_primary_id, gene_symbol, gene_fullname, gene_type, 'DATASAMPLE_NAME_(DATASET_ID)']).
column_names(gene_rpkm_report, ['Release_ID', 'FBgn', 'GeneSymbol', 'Parent_library_FBlc', 'Parent_library_name', 'RNASource_FBlc', 'RNASource_name', 'RPKM_value', 'Bin_value', 'Unique_exon_base_count', 'Total_exon_base_count', 'Count_used']).
column_names(gene_snapshots, ['FBgn', 'GeneSymbol', 'GeneName', datestamp, gene_snapshot_text]).
column_names(genomic_clone, ['FBcl', organism_abbreviation, clone_name, accession]).
column_names(insertion_mapping, [insertion_symbol, 'FBti', genomic_location, range, orientation, estimated_cytogenetic_location, observed_cytogenetic_location]).
column_names(organism_list, [genus, species, abbreviation, common_name, 'NCBI_taxon', 'drosophilid?']).
column_names(pathway_group, ['FB_group', 'FB_group_symbol', 'FB_group_name', 'Parent_FB_group', 'Parent_FB_group_symbol', 'Group_member_FB_gene', 'Group_member_FB_gene_symbol']).
column_names(physical_interactions_mitab, [listOf('ID_Interactor_A'), listOf('ID_Interactor_B'), listOf('Alt_ID_Interactor_A'), listOf('Alt_ID_Interactor_B'), listOf('Alias_Interactor_A'), listOf('Alias_Interactor_B'), listOf('Interaction_Detection_Method'), listOf('Publication_1st_Author'), listOf('Publication'), 'Taxid_Interactor_A', 'Taxid_Interactor_B', listOf('Interaction_Type'), listOf('Source_Database'), listOf('Interaction_Identifier'), listOf('Confidence_Value'), listOf('Expansion_Method'), listOf('Biological_Role_Interactor_A'), listOf('Biological_Role_Interactor_B'), listOf('Experimental_Role_Interactor_A'), listOf('Experimental_Role_Interactor_B'), listOf('Type_Interactor_A'), listOf('Type_Interactor_B'), listOf('Xref_Interactor_A'), listOf('Xref_Interactor_B'), listOf('Interaction_Xref'), listOf('Annotation_Interactor_A'), listOf('Annotation_Interactor_B'), listOf('Interaction_Annotation'), listOf('Host_Organism'), 'Interaction_Parameters', 'Creation_Date', 'Update_Date', 'Checksum_Interactor_A', 'Checksum_Interactor_B', 'Interaction_Checksum', 'Negative', listOf('Feature_Interactor_A'), listOf('Feature_Interactor_B'), 'Stoichiometry_Interactor_A', 'Stoichiometry_Interactor_B', listOf('Identification_Method_Participant_A'), listOf('Identification_Method_Participant_B')]).
column_names(pmid_fbgn_uniprot, ['FBrf', 'PMID', 'FBgn', 'UniProt_database', 'UniProt_id']).
column_names(synonym, [primary_FBid, organism_abbreviation, current_symbol, current_fullname, listOf(fullname_synonym), listOf(symbol_synonym)]).
column_names_ext(pmid_fbgn_uniprot, ['FBrf_id', 'PMID', 'FBgn_id', 'UniProt_database', 'UniProt_id']).
column_names_ext(gene_genetic_interactions, [listOf('Starting_gene_symbol', ['|']), listOf('Starting_gene_FBgn', ['|']), listOf('Interacting_gene_symbol', ['|']), listOf('Interacting_gene_FBgn', ['|']), 'Interaction_type', 'Publication_FBrf']).
column_names_ext(gene_rpkm_matrix, [gene_primary_id, gene_symbol, gene_fullname, gene_type, 'BCM_1_E2-4hr_(FBlc0000061)', 'BCM_1_E14-16hr_(FBlc0000062)', 'BCM_1_E2-16hr_(FBlc0000063)', 'BCM_1_E2-16hr100_(FBlc0000064)', 'BCM_1_L3i_(FBlc0000065)', 'BCM_1_L3i100_(FBlc0000066)', 'BCM_1_P3d_(FBlc0000067)', 'BCM_1_FA3d_(FBlc0000068)', 'BCM_1_MA3d_(FBlc0000069)', 'BCM_1_P_(FBlc0000070)', 'BCM_1_L_(FBlc0000071)', 'BCM_1_A17d_(FBlc0000072)', 'mE_mRNA_em0-2hr_(FBlc0000086)', 'mE_mRNA_em2-4hr_(FBlc0000087)', 'mE_mRNA_em4-6hr_(FBlc0000088)', 'mE_mRNA_em6-8hr_(FBlc0000089)', 'mE_mRNA_em8-10hr_(FBlc0000090)', 'mE_mRNA_em10-12hr_(FBlc0000091)', 'mE_mRNA_em12-14hr_(FBlc0000092)', 'mE_mRNA_em14-16hr_(FBlc0000093)', 'mE_mRNA_em16-18hr_(FBlc0000094)', 'mE_mRNA_em18-20hr_(FBlc0000095)', 'mE_mRNA_em20-22hr_(FBlc0000096)', 'mE_mRNA_em22-24hr_(FBlc0000097)', 'mE_mRNA_L1_(FBlc0000098)', 'mE_mRNA_L2_(FBlc0000099)', 'mE_mRNA_L3_12hr_(FBlc0000100)', 'mE_mRNA_L3_PS1-2_(FBlc0000101)', 'mE_mRNA_L3_PS3-6_(FBlc0000102)', 'mE_mRNA_L3_PS7-9_(FBlc0000103)', 'mE_mRNA_WPP_(FBlc0000104)', 'mE_mRNA_P5_(FBlc0000105)', 'mE_mRNA_P6_(FBlc0000106)', 'mE_mRNA_P8_(FBlc0000107)', 'mE_mRNA_P9-10_(FBlc0000108)', 'mE_mRNA_P15_(FBlc0000109)', 'mE_mRNA_AdF_Ecl_1days_(FBlc0000110)', 'mE_mRNA_AdF_Ecl_5days_(FBlc0000111)', 'mE_mRNA_AdF_Ecl_30days_(FBlc0000112)', 'mE_mRNA_AdM_Ecl_1days_(FBlc0000113)', 'mE_mRNA_AdM_Ecl_5days_(FBlc0000114)', 'mE_mRNA_AdM_Ecl_30days_(FBlc0000115)', 'mE_mRNA_A_MateF_1d_head_(FBlc0000207)', 'mE_mRNA_A_MateF_4d_ovary_(FBlc0000208)', 'mE_mRNA_A_MateM_1d_head_(FBlc0000209)', 'mE_mRNA_A_VirF_1d_head_(FBlc0000210)', 'mE_mRNA_A_VirF_4d_head_(FBlc0000211)', 'mE_mRNA_A_MateF_20d_head_(FBlc0000212)', 'mE_mRNA_A_MateF_4d_head_(FBlc0000213)', 'mE_mRNA_A_MateM_20d_head_(FBlc0000214)', 'mE_mRNA_A_MateM_4d_acc_gland_(FBlc0000215)', 'mE_mRNA_A_MateM_4d_head_(FBlc0000216)', 'mE_mRNA_A_MateM_4d_testis_(FBlc0000217)', 'mE_mRNA_A_1d_carcass_(FBlc0000218)', 'mE_mRNA_A_1d_dig_sys_(FBlc0000219)', 'mE_mRNA_A_20d_carcass_(FBlc0000220)', 'mE_mRNA_A_20d_dig_sys_(FBlc0000221)', 'mE_mRNA_A_4d_carcass_(FBlc0000222)', 'mE_mRNA_A_4d_dig_sys_(FBlc0000223)', 'mE_mRNA_P8_CNS_(FBlc0000224)', 'mE_mRNA_L3_CNS_(FBlc0000225)', 'mE_mRNA_L3_Wand_carcass_(FBlc0000226)', 'mE_mRNA_L3_Wand_dig_sys_(FBlc0000227)', 'mE_mRNA_L3_Wand_fat_(FBlc0000228)', 'mE_mRNA_L3_Wand_imag_disc_(FBlc0000229)', 'mE_mRNA_L3_Wand_saliv_(FBlc0000230)', 'mE_mRNA_A_VirF_20d_head_(FBlc0000231)', 'mE_mRNA_A_VirF_4d_ovary_(FBlc0000232)', 'mE_mRNA_WPP_fat_(FBlc0000233)', 'mE_mRNA_WPP_saliv_(FBlc0000234)', 'mE_mRNA_P8_fat_(FBlc0000235)', 'mE_mRNA_A_4d_Cold1_(FBlc0000237)', 'mE_mRNA_A_4d_Cold2_(FBlc0000238)', 'mE_mRNA_L3_Cu_0.5mM_(FBlc0000239)', 'mE_mRNA_L3_late_Zn_5mM_(FBlc0000240)', 'mE_mRNA_A_4d_Cu_15mM_(FBlc0000241)', 'mE_mRNA_A_4d_Zn_4.5mM_(FBlc0000242)', 'mE_mRNA_A_4d_Caffeine_25mg/ml_(FBlc0000243)', 'mE_mRNA_A_4d_Caffeine_2.5mg/ml_(FBlc0000244)', 'mE_mRNA_L3_Caffeine_1.5mg/ml_(FBlc0000245)', 'mE_mRNA_A_4d_Cd_0.1M_(FBlc0000246)', 'mE_mRNA_A_4d_Cd_0.05M_(FBlc0000247)', 'mE_mRNA_L3_Cd_12h_(FBlc0000248)', 'mE_mRNA_L3_Cd_6hr_(FBlc0000249)', 'mE_mRNA_A_4d_Paraquat_5mM_(FBlc0000250)', 'mE_mRNA_A_4d_Paraquat_10mM_(FBlc0000251)', 'mE_mRNA_L3_Rotenone_8ug_(FBlc0000252)', 'mE_mRNA_L3_Rotenone_2ug_(FBlc0000253)', 'mE_mRNA_L3_EtOH_10_(FBlc0000254)', 'mE_mRNA_L3_EtOH_5_(FBlc0000255)', 'mE_mRNA_L3_EtOH_2.5_(FBlc0000256)', 'mE_mRNA_A_4d_Heatshock_(FBlc0000257)', 'mE_mRNA_A_10d_Resveratrol_100uM_(FBlc0000672)', 'mE_mRNA_A_10d_Rotenone_Starved_(FBlc0000673)', 'mE_mRNA_F_Sindbis_virus_(FBlc0000674)', 'mE_mRNA_L_Sindbis_virus_(FBlc0000675)', 'mE_mRNA_M_Sindbis_virus_(FBlc0000676)', 'mE_mRNA_P_Sindbis_virus_(FBlc0000677)', 'mE_mRNA_CME-W2_cells_(FBlc0000261)', 'mE_mRNA_GM2_cells_(FBlc0000262)', 'mE_mRNA_mbn2_cells_(FBlc0000263)', 'mE_mRNA_BG2-c2_cells_(FBlc0000264)', 'mE_mRNA_D20-c5_cells_(FBlc0000265)', 'mE_mRNA_S3_cells_(FBlc0000266)', 'mE_mRNA_1182-4H_cells_(FBlc0000267)', 'mE_mRNA_CME_L1_cells_(FBlc0000268)', 'mE_mRNA_Kc167_cells_(FBlc0000269)', 'mE_mRNA_BG1-c1_cells_(FBlc0000270)', 'mE_mRNA_D11_cells_(FBlc0000271)', 'mE_mRNA_D16-c3_cells_(FBlc0000272)', 'mE_mRNA_D17-c3_cells_(FBlc0000273)', 'mE_mRNA_D21_cells_(FBlc0000274)', 'mE_mRNA_D32_cells_(FBlc0000275)', 'mE_mRNA_D4-c1_cells_(FBlc0000276)', 'mE_mRNA_D8_cells_(FBlc0000277)', 'mE_mRNA_D9_cells_(FBlc0000278)', 'mE_mRNA_S1_cells_(FBlc0000279)', 'mE_mRNA_S2R+_cells_(FBlc0000280)', 'mE_mRNA_Sg4_cells_(FBlc0000281)', 'mE_mRNA_OSS_cells_(FBlc0000282)', 'mE_mRNA_OSC_cells_(FBlc0000283)', 'mE_mRNA_fGS_cells_(FBlc0000284)', 'Knoblich_mRNA_L3_CNS_neuroblast_(FBlc0000505)', 'Knoblich_mRNA_L3_CNS_neuron_(FBlc0000506)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Brain_(FBlc0003619)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Crop_(FBlc0003620)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Carcass_(FBlc0003621)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Eye_(FBlc0003622)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_FatBody_(FBlc0003623)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Head_(FBlc0003624)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Hindgut_(FBlc0003625)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Midgut_(FBlc0003626)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Ovary_(FBlc0003627)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_RectalPad_(FBlc0003628)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_SalivaryGland_(FBlc0003629)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_ThoracicoAbdominalGanglion_(FBlc0003630)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_MalpighianTubule_(FBlc0003631)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Mated_Spermathecum_(FBlc0003632)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Virgin_Spermathecum_(FBlc0003633)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Whole_(FBlc0003634)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Brain_(FBlc0003635)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Crop_(FBlc0003636)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Carcass_(FBlc0003637)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Eye_(FBlc0003638)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_FatBody_(FBlc0003639)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Head_(FBlc0003640)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Hindgut_(FBlc0003641)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Midgut_(FBlc0003642)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_RectalPad_(FBlc0003643)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_SalivaryGland_(FBlc0003644)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_ThoracicoAbdominalGanglion_(FBlc0003645)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_MalpighianTubule_(FBlc0003646)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Testis_(FBlc0003647)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_AccessoryGland_(FBlc0003648)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Whole_(FBlc0003649)', 'RNA-Seq_Profile_FlyAtlas2_L3_CNS_(FBlc0003650)', 'RNA-Seq_Profile_FlyAtlas2_L3_FatBody_(FBlc0003651)', 'RNA-Seq_Profile_FlyAtlas2_L3_Hindgut_(FBlc0003652)', 'RNA-Seq_Profile_FlyAtlas2_L3_MalpighianTubule_(FBlc0003653)', 'RNA-Seq_Profile_FlyAtlas2_L3_Midgut_(FBlc0003654)', 'RNA-Seq_Profile_FlyAtlas2_L3_SalivaryGland_(FBlc0003655)', 'RNA-Seq_Profile_FlyAtlas2_L3_Trachea_(FBlc0003656)', 'RNA-Seq_Profile_FlyAtlas2_L3_Carcass_(FBlc0003657)', 'RNA-Seq_Profile_FlyAtlas2_L3_Whole_(FBlc0003658)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Heart_(FBlc0003724)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Heart_(FBlc0003725)']).

%!  flybase_tables(-TableList) is det.
%
%   Provides a list of all FlyBase tables. This predicate aggregates the names of tables
%   used in FlyBase, which can be queried or processed for various operations.
%
%   @arg TableList A list of table names as atoms, representing the schema of FlyBase.
%
%   @example
%       % Retrieve the complete list of FlyBase tables:
%       ?- flybase_tables(Tables).
%       Tables = [analysis, analysisfeature, analysisgrp, ...].
%
flybase_tables([
    % Analysis-related tables:
    analysis, analysisfeature, analysisgrp, analysisgrpmember, analysisprop,
    
    % Audit-related tables:
    audit_chado,
    
    % Cell line-related tables:
    cell_line, cell_line_loaderm, cell_line_loadermprop, cell_line_dbxref, cell_line_feature,
    cell_line_library, cell_line_libraryprop, cell_line_pub, cell_line_relationship, 
    cell_line_strain, cell_line_strainprop, cell_line_synonym, cell_lineprop, cell_lineprop_pub,

    % Contact and controlled vocabulary tables:
    contact, cv, loaderm, loaderm_dbxref, loaderm_relationship, loadermpath,
    loadermprop, loadermsynonym,

    % Database and cross-reference tables:
    db, dbxref, dbxrefprop,

    % Image and environment tables:
    eimage, environment, environment_loaderm,

    % Expression-related tables:
    expression, expression_loaderm, expression_loadermprop, expression_image,
    expression_pub, expressionprop,

    % Feature-related tables:
    feature, feature_loaderm, feature_loaderm_dbxref, feature_loadermprop, feature_dbxref,
    feature_expression, feature_expressionprop, feature_genotype, feature_grpmember,
    feature_grpmember_pub, feature_humanhealth_dbxref, feature_interaction,
    feature_interaction_pub, feature_interactionprop, feature_phenotype, feature_pub,
    feature_pubprop, feature_relationship, feature_relationship_pub, feature_relationshipprop,
    feature_relationshipprop_pub, feature_synonym, featureloc, featureloc_pub, featuremap,
    featuremap_pub, featurepos, featureprop, featureprop_pub, featurerange,

    % Genotype-related tables:
    genotype, genotype_loaderm, genotype_loadermprop, genotype_dbxref, genotype_pub,
    genotype_synonym, genotypeprop, genotypeprop_pub,

    % Group-related tables:
    grp, grp_loaderm, grp_dbxref, grp_pub, grp_pubprop, grp_relationship,
    grp_relationship_pub, grp_relationshipprop, grp_synonym, grpmember,
    grpmember_loaderm, grpmember_pub, grpmemberprop, grpmemberprop_pub, grpprop, grpprop_pub,

    % Human health-related tables:
    humanhealth, humanhealth_loaderm, humanhealth_loadermprop, humanhealth_dbxref,
    humanhealth_dbxrefprop, humanhealth_dbxrefprop_pub, humanhealth_feature,
    humanhealth_featureprop, humanhealth_phenotype, humanhealth_phenotypeprop,
    humanhealth_pub, humanhealth_pubprop, humanhealth_relationship,
    humanhealth_relationship_pub, humanhealth_synonym, humanhealthprop,
    humanhealthprop_pub,

    % Interaction-related tables:
    interaction, interaction_cell_line, interaction_loaderm, interaction_loadermprop,
    interaction_expression, interaction_expressionprop, interaction_group,
    interaction_group_feature_interaction, interaction_pub, interactionprop,
    interactionprop_pub,

    % Library-related tables:
    library, library_loaderm, library_loadermprop, library_dbxref, library_dbxrefprop,
    library_expression, library_expressionprop, library_feature, library_featureprop,
    library_grpmember, library_humanhealth, library_humanhealthprop, library_interaction,
    library_pub, library_relationship, library_relationship_pub, library_strain,
    library_strainprop, library_synonym, libraryprop, libraryprop_pub,

    % Lock and organism-related tables:
    lock, organism, organism_loaderm, organism_loadermprop, organism_dbxref,
    organism_grpmember, organism_library, organism_pub, organismprop, organismprop_pub,

    % Phenotype-related tables:
    phendesc, phenotype, phenotype_comparison, phenotype_comparison_loaderm,
    phenotype_loaderm, phenstatement,

    % Project and publication-related tables:
    project, pub, pub_dbxref, pub_relationship, pubauthor, pubprop,

    % SQL-related tables:
    sql_features, sql_implementation_info, sql_parts, sql_sizing,

    % Stock-related tables:
    stock, stock_loaderm, stock_dbxref, stock_genotype, stock_pub,
    stock_relationship, stock_relationship_pub, stockcollection,
    stockcollection_stock, stockcollectionprop, stockprop, stockprop_pub,

    % Strain-related tables:
    strain, strain_loaderm, strain_loadermprop, strain_dbxref, strain_feature,
    strain_featureprop, strain_phenotype, strain_phenotypeprop, strain_pub,
    strain_relationship, strain_relationship_pub, strain_synonym, strainprop,
    strainprop_pub,

    % Synonym and miscellaneous tables:
    synonym, tableinfo, update_track
]).

%!  table_n_type(+Table, +ColumnIndex, +ColumnName, +Type) is det.
%
%   Defines the structure of various database tables, mapping column indices 
%   to their respective names and types. Types include specific formats 
%   (e.g., 'FBgn', 'FBrf') or generic values (_).
%
%   @arg Table The name of the table.
%   @arg ColumnIndex The 1-based index of the column in the table.
%   @arg ColumnName The name of the column, typically as a string or atom.
%   @arg Type The data type or category for the column, or `_` for unspecified types.
%
%   @examples
%     % Retrieve the name and type for the first column of the 'stocks' table:
%     ?- table_n_type(stocks, 1, ColumnName, Type).
%     ColumnName = stock_id,
%     Type = 'FBst'.
%
%     % Verify if a particular column exists in the 'dmel_paralogs' table:
%     ?- table_n_type(dmel_paralogs, Index, 'Paralog_FBgn', Type).
%     Index = 6,
%     Type = 'FBgn'.
%
table_n_type(allele_genetic_interactions, 1, allele_symbol, _).
table_n_type(allele_genetic_interactions, 2, allele_FBal, 'FBal').
table_n_type(allele_genetic_interactions, 3, interaction, _).
table_n_type(allele_genetic_interactions, 4, 'FBrf', 'FBrf').
table_n_type(disease_model_annotations, 1, 'FBgn', 'FBgn').
table_n_type(disease_model_annotations, 2, 'Gene_symbol', _).
table_n_type(disease_model_annotations, 3, 'HGNC', 'HGNC').
table_n_type(disease_model_annotations, 4, 'DO_qualifier', _).
table_n_type(disease_model_annotations, 5, 'DO', _).
table_n_type(disease_model_annotations, 6, 'DO_term', _).
table_n_type(disease_model_annotations, 7, 'Allele_used_in_model_(FBal)', 'FBal').
table_n_type(disease_model_annotations, 8, 'Allele_used_in_model_(symbol)', _).
table_n_type(disease_model_annotations, 9, 'Based_on_orthology_with_(HGNC_ID)', 'HGNC').
table_n_type(disease_model_annotations, 10, 'Based_on_orthology_with_(symbol)', _).
table_n_type(disease_model_annotations, 11, 'Evidence/interacting_alleles', _).
table_n_type(disease_model_annotations, 12, 'Reference_(FBrf)', 'FBrf').
table_n_type(dmel_gene_sequence_ontology_annotations, 1, gene_primary_id, _).
table_n_type(dmel_gene_sequence_ontology_annotations, 2, gene_symbol, _).
table_n_type(dmel_gene_sequence_ontology_annotations, 3, so_term_name, _).
table_n_type(dmel_gene_sequence_ontology_annotations, 4, so_term_id, _).
table_n_type(dmel_human_orthologs_disease, 1, 'Dmel_gene', _).
table_n_type(dmel_human_orthologs_disease, 2, 'Dmel_gene_symbol', _).
table_n_type(dmel_human_orthologs_disease, 3, 'Human_gene_HGNC', 'HGNC').
table_n_type(dmel_human_orthologs_disease, 4, 'Human_gene_OMIM', 'OMIM').
table_n_type(dmel_human_orthologs_disease, 5, 'Human_gene_symbol', _).
table_n_type(dmel_human_orthologs_disease, 6, 'DIOPT_score', _).
table_n_type(dmel_human_orthologs_disease, 7, listOf('OMIM_Phenotype_IDs',[',']), 'OMIM').
table_n_type(dmel_human_orthologs_disease, 8, 'OMIM_Phenotype_IDs[name]', 'OMIM').
table_n_type(fbgn_fbtr_fbpp_expanded, 1, organism, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 2, gene_type, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 3, gene_ID, 'FBgn').
table_n_type(fbgn_fbtr_fbpp_expanded, 4, gene_symbol, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 5, gene_fullname, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 6, annotation_ID, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 7, transcript_type, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 8, transcript_ID, 'FBtr').
table_n_type(fbgn_fbtr_fbpp_expanded, 9, transcript_symbol, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 10, polypeptide_ID, 'FBpp').
table_n_type(fbgn_fbtr_fbpp_expanded, 11, polypeptide_symbol, _).
table_n_type(fbrf_pmid_pmcid_doi, 1, 'FBrf', 'FBrf').
table_n_type(fbrf_pmid_pmcid_doi, 2, 'PMID', 'PMID').
table_n_type(fbrf_pmid_pmcid_doi, 3, 'PMCID', 'PMCID').
table_n_type(fbrf_pmid_pmcid_doi, 4, 'DOI', 'DOI').
table_n_type(fbrf_pmid_pmcid_doi, 5, pub_type, _).
table_n_type(fbrf_pmid_pmcid_doi, 6, miniref, _).
table_n_type(fbrf_pmid_pmcid_doi, 7, pmid_added, _).
table_n_type(gene_genetic_interactions, 1, 'Starting_gene_symbol', _).
table_n_type(gene_genetic_interactions, 2, 'Starting_gene_FBgn', 'FBgn').
table_n_type(gene_genetic_interactions, 3, 'Interacting_gene_symbol', _).
table_n_type(gene_genetic_interactions, 4, 'Interacting_gene_FBgn', 'FBgn').
table_n_type(gene_genetic_interactions, 5, 'Interaction_type', _).
table_n_type(gene_genetic_interactions, 6, 'Publication_FBrf', 'FBrf').
table_n_type(gene_map_table, 1, organism_abbreviation, _).
table_n_type(gene_map_table, 2, current_symbol, _).
table_n_type(gene_map_table, 3, primary_FBid, 'FBgn').
table_n_type(gene_map_table, 4, recombination_loc, _).
table_n_type(gene_map_table, 5, cytogenetic_loc, _).
table_n_type(gene_map_table, 6, sequence_loc, _).
table_n_type(physical_interactions_mitab, 1, 'ID_Interactor_A', _).
table_n_type(physical_interactions_mitab, 2, 'ID_Interactor_B', _).
table_n_type(physical_interactions_mitab, 3, 'Alt_ID_Interactor_A', _).
table_n_type(physical_interactions_mitab, 4, 'Alt_ID_Interactor_B', _).
table_n_type(physical_interactions_mitab, 5, 'Alias_Interactor_A', _).
table_n_type(physical_interactions_mitab, 6, 'Alias_Interactor_B', _).
table_n_type(physical_interactions_mitab, 7, 'Interaction_Detection_Method', _).
table_n_type(physical_interactions_mitab, 8, 'Publication_1st_Author', _).
table_n_type(physical_interactions_mitab, 9, 'Publication', _).
table_n_type(physical_interactions_mitab, 10, 'Taxid_Interactor_A', _).
table_n_type(physical_interactions_mitab, 11, 'Taxid_Interactor_B', _).
table_n_type(physical_interactions_mitab, 12, 'Interaction_Type', _).
table_n_type(physical_interactions_mitab, 13, 'Source_Database', _).
table_n_type(physical_interactions_mitab, 14, 'Interaction_Identifier', _).
table_n_type(physical_interactions_mitab, 15, 'Confidence_Value', _).
table_n_type(physical_interactions_mitab, 16, 'Expansion_Method', _).
table_n_type(physical_interactions_mitab, 17, 'Biological_Role_Interactor_A', _).
table_n_type(physical_interactions_mitab, 18, 'Biological_Role_Interactor_B', _).
table_n_type(physical_interactions_mitab, 19, 'Experimental_Role_Interactor_A', _).
table_n_type(physical_interactions_mitab, 20, 'Experimental_Role_Interactor_B', _).
table_n_type(physical_interactions_mitab, 21, 'Type_Interactor_A', _).
table_n_type(physical_interactions_mitab, 22, 'Type_Interactor_B', _).
table_n_type(physical_interactions_mitab, 23, 'Xref_Interactor_A', _).
table_n_type(physical_interactions_mitab, 24, 'Xref_Interactor_B', _).
table_n_type(physical_interactions_mitab, 25, 'Interaction_Xref', _).
table_n_type(physical_interactions_mitab, 26, 'Annotation_Interactor_A', _).
table_n_type(physical_interactions_mitab, 27, 'Annotation_Interactor_B', _).
table_n_type(physical_interactions_mitab, 28, 'Interaction_Annotation', _).
table_n_type(physical_interactions_mitab, 29, 'Host_Organism', _).
table_n_type(physical_interactions_mitab, 30, 'Interaction_Parameters', _).
table_n_type(physical_interactions_mitab, 31, 'Creation_Date', _).
table_n_type(physical_interactions_mitab, 32, 'Update_Date', _).
table_n_type(physical_interactions_mitab, 33, 'Checksum_Interactor_A', _).
table_n_type(physical_interactions_mitab, 34, 'Checksum_Interactor_B', _).
table_n_type(physical_interactions_mitab, 35, 'Interaction_Checksum', _).
table_n_type(physical_interactions_mitab, 36, 'Negative', _).
table_n_type(physical_interactions_mitab, 37, 'Feature_Interactor_A', _).
table_n_type(physical_interactions_mitab, 38, 'Feature_Interactor_B', _).
table_n_type(physical_interactions_mitab, 39, 'Stoichiometry_Interactor_A', _).
table_n_type(physical_interactions_mitab, 40, 'Stoichiometry_Interactor_B', _).
table_n_type(physical_interactions_mitab, 41, 'Identification_Method_Participant_A', _).
table_n_type(physical_interactions_mitab, 42, 'Identification_Method_Participant_B', _).
table_n_type(fbal_to_fbgn, 1, 'AlleleID', 'FBal').
table_n_type(fbal_to_fbgn, 2, 'AlleleSymbol', _).
table_n_type(fbal_to_fbgn, 3, 'GeneID', 'FBgn').
table_n_type(fbal_to_fbgn, 4, 'GeneSymbol', _).
table_n_type(cDNA_clone_data, 1, 'FBcl', 'FBcl').
table_n_type(cDNA_clone_data, 2, organism_abbreviation, _).
table_n_type(cDNA_clone_data, 3, clone_name, _).
table_n_type(cDNA_clone_data, 4, dataset_metadata_name, _).
table_n_type(cDNA_clone_data, 5, cDNA_accession, _).
table_n_type(cDNA_clone_data, 6, 'EST_accession', _).
table_n_type(genomic_clone, 1, 'FBcl', 'FBcl').
table_n_type(genomic_clone, 2, organism_abbreviation, _).
table_n_type(genomic_clone, 3, clone_name, _).
table_n_type(genomic_clone, 4, accession, _).
/*
table_n_type(fbgn_uniprot, 1, primary_FBgn, 'FBgn').
table_n_type(fbgn_uniprot, 2, gene_symbol, _).
table_n_type(fbgn_uniprot, 3, 'CG', _).
table_n_type(fbgn_uniprot, 4, 'UniprotKB/Swiss-Prot/TrEMBL_accession', _).
table_n_type(pmid_fbgn_uniprot, 1, primary_FBgn, 'FBgn').
table_n_type(pmid_fbgn_uniprot, 2, gene_symbol, _).
table_n_type(pmid_fbgn_uniprot, 3, 'CG', _).
table_n_type(pmid_fbgn_uniprot, 4, 'UniprotKB/Swiss-Prot/TrEMBL_accession', _).
*/
table_n_type(automated_gene_summaries, 1, primary_FBgn, 'FBgn').
table_n_type(automated_gene_summaries, 2, summary_text, _).

table_n_type(best_gene_summary, 1, 'FBgn', 'FBgn').
table_n_type(best_gene_summary, 2, 'Gene_Symbol', _).
table_n_type(best_gene_summary, 3, 'Summary_Source', _).
table_n_type(best_gene_summary, 4, 'Summary', _).
table_n_type('Dmel_enzyme', 1, gene_group_id, _).
table_n_type('Dmel_enzyme', 2, gene_group_name, _).
table_n_type('Dmel_enzyme', 3, gene_group_GO_id, _).
table_n_type('Dmel_enzyme', 4, gene_group_GO_name, _).
table_n_type('Dmel_enzyme', 5, gene_group_EC_number, _).
table_n_type('Dmel_enzyme', 6, gene_group_EC_name, _).
table_n_type('Dmel_enzyme', 7, gene_id, 'FBgn').
table_n_type('Dmel_enzyme', 8, gene_symbol, _).
table_n_type('Dmel_enzyme', 9, gene_name, _).
table_n_type('Dmel_enzyme', 10, gene_EC_number, _).
table_n_type('Dmel_enzyme', 11, gene_EC_name, _).
table_n_type(dmel_unique_protein_isoforms, 1, 'FBgn', 'FBgn').
table_n_type(dmel_unique_protein_isoforms, 2, 'FB_gene_symbol', _).
table_n_type(dmel_unique_protein_isoforms, 3, representative_protein, _).
table_n_type(dmel_unique_protein_isoforms, 4, identical_protein, _).
table_n_type(fbgn_annotation_ID, 1, gene_symbol, _).
table_n_type(fbgn_annotation_ID, 2, organism_abbreviation, _).
table_n_type(fbgn_annotation_ID, 3, primary_FBgn, 'FBgn').
table_n_type(fbgn_annotation_ID, 4, secondary_FBgn, 'FBgn').
table_n_type(fbgn_annotation_ID, 5, annotation_ID, 'FBan').
table_n_type(fbgn_annotation_ID, 6, secondary_annotation_ID, 'FBan').
table_n_type(fbgn_exons2affy1_overlaps, 1, 'FBgn', 'FBgn').
table_n_type(fbgn_exons2affy1_overlaps, 2, affy, _).
table_n_type(fbgn_exons2affy2_overlaps, 1, 'FBgn', 'FBgn').
table_n_type(fbgn_exons2affy2_overlaps, 2, affy, _).
table_n_type(fbgn_fbtr_fbpp, 1, 'FBgn', 'FBgn').
table_n_type(fbgn_fbtr_fbpp, 2, 'FBtr', 'FBtr').
table_n_type(fbgn_fbtr_fbpp, 3, 'FBpp', 'FBpp').
table_n_type(fbgn_gleanr, 1, organism_abbreviation, _).
table_n_type(fbgn_gleanr, 2, gene_symbol, _).
table_n_type(fbgn_gleanr, 3, primary_FBgn, 'FBgn').
table_n_type(fbgn_gleanr, 4, 'GLEANR', _).
/*
table_n_type(fbgn_NAseq_Uniprot, 1, gene_symbol, _).
table_n_type(fbgn_NAseq_Uniprot, 2, organism_abbreviation, _).
table_n_type(fbgn_NAseq_Uniprot, 3, primary_FBgn, 'FBgn').
table_n_type(fbgn_NAseq_Uniprot, 4, nucleotide_accession, _).
table_n_type(fbgn_NAseq_Uniprot, 5, na_based_protein_accession, _).
table_n_type(fbgn_NAseq_Uniprot, 6, 'UniprotKB/Swiss-Prot/TrEMBL_accession', _).
table_n_type(fbgn_NAseq_Uniprot, 7, 'EntrezGene', _).
table_n_type(fbgn_NAseq_Uniprot, 8, 'RefSeq_transcripts', _).
table_n_type(fbgn_NAseq_Uniprot, 9, 'RefSeq_proteins', _).
*/
table_n_type(gene_functional_complementation, 1, 'Dmel_gene_symbol', _).
table_n_type(gene_functional_complementation, 2, 'Dmel_gene_FBgn', 'FBgn').
table_n_type(gene_functional_complementation, 3, ortholog_gene_symbol, _).
table_n_type(gene_functional_complementation, 4, ortholog_gene_FBgn_ID, 'FBgn').
table_n_type(gene_functional_complementation, 5, reference_FBrf, 'FBrf').
table_n_type(gene_group, 1, 'FB_group', 'FBgg').
table_n_type(gene_group, 2, 'FB_group_symbol', _).
table_n_type(gene_group, 3, 'FB_group_name', _).
table_n_type(gene_group, 4, 'Parent_FB_group', 'FBgg').
table_n_type(gene_group, 5, 'Parent_FB_group_symbol', _).
table_n_type(gene_group, 6, 'Group_member_FB_gene', 'FBgn').
table_n_type(gene_group, 7, 'Group_member_FB_gene_symbol', _).
table_n_type(gene_groups_HGNC, 1, 'FB_group', 'FBgg').
table_n_type(gene_groups_HGNC, 2, 'FB_group_symbol', _).
table_n_type(gene_groups_HGNC, 3, 'FB_group_name', _).
table_n_type(gene_groups_HGNC, 4, 'Parent_FB_group', 'FBgg').
table_n_type(gene_groups_HGNC, 5, 'Parent_FB_group_symbol', _).
table_n_type(gene_groups_HGNC, 6, 'Group_member_FB_gene', 'FBgn').
table_n_type(gene_groups_HGNC, 7, 'Group_member_FB_gene_symbol', _).
table_n_type(gene_rpkm_matrix, 1, gene_primary_id, 'FBgn').
table_n_type(gene_rpkm_matrix, 2, gene_symbol, _).
table_n_type(gene_rpkm_matrix, 3, gene_fullname, _).
table_n_type(gene_rpkm_matrix, 4, gene_type, _).
table_n_type(gene_rpkm_matrix, 5, 'BCM_1_E2-4hr_(FBlc0000061)', 'FBlc').
table_n_type(gene_rpkm_matrix, 6, 'BCM_1_E14-16hr_(FBlc0000062)', 'FBlc').
table_n_type(gene_rpkm_matrix, 7, 'BCM_1_E2-16hr_(FBlc0000063)', 'FBlc').
table_n_type(gene_rpkm_matrix, 8, 'BCM_1_E2-16hr100_(FBlc0000064)', 'FBlc').
table_n_type(gene_rpkm_matrix, 9, 'BCM_1_L3i_(FBlc0000065)', 'FBlc').
table_n_type(gene_rpkm_matrix, 10, 'BCM_1_L3i100_(FBlc0000066)', 'FBlc').
table_n_type(gene_rpkm_matrix, 11, 'BCM_1_P3d_(FBlc0000067)', 'FBlc').
table_n_type(gene_rpkm_matrix, 12, 'BCM_1_FA3d_(FBlc0000068)', 'FBlc').
table_n_type(gene_rpkm_matrix, 13, 'BCM_1_MA3d_(FBlc0000069)', 'FBlc').
table_n_type(gene_rpkm_matrix, 14, 'BCM_1_P_(FBlc0000070)', 'FBlc').
table_n_type(gene_rpkm_matrix, 15, 'BCM_1_L_(FBlc0000071)', 'FBlc').
table_n_type(gene_rpkm_matrix, 16, 'BCM_1_A17d_(FBlc0000072)', 'FBlc').
table_n_type(gene_rpkm_report, 1, 'Release_ID', _).
table_n_type(gene_rpkm_report, 2, 'FBgn', 'FBgn').
table_n_type(gene_rpkm_report, 3, 'GeneSymbol', _).
table_n_type(gene_rpkm_report, 4, 'Parent_library_FBlc', 'FBlc').
table_n_type(gene_rpkm_report, 5, 'Parent_library_name', _).
table_n_type(gene_rpkm_report, 6, 'RNASource_FBlc', 'FBlc').
table_n_type(gene_rpkm_report, 7, 'RNASource_name', _).
table_n_type(gene_rpkm_report, 8, 'RPKM_value', _).
table_n_type(gene_rpkm_report, 9, 'Bin_value', _).
table_n_type(gene_rpkm_report, 10, 'Unique_exon_base_count', _).
table_n_type(gene_rpkm_report, 11, 'Total_exon_base_count', _).
table_n_type(gene_rpkm_report, 12, 'Count_used', _).
table_n_type(gene_snapshots, 1, 'FBgn', 'FBgn').
table_n_type(gene_snapshots, 2, 'GeneSymbol', _).
table_n_type(gene_snapshots, 3, 'GeneName', _).
table_n_type(gene_snapshots, 4, datestamp, _).
table_n_type(gene_snapshots, 5, gene_snapshot_text, _).
table_n_type(pathway_group, 1, 'FB_group', 'FBgg').
table_n_type(pathway_group, 2, 'FB_group_symbol', _).
table_n_type(pathway_group, 3, 'FB_group_name', _).
table_n_type(pathway_group, 4, 'Parent_FB_group', 'FBgg').
table_n_type(pathway_group, 5, 'Parent_FB_group_symbol', _).
table_n_type(pathway_group, 6, 'Group_member_FB_gene', 'FBgn').
table_n_type(pathway_group, 7, 'Group_member_FB_gene_symbol', _).
table_n_type(insertion_mapping, 1, insertion_symbol, _).
table_n_type(insertion_mapping, 2, 'FBti', 'FBti').
table_n_type(insertion_mapping, 3, genomic_location, _).
table_n_type(insertion_mapping, 4, range, _).
table_n_type(insertion_mapping, 5, orientation, _).
table_n_type(insertion_mapping, 6, estimated_cytogenetic_location, _).
table_n_type(insertion_mapping, 7, observed_cytogenetic_location, _).
table_n_type('cyto-genetic-seq', 1, 'Cytogenetic_map_position', _).
table_n_type('cyto-genetic-seq', 2, 'Genetic_map_position', _).
table_n_type('cyto-genetic-seq', 3, 'Sequence_coordinates_(release_6)', _).
table_n_type('cyto-genetic-seq', 4, 'R6_conversion_notes', _).
table_n_type(dataset_metadata, 1, 'Dataset_Metadata_ID', _).
table_n_type(dataset_metadata, 2, 'Dataset_Metadata_Name', _).
table_n_type(dataset_metadata, 3, 'Item_ID', _).
table_n_type(dataset_metadata, 4, 'Item_Name', _).
table_n_type(dmel_paralogs, 1, 'FBgn', 'FBgn').
table_n_type(dmel_paralogs, 2, 'GeneSymbol', _).
table_n_type(dmel_paralogs, 3, 'Arm/Scaffold', _).
table_n_type(dmel_paralogs, 4, 'Location', _).
table_n_type(dmel_paralogs, 5, 'Strand', _).
table_n_type(dmel_paralogs, 6, 'Paralog_FBgn', 'FBgn').
table_n_type(dmel_paralogs, 7, 'Paralog_GeneSymbol', _).
table_n_type(dmel_paralogs, 8, 'Paralog_Arm/Scaffold', _).
table_n_type(dmel_paralogs, 9, 'Paralog_Location', _).
table_n_type(dmel_paralogs, 10, 'Paralog_Strand', _).
table_n_type(dmel_paralogs, 11, 'DIOPT_score', _).
table_n_type(entity_publication, 1, entity_id, _).
table_n_type(entity_publication, 2, entity_name, _).
table_n_type(entity_publication, 3, 'FlyBase_publication', 'FBrf').
table_n_type(entity_publication, 4, 'PubMed', _).
table_n_type(organism_list, 1, genus, _).
table_n_type(organism_list, 2, species, _).
table_n_type(organism_list, 3, abbreviation, _).
table_n_type(organism_list, 4, common_name, _).
table_n_type(organism_list, 5, 'NCBI_taxon', _).
table_n_type(organism_list, 6, 'drosophilid?', _).
table_n_type(stocks, 1, stock_id, 'FBst').
table_n_type(stocks, 2, dbxref_id, _).
table_n_type(stocks, 3, organism_id, _).
table_n_type(stocks, 4, name, _).
table_n_type(stocks, 5, uniquename, _).
table_n_type(stocks, 6, description, _).
table_n_type(stocks, 7, type_id, _).
table_n_type(stocks, 8, is_obsolete, _).
table_n_type(synonym, 1, primary_FBid, 'FBgn').
table_n_type(synonym, 2, organism_abbreviation, _).
table_n_type(synonym, 3, current_symbol, _).
table_n_type(synonym, 4, current_fullname, _).
table_n_type(synonym, 5, fullname_synonym, _).
table_n_type(synonym, 6, symbol_synonym, _).

%!  guess_rest(+Predicate, +ColumnIndex, +ColumnName, -Guess) is nondet.
%
%   Attempts to deduce the type of a column in a given table by analyzing its data.
%   This predicate utilizes the schema defined by `table_n_type/4` and dynamically
%   queries the table to infer the type of a column, if not already specified.
%
%   @arg Predicate The name of the table (predicate) to be queried.
%   @arg ColumnIndex The index of the column within the table (1-based index).
%   @arg ColumnName The name of the column being analyzed.
%   @arg Guess The inferred type of the column. This is unified with the result.
%
%   Details
%     - The predicate checks if the column type is unspecified (i.e., `var(Guess)`).
%     - It retrieves the arity of the table using `fb_pred/2` and constructs a callable
%       term using `functor/3` and `arg/3`.
%     - A query is executed on the table using `once(call/1)` to infer the data type.
%
%   @examples
%     % Guess the type of a column in a table:
%     ?- guess_rest(dmel_gene_sequence_ontology_annotations, 3, so_term_name, Guess).
%     Guess = 'string'.
%
%     % Attempt to infer the type of an unspecified column:
%     ?- guess_rest(fbgn_fbtr_fbpp_expanded, 2, gene_type, Guess).
%     Guess = 'atom'.
%
%   Relies on the predicate `table_n_type/4` for schema definition.
guess_rest(P, N, T, Guess) :-
    % Ensure the column type is not already specified.
    table_n_type(P, N, T, Guess),
    var(Guess),
    % Retrieve the arity of the predicate (table).
    fb_pred(P, A),
    % Construct a callable term for the predicate.
    functor(C, P, A),
    % Bind the guessed value to the argument of the callable term.
    arg(N, C, Guess),
    % Execute the query on the table, ensuring it succeeds once.
    once(call(C)).

:- dynamic numeric_value_p_n/3.

% Each goal associated with a transparent-declared predicate will inherit the context module from its parent goal.
:- module_transparent numeric_value_p_n/3.

%!  numeric_value_p_n(+Predicate, +ColumnIndex, +ColumnName) is det.
%
%   Specifies columns within various predicates that are expected to hold numeric values.
%   Each fact associates a predicate (representing a database table), a column index,
%   and the column name for clarity. This mapping aids in identifying which columns 
%   store numerical data across the tables.
%
%   @arg Predicate The name of the table or predicate being queried.
%   @arg ColumnIndex The 1-based index of the column within the table.
%   @arg ColumnName The name of the column expected to hold numeric data.
%
%   @examples
%     % Check if a column in a table holds numeric values:
%     ?- numeric_value_p_n(dmel_human_orthologs_disease, 6, 'DIOPT_score').
%     true.
%
%     % List all columns in the 'gene_rpkm_matrix' table that store numeric data:
%     ?- numeric_value_p_n(gene_rpkm_matrix, Index, Name).
%     Index = 5,
%     Name = 'BCM_1_E2-4hr_(FBlc0000061)' ;
%     Index = 6,
%     Name = 'BCM_1_E14-16hr_(FBlc0000062)' ;
%     ... (additional results).
%
numeric_value_p_n(dmel_human_orthologs_disease, 6, 'DIOPT_score').
numeric_value_p_n(dmel_human_orthologs_disease, 7, 'OMIM_Phenotype_IDs').
numeric_value_p_n(fbrf_pmid_pmcid_doi, 2, 'PMID').
numeric_value_p_n(pmid_fbgn_uniprot, 2, gene_symbol).
numeric_value_p_n(fbgn_NAseq_Uniprot, 7, 'EntrezGene').
numeric_value_p_n(gene_groups_HGNC, 4, 'Parent_FB_group').
numeric_value_p_n(gene_rpkm_matrix, 5, 'BCM_1_E2-4hr_(FBlc0000061)').
numeric_value_p_n(gene_rpkm_matrix, 6, 'BCM_1_E14-16hr_(FBlc0000062)').
numeric_value_p_n(gene_rpkm_matrix, 7, 'BCM_1_E2-16hr_(FBlc0000063)').
numeric_value_p_n(gene_rpkm_matrix, 8, 'BCM_1_E2-16hr100_(FBlc0000064)').
numeric_value_p_n(gene_rpkm_matrix, 9, 'BCM_1_L3i_(FBlc0000065)').
numeric_value_p_n(gene_rpkm_matrix, 10, 'BCM_1_L3i100_(FBlc0000066)').
numeric_value_p_n(gene_rpkm_matrix, 11, 'BCM_1_P3d_(FBlc0000067)').
numeric_value_p_n(gene_rpkm_matrix, 12, 'BCM_1_FA3d_(FBlc0000068)').
numeric_value_p_n(gene_rpkm_matrix, 13, 'BCM_1_MA3d_(FBlc0000069)').
numeric_value_p_n(gene_rpkm_matrix, 14, 'BCM_1_P_(FBlc0000070)').
numeric_value_p_n(gene_rpkm_matrix, 15, 'BCM_1_L_(FBlc0000071)').
numeric_value_p_n(gene_rpkm_matrix, 16, 'BCM_1_A17d_(FBlc0000072)').
numeric_value_p_n(gene_rpkm_matrix, 17, 'mE_mRNA_em0-2hr_(FBlc0000086)').
numeric_value_p_n(gene_rpkm_matrix, 18, 'mE_mRNA_em2-4hr_(FBlc0000087)').
numeric_value_p_n(gene_rpkm_matrix, 19, 'mE_mRNA_em4-6hr_(FBlc0000088)').
numeric_value_p_n(gene_rpkm_matrix, 20, 'mE_mRNA_em6-8hr_(FBlc0000089)').
numeric_value_p_n(gene_rpkm_matrix, 21, 'mE_mRNA_em8-10hr_(FBlc0000090)').
numeric_value_p_n(gene_rpkm_matrix, 22, 'mE_mRNA_em10-12hr_(FBlc0000091)').
numeric_value_p_n(gene_rpkm_matrix, 23, 'mE_mRNA_em12-14hr_(FBlc0000092)').
numeric_value_p_n(gene_rpkm_matrix, 24, 'mE_mRNA_em14-16hr_(FBlc0000093)').
numeric_value_p_n(gene_rpkm_matrix, 25, 'mE_mRNA_em16-18hr_(FBlc0000094)').
numeric_value_p_n(gene_rpkm_matrix, 26, 'mE_mRNA_em18-20hr_(FBlc0000095)').
numeric_value_p_n(gene_rpkm_matrix, 27, 'mE_mRNA_em20-22hr_(FBlc0000096)').
numeric_value_p_n(gene_rpkm_matrix, 28, 'mE_mRNA_em22-24hr_(FBlc0000097)').
numeric_value_p_n(gene_rpkm_matrix, 29, 'mE_mRNA_L1_(FBlc0000098)').
numeric_value_p_n(gene_rpkm_matrix, 30, 'mE_mRNA_L2_(FBlc0000099)').
numeric_value_p_n(gene_rpkm_matrix, 31, 'mE_mRNA_L3_12hr_(FBlc0000100)').
numeric_value_p_n(gene_rpkm_matrix, 32, 'mE_mRNA_L3_PS1-2_(FBlc0000101)').
numeric_value_p_n(gene_rpkm_matrix, 33, 'mE_mRNA_L3_PS3-6_(FBlc0000102)').
numeric_value_p_n(gene_rpkm_matrix, 34, 'mE_mRNA_L3_PS7-9_(FBlc0000103)').
numeric_value_p_n(gene_rpkm_matrix, 35, 'mE_mRNA_WPP_(FBlc0000104)').
numeric_value_p_n(gene_rpkm_matrix, 36, 'mE_mRNA_P5_(FBlc0000105)').
numeric_value_p_n(gene_rpkm_matrix, 37, 'mE_mRNA_P6_(FBlc0000106)').
numeric_value_p_n(gene_rpkm_matrix, 38, 'mE_mRNA_P8_(FBlc0000107)').
numeric_value_p_n(gene_rpkm_matrix, 39, 'mE_mRNA_P9-10_(FBlc0000108)').
numeric_value_p_n(gene_rpkm_matrix, 40, 'mE_mRNA_P15_(FBlc0000109)').
numeric_value_p_n(gene_rpkm_matrix, 41, 'mE_mRNA_AdF_Ecl_1days_(FBlc0000110)').
numeric_value_p_n(gene_rpkm_matrix, 42, 'mE_mRNA_AdF_Ecl_5days_(FBlc0000111)').
numeric_value_p_n(gene_rpkm_matrix, 43, 'mE_mRNA_AdF_Ecl_30days_(FBlc0000112)').
numeric_value_p_n(gene_rpkm_matrix, 44, 'mE_mRNA_AdM_Ecl_1days_(FBlc0000113)').
numeric_value_p_n(gene_rpkm_matrix, 45, 'mE_mRNA_AdM_Ecl_5days_(FBlc0000114)').
numeric_value_p_n(gene_rpkm_matrix, 46, 'mE_mRNA_AdM_Ecl_30days_(FBlc0000115)').
numeric_value_p_n(gene_rpkm_matrix, 47, 'mE_mRNA_A_MateF_1d_head_(FBlc0000207)').
numeric_value_p_n(gene_rpkm_matrix, 48, 'mE_mRNA_A_MateF_4d_ovary_(FBlc0000208)').
numeric_value_p_n(gene_rpkm_matrix, 49, 'mE_mRNA_A_MateM_1d_head_(FBlc0000209)').
numeric_value_p_n(gene_rpkm_matrix, 50, 'mE_mRNA_A_VirF_1d_head_(FBlc0000210)').
numeric_value_p_n(gene_rpkm_matrix, 51, 'mE_mRNA_A_VirF_4d_head_(FBlc0000211)').
numeric_value_p_n(gene_rpkm_matrix, 52, 'mE_mRNA_A_MateF_20d_head_(FBlc0000212)').
numeric_value_p_n(gene_rpkm_matrix, 53, 'mE_mRNA_A_MateF_4d_head_(FBlc0000213)').
numeric_value_p_n(gene_rpkm_matrix, 54, 'mE_mRNA_A_MateM_20d_head_(FBlc0000214)').
numeric_value_p_n(gene_rpkm_matrix, 55, 'mE_mRNA_A_MateM_4d_acc_gland_(FBlc0000215)').
numeric_value_p_n(gene_rpkm_matrix, 56, 'mE_mRNA_A_MateM_4d_head_(FBlc0000216)').
numeric_value_p_n(gene_rpkm_matrix, 57, 'mE_mRNA_A_MateM_4d_testis_(FBlc0000217)').
numeric_value_p_n(gene_rpkm_matrix, 58, 'mE_mRNA_A_1d_carcass_(FBlc0000218)').
numeric_value_p_n(gene_rpkm_matrix, 59, 'mE_mRNA_A_1d_dig_sys_(FBlc0000219)').
numeric_value_p_n(gene_rpkm_matrix, 60, 'mE_mRNA_A_20d_carcass_(FBlc0000220)').
numeric_value_p_n(gene_rpkm_matrix, 61, 'mE_mRNA_A_20d_dig_sys_(FBlc0000221)').
numeric_value_p_n(gene_rpkm_matrix, 62, 'mE_mRNA_A_4d_carcass_(FBlc0000222)').
numeric_value_p_n(gene_rpkm_matrix, 63, 'mE_mRNA_A_4d_dig_sys_(FBlc0000223)').
numeric_value_p_n(gene_rpkm_matrix, 64, 'mE_mRNA_P8_CNS_(FBlc0000224)').
numeric_value_p_n(gene_rpkm_matrix, 65, 'mE_mRNA_L3_CNS_(FBlc0000225)').
numeric_value_p_n(gene_rpkm_matrix, 66, 'mE_mRNA_L3_Wand_carcass_(FBlc0000226)').
numeric_value_p_n(gene_rpkm_matrix, 67, 'mE_mRNA_L3_Wand_dig_sys_(FBlc0000227)').
numeric_value_p_n(gene_rpkm_matrix, 68, 'mE_mRNA_L3_Wand_fat_(FBlc0000228)').
numeric_value_p_n(gene_rpkm_matrix, 69, 'mE_mRNA_L3_Wand_imag_disc_(FBlc0000229)').
numeric_value_p_n(gene_rpkm_matrix, 70, 'mE_mRNA_L3_Wand_saliv_(FBlc0000230)').
numeric_value_p_n(gene_rpkm_matrix, 71, 'mE_mRNA_A_VirF_20d_head_(FBlc0000231)').
numeric_value_p_n(gene_rpkm_matrix, 72, 'mE_mRNA_A_VirF_4d_ovary_(FBlc0000232)').
numeric_value_p_n(gene_rpkm_matrix, 73, 'mE_mRNA_WPP_fat_(FBlc0000233)').
numeric_value_p_n(gene_rpkm_matrix, 74, 'mE_mRNA_WPP_saliv_(FBlc0000234)').
numeric_value_p_n(gene_rpkm_matrix, 75, 'mE_mRNA_P8_fat_(FBlc0000235)').
numeric_value_p_n(gene_rpkm_matrix, 76, 'mE_mRNA_A_4d_Cold1_(FBlc0000237)').
numeric_value_p_n(gene_rpkm_matrix, 77, 'mE_mRNA_A_4d_Cold2_(FBlc0000238)').
numeric_value_p_n(gene_rpkm_matrix, 78, 'mE_mRNA_L3_Cu_0.5mM_(FBlc0000239)').
numeric_value_p_n(gene_rpkm_matrix, 79, 'mE_mRNA_L3_late_Zn_5mM_(FBlc0000240)').
numeric_value_p_n(gene_rpkm_matrix, 80, 'mE_mRNA_A_4d_Cu_15mM_(FBlc0000241)').
numeric_value_p_n(gene_rpkm_matrix, 81, 'mE_mRNA_A_4d_Zn_4.5mM_(FBlc0000242)').
numeric_value_p_n(gene_rpkm_matrix, 82, 'mE_mRNA_A_4d_Caffeine_25mg/ml_(FBlc0000243)').
numeric_value_p_n(gene_rpkm_matrix, 83, 'mE_mRNA_A_4d_Caffeine_2.5mg/ml_(FBlc0000244)').
numeric_value_p_n(gene_rpkm_matrix, 84, 'mE_mRNA_L3_Caffeine_1.5mg/ml_(FBlc0000245)').
numeric_value_p_n(gene_rpkm_matrix, 85, 'mE_mRNA_A_4d_Cd_0.1M_(FBlc0000246)').
numeric_value_p_n(gene_rpkm_matrix, 86, 'mE_mRNA_A_4d_Cd_0.05M_(FBlc0000247)').
numeric_value_p_n(gene_rpkm_matrix, 87, 'mE_mRNA_L3_Cd_12h_(FBlc0000248)').
numeric_value_p_n(gene_rpkm_matrix, 88, 'mE_mRNA_L3_Cd_6hr_(FBlc0000249)').
numeric_value_p_n(gene_rpkm_matrix, 89, 'mE_mRNA_A_4d_Paraquat_5mM_(FBlc0000250)').
numeric_value_p_n(gene_rpkm_matrix, 90, 'mE_mRNA_A_4d_Paraquat_10mM_(FBlc0000251)').
numeric_value_p_n(gene_rpkm_matrix, 91, 'mE_mRNA_L3_Rotenone_8ug_(FBlc0000252)').
numeric_value_p_n(gene_rpkm_matrix, 92, 'mE_mRNA_L3_Rotenone_2ug_(FBlc0000253)').
numeric_value_p_n(gene_rpkm_matrix, 93, 'mE_mRNA_L3_EtOH_10_(FBlc0000254)').
numeric_value_p_n(gene_rpkm_matrix, 94, 'mE_mRNA_L3_EtOH_5_(FBlc0000255)').
numeric_value_p_n(gene_rpkm_matrix, 95, 'mE_mRNA_L3_EtOH_2.5_(FBlc0000256)').
numeric_value_p_n(gene_rpkm_matrix, 96, 'mE_mRNA_A_4d_Heatshock_(FBlc0000257)').
numeric_value_p_n(gene_rpkm_matrix, 97, 'mE_mRNA_A_10d_Resveratrol_100uM_(FBlc0000672)').
numeric_value_p_n(gene_rpkm_matrix, 98, 'mE_mRNA_A_10d_Rotenone_Starved_(FBlc0000673)').
numeric_value_p_n(gene_rpkm_matrix, 99, 'mE_mRNA_F_Sindbis_virus_(FBlc0000674)').
numeric_value_p_n(gene_rpkm_matrix, 100, 'mE_mRNA_L_Sindbis_virus_(FBlc0000675)').
numeric_value_p_n(gene_rpkm_matrix, 101, 'mE_mRNA_M_Sindbis_virus_(FBlc0000676)').
numeric_value_p_n(gene_rpkm_matrix, 102, 'mE_mRNA_P_Sindbis_virus_(FBlc0000677)').
numeric_value_p_n(gene_rpkm_matrix, 103, 'mE_mRNA_CME-W2_cells_(FBlc0000261)').
numeric_value_p_n(gene_rpkm_matrix, 104, 'mE_mRNA_GM2_cells_(FBlc0000262)').
numeric_value_p_n(gene_rpkm_matrix, 105, 'mE_mRNA_mbn2_cells_(FBlc0000263)').
numeric_value_p_n(gene_rpkm_matrix, 106, 'mE_mRNA_BG2-c2_cells_(FBlc0000264)').
numeric_value_p_n(gene_rpkm_matrix, 107, 'mE_mRNA_D20-c5_cells_(FBlc0000265)').
numeric_value_p_n(gene_rpkm_matrix, 108, 'mE_mRNA_S3_cells_(FBlc0000266)').
numeric_value_p_n(gene_rpkm_matrix, 109, 'mE_mRNA_1182-4H_cells_(FBlc0000267)').
numeric_value_p_n(gene_rpkm_matrix, 110, 'mE_mRNA_CME_L1_cells_(FBlc0000268)').
numeric_value_p_n(gene_rpkm_matrix, 111, 'mE_mRNA_Kc167_cells_(FBlc0000269)').
numeric_value_p_n(gene_rpkm_matrix, 112, 'mE_mRNA_BG1-c1_cells_(FBlc0000270)').
numeric_value_p_n(gene_rpkm_matrix, 113, 'mE_mRNA_D11_cells_(FBlc0000271)').
numeric_value_p_n(gene_rpkm_matrix, 114, 'mE_mRNA_D16-c3_cells_(FBlc0000272)').
numeric_value_p_n(gene_rpkm_matrix, 115, 'mE_mRNA_D17-c3_cells_(FBlc0000273)').
numeric_value_p_n(gene_rpkm_matrix, 116, 'mE_mRNA_D21_cells_(FBlc0000274)').
numeric_value_p_n(gene_rpkm_matrix, 117, 'mE_mRNA_D32_cells_(FBlc0000275)').
numeric_value_p_n(gene_rpkm_matrix, 118, 'mE_mRNA_D4-c1_cells_(FBlc0000276)').
numeric_value_p_n(gene_rpkm_matrix, 119, 'mE_mRNA_D8_cells_(FBlc0000277)').
numeric_value_p_n(gene_rpkm_matrix, 120, 'mE_mRNA_D9_cells_(FBlc0000278)').
numeric_value_p_n(gene_rpkm_matrix, 121, 'mE_mRNA_S1_cells_(FBlc0000279)').
numeric_value_p_n(gene_rpkm_matrix, 122, 'mE_mRNA_S2R+_cells_(FBlc0000280)').
numeric_value_p_n(gene_rpkm_matrix, 123, 'mE_mRNA_Sg4_cells_(FBlc0000281)').
numeric_value_p_n(gene_rpkm_matrix, 124, 'mE_mRNA_OSS_cells_(FBlc0000282)').
numeric_value_p_n(gene_rpkm_matrix, 125, 'mE_mRNA_OSC_cells_(FBlc0000283)').
numeric_value_p_n(gene_rpkm_matrix, 126, 'mE_mRNA_fGS_cells_(FBlc0000284)').
numeric_value_p_n(gene_rpkm_matrix, 127, 'Knoblich_mRNA_L3_CNS_neuroblast_(FBlc0000505)').
numeric_value_p_n(gene_rpkm_matrix, 128, 'Knoblich_mRNA_L3_CNS_neuron_(FBlc0000506)').
numeric_value_p_n(gene_rpkm_matrix, 129, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Brain_(FBlc0003619)').
numeric_value_p_n(gene_rpkm_matrix, 130, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Crop_(FBlc0003620)').
numeric_value_p_n(gene_rpkm_matrix, 131, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Carcass_(FBlc0003621)').
numeric_value_p_n(gene_rpkm_matrix, 132, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Eye_(FBlc0003622)').
numeric_value_p_n(gene_rpkm_matrix, 133, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_FatBody_(FBlc0003623)').
numeric_value_p_n(gene_rpkm_matrix, 134, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Head_(FBlc0003624)').
numeric_value_p_n(gene_rpkm_matrix, 135, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Hindgut_(FBlc0003625)').
numeric_value_p_n(gene_rpkm_matrix, 136, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Midgut_(FBlc0003626)').
numeric_value_p_n(gene_rpkm_matrix, 137, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Ovary_(FBlc0003627)').
numeric_value_p_n(gene_rpkm_matrix, 138, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_RectalPad_(FBlc0003628)').
numeric_value_p_n(gene_rpkm_matrix, 139, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_SalivaryGland_(FBlc0003629)').
numeric_value_p_n(gene_rpkm_matrix, 140, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_ThoracicoAbdominalGanglion_(FBlc0003630)').
numeric_value_p_n(gene_rpkm_matrix, 141, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_MalpighianTubule_(FBlc0003631)').
numeric_value_p_n(gene_rpkm_matrix, 142, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Mated_Spermathecum_(FBlc0003632)').
numeric_value_p_n(gene_rpkm_matrix, 143, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Virgin_Spermathecum_(FBlc0003633)').
numeric_value_p_n(gene_rpkm_matrix, 144, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Whole_(FBlc0003634)').
numeric_value_p_n(gene_rpkm_matrix, 145, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Brain_(FBlc0003635)').
numeric_value_p_n(gene_rpkm_matrix, 146, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Crop_(FBlc0003636)').
numeric_value_p_n(gene_rpkm_matrix, 147, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Carcass_(FBlc0003637)').
numeric_value_p_n(gene_rpkm_matrix, 148, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Eye_(FBlc0003638)').
numeric_value_p_n(gene_rpkm_matrix, 149, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_FatBody_(FBlc0003639)').
numeric_value_p_n(gene_rpkm_matrix, 150, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Head_(FBlc0003640)').
numeric_value_p_n(gene_rpkm_matrix, 151, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Hindgut_(FBlc0003641)').
numeric_value_p_n(gene_rpkm_matrix, 152, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Midgut_(FBlc0003642)').
numeric_value_p_n(gene_rpkm_matrix, 153, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_RectalPad_(FBlc0003643)').
numeric_value_p_n(gene_rpkm_matrix, 154, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_SalivaryGland_(FBlc0003644)').
numeric_value_p_n(gene_rpkm_matrix, 155, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_ThoracicoAbdominalGanglion_(FBlc0003645)').
numeric_value_p_n(gene_rpkm_matrix, 156, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_MalpighianTubule_(FBlc0003646)').
numeric_value_p_n(gene_rpkm_matrix, 157, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Testis_(FBlc0003647)').
numeric_value_p_n(gene_rpkm_matrix, 158, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_AccessoryGland_(FBlc0003648)').
numeric_value_p_n(gene_rpkm_matrix, 159, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Whole_(FBlc0003649)').
numeric_value_p_n(gene_rpkm_matrix, 160, 'RNA-Seq_Profile_FlyAtlas2_L3_CNS_(FBlc0003650)').
numeric_value_p_n(gene_rpkm_matrix, 161, 'RNA-Seq_Profile_FlyAtlas2_L3_FatBody_(FBlc0003651)').
numeric_value_p_n(gene_rpkm_matrix, 162, 'RNA-Seq_Profile_FlyAtlas2_L3_Hindgut_(FBlc0003652)').
numeric_value_p_n(gene_rpkm_matrix, 163, 'RNA-Seq_Profile_FlyAtlas2_L3_MalpighianTubule_(FBlc0003653)').
numeric_value_p_n(gene_rpkm_matrix, 164, 'RNA-Seq_Profile_FlyAtlas2_L3_Midgut_(FBlc0003654)').
numeric_value_p_n(gene_rpkm_matrix, 165, 'RNA-Seq_Profile_FlyAtlas2_L3_SalivaryGland_(FBlc0003655)').
numeric_value_p_n(gene_rpkm_matrix, 166, 'RNA-Seq_Profile_FlyAtlas2_L3_Trachea_(FBlc0003656)').
numeric_value_p_n(gene_rpkm_matrix, 167, 'RNA-Seq_Profile_FlyAtlas2_L3_Carcass_(FBlc0003657)').
numeric_value_p_n(gene_rpkm_matrix, 168, 'RNA-Seq_Profile_FlyAtlas2_L3_Whole_(FBlc0003658)').
numeric_value_p_n(gene_rpkm_matrix, 169, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Heart_(FBlc0003724)').
numeric_value_p_n(gene_rpkm_matrix, 170, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Heart_(FBlc0003725)').
numeric_value_p_n(gene_rpkm_report, 8, 'RPKM_value').
numeric_value_p_n(gene_rpkm_report, 9, 'Bin_value').
numeric_value_p_n(gene_rpkm_report, 10, 'Unique_exon_base_count').
numeric_value_p_n(gene_rpkm_report, 11, 'Total_exon_base_count').
numeric_value_p_n(gene_snapshots, 4, datestamp).
numeric_value_p_n(insertion_mapping, 5, orientation).
numeric_value_p_n(dmel_paralogs, 5, 'Strand').
numeric_value_p_n(dmel_paralogs, 10, 'Paralog_Strand').
numeric_value_p_n(dmel_paralogs, 11, 'DIOPT_score').
numeric_value_p_n(entity_publication, 4, 'PubMed').
numeric_value_p_n(organism_list, 5, 'NCBI_taxon').


%!  ncRNA_genes_fb_scheme(+Schema) is det.
%
%   Represents the JSON schema for RNAcentral ncRNA objects, used to describe
%   non-coding RNA sequences. The schema outlines the required structure and 
%   additional properties associated with the ncRNA entities.
%
%   @arg Schema A string containing the JSON schema definition for ncRNA objects.
%
%   @details
%   - The schema adheres to Draft 4 of the JSON Schema standard.
%   - It describes the properties required for an ncRNA object, including primary IDs,
%     taxon information, sequence data, and cross-references.
%   - Properties like localization, secondary structure, and sequence features are also supported.
%   - This schema is typically used for importing ncRNA sequences into databases
%     like RNAcentral and ensures uniformity in data representation.
%
%
%   @see JSON Schema Draft 4 (http://json-schema.org/draft-04/schema#)
ncRNA_genes_fb_scheme(
'
{
        "$schema": "http://json-schema.org/draft-04/schema#",
        "title": "RNAcentral ncRNA object",
        "description": "A sequence to import to RNAcentral. These entries must not be pseudogenes or coding RNA transcripts.",
        "id": "ncrna.json#",
        "type": "object",
        "required": ["primaryId", "taxonId", "soTermId", "sequence", "url"],
        "additionalProperties": false,
        "properties": {
                "primaryId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description": "The prefixed primary (MOD) ID for an entity. For internal use, e.g. FLYBASE:FBgn0003301, MGI:87917."
                },
                "taxonId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description" : "The NCBI taxonId for the species of the gene entity."
                },
                "soTermId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description": "The SO Term that represents as best we can, the bioType, or featureType of the object in the file."
                },
                "ecoTermId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description": "The ECO Term that represents the evidence for this sequence having the given SO term."
                },
                "sequence": {
                        "$ref": "sequence.json#properties/sequence",
                        "description": "The DNA sequence of this entry."
                },
                "name": {
                        "type": "string",
                        "description": "The name of this sequence."
                },
                "description": {
                        "type": "string",
                        "description": "An informative human readable description. If not provided then it will be generated using the name and species information."
                },
                "symbol": {
                        "type": "string",
                        "description": "The symbol for this ncRNA"
                },
                "symbolSynonyms": {
                        "type": "array",
                        "items": {
                                "type": "string"
                        },
                        "uniqueItems": true,
                        "description": "A list of other symbols for the ncRNA"
                },
                "version": {
                        "description": "The version of this sequence, if any"
                },
                "gene": {
                        "$ref": "gene.json#"
                },
                "crossReferenceIds": {
                        "description":"Collection holding a limited set (for now) of database cross references for each gene.  That set is defined in geneCrossReferences.json dataSoruce enumeration.  NCBI GENE means just the NCBI Gene reference.  UniProtKB is swissprot and trembl.  Ensembl is just the GENE id for now (not transcript nor protein).",
                        "type": "array",
                        "items": {
                                "$ref" : "global-id.json#/properties/globalId"
                        },
                        "uniqueItems": true
                },
                "genomeLocations": {
                        "description":"Collection holding the set of locations for this sequence. This should include the exon/intron structure of the sequence.",
                        "type": "array",
                        "items": {
                                "$ref" : "location.json#"
                        },
                        "uniqueItems": true
                },
                "url": {
                        "type": "string",
                        "format": "uri",
                        "description": "URL to the page for this entry in the database"
                },
                "externalUrl": {
                        "type": "string",
                        "format": "uri",
                        "description": "URL to a synopsis outside the database, like wikipedia."
                },
                "secondaryStructure": {
                        "description": "The secondary structure in dot-bracket notation for this sequence",
                        "$ref": "secondary-structure.json#/properties/dot_bracket"
                },
                "publications": {
                        "description": "List of references to associate with this sequence.",
                        "type": "array",
                        "items": {
                                "anyOf": [
                                        { "$ref": "publications.json#/properties/pubMedId" },
                                        { "$ref": "publications.json#/properties/doi" }
                                ]
                        },
                        "uniqueItems": true
                },
                "localization": {
                        "description": "Name of the organelle or plasmid this sequence is found in",
                        "enum": [
                                "mitochondria",
                                "plastid",
                                "chloroplast",
                                "plasmid"
                        ]
                },
                "sequenceFeatures": {
                        "anticodon": {
                                "description": "Information about the anticodon",
                                "$ref": "anticodon.json#"
                        },
                        "modifications": {
                                "description": "List of the modifications in this sequence",
                                "type": "array",
                                "items": {
                                        "$ref": "modification.json#"
                                },
                                "uniqueItems": true
                        }
                },
                "sourceModel": {
                        "description": "Information about the model used to find this sequence",
                        "$ref" : "global-id.json#/properties/globalId"
                },
                "product": {
                        "description": "The name of the product, this should be more specific than the name of the SO term that was provided",
                        "type": "string"
                },
                "additionalAnnotations": {
                        "isoType": {
                                "description": "The isoType of this tRNA",
                                "enum": [
                                        "Ala",
                                        "Cys",
                                        "Asp",
                                        "Glu",
                                        "Phe",
                                        "Gly",
                                        "His",
                                        "Ile",
                                        "Lys",
                                        "Leu",
                                        "iMet",
                                        "Met",
                                        "Asn",
                                        "Pro",
                                        "Gln",
                                        "Arg",
                                        "Ser",
                                        "Thr",
                                        "Val",
                                        "Trp",
                                        "Tyr"
                                ]
                        }
                },
                "relatedSequences": {
                        "description": "Information about related sequences, like precursors or targets",
                        "type": "array",
                        "items": {
                                "$ref": "related-sequence.json#"
                        },
                        "uniqueItems": true
                },
                "inferredPhylogeny": {
                        "description": "A phylogeny that has been inferred for this sequence",
                        "$ref": "inferred-phylogeny.json#"
                }
        }
}').

%!  ucn(+Table, +Columns) is det.
%
%   Represents user-customized column numbering (UCN) for various database tables.
%   This predicate specifies either the columns that require special processing
%   or a list of all columns for tables where customization is not needed.
%
%   @arg Table The name of the table or dataset (predicate) being referenced.
%   @arg Columns The specific columns or an empty list `[]` if no customization applies.
%
%   @examples
%     % Retrieve the UCN for a table:
%     ?- ucn(dmel_unique_protein_isoforms, Columns).
%     Columns = 3.
%
%     % Check if a table requires UCN processing:
%     ?- ucn(fbal_to_fbgn, Columns).
%     Columns = 1.
%
%     % Retrieve all tables without UCN customization:
%     ?- ucn(Table, []).
%     Table = allele_genetic_interactions ;
%     Table = dataset_metadata ;
%     ... (other results).
%
ucn(allele_genetic_interactions,[]).
ucn(dataset_metadata,[]).
ucn(disease_model_annotations,[]).
ucn('Dmel_enzyme',[]).
ucn(dmel_gene_sequence_ontology_annotations,[]).
ucn(dmel_human_orthologs_disease,[]).
ucn(dmel_paralogs,[]).
ucn(entity_publication,[]).
ucn(fbgn_gleanr,[]).
ucn(fbgn_NAseq_Uniprot,[]).
ucn(fbgn_uniprot,[]).
ucn(gene_functional_complementation,[]).
ucn(gene_genetic_interactions,[]).
ucn(gene_group,[]).
ucn(gene_groups_HGNC,[]).
ucn(genotype_phenotype,[]).
ucn(insertion_mapping,[]).
ucn(organism_list,[]).
ucn(pathway_group,[]).
ucn(pmid_fbgn_uniprot,[]).
ucn('scRNA-Seq_gene_expression',[]).
ucn(dmel_unique_protein_isoforms, 3). % ,a-PA,))
ucn(best_gene_summary, 1). % ,FBgn0031081,) (2 ,Nep3,))
ucn(cDNA_clone_data, 1). % ,FBcl0000001,) (3 ,UUGC0315,))
ucn(fb_synonym, 1). % ,FBal0000001,))
ucn(fbal_to_fbgn, 1). % ,FBal0137236,))
ucn(fbgn_annotation_ID.tsv, 1). % ,7SLRNA:CR32864,) (3 ,FBgn0000003,) (5 ,CR32864,))
ucn(fbgn_annotation_ID, 1). % ,7SLRNA:CR32864,) (3 ,FBgn0000003,) (5 ,CR32864,))
ucn(fbgn_fbtr_fbpp_expanded, 8). % ,FBtr0081624,) (9 ,7SLRNA:CR32864-RA,))
ucn(fbgn_fbtr_fbpp, 2). % ,FBtr0081624,))
ucn(fbrf_pmid_pmcid_doi, 1). % ,FBrf0026179,) (2 ,37280885,))
ucn(gene_map_table, 2). % ,snRNA:4.5S,) (3 ,FBgn0000001,))
ucn(gene_rpkm_matrix, 1). % ,FBgn0031081,) (2 ,Nep3,))
ucn(gene_rpkm_report,[]).
ucn(gene_snapshots, 1). % ,FBgn0052532,) (2 ,CG32532,))
ucn('genome-cyto-seq', 1). % ,21B2,) (2 ,98620,) (3 ,134010,))
ucn('cyto-genetic-seq', 1). % ,1A,))
ucn(genomic_clone, 1). % ,FBcl0297251,) (3 ,BACR13J02,))
ucn(stocks ,1). % ,FBst,))
ucn(physical_interactions_mitab, 14). %,flybase:FBrf0218395-7641.DPiM,))

%!  list_column_names is det.
%
%   Lists column names for all tables where:
%     - The table has at least two columns.
%     - The table predicate (`fb_pred_nr/2`) matches the length of the column names.
%
%   This predicate uses the `column_names/2` and `fb_pred_nr/2` predicates to retrieve
%   and validate the column names for each table. It then prints the results to the console.
%
%   @details
%     - `column_names(T, CNs)` retrieves the column names (`CNs`) for a table (`T`).
%     - The length of the column names list is validated to ensure it matches the
%       predicate's arity as specified by `fb_pred_nr(T, Len)`.
%     - The predicate is deterministic and prints the results to standard output.
%
%   @examples
%     % Assuming column_names and fb_pred_nr are properly defined:
%     ?- list_column_names.
%     column_names(allele_genetic_interactions, [allele_symbol, allele_FBal, interaction, 'FBrf'])
%     column_names(disease_model_annotations, ['FBgn', 'Gene_symbol', 'HGNC', ...])
%     ...
%
%   @see Relies on `column_names/2` and `fb_pred_nr/2` for data retrieval and validation.

list_column_names :-
    for_all(
        (   % Retrieve column names and ensure they meet the criteria.
            column_names(T, CNs),
            once((length(CNs, Len), Len >= 2, fb_pred_nr(T, Len)))),
        (   % Print the table and its column names.
            print(column_names(T, CNs)), nl)).

%:- ensure_loaded(read_obo).

%!  show_cmds is det.
%
%   Displays commands related to the current Prolog source file.
%   This predicate identifies all zero-arity predicates (`F/0`) defined in the current
%   source file and outputs them in a formatted manner.
%
%   - `prolog_load_context(source, This)` retrieves the current source file being loaded.
%   - `source_file(P0, This)` identifies predicates (`P0`) that belong to the current file.
%   - The `functor/3` predicate checks that the predicate has zero arity (`F/0`).
%   - For each matching predicate, the `add_history1(F)` command is printed.
%
%   @examples
%     % Assuming the current file defines `foo/0` and `bar/0`:
%     ?- show_cmds.
%     add_history1(foo)
%     add_history1(bar)
%
%   Useful for debugging or generating a list of commands for interactive use.
show_cmds :-
    % Retrieve the current source file being loaded.
    prolog_load_context(source, This),
    % Iterate over all zero-arity predicates defined in the source file.
    for_all(
        (   source_file(P0, This),
            functor(P0, F, 0)
        ),
        % Print the command for each predicate.
        writeln(add_history1(F))).

%add_history1(setup_flybase_cols)
%add_history1(pmt)

%!  ah is det.
%
%   Adds a set of predefined commands to the command history for interactive use.
%   This predicate uses both `add_history1/1` and `add_history/1` to register commands.
%
%   Details
%   - `add_history1/1` and `add_history/1` are assumed to be predicates that register
%     commands for later invocation in an interactive Prolog session.
%   - The predicate is idempotent, meaning the same commands can be added multiple times
%     without causing issues.
%
%   Commands added to the history include:
%   - `fb_stats`
%   - `mine_overlaps`
%   - `load_flybase`
%   - `try_overlaps`
%
%   @examples
%     % Register commands to the history:
%     ?- ah.
%     true.
%
%     % Verify the commands are added (assuming a history inspection command):
%     ?- history.
%     fb_stats
%     mine_overlaps
%     load_flybase
%     try_overlaps
%
%   This predicate is typically used for setting up a convenient command environment.
ah :-
    % Register commands using add_history1/1.
    add_history1(fb_stats),
    add_history1(mine_overlaps),
    add_history1(load_flybase),
    % Register commands using add_history/1.
    add_history(fb_stats),
    add_history(mine_overlaps),
    add_history(try_overlaps),
    add_history(load_flybase).
% Uncomment to execute `ah` multiple times during initialization.
% :- ah, ah, ah.

%:- initialization(load_flybase).

%!  fb_pred(+Table, +Arity) is det.
%
%   Represents the predicate definitions for various FlyBase database tables.
%
%   This predicate provides metadata about FlyBase tables, aiding in schema
%   validation, query construction, and other table-related operations.
fb_pred('allele_genetic_interactions',4).
fb_pred('automated_gene_summaries',2).
fb_pred('automated_gene_summaries',3).
fb_pred('best_gene_summary',4).
fb_pred('cDNA_clone_data',6).
fb_pred('cDNA_clone_data',5).
fb_pred('cDNA_clone_data',6).
fb_pred('cyto_genetic_seq',4).
fb_pred('cyto-genetic-seq',4).
fb_pred('dataset_metadata',4).
fb_pred('directory',2).
fb_pred('disease_model_annotations',12).
fb_pred('disease_model_annotations',9).
fb_pred('Dmel_enzyme',11).
fb_pred('Dmel_enzyme_data',11).
fb_pred('Dmel_enzyme_data',7).
fb_pred('dmel_gene_sequence_ontology_annotations',4).
fb_pred('dmel_human_orthologs_disease',8).
fb_pred('dmel_paralogs',11).
fb_pred('dmel_unique_protein_isoforms',4).
fb_pred('entity_publication',3).
fb_pred('entity_publication',4).
fb_pred('fb_synonym',4).
fb_pred('fb_synonym',6).
fb_pred('fbal_to_fbgn',4).
fb_pred('fbgn_annotation_ID',4).
fb_pred('fbgn_annotation_ID',6).
fb_pred('fbgn_exons2affy1_overlaps',2).
fb_pred('fbgn_exons2affy2_overlaps',2).
fb_pred('fbgn_fbtr_fbpp',2).
fb_pred('fbgn_fbtr_fbpp',3).
fb_pred('fbgn_fbtr_fbpp_expanded',11).
fb_pred('fbgn_fbtr_fbpp_expanded',9).
fb_pred('fbgn_gleanr',4).
fb_pred('fbgn_NAseq_Uniprot',4).
fb_pred('fbgn_NAseq_Uniprot',9).
fb_pred('fbgn_uniprot',4).
fb_pred('fbrf_pmid_pmcid_doi',2).
fb_pred('fbrf_pmid_pmcid_doi',6).
fb_pred('fbrf_pmid_pmcid_doi',7).
fb_pred('gene_functional_complementation',5).
fb_pred('gene_genetic_interactions',6).
fb_pred('gene_group',7).
fb_pred('gene_group_data',2).
fb_pred('gene_group_data',7).
fb_pred('gene_groups_HGNC',2).
fb_pred('gene_groups_HGNC',3).
fb_pred('gene_groups_HGNC',4).
fb_pred('gene_map_table',5).
fb_pred('gene_map_table',6).
fb_pred('gene_rpkm_matrix',170).
fb_pred('gene_rpkm_report',12).
fb_pred('gene_snapshots',5).
fb_pred('genome_cyto_seq',3).
fb_pred('genome-cyto-seq',3).
fb_pred('genomic_clone',4).
fb_pred('genomic_clone_data',4).
fb_pred('genotype_phenotype',7).
fb_pred('genotype_phenotype_data',5).
fb_pred('genotype_phenotype_data',7).
fb_pred('gp_information',10).
fb_pred('gp_information',8).
fb_pred('id_type',2).
fb_pred('insertion_mapping',4).
fb_pred('insertion_mapping',7).
fb_pred('obo-adjacent-to',3).
fb_pred('obo-alt-id',2).
fb_pred('obo-arity',2).
fb_pred('obo-attached-to',3).
fb_pred('obo-attached-to-part-of',3).
fb_pred('obo-auto-generated-by',2).
fb_pred('obo-basename',2).
fb_pred('obo-broadMatch',2).
fb_pred('obo-charge',2).
fb_pred('obo-ChEBI-has-part',2).
fb_pred('obo-ChEBI-has-role',2).
fb_pred('obo-CL-capable-of',2).
fb_pred('obo-CL-capable-of-part-of',2).
fb_pred('obo-CL-has-part',2).
fb_pred('obo-closeMatch',2).
fb_pred('obo-comment',2).
fb_pred('obo-composed-primarily-of',3).
fb_pred('obo-conditionality',3).
fb_pred('obo-connected-to',3).
fb_pred('obo-consider',2).
fb_pred('obo-contains',3).
fb_pred('obo-continuous-with',3).
fb_pred('obo-created-by',2).
fb_pred('obo-creation-date',2).
fb_pred('obo-data-version',2).
fb_pred('obo-date',2).
fb_pred('obo-def',3).
fb_pred('obo-def',4).
fb_pred('obo-default-namespace',2).
fb_pred('obo-derives-from',3).
fb_pred('obo-develops-directly-from',3).
fb_pred('obo-develops-from',3).
fb_pred('obo-develops-from-part-of',3).
fb_pred('obo-develops-into',3).
fb_pred('obo-directory',2).
fb_pred('obo-disjoint-from',3).
fb_pred('obo-electrically-synapsed-to',3).
fb_pred('obo-exactMatch',2).
fb_pred('obo-fasciculates-with',3).
fb_pred('obo-FBcv-namespace',2).
fb_pred('obo-format-version',2).
fb_pred('obo-formula',2).
fb_pred('obo-GO-capable-of',2).
fb_pred('obo-GO-capable-of-part-of',2).
fb_pred('obo-GO-directly-negatively-regulates',2).
fb_pred('obo-GO-directly-positively-regulates',2).
fb_pred('obo-GO-directly-regulates',2).
fb_pred('obo-GO-has-part',2).
fb_pred('obo-GO-has-participant',2).
fb_pred('obo-GO-negatively-regulates',2).
fb_pred('obo-GO-occurs-in',2).
fb_pred('obo-GO-positively-regulates',2).
fb_pred('obo-GO-regulates',2).
fb_pred('obo-GO-results-in-formation-of',2).
fb_pred('obo-GO-results-in-growth-of',2).
fb_pred('obo-guided-by',3).
fb_pred('obo-has-definition',2).
fb_pred('obo-has-fasciculating-neuron-projection',3).
fb_pred('obo-has-functional-parent',2).
fb_pred('obo-has-name',2).
fb_pred('obo-has-origin',3).
fb_pred('obo-has-parent-hydride',2).
fb_pred('obo-has-part',2).
fb_pred('obo-has-part',3).
fb_pred('obo-has-quality',3).
fb_pred('obo-has-role',2).
fb_pred('obo-has-sensory-dendrite-in',3).
fb_pred('obo-has-soma-location',3).
fb_pred('obo-has-synaptic-IO-in',3).
fb_pred('obo-has-synaptic-IO-throughout',3).
fb_pred('obo-holds-over-chain',2).
fb_pred('obo-id-type',2).
fb_pred('obo-immediately-preceded-by',3).
fb_pred('obo-inchi',2).
fb_pred('obo-inchikey',2).
fb_pred('obo-Inheritance',2).
fb_pred('obo-innervated-by',3).
fb_pred('obo-innervates',3).
fb_pred('obo-intersection-of',4).
fb_pred('obo-inverse-of',2).
fb_pred('obo-inverse-of',3).
fb_pred('obo-is-a',2).
fb_pred('obo-is-class-level',2).
fb_pred('obo-is-conjugate-acid-of',2).
fb_pred('obo-is-conjugate-base-of',2).
fb_pred('obo-is-cyclic',2).
fb_pred('obo-is-enantiomer-of',2).
fb_pred('obo-is-functional',2).
fb_pred('obo-is-inverse-functional',2).
fb_pred('obo-is-metadata-tag',2).
fb_pred('obo-is-obsolete',2).
fb_pred('obo-is-substituent-group-from',2).
fb_pred('obo-is-symmetric',2).
fb_pred('obo-is-tautomer-of',2).
fb_pred('obo-is-transitive',2).
fb_pred('obo-mass',2).
fb_pred('obo-member-of',3).
fb_pred('obo-monoisotopicmass',2).
fb_pred('obo-name',2).
fb_pred('obo-namespace',2).
fb_pred('obo-namespace-id-rule',2).
fb_pred('obo-narrowMatch',2).
fb_pred('obo-negatively-regulates',3).
fb_pred('obo-non-functional-homolog-of',3).
fb_pred('obo-OBI:9991118',2).
fb_pred('obo-ontology',2).
fb_pred('obo-overlaps',3).
fb_pred('obo-owl:versionInfo',2).
fb_pred('obo-part-of',3).
fb_pred('obo-pathname',2).
fb_pred('obo-positively-regulates',3).
fb_pred('obo-property-value',2).
fb_pred('obo-property-value',3).
fb_pred('obo-receives-input-from',3).
fb_pred('obo-receives-synaptic-input-from-neuron',3).
fb_pred('obo-receives-synaptic-input-in-region',3).
fb_pred('obo-receives-synaptic-input-throughout',3).
fb_pred('obo-regulates',3).
fb_pred('obo-relatedMatch',2).
fb_pred('obo-remark',2).
fb_pred('obo-replaced-by',2).
fb_pred('obo-RO',4).
fb_pred('obo-saved-by',2).
fb_pred('obo-sends-synaptic-output-throughout',3).
fb_pred('obo-sends-synaptic-output-to-cell',3).
fb_pred('obo-sends-synaptic-output-to-region',3).
fb_pred('obo-smiles',2).
fb_pred('obo-subset',2).
fb_pred('obo-subsetdef',3).
fb_pred('obo-substage-of',3).
fb_pred('obo-synapsed-via-type-Ib-bouton-to',3).
fb_pred('obo-synapsed-via-type-II-bouton-to',3).
fb_pred('obo-synapsed-via-type-III-bouton-to',3).
fb_pred('obo-synapsed-via-type-Is-bouton-to',3).
fb_pred('obo-synonym',3).
fb_pred('obo-synonym',4).
fb_pred('obo-synonym',5).
fb_pred('obo-synonymtypedef',3).
fb_pred('obo-synonymtypedef',4).
fb_pred('obo-transcribed-from',3).
fb_pred('obo-transcribed-to',3).
fb_pred('obo-transitive-over',3).
fb_pred('obo-variant-of',3).
fb_pred('obo-xref',2).
fb_pred('obo-xref',3).
fb_pred('obo-xref',4).
fb_pred('obo-xref-analog',2).
fb_pred('organism_list',2).
fb_pred('organism_list',6).
fb_pred('pathname',2).
fb_pred('pathway_group',7).
fb_pred('pathway_group_data',2).
fb_pred('pathway_group_data',7).
fb_pred('physical_interactions_mitab',42).
fb_pred('pmid_fbgn_uniprot',5).
fb_pred('stocks',7).
fb_pred('stocks_FB2023_05',7).
fb_pred('synonym',6).
fb_pred('transposon_sequence_set',10).
fb_pred('transposon_sequence_set',11).
fb_pred('transposon_sequence_set',12).
fb_pred('transposon_sequence_set',13).
fb_pred('transposon_sequence_set',14).
fb_pred('transposon_sequence_set',15).
fb_pred('transposon_sequence_set',17).
fb_pred('transposon_sequence_set',18).
fb_pred('transposon_sequence_set',19).
fb_pred('transposon_sequence_set',20).
fb_pred('transposon_sequence_set',21).
fb_pred('transposon_sequence_set',22).
fb_pred('transposon_sequence_set',23).
fb_pred('transposon_sequence_set',25).
fb_pred('transposon_sequence_set',26).
fb_pred('transposon_sequence_set',27).
fb_pred('transposon_sequence_set',28).
fb_pred('transposon_sequence_set',29).
fb_pred('transposon_sequence_set',3).
fb_pred('transposon_sequence_set',30).
fb_pred('transposon_sequence_set',31).
fb_pred('transposon_sequence_set',32).
fb_pred('transposon_sequence_set',33).
fb_pred('transposon_sequence_set',34).
fb_pred('transposon_sequence_set',35).
fb_pred('transposon_sequence_set',36).
fb_pred('transposon_sequence_set',37).
fb_pred('transposon_sequence_set',38).
fb_pred('transposon_sequence_set',4).
fb_pred('transposon_sequence_set',40).
fb_pred('transposon_sequence_set',41).
fb_pred('transposon_sequence_set',42).
fb_pred('transposon_sequence_set',44).
fb_pred('transposon_sequence_set',45).
fb_pred('transposon_sequence_set',46).
fb_pred('transposon_sequence_set',47).
fb_pred('transposon_sequence_set',48).
fb_pred('transposon_sequence_set',49).
fb_pred('transposon_sequence_set',5).
fb_pred('transposon_sequence_set',50).
fb_pred('transposon_sequence_set',51).
fb_pred('transposon_sequence_set',52).
fb_pred('transposon_sequence_set',53).
fb_pred('transposon_sequence_set',54).
fb_pred('transposon_sequence_set',55).
fb_pred('transposon_sequence_set',56).
fb_pred('transposon_sequence_set',57).
fb_pred('transposon_sequence_set',58).
fb_pred('transposon_sequence_set',59).
fb_pred('transposon_sequence_set',60).
fb_pred('transposon_sequence_set',61).
fb_pred('transposon_sequence_set',62).
fb_pred('transposon_sequence_set',7).
fb_pred('transposon_sequence_set',8).
fb_pred('transposon_sequence_set',9).

% ==============
% METTA COMPILER
% ==============
%:- ensure_loaded(metta_compiler).
:- ensure_loaded(library(metta_interp)).
%:- ensure_loaded(metta_python).

%!  ping_file_loaders is det.
%
%   Ensures that all external loader files matching the pattern `library/*/ext_loader_*.pl`
%   are loaded into the current Prolog environment.
%
%   This predicate performs the following steps:
%   1. Expands the file name pattern `library/*/ext_loader_*.pl` to retrieve a list of matching files.
%   2. Converts each relative file path to its absolute path using `absolute_file_name/2`.
%   3. Ensures that each file in the list is loaded using `ensure_loaded/1`.
%
%   The `:- ping_file_loaders.` directive ensures that this process is executed at
%   compile-time or startup.
%
%   @examples
%     % Load all external loader files:
%     ?- ping_file_loaders.
%     true.
%
%   This is commonly used to automatically load extensions or plugins defined
%   in external files.
ping_file_loaders :-
    % Expand the file name pattern to a list of matching files.
    expand_file_name('library/*/ext_loader_*.pl', List),
    % Convert relative paths to absolute paths.
    maplist(absolute_file_name, List, AList),
    % Ensure each file is loaded.
    maplist(ensure_loaded, AList).
% Automatically execute ping_file_loaders during initialization.
:- ping_file_loaders.
