:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- multifile(fb_pred/2).
:- discontiguous(fb_pred/2).
:- dynamic(fb_pred/2).


:- ensure_loaded(swi_support).


fb_stats:- metta_stats,!.

'&flybase':for_metta('&flybase',P):- fb_pred_nr(F,A),current_predicate(F/A),length(L,A),P=[F|L],apply(F,L).

% ==============
% OBO LOADER
% ==============
:- set_option_value(encoding,utf8).
:- ensure_loaded(flybase_obo).


:- ensure_loaded(metta_interp).


create_flybase_qlf:-
   shell('swipl -g "qcompile(whole_flybase)').

create_flybase_pl(_FtpDir):- create_flybase_pl.

create_flybase_pl:-
   all_data_once,
  % all_metta_once,
   load_flybase_full,
   all_data_done.
  % all_metta_done,
   %shell('mv whole_metta.pl whole_flybase.pl').

create_flybase_pl_tiny:-
   all_data_once,
  % all_metta_once,
   load_flybase_tiny,
   all_data_done,
  % all_metta_done,
   shell('mv whole_metta.pl tiny_flybase.pl').

save_to_pl:- tell('flybase_metta.pl'),
   forall(fb_pred(F,A),listing(F/A)),told.


real_assert(OBO):- is_converting, \+ is_loading_file(_File), !, throw(real_assert(OBO)).
real_assert(OBO):-
   ignore(real_assert1(OBO)),
   real_assert2(OBO).

%real_assert(OBO):- is_converting,!,print_src(OBO).
real_assert1(OBO):- all_metta_to(Out),!,with_output_to(Out,print_src(OBO)).
real_assert2(OBO):- all_data_to(Out),!,write_canonical(Out,OBO),!,writeln(Out,'.').
real_assert2(OBO):- is_converting,!,throw(real_assert2(OBO)).
real_assert2(OBO):- call(OBO),!.
real_assert2(OBO):- assert(OBO).

print_src(OBO):- format('~N'), uncompound(OBO,Src),!, write_srcH(Src).
write_srcH([F|Args]):- write('( '),write_src(F),maplist(write_srcE,Args),writeln(' )').
write_srcE(Arg):- write(' '),write_src(Arg).



is_loading_file(File):- nb_current(loading_file,File),File\==[],!.
is_loading_file(File):- nb_current(saving_file,File),File\==[].



all_data_once:- is_loading_file(_File),!.
all_data_once:- nb_setval(saving_file,'whole_metta'),
  all_data_to(Out),
  writeln(Out,':- encoding(utf8).'),
  writeln(Out,':- style_check(-discontiguous).'),
  flush_output(Out),!,
  all_data_preds.
all_data_preds:-
 all_data_to(Out), with_output_to(Out,all_data_preds0),!.
all_data_preds.
all_data_preds0:-
 listing_c(table_n_type/3),
  listing_c(load_state/2),
  listing_c(is_loaded_from_file_count/2),
  listing_c(fb_pred/2),
  listing_c(fb_arg_type/1),
  listing_c(fb_arg_table_n/3),
  listing_c(fb_arg/1),
  listing_c(done_reading/1),!.
:- dynamic(is_all_data_to/2).


all_data_to(Out):- once((is_all_data_to(File1,Out1),is_loading_file(File2))),File1==File2,!, Out = Out1.
all_data_to(Out):- is_all_data_to(File1,Out1),is_loading_file(File2),!,
   close(Out1), atom_concat(File2,'.metta.datalog.tmp',File2Data),
   open(File2Data,write,Out2,[alias(all_data),encoding(utf8),lock(write)]),
   retract(is_all_data_to(File1,Out1)),assert(is_all_data_to(File2,Out2)),
   fbug(all_data_to_switch(File1,File2)),!,
   Out = Out2.
all_data_to(Out):- is_all_data_to(_File1,Out),!.
all_data_to(Out):-  is_converting, is_loading_file(File2),!, atom_concat(File2,'.metta.datalog.tmp',File2Data),
   open(File2Data,write,Out2), assert(is_all_data_to(File2,Out2)),
   fbug(all_data_to(File2)),!,
   Out = Out2.

all_data_done:-
  all_data_preds,
  nb_delete(saving_file),
  all_metta_done,
  forall(retract(is_all_data_to(_,Out)), catch_ignore(close(Out))).

:- if(is_converting).
:- at_halt(all_data_done).
:- endif.

listing_c(F/A):-
   format('~N~q.~n',[:-multifile(F/A)]),
   format('~q.~n',[:-dynamic(F/A)]),
   functor(P,F,A),
   catch(forall(P,format('~q.~n',[P])),E, fbug(caused(F/A,E))).


:- dynamic(is_all_metta_to/2).
all_metta_to(Out):- once((is_all_metta_to(File1,Out1),is_loading_file(File2))),File1==File2,!, Out = Out1.
all_metta_to(Out):- is_all_metta_to(File1,Out1),is_loading_file(File2),!,
   close(Out1), atom_concat(File2,'.metta.tmp',File2Data),
   open(File2Data,write,Out2,[alias(all_metta),encoding(utf8),lock(write)]),
   retract(is_all_metta_to(File1,Out1)),assert(is_all_metta_to(File2,Out2)),
   fbug(all_metta_to_switch(File1,File2)),!,
   Out = Out2.
all_metta_to(Out):- is_all_metta_to(_File1,Out),!.
all_metta_to(Out):- is_loading_file(File2),!, atom_concat(File2,'.metta.tmp',File2Data),
   open(File2Data,write,Out2), assert(is_all_metta_to(File2,Out2)),
   fbug(all_metta_to(File2)),!,
   Out = Out2.

all_metta_done:-
    forall(retract(is_all_metta_to(_,Out)), catch_ignore(close(Out))).





loaded_from_file_count(X):- flag(loaded_from_file_count,X,X).
incr_file_count(X):- flag(loaded_from_file_count,X,X+1),  flag(total_loaded_symbols,TA,TA+1), flag(total_loaded_atoms,TA,TA+1).

should_cache:- fail, loaded_from_file_count(X), option_else(max_disk_cache,Num,1000), X=<Num.
reached_file_max:- option_value(max_per_file,Y),Y\==inf,Y\==0,loaded_from_file_count(X),X>=Y.
should_fix_args :- fail, \+ should_sample.
should_sample :- !, fail.
should_sample :- should_show_data(_),!.
should_sample :-
  once(option_value(samples_per_million,Fifty);Fifty=50), loaded_from_file_count(X), Y is X mod 1_000_000,!, Y >= 0, Y =< Fifty,!.
should_show_data(X):- loaded_from_file_count(X),!,
  once((X=<13,X>=10); (X>0,(0 is X rem 1_000_000))),
  format(user_error,'~N',[]),
  format(user_output,'~N',[]),!,
  heartbeat.
should_show_data(X):- nb_current(loading_file,F),F\==[],symbol_concat(_,'.obo',F),
  loaded_from_file_count(X),Y is X mod 100_000, Y=<15,Y>=10.


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

% FlyBase prefixes
symbol_prefix(Prefix, flybase, Desc):- flybase_identifier(Prefix, Desc).
% Some common OBO prefixes (Note: these are more generalized and not specific to FlyBase)
symbol_prefix('GO', obo, 'Gene Ontology').
symbol_prefix('PO', obo, 'Plant Ontology').
symbol_prefix('DOID', obo, 'Disease Ontology').
symbol_prefix('UBERON', obo, 'Uber-anatomy ontology').
symbol_prefix('CHEBI', obo, 'Chemical Entities of Biological Interest').


%:- abolish(gp_information/0).
:- forall(retract(fb_pred(F,0)),abolish(F/0)).
:- include(flybase_learn).

%fbd(X,P):- fb_pred(F,A),functor(P,F,A),arg(_,P,X), no_repeats(P,call(P)).
fbdead:- fb_pred_nr(F,A),functor(P,F,A),arg(_,P,xxxxxxxxxxxxxxxxx),no_repeats(P,call(P)),
 writeln(fbdead=P),fail.
fbdead.

:- use_module(library(csv)).

%:- current_prolog_flag(libswipl,_)->use_module(library(logicmoo_utils)); true.





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

recount_total_loaded_symbols:- flag(total_loaded_symbols,_,0),full_atom_count(Was),flag(total_loaded_symbols,_,Was).

% Convert flybase data from CSV to Prolog format.
load_flybase:- is_scryer,!,load_flybase_files.
load_flybase:-
  with_option(mettafiles,false,
     (make,recount_total_loaded_symbols,!,load_flybase_files,!,cleanup_arities,!,fb_stats)).
load_flybase_dirs:-
  load_flybase('./data/ftp.flybase.net/releases/current/das_precomputed/*'),
  load_flybase('./precomputed_files/*'),
  load_flybase('./data/ftp.flybase.net/releases/current/./*sv'),!.



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

load_flybase_files:-
   ftp_data(Dir),
    with_cwd(Dir,load_flybase_files_ftp).



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

 gene_sequences:-
    load_flybase('./dmel_r6.55/gff/dmel-all-r6.55.gff'),
    load_flybase('./dmel_r6.55/fasta/*.fasta'),
    load_flybase('./dmel_r6.55/gtf/*.gft'),!.


load_fbase_after_17:-
  %load_flybase('./precomputed_files/genes/scRNA-Seq_gene_expression*.tsv'),
  must_det_ll(load_flybase('./precomputed_files/transposons/transposon_sequence_set.gff*')),
  load_flybase('./precomputed_files/transposons/transposon_sequence_set.fa'),
  load_flybase('./precomputed_files/*/ncRNA_genes_fb_*.json'),
  load_obo_files,
 %% load_flybase_chado,
  !.

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

est_size_loaded(N,F/A):- fb_pred_major(F,A),metta_stats(F,A,N).

fb_pred_major(F,A):- fb_pred_m(F,A).
fb_pred_major(F,A):- fb_pred_nr(F,A),
  \+ fb_pred_m(F,A), \+ (fb_pred(F,A2),A2>A).

fb_pred_m(fbgn_exons2affy1_overlaps,2).
fb_pred_m(fbgn_exons2affy2_overlaps,2).

print_loaded_from_files:-
  findall(print_est_size(loaded_from_file,N,F),
     is_loaded_from_file_count(F,N),L),
  sort(L,S),reverse(S,R),maplist(call,R),
  print_est_sizes.

fb_info:- print_loaded_from_files,fb_stats.

fb_show:- print_loaded_from_files,fb_stats.

print_est_sizes:-
  findall(print_est_size(est_size_loaded,N,F),
     est_size_loaded(N,F),L),
  sort(L,S),reverse(S,R),maplist(call,R).

print_est_size(F,N1,S):- number(S), \+ number(N1),!,print_est_size(F,S,N1).
print_est_size(F,N1,S):- format('~N (~q ~@ ~q) ',[F,pad_number(N1,15),S]),!.

% pad_number(Number, N) pads Number with spaces to occupy N spaces in total
% and includes underscores as the thousand separator.
pad_number(Number, N) :-
  sformat(S,"~t~D~*|", [Number,N]),symbolic_list_concat(L,',',S),
  symbolic_list_concat(L,'_',SS),write(SS).


% Process a file or directory path with a given predicate.
with_wild_path(Fnicate, Dir) :- extreme_debug(fbug(with_wild_path(Fnicate, Dir))),fail.
with_wild_path(_Fnicate, []) :- !.
with_wild_path(Fnicate, Dir) :-  is_scryer, symbol(Dir), !, must_det_ll((path_chars(Dir,Chars), with_wild_path(Fnicate, Chars))).
with_wild_path(Fnicate, Chars) :-  \+ is_scryer, \+ symbol(Chars), !, must_det_ll((name(Atom,Chars), with_wild_path(Fnicate, Atom))).
with_wild_path(Fnicate, File) :- exists_file(File), !, must_det_ll(( call(Fnicate, File))).
with_wild_path(Fnicate, File) :- !, with_wild_path_swi(Fnicate, File).
with_wild_path(Fnicate, Dir) :-  exists_directory(Dir), !,
  must_det_ll((directory_files(Dir, Files),
  maplist(directory_file_path(Dir,Files),Paths),
  maplist(path_chars,Paths,CharPaths),
  maplist(with_wild_path(Fnicate), CharPaths))), !.
with_wild_path(Fnicate, File) :- is_list(File), !,  must_det_ll((maplist(with_wild_path(Fnicate), File))).
with_wild_path(Fnicate, File) :- must_det_ll((call(Fnicate, File))).

path_chars(A,C):- symbol_chars(A,C).

with_wild_path_swi(Fnicate, File) :-
  compound(File),
  absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(directory)]),
  '\\=@='(Dir, File), !,
  with_wild_path(Fnicate, Dir).
with_wild_path_swi(Fnicate, File) :-
  compound(File), !,
  absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(['csv', 'tsv', ''])]),
  '\\=@='(Dir, File), !,
  with_wild_path(Fnicate, Dir).
with_wild_path_swi(Fnicate, File) :-
  symbol_contains(File, '*'),
  expand_file_name(File, List), !,
  maplist(with_wild_path(Fnicate), List).
with_wild_path_swi(Fnicate, File) :-
  exists_directory(File),
  directory_file_path(File, '*.*sv', Wildcard),
  expand_file_name(Wildcard, List), !,
  maplist(Fnicate, List).


:- dynamic(fix_columns_nth/2).
needs_fixed(X,Y):- (var(X)->fb_arg(X);true),fix_concept(X,L),(L\=@=[X],L\=@=X),(L=[Y]->true;Y=L).
mine_args_that_need_reduced:-
  writeln('\n\n\n=====\n\n\n'),
  forall(needs_fixed(X,Y),(pp_as(needs_fixed(X->Y)),fix_columns_with_arg(X))),
  listing(fix_columns_nth).

fix_columns_with_arg(Arg):-
  forall(fb_arg_table_n(Arg,Fn,N),
    fix_columns_n(Fn,N)).
fix_columns_n(Fn,N):-
  assert_new(fix_columns_nth(Fn,N)).


load_fb_mask(Filename):- is_scryer,symbol(Filename),name(Filename,Chars),!,load_fb_mask(Chars).
load_fb_mask(Filename):- expand_file_name(Filename,Files1),maplist(load_fb_cache,Files1).
load_fb_cache(File):- with_wild_path(load_fb_cache0,File).
load_fb_cache0(RFile):-
  absolute_file_name(RFile,File),
  file_name_extension(Name,_E,File),
  symbolic_list_concat([Pub,Table],'.',Name),
  symbolic_list_concat([Pub,Table,qlf],'.',OutputFile),!,
  load_fb_cache(File,OutputFile,Table).
load_fb_cache0(File):- file_name_extension(Name,_E,File),
  symbolic_list_concat([Table],'.',Name),
  symbolic_list_concat([Table,qlf],'.',OutputFile),
  load_fb_cache(File,OutputFile,Table).

% ============================================================================
%  LOAD FB Files
% ============================================================================
track_load_into_file(RelFilename,Goal):-
 must_det_ll(absolute_file_name(RelFilename,Filename)),
 must_det_ll(track_load_into_file0(Filename,Goal)),!.

track_load_into_file0(Filename,Goal):- nb_current(tracking_file,FilenameW),  Filename==FilenameW, !,call(Goal).
track_load_into_file0(Filename,Goal):-
  must_det_ll((
  nb_setval(tracking_file,Filename),
  start_html_of(Filename),
  fbug(track_load_into_file(Filename)),
  flag(loaded_from_file_count,Was,0))),
  must_det_ll(with_option(loading_file, Filename, time(must_det_ll(Goal)))),
  must_det_ll((
  flag(loaded_from_file_count,New,Was),
  ((New>0 ; \+ is_loaded_from_file_count(Filename,_))->assert(is_loaded_from_file_count(Filename,New));true),
  fbug(Filename=New),
  rename_tmp_files(Filename),
  save_html_of(Filename))),!.

rename_tmp_files(_Filename):- \+ is_converting,!.
rename_tmp_files(Filename):- rename_tmp_files(Filename,'.metta'),rename_tmp_files(Filename,'.metta.datalog').

rename_tmp_files(Filename,NonTmp):- atomic_list_concat([Filename,NonTmp,'.tmp'],From),
   atomic_list_concat([Filename,NonTmp],To),
   fbug(rename_file(From,To)),
   ignore((exists_file(From),rename_file(From,To))).

:- dynamic(is_loaded_from_file_count/2).

:- use_module(library(http/json)).
:- ensure_loaded(flybase_json).
load_fb_json(Fn,File):- fbug(load_fb_json(Fn,File)),
  current_predicate(load_flybase_json/2),
     absolute_file_name(File,Filename),
       track_load_into_file(Filename,load_flybase_json(Fn,File)).

load_fb_json(Fn,File):- fbug(load_fb_json(Fn,File)),
 setup_call_cleanup(open(File,read,In,[encoding(utf8)]),  json:json_read(In,Term,[]), close(In)),
    time(assert(saved_fb_json(File,Term,Fn))).


% ==============================
% GFF/GTF/GFF3 Reader
% ==============================

load_fb_gff(Fn,Filename):-
 track_load_into_file(Filename,
  must_det_ll((
    fbug(load_fb_gff(Fn,Filename)),
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
    Type = 'SequenceFile',
    assert_OBO(id_type(Id,Type)),
    assert_OBO(pathname(Id,Filename)),!,
    assert_OBO(basename(Id,BaseName)),!,
    assert_OBO(directory(Id,Directory)),!,
    setup_call_cleanup(open(Filename,read,In), (repeat,load_fb_gff_read(Id,In)), close(In))))).
 % Main predicate to parse a GFF line and store it as facts
load_fb_gff_read(_Fn,In):- (at_end_of_stream(In);reached_file_max),!.
load_fb_gff_read(Fn,In):- read_line_to_string(In,Line), load_fb_gff_line(Fn,Line),!,fail.

load_fb_gff_line(Fn,Line) :- % Predicate to process a line starting with ##sequence-region
    split_string(Line, " \t", " \t", ['##sequence-region', SeqID, StartStr, EndStr]),
    atom_number(StartStr, Start), atom_number(EndStr, End),!,
    assert_MeTTa(genomic_sequence_region(Fn,SeqID,Start,End)).
load_fb_gff_line(_Fn,Line) :- split_string(Line, " \t", " \t", ['##gff-version'|_]),!.
load_fb_gff_line(_Fn,Line) :- string_concat('#', _, Line),!.
load_fb_gff_line(Fn,Line) :-
    split_string(Line, "\t", "", [SeqID, Source, Type, StartStr, EndStr, ScoreStr, Strand, Phase | MoreProps]),
    atom_number(StartStr, Start),
    atom_number(EndStr, End),
    store_gff_fact(Fn,SeqID, "source", Source),
    store_gff_fact(Fn,SeqID, "type", Type),
    store_gff_fact(Fn,SeqID, "start", Start),
    store_gff_fact(Fn,SeqID, "end", End),
    store_gff_fact(Fn,SeqID, "score", ScoreStr),
    store_gff_fact(Fn,SeqID, "strand", Strand),
    store_gff_fact(Fn,SeqID, "phase", Phase),
    parse_and_store_attributes(SeqID, MoreProps).
load_fb_gff_line(Fn,Line):- fbug(load_fb_gff_line(Fn,Line)).
% Predicate to store each field as a fact
store_gff_fact(Fn,SeqID, Key, Value) :-
    Value \= ".",
    assert_MeTTa(genomic_sequence_feature(Fn, SeqID, Key, Value)).
% Predicate to handle the attributes field
parse_and_store_attributes(Fn, SeqID, [AttributesStr | _]) :-
    split_string(AttributesStr, ";", "", AttrList),
    maplist(parse_and_store_attribute(Fn, SeqID), AttrList).
% Parse individual attribute and store it
parse_and_store_attribute(Fn, SeqID, AttrStr) :-
    (split_string(AttrStr, "=", "\"", [Key, Value])->true;split_string(AttrStr, " ", "\"", [Key | Value])),
    store_gff_fact(Fn,SeqID, Key, Value).
/*

find . \( -name "*.fa" -o -name "*.gff" -o -name "*.json" \) -execdir bash -c 'for file; do metta_pattern="${file%.*}"*metta*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Metta file does not exist for $full_path"; fi; done' bash {} \; | sort -r

find .  ! -name "*.metta" - -execdir bash -c 'for file; do metta_pattern="${file%.*}"*metta*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Metta file does not exist for $full_path"; fi; done' bash {} \; | sort -r

find . \( -name "*.fa" -o -name "*.gff" -o -name "*.json" \) -execdir bash -c 'for file; do metta_pattern="${file%.*}"*datalog*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Datalog file does not exist for $full_path"; fi; done' bash {} \; | sort -r

*/

% ==============================
% FA/FASTA Reader
% ==============================

load_fb_fa(Fn,Filename):-
 track_load_into_file(Filename,
  must_det_ll((
    fbug(load_fb_fa(Fn,Filename)),
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
    Type = 'SequenceFile',
    assert_OBO(id_type(Id,Type)),
    assert_OBO(pathname(Id,Filename)),!,
    assert_OBO(basename(Id,BaseName)),!,
    assert_OBO(directory(Id,Directory)),!,
    setup_call_cleanup(open(Filename,read,In), load_fb_fa_read(Id,In,_,0), close(In))))).
load_fb_fa_read(_Fn,In, _, _):- (at_end_of_stream(In);reached_file_max),!.
load_fb_fa_read(Fn,In,FBTe,At):- read_line_to_string(In,Str), load_fb_fa_read_str(Fn,In,FBTe,Str,At).

load_fb_fa_read_str(Fn,In,_,Str,_):- string_concat('>',Line,Str),!,
     split_string(Line, " \t", " \t", [FBTe|Props]),!,
     parse_and_store_attributes(FBTe, Props),
     load_fb_fa_read(Fn,In,FBTe,0).

load_fb_fa_read_str(Fn,In,FBTe,Str,From):-
   atom_chars(Str,Chars),
   Data =..[fasta_sequence,Fn,FBTe,From,Chars],
   assert_MeTTa(Data),!,
   length(Chars,Plus),
   At is From+Plus,
   load_fb_fa_read(Fn,In,FBTe,At).

maybe_sample(_Fn,_Args):- \+ should_sample,!.
maybe_sample( Fn, Args):- assert_arg_samples(Fn,1,Args).

:- dynamic(fb_arg/1).
:- dynamic(fb_arg_table_n/3).
assert_arg_table_n(A,Fn,N):-    assert_new(fb_arg(A)), assert_new(fb_arg_table_n(A,Fn,N)).

assert_arg_samples(Fn,N,[A|Args]):-
   (dont_sample(A)->true;assert_arg_table_n(A,Fn,N)),
   N2 is N+1, assert_arg_samples(Fn,N2,Args).
assert_arg_samples(_,_,_).

dont_sample(N):- \+ symbol(N).  dont_sample(''). dont_sample('-').

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


%file_to_sep(_File,9).
file_to_sep(csv,',').
file_to_sep(tsv,'\t').
file_to_sep(metta_x,'\t').
file_to_sep(File,Sep):- file_name_extension(_,Ext,File),clause(file_to_sep(Ext,Sep),true),!.
file_to_sep(_,'\t').

is_swipl:- \+ is_scryer.

:- if(is_scryer).
read_line_to_chars(S,L):- is_scryer,!,get_line_to_chars(S,L,[]).
:- endif.
read_line_to_chars(S,L):- read_line_to_string(S,Str),string_chars(Str,L).


% Assert a given term if no variant of it already exists in the database.
% Usage: fb_assert(+Term).
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

use_metta_x:- fail.

load_fb_cache(_File,OutputFile,_Fn):- exists_file(OutputFile),!,ensure_loaded(OutputFile),!.
load_fb_cache(File,_OutputFile,_Fn):- load_files([File],[qcompile(large)]).


'load_flybase_tiny':- load_flybase(20_000).
'load_flybase_full':- load_flybase(1_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000).
'save_flybase_full':- load_flybase_full,qsave_program.

load_flybase(N):- (number(N)->true;N==inf),!,
 with_option([max_per_file=N],
  (option_value(max_per_file,Max),write(max_per_file=Max),load_flybase)).
load_flybase(File):- file_name_extension(_,Ext,File),!, load_flybase(File,Ext).

:- export(load_flybase/1).
:- system:import(load_flybase/1).

load_flybase(File,Ext):-
   with_wild_path(load_flybase0(Ext),File),!.


exists_with_ext(File,Ext):- atom_concat(File,Ext,Res),exists_file(Res),!.

load_flybase0(Ext,File):- Ext=='',file_name_extension(_,Ext2,File),Ext2\=='',!,load_flybase0(Ext2,File).
load_flybase0(Ext,_File):-  Ext=='pl',!.
load_flybase0(Ext,_File):-  Ext=='metta', is_converting,!.
load_flybase0(Ext,_File):-  Ext=='datalog', is_converting,!.
load_flybase0(_Ext, File):-  is_converting,exists_with_ext(File,'.metta'),(exists_with_ext(File,'.datalog');exists_with_ext(File,'.metta.datalog')),!.
load_flybase0(Ext,File):-
  must_det_ll((file_name_extension(Name,_,File),
  data_pred(Name,Fn),load_flybase(Ext,File,Fn))).

:- dynamic(load_state/2).
%load_flybase(_Ext,_File,OutputFile,_Fn):- exists_file(OutputFile),size_file(OutputFile,N),N>100,!.
load_flybase(_Ext,File,_Fn):- load_state(File,_),!.
load_flybase(Ext,File,Fn):-
 must_det_ll((
  assert(load_state(File,loading)),
  fbug(load_flybase(Ext,File,Fn)),
  load_flybase_ext(Ext,File,Fn),
  ignore(retract(load_state(File,loading))),
  assert(load_state(File,loaded)),fb_stats)).

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
load_flybase_ext(Ext,File, Fn):-  fbug(missed_loading_flybase(Ext,File,Fn)),!.

%load_flybase_metta(File):- !, load_metta('&flybase',File).
load_flybase_metta(File):-
   with_option('trace-on-load',false,
    with_option('current_self','&flybase',
      include_atomspace_1_0(File))).
/*
   (EvaluationLink
    (PredicateNode "has_name")
    (ListLink
        (DiseaseOntologyNode "DOID:0001816")
        (ConceptNode "angiosarcoma")))


                  load_metta('&flybase',File)).
*/
include_atomspace_1_0(RelFilename):-
 absolute_file_name(RelFilename,Filename),
 track_load_into_file(Filename,
 must_det_ll((
  atom(RelFilename),
  current_self(Self),
  exists_file(RelFilename),!,
   must_det_ll((setup_call_cleanup(open(Filename,read,In, [encoding(utf8)]),
    ((directory_file_path(Directory, _, Filename),
      assert(metta_file(Self,Filename,Directory)),
      with_cwd(Directory,
        must_det_ll( load_atomspace_1_0_file_stream(Filename,Self,In))))),close(In))))))).

load_atomspace_1_0_file_stream(Filename,Self,In):-
  once((is_file_stream_and_size(In, Size) , Size>102400) -> P2 = read_sform2 ; P2 = read_metta),!,
  with_option(loading_file,Filename,
   %current_exec_file(Filename),
   ((must_det_ll((
       set_exec_num(Filename,1),
       %load_answer_file(Filename),
       set_exec_num(Filename,0))),
       once((repeat, ((
            current_read_mode(Mode),
            once(call(P2, In,Expr)), %write_src(read_atomspace_1_0=Expr),nl,
            must_det_ll((((do_atomspace_1_0(file(Filename),Mode,Self,Expr,_O)))->true;
                 pp_m(unknown_do_atomspace_1_0(file(Filename),Mode,Self,Expr)))),
           flush_output)),
          at_end_of_stream(In)))))),!.

%  ['InheritanceLink',['DiseaseOntologyNode','DOID:0112326'],['DiseaseOntologyNode','DOID:0050737']]
do_atomspace_1_0(_W,_M,_S,end_of_file,_O):-!.
do_atomspace_1_0(W,M,S,E,_O):-
    rewrite_as10_to_as20(E,E2,Extras),!,
    maplist(do_atomspace_2_0(W,M,S),[E2|Extras]).

do_atomspace_2_0(_W,_M,_S,E):-
    assert_OBO(E),
    !. % writeq(E),!,nl.

rewrite_as10_to_as20(A,A,[]):- \+ is_list(A).
rewrite_as10_to_as20([CN,Arg],Arg,[]):- CN='ConceptNode',!.
rewrite_as10_to_as20([ConceptNode,Arg1],Arg,[is_a(Arg,ConceptNode)|R]):- atom(ConceptNode),atom_concat(_Concept,'Node',ConceptNode),!,
   rewrite_as10_to_as20(Arg1,Arg,R),!.
rewrite_as10_to_as20(['EvaluationLink',['PredicateNode',Pred],['ListLink'|Args]], Res,
  [arity(Pred,Len),is_a(Pred,'Predicate')|ExtrasF]):-
   length(Args,Len),maplist(rewrite_as10_to_as20,Args,ArgsL,Extras), flatten(Extras,ExtrasF),
   Res =..  [Pred|ArgsL].


rewrite_as10_to_as20([InheritanceLink|Args],[Inheritance|ArgsL],ExtrasF):-
    atom(InheritanceLink),atom_concat(Inheritance,'Link',InheritanceLink),
    maplist(rewrite_as10_to_as20,Args,ArgsL,Extras), flatten(Extras,ExtrasF),!.

rewrite_as10_to_as20(A,A,[]).

fix_list_args(_,_,Y,Y):- option_value(early_canon,[]), \+ should_sample,!.
%fix_list_args(_Fn,_ArgTypes,[X],[X]):- !.
fix_list_args(Fn,ArgTypes,Args,NewArgs):-
 must_det_ll((
  primary_term(Fn,ArgTypes,Args,Term,NewArgTypes),
  fix_elist_args(Term,Fn,1,NewArgTypes,Args,NewArgs),
  extreme_debug(ignore(((Args \== NewArgs,fbug(NewArgs))))))).
fix_list_args(_Fn,_ArgTypes,Args,Args):-!.

primary_term(_Fn,[N|ArgTypes],_Args,_Term,ArgTypes):- number(N),!.
primary_term(_Fn,[N|ArgTypes],Args,Term,ArgTypes):- number(N),!,nth1(N,Args,Term).
primary_term(_Fn,ArgTypes,_Args,_Term,ArgTypes):-!.
primary_term(_Fn,ArgTypes,Args,Term,NewArgTypes):-
   append(L,[primary(Name)|R],ArgTypes),
   append(L,[Name|R],NewArgTypes),
   length(L,N),nth0(N,Args,Term).
primary_term( Fn,ArgTypes,Args,Term,ArgTypes):-
   primary_column(Fn,Name),
   nth1(N,ArgTypes,Name),!,
   nth1(N,Args,Term),!.
primary_term(_Fn,ArgTypes,[Term|_],Term,ArgTypes):-!.
primary_term(_Fn,ArgTypes,_Args,_Term,ArgTypes).

fix_elist_args(Term,Fn,N,[Nth|ArgTypes],Args,NewArgs):- number(Nth),!,fix_elist_args(Term,Fn,N,ArgTypes,Args,NewArgs).
fix_elist_args(Term,Fn,N,[Type|ArgTypes],[Concept|Args],[Arg|NewArgs]):- !,
   must_det_ll((adjust_type(Term,Fn,N,Type,Concept,Arg), N2 is N +1,  fix_elist_args(Term,Fn,N2,ArgTypes,Args,NewArgs))).
fix_elist_args(_Term,_Fn,_N,_,X,X).

adjust_type(Term,Fn,N,listOf(Type),Arg,NewL):- must_det_ll((nonvar(Type),as_list([],Arg,New),is_list(New),
   maplist(adjust_type(Term,Fn,N,Type),New,NewL))).
adjust_type(Term,Fn,N,listOf(Type,Seps),Arg,NewL):- must_det_ll((nonvar(Type),as_list(Seps,Arg,New),is_list(New),
   maplist(adjust_type(Term,Fn,N,Type),New,NewL))).
adjust_type(Term,Fn,N,Type,Concept,Arg):- numeric_value_p_n(Fn,N,_),!,
   must_det_ll(((into_number(Concept,Arg)->true;(Concept=Arg)),assert_type_of(Term,Fn,N,Type,Arg))).
adjust_type(Term,Fn,N,Type,Concept,Arg):- must_det_ll((fix_concept(Concept,Arg), assert_type_of(Term,Fn,N,Type,Arg))).
adjust_type(_Term,_Fn,_N,_,X,X).

into_number(Concept,Arg):- number(Concept),!,Arg = Concept.
into_number(Concept,Arg):- symbol_number(Concept,Arg),!.
into_number(Concept,Arg):- Concept=Arg,!.

:- dynamic(fb_arg/1).
:- dynamic(fb_arg_table_n/3).
assert_type_of(_Term,_Fn,_N,_Type,_Arg):- \+ should_sample,!.
assert_type_of(Term,Fn,N,Type,Arg):- is_list(Arg),!,maplist(assert_type_of(Term,Fn,N,Type),Arg).
assert_type_of(_Term,Fn,N,_Type,Arg):-
 must_det_ll((
   assert_new(fb_arg(Arg)),
   assert_new(fb_arg_table_n(Arg,Fn,N)))).

:- dynamic(fb_arg_type/1).
:- dynamic(table_n_type/3).
add_table_n_types(_Fn,_,ArgTypes):- \+ is_list(ArgTypes),!.
add_table_n_types(Fn,1,[N|ArgTypes]):- number(N),!,
   add_table_n_types(Fn,1,ArgTypes).
add_table_n_types(Fn,N,[Type|ArgTypes]):-!,
  sub_term(Sub,Type),symbol(Sub),!,
  assert_new(fb_arg_type(Sub)),
  assert_new(table_n_type(Fn,N,Sub)),
  N2 is N+1, add_table_n_types(Fn,N2,ArgTypes),!.
add_table_n_types(_Fn,_,[]).

is_concept(Arg):- fb_arg(Arg).
is_concept_type(Type):- fb_arg_type(Type).

arg_table_n_type(Arg,Fn,N,Type):- table_n_type(Fn,N,Type),once((fb_pred(Fn,A),functor(G,Fn,A), arg(N,G,Arg),call(G),
  \+ is_list(Arg), \+ as_list(Arg,[]))).

is_valuesymbol(Fn,N,Type):- arg_table_n_type(Arg,Fn,N,Type),symbol_number(Arg,_).

:- dynamic(numeric_value_p_n/3).
fis_valuesymbol(PNList,Len):- findall(P-N,is_valuesymbol(P,N,_Type),PNList),length(PNList,Len).

save_value_symbol_cols:- for_all(is_valuesymbol(Fn,N,Type),assert_new(numeric_value_p_n(Fn,N,Type))),
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

write_flybase_data(_ArgTypes,_Fn,[]):-!.
write_flybase_data(_ArgTypes,_Fn,['']):-!.
write_flybase_data(_ArgTypes,_Fn,[_]):-!.
write_flybase_data(_ArgTypes,Fn,DataL0):-
 maplist(fast_column,DataL0,DataL), !, Data=..[Fn|DataL], assert_MeTTa(Data).
%write_flybase_data(_ArgTypes,Fn,DataL):- into_datum(Fn,DataL,Data), assert_OBO(Data).


/*

assert_MeTTa(Data):- Data=..[Fn|DataL],assert_MeTTa(Fn,DataL),!.

assert_MeTTa(Fn,DataL0):-
  make_assertion(Fn,DataL0,Data,OldData),
  ignore((
    heartbeat,
    functor(Data,F,A), A>=2,
   decl_fb_pred(F,A),
    flag(loaded_from_file_count,X,X+1),
    flag(total_loaded_symbols,TA,TA+1),
    assert(Data),
    ignore((((has_list(_ArgTypes)->(X<23,X>20); (X<13,X>10)); (X>0,(0 is X rem 1_000_000),fb_stats)),nl,nl,fbug(X=Data),ignore((OldData\==DataL0,fbug(oldData=OldData))))),
    ignore((fail,catch_ignore(ignore((X<1000,must_det_ll((write_canonical(OutputStream,Data),writeln(OutputStream,'.')))))))))),!.
 */

make_assertion(Fn, Cols, NewData, OldData):- !, make_assertion4(Fn, Cols, NewData, OldData).

make_assertion(Fn,DataL0,Data,DataL0):-
 must_det_ll((
    into_datum(Fn,DataL0,Data0),
    Data0=..[F|Args],
    Args=DataL,
    Data=..[F|DataL])).

make_assertion(ArgTypes,Fn,DataL0,Data,DataL0):-
 must_det_ll((
    into_datum(Fn,DataL0,Data0),
    Data0=..[F|Args],
    skip(if_t(var(ArgTypes),must_det_ll((once((length(Args,Len),length(ArgTypes,Len),once((table_columns(Fn,ArgTypes);table_columns(F,ArgTypes))))))))),
    fix_list_args(Fn,ArgTypes,Args,DataL),
    Data=..[F|DataL])).


% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743
%reprefix(['GO_','GO--','FBgn','BiologicalProcess:GO:'],'GO:').
reprefix(['GO_','GO--','BiologicalProcess:GO:'],'GO:').
reprefix(['flybase:','FLYBASE:','comment:'],'').
reprefix(['FBpp:'],'FBpp').
reprefix(['FBgn:'],'FBgn').
reprefix(['FB:FB'],'FB').
%./KBs/SUMO-OBO/gene-merged-SUMO.kif
%#
%FBbt_00051628=

as_list(A,New):- is_list(A),!,A=New.
as_list(N,[N]):- \+ symbol(N), \+ string(N),!.
%as_list(A,New):- var(A),!,New = [A].
as_list('-',[]). as_list("-",[]). as_list('',[]).
as_list("",[]). as_list(' ',[]). as_list(" ",[]).
%as_list(N,[N]):- !.
as_list(_,S,O):- as_list(S,O),!.
as_list(SepL,A,List):-  member(Sep,SepL),catch_ignore(symbolic_list_concat(List,Sep,A)),List\=[_],!.
as_list([],A,ListO):-  member(Sep,['|',',',';']),catch_ignore(symbolic_list_concat(List,Sep,A)),List\=[_],!,maplist(fix_concept,List,ListO).
as_list(_Sep,A,[A]).
has_list(Header):- is_list(Header),member(listOf(_),Header).

% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743

% =======================================
% Fix Concept1
% =======================================

fix_concept1(A,L):- as_list(['|'],A,L),(L\=@=[A],A\=@=L).
fix_concept1(A,N):-  symbol_number(A,N),!.
%fix_concept1(A,AO):- reprefix(List,To),member(E,List),symbol_concat(E,AM,A),symbol_concat(To,AM,AO).
%fix_concept1(A,AO):- symbol_concat('FB',_,A),symbolic_list_concat([Type,Number],':',A),!,symbol_concat(Type,Number,AO).
fix_concept1(A,AO):- symbol_concat('"',Mid,A),symbol_concat(AS,'"',Mid),symbol_string(AS,AO).
fix_concept1(A,AO):- symbol_concat(AO,'(gene name)',A),AO\==''.

fix_concept1(A,N):- symbol(A),!,N=A.
%fix_concept(S,A):- number_string(A,S),!.


% =======================================
% Fix Concept
% =======================================

fix_concept(A,New):- is_list(A),!,maplist(fix_concept,A,L),!,New=L.
fix_concept(A,New):- \+ symbol(A), !,New=A.
fix_concept(S,O):- once(fix_concept1(S,M)),S\=@=M,!,fix_concept(M,O).
fix_concept(A,New):- =(A,New),!.


fix_columns_nth('genome-cyto-seq', 1).
fix_columns_nth('genome-cyto-seq', 2).
fix_columns_nth('genome-cyto-seq', 3).
fix_columns_nth(allele_genetic_interactions, 3).
fix_columns_nth(dmel_human_orthologs_disease, 6).
fix_columns_nth(dmel_human_orthologs_disease, 7).
fix_columns_nth(dmel_paralogs, 10).
fix_columns_nth(dmel_paralogs, 11).
fix_columns_nth(dmel_paralogs, 5).
fix_columns_nth(dmel_paralogs, 8).
fix_columns_nth(entity_publication, 4).
fix_columns_nth(fbgn_NAseq_Uniprot, 7).
fix_columns_nth(fbrf_pmid_pmcid_doi, 2).
fix_columns_nth(gene_genetic_interactions, 1).
fix_columns_nth(gene_genetic_interactions, 2).
fix_columns_nth(gene_genetic_interactions, 3).
fix_columns_nth(gene_genetic_interactions, 4).
fix_columns_nth(gene_groups_HGNC, 4).
fix_columns_nth(gene_rpkm_matrix, _).
fix_columns_nth(gene_rpkm_report, 10).
fix_columns_nth(gene_rpkm_report, 11).
fix_columns_nth(gene_rpkm_report, 8).
fix_columns_nth(gene_rpkm_report, 9).
fix_columns_nth(gene_snapshots, 4).
fix_columns_nth(genotype_phenotype, 5).
fix_columns_nth(genotype_phenotype, 6).
fix_columns_nth(gp_information, 9).
fix_columns_nth(insertion_mapping, 5).
fix_columns_nth(insertion_mapping, 6).
fix_columns_nth(physical_interactions_mitab, _).
fix_columns_nth(pmid_fbgn_uniprot, 2).
fix_columns_nth(stocks, 7).
fix_columns_nth(synonym, 5).
fix_columns_nth(synonym, 6).
fix_columns_nth(transposon_sequence_set, 4).
fix_columns_nth(transposon_sequence_set, 5).
fix_columns_nth(transposon_sequence_set, 8).



:- discontiguous column_description/4.
:- discontiguous primary_column/2.
:- discontiguous column_names/2.
:- discontiguous file_location/2.



% 466_896_429
% Descriptions for allele_genetic_interactions columns
% Descriptions for genotype_phenotype_data columns
% For the file allele_genetic_interactions_*.tsv
% For the file genotype_phenotype_data_*.tsv



load_flybase(Sep,File,Stream,Fn):-
 must_det_ll((
  %ignore(swi_only(format(":- ~q.\n",[encoding(utf8)]))),
  symbolic_list_concat([data,Fn],'_',Fn0),
  data_pred(Fn0,Fn),
  load_flybase_sv(Sep,File,Stream,Fn))).

% Sep,File,Stream,OutputStream,Fn
load_flybase_sv(Sep,File,Stream,Fn):- at_end_of_stream(Stream),!,
  once(load_fb_data(_ArgTypes,File,Stream,Fn,Sep,end_of_file)).

load_flybase_sv(Sep,File,Stream,Fn):-
 must_det_ll((
  flag(loaded_from_file_count,_,0),
  ignore(once((table_columns(File,Header);table_columns(Fn,Header)))),
  fix_header_names(Fn,Header,ArgTypes),
  forall((table_columns(File,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(File,ColInfo))),
  forall((table_columns(Fn,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(Fn,ColInfo))),
  ((primary_column(Fn,Name),nth1(N,ArgTypes,Name))->NArgTypes=[N|ArgTypes];NArgTypes=[1|ArgTypes]),
  if_t(is_list(ArgTypes),add_table_n_types(Fn,1,ArgTypes)),
  ground(NArgTypes),
  if_t(is_list(ArgTypes),ignore((length(ArgTypes,A),decl_fb_pred(Fn,A)))),
  time((repeat,
  read_line_to_chars(Stream, Chars),
  once(load_flybase_chars(NArgTypes,File,Stream,Fn,Sep,Chars)),
  once(reached_file_max;done_reading(File);at_end_of_stream(Stream)),!,
  once(load_fb_data(NArgTypes,File,Stream,Fn,Sep,end_of_file)))),
  loaded_from_file_count(X),!,
  metta_stats(Fn),
  pl_stats(File,X))),!.


%save_conversion_data(ArgTypes,Fn,OutputStream,Data):- maplist(write_flybase_data(ArgTypes,ArgTypes,Fn,OutputStream),Data).

is_really_header_row([H|_],_Names):- symbol_concat('',_,H),!.

process_metta_x_file(MXFile):-
  data_pred(MXFile,Fn),
  setup_call_cleanup(open(MXFile,read,In,[encoding(utf8)]),
    ((repeat,
       read_line_to_string(In,Chars),
       (In == end_of_file -> ! ;
        once((atomic_list_concat(Row0,'\t', Chars),
          maplist(fast_column,Row0,Row),
          assert_MeTTa([Fn|Row])))))),
     close(In)).

fast_column(X,X):- !.
fast_column(X,Y):- into_fb_term(X,Y),!.
fast_column(X,X).

%read_csv_stream(Sep,CharsStream,Header):- read_string(CharsStream, "\n", "\r\t ",_,)
read_csv_stream(Sep,CharsStream,Header):- %  \+ option_value(full_canon,[]),!,
  read_line_to_string(CharsStream,Chars),
  (Chars == end_of_file -> Header= Chars ; symbolic_list_concat(Header, Sep, Chars)).
read_csv_stream(Sep,CharsStream,Header):- \+ option_value(full_canon,[]),!, read_line_to_string(CharsStream,Chars),
  (Chars == end_of_file -> Header= Chars ; split_string(Chars, Sep, "\s\t\n", Header)).
read_csv_stream(Sep,CharsStream,Header):-
  name(Sep,[SepCode]),
  csv_options(CompiledHeaderOptions,[separator(SepCode)]),
  csv_read_row(CharsStream, HeaderRow, CompiledHeaderOptions),
  HeaderRow=..[_|Header],!.

read_csv(Sep,Chars,Header):- \+ option_value(full_canon,[]),!, split_string(Chars, Sep, "\s\t\n", Header).
read_csv(Sep,Chars,Header):-
  open_string(Chars,CharsStream),read_csv_stream(Sep,CharsStream,Header).


attempt_header_row(Sep,Chars,Fn,Header,ArgTypes):-
  read_csv(Sep,Chars,Header),
  fix_header_names(Fn,Header,ArgTypes),!.

:- dynamic(t_h_n/3).

load_flybase_chars(ArgTypes,File,_Stream,_Fn,Sep,Chars):-
  ( \+ member(Sep,Chars); (['#','#',' '|_]=Chars) ;  (ground(ArgTypes),['#'|_]=Chars)),
  %writeln(comment(Sep)=Chars),!,
  (format("~n ; ~s",[Chars])),
  ignore((loaded_from_file_count(X),X>2000,!,assert(done_reading(File)))).

load_flybase_chars([N|ArgTypes],File,Stream,Fn,Sep,Chars):-
  var(ArgTypes),member(Sep,Chars),['#'|_]=Chars,
  (format("~n ; Maybe Header: ~s",[Chars])),
  attempt_header_row(Sep,Chars,Fn,Header,ArgTypes),
  is_really_header_row(Header,ArgTypes),
  (fbug(t_h_n(Fn,Header,ArgTypes)),fb_assert(t_h_n(Fn,Header,ArgTypes))),!,
  load_fb_data([N|ArgTypes],File,Stream,Fn,Sep,is_swipl).

load_flybase_chars([N|ArgTypes],File,Stream,Fn,Sep,Chars):- is_swipl,
  attempt_header_row(Sep,Chars,Fn,Header,_),
  write_flybase_data([N|ArgTypes],Fn,Header),!,
  load_fb_data([N|ArgTypes],File,Stream,Fn,Sep,is_swipl).


load_fb_data(_ArgTypes,File,_Stream,_Fn,_Sep,Data):-
  (Data == end_of_file;done_reading(File)),!.

load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl):-  % \+ option_value(full_canon,[]), !,
  (option_value(max_per_file,Max)->true;Max=inf),
  fbug(load_fb_data(ArgTypes,File,Max,Fn,Sep)),
  add_table_n_types(Fn,1,ArgTypes),!,% trace,
   repeat,
     once(read_csv_stream(Sep,Stream,Data)),
     loaded_from_file_count(X),
      (((Data== end_of_file);reached_file_max;(X>Max)) -> assert(done_reading(File)) ;
       (once(write_flybase_data(ArgTypes,Fn,Data)),fail)),!.

load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl):- !,
   name(Sep,[SepCode]),
  csv_options(CompiledOptions,[separator(SepCode)]),
  (option_value(max_per_file,Max)->true;Max=inf),
  fbug(load_fb_data(ArgTypes,File,Max,Fn,Sep)),
  add_table_n_types(Fn,1,ArgTypes),!,
   repeat,
     once((csv_read_row(Stream, RData, CompiledOptions))),
     loaded_from_file_count(X),
      (((RData== end_of_file);reached_file_max;(X>Max)) -> assert(done_reading(File)) ;
       (RData =..[_|Data],
       once(write_flybase_data(ArgTypes,Fn,Data)),fail)),!.

% recursion depth 16 million rows
load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl):-
  name(Sep,[SepCode]),
  csv_options(CompiledOptions,[strip(true),convert(true),separator(SepCode)]),
   (option_value(max_per_file,Max)->true;Max=inf),
     once((csv_read_row(Stream, RData, CompiledOptions))),
     loaded_from_file_count(X),
      (((RData== end_of_file);(X>Max)) -> assert(done_reading(File)) ;
       (RData =..[_|Data], once(write_flybase_data(ArgTypes,Fn,Data)),
         load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl))),!.



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

column_names('allele_genetic_interactions', ['allele_symbol', 'allele_FBal#', 'interaction', 'FBrf#']).
column_names('fb_synonym', ['primary_FBid', 'organism_abbreviation', 'current_symbol', 'current_fullname', listOf(fullname_synonym, ['|']), listOf(symbol_synonym, ['|'])]).
column_names('gene_genetic_interactions', [listOf('Starting_gene_symbol', ['|']), listOf('Starting_gene_FBgn', ['|']), listOf('Interacting_gene_symbol', ['|']), listOf('Interacting_gene_FBgn', ['|']), 'Interaction_type', 'Publication_FBrf']).
column_names('gene_rpkm_matrix', ['gene_primary_id', 'gene_symbol', 'gene_fullname', 'gene_type', 'DATASAMPLE_NAME_(DATASET_ID)']).
column_names('gene_rpkm_report', ['Release_ID', 'FBgn#', 'GeneSymbol', 'Parent_library_FBlc#', 'Parent_library_name', 'RNASource_FBlc#', 'RNASource_name', 'RPKM_value', 'Bin_value', 'Unique_exon_base_count', 'Total_exon_base_count', 'Count_used']).
column_names('genotype_phenotype_data', [listOf('genotype_symbols', ['/', ' ']), listOf('genotype_FBids', ['/', ' ']), 'phenotype_name', 'phenotype_id', listOf('qualifier_names', ['|']), listOf('qualifier_ids', ['|']), 'reference']).
column_names('pmid_fbgn_uniprot', ['FBrf_id', 'PMID', 'FBgn_id', 'UniProt_database', 'UniProt_id']).
column_names('scRNA-Seq_gene_expression', ['Pub_ID', 'Pub_miniref', 'Clustering_Analysis_ID', 'Clustering_Analysis_Name', 'Source_Tissue_Sex', 'Source_Tissue_Stage', 'Source_Tissue_Anatomy', 'Cluster_ID', 'Cluster_Name', 'Cluster_Cell_Type_ID', 'Cluster_Cell_Type_Name', 'Gene_ID', 'Gene_Symbol', 'Mean_Expression', 'Spread']).

file_location('allele_genetic_interactions', "path_to_file/allele_genetic_interactions_*.tsv").
file_location('genotype_phenotype_data', "path_to_file/genotype_phenotype_data_*.tsv").

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

too_generic(Var):- var(Var),!,fail.
too_generic(pub_id).
too_generic(X):- \+ symbolic_list_concat([_,_,_|_],'_',X).


fix_header_names(Fn,Header,GNames):-
   maplist(fix_header_names(Header,Fn),Header,ArgTypes),
   include( \=(''),ArgTypes,GNames).


%fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_','_id','_ID']),symbol_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(_FL,_Fn,ID,Out):- number(ID),!,Out=ID.
fix_header_names(FL,Fn,listOf(ID),listOf(Out)):- fix_header_names(FL,Fn,ID,Out),!.
fix_header_names(FL,Fn,listOf(ID,Sep),listOf(Out,Sep)):- fix_header_names(FL,Fn,ID,Out),!.
fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_']),symbol_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_']),symbol_concat(RF,MID,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,Out):- member(RF,['__',' ']),symbolic_list_concat(MIDL,RF,ID),MIDL\=[_],symbolic_list_concat(MIDL,'_',MID),!,
   fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,listOf(AOut)):- member(RF,['(es)','(s)','ids']),symbolic_list_concat([Left,Right],RF,ID),symbolic_list_concat([Left,Right],'_',MID),!,
   fix_header_names(FL,Fn,MID,AOut),!. % symbol_concat('ListOf_',AOut,Out),!.
fix_header_names(FL,Fn,TT,listOf(AOut)):-
   member(IDs=ID,['IDs'='ID']),
   symbol_concat(Type,IDs,TT),
   symbol_concat(Type,ID,MID),
   fix_header_names(FL,Fn,MID,AOut),!.
fix_header_names(FL,Fn,ID,listOf(AOut)):- member(RFS=RF,['_IDs'='_ID','IDs'='ID']),
   symbolic_list_concat([Left,Right],RFS,ID),
   symbolic_list_concat([Left,Right],RF,MID),!,
   fix_header_names(FL,Fn,MID,AOut),!. % symbol_concat('ListOf_',AOut,Out),!.


fix_header_names(_,_,Name,Name):- \+ too_generic(Name),!.
fix_header_names(_,_,Name,Name):- symbolic_list_concat([_,_|_],'_',Name),!.
%fix_header_names(_,Fn,ID,Out):- symbolic_list_concat([Fn,ID],'_column_',Out).
%fix_header_names(FieldList,Fn,ID,Out):- symbolic_list_concat([Fn,ID],'_',Out), \+ member(Out,FieldList).
fix_header_names(_,_,Name,Name).


pmt :-flybase_tables(FBT),for_all(member(T,FBT), ( '\\+'(flybase_cols(T,_)) -> format('~N~q.~n',[get_fbt(T)]);true)).
use_flybase_cols(Table,Columns):-
 must_det_ll((
  maplist(fix_header_names(Columns,Table),Columns,ArgTypes),
  assert(flybase_col_names(Table,Columns,ArgTypes)),
  do_arity_2_names(Table,ArgTypes))).

do_arity_2_names(Table,[ID|ArgTypes]):-
  must_det_ll((
  symbol_concat('data_',Table,F),
  length([ID|ArgTypes],Arity),
  length(Args,Arity),
  DataCall=..[F|Args],
  do_arity_2_names_dc(Table,DataCall,2,ArgTypes))).

do_arity_2_names_dc(Table,DataCall,N,[Nth|ArgTypes]):-
  do_arity_2_names_dc1(Table,DataCall,N,Nth),!,
  N2 is N+1, do_arity_2_names_dc(Table,DataCall,N2,ArgTypes).
do_arity_2_names_dc(_Table,_DataCall,_N,[]).

do_arity_2_names_dc1(Table,DataCall,N,Nth):-
 must_det_ll((
  arg(1,DataCall,Arg1Data),
  arg(N,DataCall,Arg2Data),
  make_arity_2_name(Table,Nth,Arity2),
  Arg1=..[Table,Arg1Data],
  clip_id(Nth,NthNoID),
  (Nth==NthNoID -> Arg2=Arg2Data ;  Arg2 =..[NthNoID,Arg2Data]),
  Arity2Call=..[Arity2,Arg1,Arg2],
  fbug((Arity2Call:-DataCall)),
  fb_assert((Arity2Call:-DataCall)))).

make_arity_2_name(Table,Nth,Arity2):-
  clip_id(Nth,NthNoID),
  (symbol_concat(Table,_,Nth)
    -> Arity2 = Nth
    ; symbolic_list_concat([Table,NthNoID],'_',Arity2)).


clip_id(Nth,ID):- (symbol_concat(ID,'_id',Nth)->true;Nth=ID),!.




setup_flybase_cols:-
 for_all(flybase_cols(Table,Columns),
  use_flybase_cols(Table,Columns)).

%:- load_flybase("das_precomputed/allele_genetic_interactions_fb_2022_06.tsv").




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


table_columns(T,List):- table_columns_tt(TT,List), eigther_contains(T,TT),!.

table_columns_tt(TT,List):- column_names(TT,List).
table_columns_tt(TT,List):- flybase_cols(TT,List).
table_columns_tt(TT,List):- t_h_n(TT,_,List).

eigther_contains(TT,T):- TT=T,!.
eigther_contains(T,TT):- symbol_contains(T,TT),!.
eigther_contains(TT,T):- symbol_contains(T,TT),!.





column_names('cyto-genetic-seq', ['Cytogenetic_map_position', 'Genetic_map_position', 'Sequence_coordinates_(release_6)', 'R6_conversion_notes']).
column_names('Dmel_enzyme', [gene_group_id, gene_group_name, listOf(gene_group_GO_id), listOf(gene_group_GO_name), listOf(gene_group_EC_number), listOf(gene_group_EC_name), gene_id, gene_symbol, gene_name, listOf(gene_EC_number), listOf(gene_EC_name)]).
column_names('scRNA-Seq_gene_expression', ['Pub_ID', 'Pub_miniref', 'Clustering_Analysis_ID', 'Clustering_Analysis_Name', 'Source_Tissue_Sex', 'Source_Tissue_Stage', 'Source_Tissue_Anatomy', 'Cluster_ID', 'Cluster_Name', 'Cluster_Cell_Type_ID', 'Cluster_Cell_Type_Name', 'Gene_ID', 'Gene_Symbol', 'Mean_Expression', 'Spread']).
column_names(allele_genetic_interactions, [allele_symbol, allele_FBal, interaction, 'FBrf']).
column_names(allele_phenotypic,           [allele_symbol, allele_FBal, phenotype, 'FBrf']).
column_names(fbal_to_fbgn,             ['AlleleID', 'AlleleSymbol', 'GeneID', 'GeneSymbol']).
column_names(genotype_phenotype_data, [listOf(genotype_symbols, [/, ' ']), listOf(genotype_FBids, [/, ' ']), phenotype_name, phenotype_id, listOf(qualifier_names, ['|']), listOf(qualifier_ids, ['|']), reference]).
%                                        #genotype_symbols           	genotype_FBids	phenotype_name	phenotype_id	qualifier_names	qualifier_ids	reference
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

flybase_tables([
analysis, analysisfeature, analysisgrp, analysisgrpmember, analysisprop, audit_chado, cell_line, cell_line_loaderm, cell_line_loadermprop,
cell_line_dbxref, cell_line_feature, cell_line_library, cell_line_libraryprop, cell_line_pub, cell_line_relationship, cell_line_strain,
cell_line_strainprop, cell_line_synonym, cell_lineprop, cell_lineprop_pub, contact, cv, loaderm, loaderm_dbxref, loaderm_relationship,
loadermpath, loadermprop, loadermsynonym, db, dbxref, dbxrefprop, eimage, environment, environment_loaderm, expression, expression_loaderm,
expression_loadermprop, expression_image, expression_pub, expressionprop, feature, feature_loaderm, feature_loaderm_dbxref,
feature_loadermprop, feature_dbxref, feature_expression, feature_expressionprop, feature_genotype, feature_grpmember,
feature_grpmember_pub, feature_humanhealth_dbxref, feature_interaction, feature_interaction_pub, feature_interactionprop,
feature_phenotype, feature_pub, feature_pubprop, feature_relationship, feature_relationship_pub, feature_relationshipprop,
feature_relationshipprop_pub, feature_synonym, featureloc, featureloc_pub, featuremap, featuremap_pub, featurepos, featureprop,
featureprop_pub, featurerange, genotype, genotype_loaderm, genotype_loadermprop, genotype_dbxref, genotype_pub, genotype_synonym,
genotypeprop, genotypeprop_pub, grp, grp_loaderm, grp_dbxref, grp_pub, grp_pubprop, grp_relationship, grp_relationship_pub,
grp_relationshipprop, grp_synonym, grpmember, grpmember_loaderm, grpmember_pub, grpmemberprop, grpmemberprop_pub, grpprop,
grpprop_pub, humanhealth, humanhealth_loaderm, humanhealth_loadermprop, humanhealth_dbxref, humanhealth_dbxrefprop,
humanhealth_dbxrefprop_pub, humanhealth_feature, humanhealth_featureprop, humanhealth_phenotype, humanhealth_phenotypeprop,
humanhealth_pub, humanhealth_pubprop, humanhealth_relationship, humanhealth_relationship_pub, humanhealth_synonym, humanhealthprop,
humanhealthprop_pub, interaction, interaction_cell_line, interaction_loaderm, interaction_loadermprop, interaction_expression,
interaction_expressionprop, interaction_group, interaction_group_feature_interaction, interaction_pub, interactionprop,
interactionprop_pub, library, library_loaderm, library_loadermprop, library_dbxref, library_dbxrefprop, library_expression,
library_expressionprop, library_feature, library_featureprop, library_grpmember, library_humanhealth, library_humanhealthprop,
library_interaction, library_pub, library_relationship, library_relationship_pub, library_strain, library_strainprop, library_synonym,
 libraryprop, libraryprop_pub, lock, organism, organism_loaderm, organism_loadermprop, organism_dbxref, organism_grpmember,
 organism_library, organism_pub, organismprop, organismprop_pub, phendesc, phenotype, phenotype_comparison, phenotype_comparison_loaderm,
  phenotype_loaderm, phenstatement, project, pub, pub_dbxref, pub_relationship, pubauthor, pubprop, sql_features, sql_implementation_info,
  sql_parts, sql_sizing, stock, stock_loaderm, stock_dbxref, stock_genotype, stock_pub, stock_relationship, stock_relationship_pub,
  stockcollection, stockcollection_stock, stockcollectionprop, stockprop, stockprop_pub, strain, strain_loaderm, strain_loadermprop,
  strain_dbxref, strain_feature, strain_featureprop, strain_phenotype, strain_phenotypeprop,
strain_pub, strain_relationship, strain_relationship_pub, strain_synonym, strainprop, strainprop_pub, synonym, tableinfo, update_track]).



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


guess_rest(P,N,T,Guess):- table_n_type(P,N,T,Guess),var(Guess),fb_pred(P,A),functor(C,P,A),arg(N,C,Guess),once(call(C)).

:- dynamic numeric_value_p_n/3.
:- module_transparent numeric_value_p_n/3.

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


list_column_names:-
  for_all((column_names(T,CNs),once((length(CNs,Len),Len>=2,fb_pred_nr(T,Len)))),
  (print(column_names(T,CNs)),nl)).


%:- ensure_loaded(read_obo).

show_cmds:- prolog_load_context(source,This),for_all((source_file(P0,This),functor(P0,F,0)),writeln(add_history1(F))).
%add_history1(setup_flybase_cols)
%add_history1(pmt)
ah:- add_history1(fb_stats),
  add_history1(mine_overlaps),
  add_history1(load_flybase),
  add_history(fb_stats),
  add_history(mine_overlaps),
  add_history(try_overlaps),
  add_history(load_flybase).
%:- ah,ah,ah.

%:- initialization(load_flybase).





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
:- ensure_loaded(metta_interp).
%:- ensure_loaded(metta_python).




if_m2(G):- once(G).

datalog_to_termlog(File):- atom_contains(File,'*'),expand_file_name(File,List),!,maplist(datalog_to_termlog,List).
datalog_to_termlog(File):-
 nb_setval(src_indents,'False'),
   atom_concat(File,'2',File2),
   fbug(datalog_to_termlog(File)),
  if_m2(atom_concat(File,'.metta',M)),
   setup_call_cleanup((open(File,read,In), open(File2,write,Out), if_m2(open(M,write,OutM))),
  (repeat,
   read_term(In,Term,[]),
   (Term==end_of_file -> ! ; (process_datalog(Out,OutM,Term),fail))),
   ((close(In),close(Out),if_m2(close(OutM)),listing(fb_pred/2)))).


process_datalog(Out,OutM,Term):-
   Term=..[F|Args],
   process_datalog(Out,OutM,F,Args).


process_datalog(Out,OutM,F,Args):-
  must_det_ll((maplist(better_arg,Args,ArgsL),
   Src=[F|ArgsL],OBO=..Src,
   length(ArgsL,N),
   assert_if_new(fb_pred(F,N)),
   write_canonical(Out,OBO),!,writeln(Out,'.'),
   if_m2((with_output_to(OutM,write_srcH(Src)))))).


   % Split a string or atom by a specified delimiter.
split_by_delimiter(Input, Delimiter, Parts) :-
    atomic_list_concat(Parts, Delimiter, Input),
    Parts = [_,_|_].  % Ensure that there's more than one part.

always_delistify(A,A):- \+ compound(A),!.
always_delistify(s(A,M,E),s(A,MM,E)):- !, always_delistify(M,MM).
always_delistify(A,A):- \+ is_list(A),!.
always_delistify([A],A):-!.
always_delistify(A,A).

better_arg(S,A):- string(S),string_to_syms,atom_string(A,S),!.
%better_arg1(A,B):- fix_concept(A,B),!.
better_arg(A,A):- !.

better_arg(A,B):- better_arg1(A,AA),always_delistify(AA,B).
% Main predicate to try different delimiters.
better_arg1(Input,s(A,Mid,E)) :- fail, (string(Input);atom(Input)),
  once(to_case_breaks(Input,CB)), CB=[_,_,_|_],  once(cb_better_args(CB,[A|ABCParts])),
   ABCParts=[_,_|_], append(Mid,[E],ABCParts),!.
better_arg1(S,A):- string(S),string_to_syms,tom_string(A,S),!.
%better_arg1(A,B):- fix_concept(A,B),!.
better_arg1(A,A).

is_FB_input([xti(_, upper), xti(":", punct), xti(_, digit)]):-!.
is_FB_input([xti("FB", upper), xti(_,lower), xti(_, digit)]):-!.
cb_better_args([_],_):-!,fail.
cb_better_args(X,_):- is_FB_input(X),!,fail.
cb_better_args(CB,Parts):-cb_better_args_ni(CB,Parts),!.
cb_better_args_ni([A,B,C|L],[I|Parts]):- is_FB_input([A,B,C]),maplist(arg(1),[A,B,C],ABC),atomic_list_concat(ABC,I),cb_better_args_ni(L,Parts).
cb_better_args_ni([XTI|L],[I|Parts]):-arg(1,XTI,S),string_to_syms,!,atom_string(I,S),cb_better_args_ni(L,Parts).
cb_better_args_ni([],[]):-!.
datalog_to_termlog:-
  datalog_to_termlog('./data/*/*.datalog'),
  datalog_to_termlog('./data/*/*/*.datalog'),
  datalog_to_termlog('./data/*/*/*/*.datalog'),
  datalog_to_termlog('./data/*/*/*/*/*.datalog'),
  datalog_to_termlog('./data/*/*/*/*/*/*.datalog'),
  datalog_to_termlog('./data/*/*/*/*/*/*/*.datalog').

%datalog_to_termlog:- datalog_to_termlog('whole_flybase.datalog').

