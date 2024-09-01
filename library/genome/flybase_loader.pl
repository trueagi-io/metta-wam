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
:- discontiguous column_names/2.

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
%real_assert2(OBO):- call(OBO),!.
real_assert2(OBO):- pfcAdd_Now(OBO).

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
  pfcAdd_Now(fix_columns_nth(Fn,N)).


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

rename_tmp_files(Filename,NonTmp):- symbolic_list_concat([Filename,NonTmp,'.tmp'],From),
   symbolic_list_concat([Filename,NonTmp],To),
   fbug(rename_file(From,To)),
   ignore((exists_file(From),rename_file(From,To))).

:- dynamic(is_loaded_from_file_count/2).

:- use_module(library(http/json)).
%:- ensure_loaded(json_loader).
load_fb_json(Fn,File):- fbug(load_fb_json(Fn,File)),
  current_predicate(load_flybase_json/2),
     absolute_file_name(File,Filename),
       track_load_into_file(Filename,load_flybase_json(Fn,File)).

load_fb_json(Fn,File):- fbug(load_fb_json(Fn,File)),
 setup_call_cleanup(open(File,read,In,[encoding(utf8)]),  json:json_read(In,Term,[]), close(In)),
    time(assert(saved_fb_json(File,Term,Fn))).


maybe_sample(_Fn,_Args):- \+ should_sample,!.
maybe_sample( Fn, Args):- assert_arg_samples(Fn,1,Args).

:- dynamic(fb_arg/1).
:- dynamic(fb_arg_table_n/3).
assert_arg_table_n(A,Fn,N):-    pfcAdd_Now(fb_arg(A)), pfcAdd_Now(fb_arg_table_n(A,Fn,N)).

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

use_metta_x:- fail, fail_flag.

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


lfbm:- load_flybase('tests/performance/knowledge_graphs/graphml_csv/cml/ckg_neighbors_cml_edges_e21425.csv',csv).

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
   pfcAdd_Now(fb_arg(Arg)),
   pfcAdd_Now(fb_arg_table_n(Arg,Fn,N)))).

:- dynamic(fb_arg_type/1).
:- dynamic(table_n_type/3).
add_table_n_types(_Fn,_,ArgTypes):- \+ is_list(ArgTypes),!.
add_table_n_types(Fn,1,[N|ArgTypes]):- number(N),!,
   add_table_n_types(Fn,1,ArgTypes).
add_table_n_types(Fn,N,[Type|ArgTypes]):-!,
  sub_term(Sub,Type),symbol(Sub),!,
  pfcAdd_Now(fb_arg_type(Sub)),
  pfcAdd_Now(table_n_type(Fn,N,Sub)),
  N2 is N+1, add_table_n_types(Fn,N2,ArgTypes),!.
add_table_n_types(_Fn,_,[]).

is_concept(Arg):- fb_arg(Arg).
is_concept_type(Type):- fb_arg_type(Type).

arg_table_n_type(Arg,Fn,N,Type):- table_n_type(Fn,N,Type),once((fb_pred(Fn,A),functor(G,Fn,A), arg(N,G,Arg),call(G),
  \+ is_list(Arg), \+ as_list(Arg,[]))).

is_valuesymbol(Fn,N,Type):- arg_table_n_type(Arg,Fn,N,Type),symbol_number(Arg,_).

:- dynamic(numeric_value_p_n/3).
fis_valuesymbol(PNList,Len):- findall(P-N,is_valuesymbol(P,N,_Type),PNList),length(PNList,Len).

save_value_symbol_cols:- for_all(is_valuesymbol(Fn,N,Type),pfcAdd_Now(numeric_value_p_n(Fn,N,Type))),
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

process_metta_x_file(MXFile):-
  data_pred(MXFile,Fn),
  setup_call_cleanup(open(MXFile,read,In,[encoding(utf8)]),
    ((repeat,
       read_line_to_string(In,Chars),
       (In == end_of_file -> ! ;
        once((symbolic_list_concat(Row0,'\t', Chars),
          maplist(fast_column,Row0,Row),
          assert_MeTTa([Fn|Row])))))),
     close(In)).

fast_column(X,X):- !.
fast_column(X,Y):- into_fb_term(X,Y),!.
fast_column(X,X).





if_m2(G):- once(G).

datalog_to_termlog(File):- atom_contains(File,'*'),expand_file_name(File,List),!,maplist(datalog_to_termlog,List).
datalog_to_termlog(File):-
 nb_setval(src_indents,'False'),
   atom_concat(File,'2',File2),
   fbug(datalog_to_termlog(File)),
  if_m2(atom_concat(File,'.metta',M)),
   setup_call_cleanup((open(File,read,In,[encoding(utf8)]),
                       open(File2,write,Out,[encoding(utf8)]),
                       if_m2(open(M,write,OutM,[encoding(utf8)]))),
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
    symbolic_list_concat(Parts, Delimiter, Input),
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
cb_better_args_ni([A,B,C|L],[I|Parts]):- is_FB_input([A,B,C]),maplist(arg(1),[A,B,C],ABC),symbolic_list_concat(ABC,I),cb_better_args_ni(L,Parts).
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

