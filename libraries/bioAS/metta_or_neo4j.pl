:- use_module(library(csv)).
:- use_module(library(filesex)). % For recursive file searching
:- use_module(library(apply)).   % For maplist/2

:- use_module(library(readutil)).  % For getting terminal width

mw_cols(P1, List) :-
    tty_size(_, Width),                  % Fixed terminal width
    mw_cols(P1, Width, List), format('~N').

mw_cols(P1, Width, List):-
    length(List, Cols),             % Use the number of items as column count
    ColWidth is (Width // Cols),      % Calculate width for each column
    \+ \+ (mwnv(List),
    print_row(P1, List, 1, Cols, ColWidth)).

print_row(_P1, [], _, _, _) :- !.
print_row(P1, [H|T], N, Cols, Width) :-
    print_col(P1, H, T, N, Cols, Width),
    succ(N,Np1),
    print_row(P1, T, Np1, Cols, Width).

print_col(_,  H, _, N, Cols,_Width):- Cols>3, H\==[], is_list(H), length(H,Len), N+Len>6, write(' ...'), nl, write('    '), fail.
print_col(P1, H, _,_N, Cols,_Width):- Cols>3, compound(H), compound_name_arguments(H,':',[_,T]),atom(T),
              (call(P1, H), write('    ')),!.

print_col(P1, H,_T,_N,_Cols, Width):-
   format("~|~@~t~*+", [(call(P1, H), write(' ')), Width]), !. % Left-align within calculated width

mwnv(Term) :-  numbervars(Term, 0, _, [singletons(true), attvar(bind)]).
mw_writeq(Term):- format('~N'), ignore( \+ ( mw_print(Term))), format('~N').

mw_rjust(Width, Goal) :-
    with_output_to(string(S), Goal), % Capture output as a string
    string_length(S, Len),
    Pad is max(0, Width - Len),    % Compute necessary padding
    forall(between(1, Pad, _), write(' ')), % Write Pad spaces
    write(S).                         % Print the original output

mw_print(Term):- \+ compound(Term), mw_rjust(20, write_src_rec(Term)), write(' ').
mw_print((P:-B)):- !, mw_cols(mw_print, [P, :-(B)]).
%mw_print(List):- is_list(List), !, mw_cols(P1, List).
mw_print(:-(B)):- format(':- ~q.', [B]).
mw_print(Term):- format('~q', [Term]).

mw_print_ans(S):- \+ compound(S), write_src_rec(S),!.
mw_print_ans(S:T):- T=@='$VAR'('_'),!,mw_print_ans(S).
%mw_print_ans(S:_):- !,mw_print_ans(S).
mw_print_ans(S):- mw_print(S).


write_src_rec(S):- with_write_override(mw_print_fail,write_src(S)),!.
mw_print_fail(_):- fail.

mw_print_arg(List):- is_list(List),!,fail.
mw_print_arg(H):- \+ compound(H), !, write_src_rec(H).
mw_print_arg(H):- compound_name_arguments(H,':',[I,T]),atom(T),write_src(I),write(':'),write_src(T).
mw_print_arg(A) :- fail, \+ is_list(A), compound(A), A \= exec(_),
      \+ woc((sub_term(E,A), is_list(E))),
      catch(portray_clause(A),_,fail), !.

mw_print_arg(H):- compound_name_arguments(H,F,Args), \+ atom_concat('$',_,F), write_src([F|Args]),!.
%mw_print_arg(neo_P(X,Y,Z)):- write_src([neo_P,X,Y,Z]),!.

mw_write_src(S):- !,
  \+ \+ (mwnv(S),
  with_write_override(mw_print_arg,write_src_wi(S))), nl.
%mw_write_src(S):- p2m(S, X), !, write_src_wi(X), nl.

wm_write_vars(Vars):- \+ \+ (mwnv(Vars), mw_cols(mw_print_ans, Vars)).

mw_note(Note):- ignore(format(user_error, '~N% MW-NOTE: ~q. ~N', [Note])).
%cmd_note(Cmd):- write_src_nl(time(Cmd)).
cmd_note(Cmd):- mw_note(time(Cmd)).

:- cmd_note((use_mw_directory(neo4j_out_v3_mw))).
use_mw_directory(KB) :-
    load_mw_data(KB),
    link_mw_data(KB).

:- cmd_note((load_mw_data(neo4j_out_v3_mw))).
% Recursively find and use all _mw.pl files in a given base directory
load_mw_data(KB) :-
    expand_file_name(KB, [KBExpanded]), % Expand '~' or other paths
    load_mw_info(KB),
    directory_files_ending_with(KBExpanded, '.qlf', Files),
    length(Files, Len),
    mw_note(KBExpanded=Len),
    maplist(load_mw_file(KB), Files), !.

is_global_linkage(GL):- text_contains_text(GL, '/global_links/').

:- dynamic(mw_file/2).
load_mw_file(KB, File):- mw_file(KB, File), !.
load_mw_file(_, File):- is_global_linkage(File),!. % prevents about 40gb of 70 seconds of load time
load_mw_file(KB, File):-
    assert(mw_file(KB, File)),
    mw_note(start(load_mw_file(KB, File))),
    ensure_loaded(File),
    mw_note(ended(load_mw_file(KB, File))), !.

:- cmd_note((load_mw_info(neo4j_out_v3_mw))).
load_mw_info(KB):- expand_file_name(KB, [KBExpanded]), % Expand '~' or other paths
    KBExpanded\==KB, !, load_mw_info(KBExpanded).
load_mw_info(KB) :-
    exists_directory(KB),
    directory_files(KB, Files), !, Files \== [],
    include(is_file_extension('txt'), Files, TxtFiles),
    maplist(load_mw_info_file(KB), TxtFiles),
    !.

is_file_extension(Ext, File) :-
    file_name_extension(_, Ext, File).

load_mw_info_file(KB, File) :-
    directory_file_path(KB, File, FullPath),
    setup_call_cleanup(
        open(FullPath, read, Stream),
        stream_mw_info(KB, Stream),
        close(Stream)
    ).


:- dynamic(mw_scheme_info/2).
stream_mw_info(KB, Stream) :-
   repeat,
    read(Stream, Term),
    ( Term == end_of_file
      -> !
    ; ( assert_neo_new(KB, mw_scheme_info(KB, Term)),
        fail)
    ).

assert_neo_new(_KB, Term):- mw_writeq(Term), fail.
assert_neo_new(_KB, Term):- neo_clause_asserted(Term), !.
assert_neo_new(_KB, Term):- assert(Term).

neo_clause_asserted(H:-B):- !, copy_term(H+B,HH+BB),clause(HH,BB,Ref),clause(HHH,BBB,Ref),(HHH+BBB)=@=(H+B),!.
neo_clause_asserted(H):- !, copy_term(H+true,HH+BB),clause(HH,BB,Ref),clause(HHH,BBB,Ref),(HHH+BBB)=@=(H+true),!.

select_id_field(List, Subj, Rest):- priority_id_name(Subj), select(Subj, List, Rest).
priority_id_name('id').
priority_id_name('source_id').
priority_id_name(_).

% Recursively find and use all CSV files in a given base directory
use_csv_directory(KB) :-
    expand_file_name(KB, [KBExpanded]), % Expand '~' or other paths
    %absolute_file_name(column_analysis_log, InfoFile, [relative_to(KBExpanded), file_type(prolog)]),
    %load_mw_info(KB, InfoFile),
    directory_files_ending_with(KBExpanded, '.csv', CsvFiles),
    maplist(use_csv_file, CsvFiles).

% Recursively find files matching a given extension
directory_files_ending_with(KBExpanded, Pattern, Files) :-
    directory_files_ending_with(KBExpanded, Pattern, [], Files).

% Recursive helper predicate
directory_files_ending_with(Dir, Pattern, Acc, Files) :-
    exists_directory(Dir),
    directory_files(Dir, Entries),
    exclude(is_special_directory_dfew, Entries, CleanEntries),
    maplist({Dir}/[Entry, Path]>>atomic_list_concat([Dir, '/', Entry], Path), CleanEntries, Paths),
    partition(exists_directory, Paths, Dirs, FilePaths),
    include({Pattern}/[FilePath]>>sub_atom(FilePath, _, _, 0, Pattern), FilePaths, MatchedFiles),
    append(Acc, MatchedFiles, NewAcc),
    foldl(directory_files_ending_with_helper(Pattern), Dirs, NewAcc, Files).

% Fold helper predicate
directory_files_ending_with_helper(Pattern, Dir, AccIn, AccOut) :-
    directory_files_ending_with(Dir, Pattern, AccIn, AccOut).

% Helper predicate to filter out '.' and '..'
is_special_directory_dfew('.').
is_special_directory_dfew('..').



% Process a single CSV file
use_csv_file(CsvFile) :-
    file_name_extension(Base, _, CsvFile), % Extract base name
    atom_string(Predicate, Base),        % Convert to atom for predicate name
    csv_read_file(CsvFile, Rows, [functor(Predicate)]), % Use as functor
    replace_extension(CsvFile, '.pl', PlFile),
    open(PlFile, write, Stream),
    write_prolog_terms(Stream, Rows),
    close(Stream),
    format('Converted ~w -> ~w~n', [CsvFile, PlFile]).

% Replace the extension of a file
replace_extension(File, NewExt, NewFile) :-
    file_name_extension(Base, _, File),
    atom_concat(Base, NewExt, NewFile).

% Write Prolog facts to a file
write_prolog_terms(_, []).
write_prolog_terms(Stream, [Row | Rows]) :-
    write_term(Stream, Row, [fullstop(true), nl(true)]),
    write_prolog_terms(Stream, Rows).

:- abolish(query_summary/5).
:- discontiguous(query_summary/5).
:- discontiguous(sample_query/5).
% Example usage:
% ?- use_mw_directory(neo4j_out_v4_mw).
% query_summary(QueryID, ExecutionTimeSeconds, ResultCount, NodeCount, EdgeCount).
query_summary(1, stopped_after(4980), 151956, 1908, 1882).  % Stopped after 83 min (4980 sec)
query_summary(2, about(180), 41536, 144, 143).      % 3 min (180 sec)
query_summary(3, stopped_after(1200), too_many_results, unknown, unknown). % Stopped after 20 min (1200 sec)
query_summary(4, stopped_after(2400), 794742, 2693, 2698).  % Stopped after 40 min (2400 sec)
query_summary(5, 25, 1932, 45, 49).          % 25 sec
query_summary(6, squiggly_mark, 5790204:adam_says('didnt run would be ~56gb results'), 3042, 3046). % Unknown time
query_summary(7, 8, 142, 106, 284).          % 8 sec
query_summary(8, 420, 62248, 2040, 4047).    % 7 min (420 sec)
query_summary(9, 6, 1827, 269, 268).         % 6 sec
query_summary(10, stopped_after(3000), 40172978, 51599, 1427846). % Stopped after 50 min (3000 sec)
query_summary(11, 1, 42, 52, 51).            % 1 sec
query_summary(12, 3, 445, 446, 445).         % 3 sec
query_summary(13, unknown, 58854894, 18900, 377127). % Unknown time
query_summary(14, 5, 11967, 5665, 11996).    % 5 sec

sample_query(s2a-"Saulo - returns 2 answers ",
    "MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.id, e.id",
    ['T', 'E'],
    [T, E], [
    neo(T:transcript,transcript_id,'ENST00000472835.1'),
    neo(T:transcript,includes,E:exon)]).
query_summary(s2a, 0.0001, 2, 3, 2).

sample_query(s2b-"Saulo - returns 2 answers ",
    "MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.transcript_name, e.exon_id",
    ['TranscriptName', 'ExonID'],
    [TranscriptName, ExonID], [
    neo(T:transcript,transcript_id,'ENST00000472835.1'),
    neo(T:transcript,includes,E:exon),
    neo(T:transcript,transcript_name,TranscriptName),
    neo(E:exon,exon_id,ExonID)]).
query_summary(s2b, 0.0001, 2, 3, 2).


sample_query(s502a-"Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc",
    "MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target",
    ['Gene1', 'Gene2'],
    [Gene1, Gene2], [
    neo(Gene1,gene_name,"FTO"),
    neo(Gene2,regulates,Gene1)]).
query_summary(s502a, 0.018, 502, 503, 503).

sample_query(s502b-"Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc",
    "MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target",
    ['Regulator', 'Target'],
    [Regulator, Target], [
    neo(Gene1:gene,gene_name,"FTO"),
    neo(Gene2:gene,regulates,Gene1),
    neo(Gene1:gene,gene_name,Target),
    neo(Gene2:gene,gene_name,Regulator)]).
query_summary(s502b, 0.028, 502, 503, 503).

sample_query(1-"1. Find Interactions of BRCA2 Gene",
    "Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways.",
    ['Gene1', 'Transcript1', 'Protein1', 'Protein2', 'Pathway1'],
    [Gene, Transcript, Protein1, Protein2, Pathway], [
    neo(Gene, gene_name, startsWith("BRCA2")),
    neo(Gene, transcribed_to, Transcript),
    neo(Transcript, translates_to, Protein1),
    neo(Protein1, interacts_with, Protein2),
    neo(Gene, genes_pathways, Pathway)]).


sample_query(2-"2. Find Components Associated with IGF2",
    "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene.",
    ['Promoter', 'Gene', 'Enhancer', 'Pathway', 'Pathway1Child'],
    [Promoter, Gene, Enhancer, Pathway, Pathway1Child], [
    neo(Gene:gene, gene_name, startsWith("IGF2")),
    neo(Promoter:promoter, associated_with, Gene:gene),
    neo(Enhancer:enhancer, associated_with, Gene:gene),
    neo(Gene:gene, genes_pathways, Pathway:pathway),
    neo(Pathway1Child:pathway, child_pathway_of, Pathway:pathway)]).

sample_query(3-"3. Gene Interactions and GO Terms",
    "Find gene interactions and associated GO terms including proteins and transcripts.",
    ['Gene', 'Transcript', 'Exon', 'Protein1', 'Protein2', 'GO1Term'],
    [Gene, Transcript, Exon, Protein1, Protein2, GO1Term], [
    neo(Gene, transcribed_to, Transcript),
    neo(Transcript, includes, Exon),
    neo(Protein1, translation_of, Transcript),
    neo(Protein1, interacts_with, Protein2),
    neo(GO1Term, go_gene_product, Protein1)]).

sample_query(4-"4. Interactions Involving 1433B Protein",
    "Find interactions involving 1433B protein including transcripts, exons, and GO terms.",
    ['Gene', 'Transcript', 'Exon', 'Protein1', 'Protein2', 'GO1Term'],
    [Gene, Transcript, Exon, Protein1, Protein2, GO1Term], [
    neo(Protein1, protein_name, startsWith("1433B")),
    neo(Gene, transcribed_to, Transcript:transcript),
    neo(Transcript:transcript, includes, Exon),
    neo(Protein1, translation_of, Transcript:transcript),
    neo(Protein1, interacts_with, Protein2),
    neo(GO1Term, go_gene_product, Protein1)]).

sample_query(5-"5. Components Associated with IGF1",
    "Find enhancers, pathways, and transcripts associated with the IGF1 gene.",
    ['Gene', 'Pathway', 'Enhancer', 'Transcript1', 'Protein'],
    [Gene, Pathway, Enhancer, Transcript, Protein], [
    neo(Gene:gene, gene_name, "IGF1"),
    neo(Gene:gene, genes_pathways, Pathway),
    neo(Enhancer:enhancer, associated_with, Gene:gene),
    neo(Transcript:transcript, transcribed_from, Gene:gene),
    neo(Transcript:transcript, translates_to, Protein:protein)]).

sample_query(6-"6. Pathways and Protein Interactions for IGF1",
    "Find pathways and interacting proteins for the IGF1 gene including all associated components.",
    ['Gene', 'Pathway', 'Enhancer', 'Transcript1', 'Protein1', 'Protein2'],
    [Gene, Pathway, Enhancer, Transcript, Protein1, Protein2], [
    neo(Gene:gene, gene_name, "IGF1"),
    neo(Gene:gene, genes_pathways, Pathway),
    neo(Enhancer:enhancer, associated_with, Gene:gene),
    neo(Transcript:transcript, transcribed_from, Gene:gene),
    neo(Transcript:transcript, translates_to, Protein1),
    neo(Protein1, interacts_with, Protein2)]).

sample_query(7-"7. Transcripts and Exons for TP73-AS1",
    "Find transcripts and exons associated with the TP73-AS1 gene.",
    ['Transcript', 'Exon', 'Gene'],
    [Transcript, Exon, Gene], [
    neo(Transcript:transcript, includes, Exon:exon),
    neo(Transcript:transcript, transcribed_from, Gene:gene),
    neo(Gene:gene, gene_name, endsWith("TP73-AS1"))]).

sample_query(8-"8. Interactions Involving 1433S Protein",
    "Find proteins interacting with 1433S and associated GO terms.",
    ['GO1Term', 'Protein1', 'Protein2'],
    [GO1Term, Protein1, Protein2], [
    neo(Protein1, protein_name, stringEqual("1433S")),
    neo(GO1Term, go_gene_product, Protein1),
    neo(Protein1, interacts_with, Protein2)]).

sample_query(9-"9. IGF1 Expression in Tissues and Transcripts",
    "Find IGF1 expression in tissues and related transcripts.",
    ['Gene', 'Uberon', 'Transcript'],
    [Gene, Uberon, Transcript], [
    neo(Gene, gene_name, startsWith("IGF1")),
    neo(Gene, expressed_in, Uberon),
    neo(Gene, transcribed_to, Transcript)]).

sample_query(10-"10. Transcripts and Exons on Chromosome 1",
    "Find transcripts, exons, and interacting proteins located on chromosome 1.",
    ['Transcript', 'Exon', 'Protein1', 'Protein2'],
    [Transcript, Exon, Protein1, Protein2], [
    neo(Exon, chr, chr1),
    neo(Transcript, includes, Exon),
    neo(Transcript, translates_to, Protein1),
    neo(Protein2, interacts_with, Protein1)]).

sample_query(11-"11. IGF1 Gene Expression in Cell Lines",
    "Find IGF1 gene expression in cell lines and related subclass relationships.",
    ['Gene', 'CL1', 'CL2'],
    [Gene, CL1, CL2], [
    neo(Gene, gene_name, "IGF1"),
    neo(Gene, expressed_in, CL1),
    neo(CL2, subclass_of, CL1)]).

sample_query(12-"12. IGF1 Gene Regulation by SNP Activity",
    "Find regulation of the IGF1 gene by SNP activity.",
    ['SNP', 'Gene'],
    [SNP, Gene], [
    neo(Gene, gene_name, startsWith("IGF1")),
    neo(SNP, activity_by_contact, Gene)]).

sample_query(13-"13. IGF1 Gene Interactions and Regulations",
    "Find IGF1 gene interactions, regulations, and pathways including transcripts and proteins.",
    ['Gene', 'CL1', 'CL2', 'Gene2Regulating', 'Transcript', 'Protein1', 'Protein2'],
    [Gene, CellLine1, CellLine2, RegulatingGene, Transcript, Protein1, Protein2], [
    neo(Gene, gene_name, startsWith("IGF1")),
    neo(Gene, expressed_in, CellLine1:cl),
    neo(CellLine2:cl, subclass_of, CellLine1:cl),
    neo(RegulatingGene, regulates, Gene),
    neo(RegulatingGene, transcribed_to, Transcript),
    neo(Transcript, translates_to, Protein1),
    neo(Protein2, interacts_with, Protein1)]).

sample_query(14-"14. Pathway Associations for SNAP25",
    "Locate SNAP25 in pathways with other genes.",
    ['Gene1', 'Pathway', 'Gene2'],
    [Gene1, Pathway, Gene2], [
    neo(Gene1, gene_name, "SNAP25"),
    neo(Gene1, genes_pathways, Pathway),
    neo(Gene2, genes_pathways, Pathway)]).



clear_linked:-
  forall(is_registered_link(F, A), clear_linked(F, A)).
clear_linked(F, A):- dynamic(F/A), functor(P, F, A), retractall(P).

dyn_linked:-
  forall(is_registered_link(F, A), dyn_linked(F, A)).
dyn_linked(F, A):- dynamic(F/A).


:- cmd_note(show_linked).
show_linked:-  show_linked(3).
:- cmd_note(show_linked(1)).
show_linked(Max) :- forall(is_registered_link(F, A), show_linked(F, A, Max)).
% Predicate to display the rules and up to Max facts of a predicate P
show_linked(F, A, Max) :- functor(P, F, A), predicate_property(P, number_of_clauses(N)), N>0, !,
   ignore((N<100, findall((P:-B), (clause(P, B), B\==true), L), sort(L, S), forall(member((Term), S), mw_writeq(Term)))),
   nop((ignore((predicate_property(P, number_of_rules(0))
           %->forall(limit(Max, P), mw_writeq(P))
           ->forall(limit(Max, clause(P, true)), mw_writeq(P))
            ;forall(limit(Max, clause(P, true)), mw_writeq(P)))))).
show_linked(F, A, _Max):- nop(format('No Facts/Rules: ~q~n', [F/A])), !.


mtc:-
 eval("!(let ($m $n) (ensg00000017427 cl_0000576) (match &neo4j_out_v3 (neo $m $r $n) (result $m $r $n)))").

mtc2:-
  eval("
!(let ($m $n)
     ((superpose (ensg00000017427 cl_0000576))
      (superpose (ensg00000017427 cl_0000576)))
   (match &neo4j_out_v3 (neo $m $r $n) (result $m $r $n)))").

:- cmd_note(show_three).
show_three:- make,
   mw_stats,
   forall(each_table(T), ignore((mw_scheme_info(_, predicate_schema(T, List)), show_three(T, List)))).

show_three(T, List):-
 must_det_lls((
   length(List, A),
   format('~N~n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n', []),
   TList=..[T|List],
   format('~N%     ~w.~n', [TList]),
   forall(mw_scheme_info(_, analysis_source_file(T, File, Size)), mw_writeq(File=Size)),
   show_linked(T, A, 3), !,
   forall(table_types(T, ET), mw_writeq(ET)),
   table_label(T, Label),
   forall(mw_scheme_info(_, analysis_column(T, F, S, Type, VL, VH)), mw_cols(mw_print,  ([v(Type- VL- VH), f(F, Label), S, T]))),

   %A\==2,
   format('~N~n', []),
   mw_writeq(neo_props(List):- types(TList)),
   link_mw_term(_KB, predicate_schema(T, List)),
   format('~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n', []))), !.


:- cmd_note((link_mw_data(neo4j_out_v3_mw))).
link_mw_data(KB):-
    %abolish(neo_triple/3),
    clear_linked, dyn_linked, !,
    forall(mw_scheme_info(KB, Term),
         must_link_mw_term(KB, Term)),
    show_linked.

mw_note_xtreme(_).


show_m2(N):- m2(N,Q),m(Q),writeln(Q).

:- discontiguous m2/2.

m2(2, (neo(_A, _R1, B),
      neo(B, _R2, _C))).

m2(3, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, _D))).

m2(4, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, _F))).

m2(5, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, _F))).

/*
(neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, _F))


m(neo(_A, _R1, B),
  neo(B, _R2, C),
  neo(C, _R3, D),
  neo(D, _R4, E),
  neo(E, _R5, _F))

*/

m2(6, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, F),
      neo(F, _R6, _G))).

m2(7, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, F),
      neo(F, _R6, G),
      neo(G, _R7, _H))).

m7:- eval("
;; basically i am going for
```no-wrap
(let $_
 ((apply different (each-pair ($R1 $R2 $R3 $R4 $R5)))
  (apply different (each-pair ($A $B $C $D $E $F))))
 (limit 3
  (match &neo4j_out_v3
   (,
    (neo $A $R1 $B)
    (neo $B $R2 $C)
    (neo $C $R3 $D)
    (neo $D $R4 $E)
    (neo $E $R5 $F))
  ($A $R1 $B $R2 $C $R3 $D $R4 $E $R5 $F))))
```
;; whichj mines various relationships the will in this case 5 5 steps appart
;; MeTTaLog, which is using SWI-Prolog is using JITI indexing https://www.swi-prolog.org/pldoc/doc_for?object=jiti_list/1
;; so it is much most automatic
").

m2(8, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, F),
      neo(F, _R6, G),
      neo(G, _R7, H),
      neo(H, _R8, _I))).

m2(9, (neo(_A, _R1, B),
      neo(B, _R2, C),
      neo(C, _R3, D),
      neo(D, _R4, E),
      neo(E, _R5, F),
      neo(F, _R6, G),
      neo(G, _R7, H),
      neo(H, _R8, I),
      neo(I, _R9, _J))).

m2(10, (neo(_A, _R1, B),
       neo(B, _R2, C),
       neo(C, _R3, D),
       neo(D, _R4, E),
       neo(E, _R5, F),
       neo(F, _R6, G),
       neo(G, _R7, H),
       neo(H, _R8, I),
       neo(I, _R9, J),
       neo(J, _R10, _K))).

/*
time(limit(3,(neo(A, R1, B),
      neo(B, R2, C),
      neo(C, R3, D),
      neo(D, R4, E),
      neo(E, R5, F),neo(F, R6, G),neo(G, R7, H),neo(H, R8, I),neo(I, R9, J),neo(J, RA, K),all_dif([A,B,C,D,E,F,G,H,I,J,K,R1,R2,R3,R4,R5,R6,R7,R8,R9,RA])))).


 time(limit(3,m(neo(A, R1, B),
      neo(B, R2, C),
      neo(C, R3, D),
      neo(D, R4, E),
      neo(E, R5, F),
      neo(F, R6, G),
      neo(G, R7, H),
      neo(H, R8, I)))).
*/

must_link_mw_term( KB, Term):- link_mw_term(KB, Term), !, mw_note_xtreme(confirmed(do_link_mw_term(KB, Term))).
must_link_mw_term(_KB, Term):- \+ \+ unused_link_mw_term(Term), !.
must_link_mw_term( KB, Term):- mw_note(skipped(must_link_mw_term(KB, Term))), !.

unused_link_mw_term(predicate_schema(_, _)):-!, fail.
unused_link_mw_term(_).

into_cmpd(Symb):- arg(_, v(containsString, all, exists, startsWith, endsWith, stringEqual, caseI, fnR, fnL), Symb).

% Convert various forms to Prolog atoms or terms
neo2p(Q, A) :- string(Q), atom_string(A, Q).
neo2p(Q, A) :- \+ compound(Q), !, Q = A.
neo2p([Fn | Args], A) :- atom(Fn), into_cmpd(Fn), Cmpd =.. [Fn | Args], !, neo2p(Cmpd, A).
neo2p(exists(Q, Reqs), Q) :- !, neo2p(Reqs, Q).
neo2p(caseI(A), Q) :- compound(A), push_to_context(args, caseI, neo2p(A, Q)), !.
neo2p(caseI(A), Q) :- \+ compound(A), push_to_context(args, caseI, neo2p(equal(A), Q)), !.
neo2p(not(A), Q) :- compound(A), push_to_context(call, not, neo2p(A, Q)), !.
neo2p(not(A), Q) :- \+ compound(A), !, dif(A, Q).
neo2p(containsString(A), Q) :- neo2p(fnL(text_contains_text, Q, A), Q).
neo2p(equal(A), Q) :- neo2p(fnL(=, Q, A), Q).
neo2p(startsWith(A), Q) :- neo2p(fnL(text_starts_with, Q, A), Q).
neo2p(endsWith(A), Q) :- neo2p(fnL(text_ends_with, Q, A), Q).
neo2p(stringEqual(A), Q) :- neo2p(fnL(text_equals, Q, A), Q).
neo2p(fnR(P, Q, A), A) :- !, makeTheCall(P, Q, A, TheCall), the_var(A, AV), the_var(Q, QV), optimize_pl_call(freeze(P, freeze_t(QV, freeze_t(AV, TheCall))), TODO), call(TODO).
neo2p(fnL(P, Q, A), Q) :- !, makeTheCall(P, Q, A, TheCall), the_var(A, AV), the_var(Q, QV), optimize_pl_call(freeze(P, freeze_t(QV, freeze_t(AV, TheCall))), TODO), call(TODO).
neo2p(Q, Q).

the_var(A, AV):- var(A), !, AV=A.
the_var(A, AV):- compound(A), arg(1, A, Q), var(Q), !, AV=Q.
the_var(A, A).

freeze_t(AV, TheCall):- ground(AV), !, call(TheCall).
freeze_t(AV, TheCall):- compound(AV), arg(1, AV, V), !, freeze_t(V, TheCall).
freeze_t(AV, TheCall):- var(AV), !, freeze(AV, freeze_t(AV, TheCall)).

call_each_neo(_M, List):-
  call_each_unique(List, _Vars).

unique_res(List,Vars):-
 must_det_lls((
   mark_non_difs(List),
   term_variables(List, IVars),
   all_dif(IVars),
   if_t(var(Vars), Vars=IVars))).

call_each_unique(List, Vars):-
  setup_call_cleanup(notrace((must_det_lls((
   unique_res(List,Vars),
   term_variables(List+Vars, RmDifs),
   nop(no_repeats_var(NRV)))))),
   match_template(List),
   notrace((((NRV=Vars->true;(nop(wdmsg(repeated(Vars))), fail)),
   maplist(remove_dif2, RmDifs))))).

mark_non_difs(Var):- var(Var), !.
mark_non_difs(List):- is_list(List), !, maplist(mark_non_difs, List).
mark_non_difs(neo_P(_, _, _, ND)):- var(ND), put_attr(ND, ldif, list), !.
mark_non_difs(Cmd):- compound(Cmd), compound_name_arguments(Cmd, _, Args), !, maplist(mark_non_difs, Args).
mark_non_difs(_).
ldif:attr_unify_hook(_, _).

remove_dif2(V):- \+ attvar(V), !.
remove_dif2(V):- del_attr(V, dif), del_attr(V, ldif).
/*

  m( neo(Gene:gene, gene_name, startsWith("IGF2")),
    neo_P(Promoter:promoter, associated_with, Gene:gene, A),
    neo_P(Enhancer:enhancer, associated_with, Gene:gene, B),
    neo_P(Gene:gene, genes_pathways, Pathway:pathway, C),
    neo_P(Pathway1Child:pathway, child_pathway_of, Pathway:pathway, D)).




      m( neo(Gene, X, startsWith("IGF2")),
    neo_P(Promoter:promoter, associated_with, Gene:gene, A),
    neo_P(Enhancer:enhancer, associated_with, Gene:gene, B),
    neo_P(Gene:gene, genes_pathways, Pathway:pathway, C),
    neo_P(Pathway1Child:pathway, child_pathway_of, Pathway:pathway, D)).

    */

all_dif([]).     % Base case: empty list is trivially distinct
all_dif([_]).    % Base case: a single element is trivially distinct
all_dif([X | Xs]) :-
    dif_all(X, Xs), % Ensure X is different from every element in Xs
    all_dif(Xs).     % Recursively check the rest of the list

% Ensure X is different from every element in the list
dif_all(_, []).
dif_all(X, [Y | Ys]) :-
    if_t(( \+ is_ldif(X), \+ is_ldif(Y)), dif(X, Y)), % Constraint: X must be different from Y
    dif_all(X, Ys). % Continue checking with the rest

is_ldif(Y):- attvar(Y), get_attr(Y, ldif, _).

produce_list_call(M):-
 forall(between(1, 30, N),
    ( functor(Head, M, N),
      Head =.. [M | Args],
      retractall(Head),
      Body =.. [call_each_neo, M, Args],
      assert_neo_new(_,(Head :- Body))
    )).

:- produce_list_call(m).
:- produce_list_call(n).
:- produce_list_call(o).


% Handles context-based modifications before making the call
makeTheCall(P, Q, A, call(Pop, TheCall)) :- pop_from_context(call, Pop), !, makeTheCall(P, Q, A, TheCall).
makeTheCall(P, Q, A, Goals) :- pop_from_context(args, Pop), !, makeTheCall(P, QQ, AA, TheCall),
   optimize_pl_call((call(Pop, Q, QQ), call(Pop, A, AA), TheCall), Goals).
makeTheCall(P, Q, A, call(P, Q, A)).

optimize_pl_call((A, B), AB):- optimize_pl_call(A, AA), optimize_pl_call(B, BB), (AA\=@=A;BB\=@=B), !, optimize_pl_call((AA, BB), AB).
optimize_pl_call((A, B), AB):- A==true, !, optimize_pl_call(B, AB).
optimize_pl_call((B, A), AB):- A==true, !, optimize_pl_call(B, AB).
optimize_pl_call(call(P, A, B), true):- atom(P), ground(A), var(B), call(P, A, B), !.
optimize_pl_call(call(P, A, B), ABC):- atom(P), !, AB=..[P, A, B], optimize_pl_call(AB, ABC).
optimize_pl_call(call(P, A), ABC):- atom(P), !, AB=..[P, A], optimize_pl_call(AB, ABC).
optimize_pl_call(freeze_t(V, Body), OBody):- ground(V), !, optimize_pl_call(Body, OBody).
optimize_pl_call(freeze_t(V, Body), freeze_t(V, OBody)):- var(V), !, optimize_pl_call(Body, OBody).
optimize_pl_call(freeze(V, Body), OBody):- nonvar(V), !, optimize_pl_call(Body, OBody).
optimize_pl_call(freeze(V, Body), freeze(V, OBody)):- var(V), !, optimize_pl_call(Body, OBody).
optimize_pl_call(AB, AB).

% Convert atom to lowercase
caseI(In, Out) :- to_str(In, Mid), downcase_atom(Mid, Out).

:- dynamic(context_stack/2).

% Push to context stack
push_to_context(Type, Key, Action) :-
    assert(context_stack(Type, Key)),
    call(Action).



% Pop from context stack
pop_from_context(Type, Key) :-
    retract(context_stack(Type, Key)), !.
%pop_from_context(_, _) :- fail. % Ensure failure if not found


% Convert string to atom if needed
to_str(String, Text) :- string(String), atom_string(Text, String), !.
to_str(Symb, Text) :- atom(Symb), !, Symb=Text.
to_str(Term, Text) :- compound(Term), !, arg(1, Term, Symb), to_str(Symb, Text).
to_str(Symb, Symb) :- dmsg(to_str(Symb)), !, fail.

% Check if Symb/String contains SubSymb/String
text_contains_text(A, B) :-
    to_str(A, Symb), to_str(B, SubSymb),
    sub_atom(Symb, _, _, _, SubSymb).

% Check if Symb/String starts with Prefix (Symb or String)
text_starts_with(A, P) :-
    to_str(A, Symb), to_str(P, Prefix),
    sub_atom(Symb, 0, _, After, Prefix), After>=0.

% Check if Symb/String ends with Suffix (Symb or String)
text_ends_with(A, S) :-
    to_str(A, Symb), to_str(S, Suffix),
    sub_atom(Symb, Start, _, 0, Suffix), Start>=0.

% Check if two Symbs/Strings are equal
text_equals(A, B) :-
    to_str(A, SymbA), to_str(B, SymbB),
    SymbA == SymbB.

%neo2p(Q, A):- arg(2, Q, M), nonvar(M), neo2p(M, A), !.
%neo2p(Q, A):- arg(1, Q, M), nonvar(M), neo2p(M, A), !.

text_contains_ci(A, B):- downcase_atom(A, AA), downcase_atom(B, BB), text_contains_text(AA, BB).

metta_atomspace('&neo4j_out_v3', Query):- dfq(Query).

dfq([Neo|Args]):- is_list(Args), match_template([Neo|Args]).


neo(S, P, O):- neo2p(S, S2), neo2p(P, P2), neo2p(O, O2), neo3(S2, P2, O2).
neo_P(S, P, O, Props):- neo2p(S, S2), neo2p(P, P2), neo2p(O, O2), neo4(S2, P2, O2, Props).

s2_s_st(S, I, T):- \+ compound(S), \+ var(S), !, I=S, T=_.
%s2_s_st(S, I, T):- var(S), !, S=I:T.
%s2_s_st(S, I, T):- var(S), !, I=S, T=_.
s2_s_st(I:T, I, T).

neo4(S2, Label, O2, Props):-s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_edge_proplist(ST, TT, Props, Label, O, S)).

neo4(S2, Label, O2, Props):-
    s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_node_prop(Label, ST, S, O)), maybe_typed(ST, S), ignore(maybe_typed(TT, O)),
    ignore(dfi(neo_node_proplist(S, SProps, ST))),
    ignore(dfi(neo_node_proplist(O, OProps, TT))),
    ignore(((OProps=[]))), ignore(((SProps=[]))),
    append(OProps, SProps, Props).



maybe_typed(ST, S):- if_t(nonvar(ST), dfi(neo_node_inst(ST, S))).


neo3a(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), (nonvar(ST);nonvar(TT)),dfi(neo_edge_typed(Label, ST, TT, O, S)).
neo3b(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), var(ST),var(TT),dfi(neo_edge_link(Label, S, O)), maybe_typed(ST, S), maybe_typed(TT, O).
neo3c(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_edge_link(Label, S, O)), maybe_typed(ST, S), maybe_typed(TT, O).
neo3(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_edge_typed(Label, ST, TT, O, S)).
neo3(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_node_prop(Label, ST, S, O)), maybe_typed(ST, S), ignore(maybe_typed(TT, O)).
%neo3(S2, P2, O2):- dfi(neo_node(ST, S)):- Pred)),
%  assert_neo_new(KB, (dfi(neo_node_proplist(ST, Props, S)):- Pred)),
%  assert_neo_new(KB, (dfi(neo_node_prop(F, ST, S, O)):- Pred))))).
neo3(S2, P2, O2):- bi_triple(S2, P2, O2).

mw_stats('metta-atoms', Total):-
  findall(X, analysis_source_file(_, _, X), Each),
  sumlist(Each, Total).

mw_stats('metta-triples', Total):-
  findall(Triples, (analysis_source_file(S, _, X), predicate_schema(S, Fs), length(Fs, Len), Triples is ((Len-1)*X)), Each),
  sumlist(Each, Total).

mw_stats('metta-symbols', Total):- statistics(atoms, Total).

mw_stats('edge-files', Total):- count_of(edges_table(_), Total).
mw_stats('node-files', Total):- count_of(nodes_table(_), Total).
mw_stats('unknown-files', Total):- count_of(unknown_table(_), Total).
mw_stats('edge-count', Total):- findall(Count, (edges_table(T), table_text_count(T, Count)), Counts), sumlist(Counts, Total).
mw_stats('node-count', Total):- findall(Count, (nodes_table(T), table_text_count(T, Count)), Counts), sumlist(Counts, Total).


source_id_f(ID):- \+ atom(ID), !, fail.
source_id_f(id).
source_id_f(source_id).


% dfi = DataFrame Interface
link_mw_term(KB, analysis_source_file(T, GL, _Size)):- is_global_linkage(GL), unknown_table(T),
    Pred=..[T, S, O], !, dynamic(T/2),
    assert_neo_new(KB, (dfi(edge_link(T)))),
    assert_neo_new(KB, (dfi(neo_edge_link(T, S, O)):- Pred)), !.

link_mw_term(KB, predicate_schema(T, List)):- edges_table(T),
  length(List, A), functor(Pred, T, A),
  mw_scheme_info(KB, analysis_column(T, 'label', 1, 'str', Label, _)),
  pred_arg(Pred, 'target_id', O),
  pred_arg(Pred, 'source_id', S),
  ignore(single_value(T, 'source_type', ST)),
 ignore((single_value(T, 'target_type', TT)-> true ; pred_arg(Pred, 'target_type', TT))),
  edge_proplist(T, Proplist), maplist(pred_arg(Pred), Proplist, Args),
  maplist(key_value_equals, Proplist, Args, Props),
  assert_neo_new(KB, (dfi(neo_edge_typed(Label, ST, TT, O, S)):- Pred)),
  assert_neo_new(KB, (dfi(neo_edge_proplist(ST, TT, Props, Label, O, S)):- Pred)),
    if_t(single_value(T, 'source_type', ST),
       assert_neo_new(KB, (dfi(neo_type_of(ST, s, S)):- Pred))),
    if_t(single_value(T, 'target_type', TT),
       assert_neo_new(KB, (dfi(neo_type_of(TT, t, O)):- Pred))),
    !,
  forall(member(F, Proplist),
    ignore((pred_arg(Pred, F, V),
     assert_neo_new(KB, (dfi(edge_prop_for(F, Label)))),
     assert_neo_new(KB, (dfi(edge_prop(F)))),
     assert_neo_new(KB, (dfi(neo_edge_prop(F, Label, S, O, V)):- Pred))))).


link_mw_term(KB, predicate_schema(T, List)):- nodes_table(T),
  length(List, A), functor(Pred, T, A),
  pred_arg(Pred, 'id', S),
  single_value(T, 'label', ST),
  edge_proplist(T, Proplist), maplist(pred_arg(Pred), Proplist, Args),
  maplist(key_value_equals, Proplist, Args, Props),
  assert_neo_new(KB, (dfi(neo_node_inst(ST, S)):- Pred)),
  assert_neo_new(KB, (dfi(neo_node_proplist(ST, Props, S)):- Pred)),
  forall(member(F, Proplist),
  ignore((pred_arg(Pred, F, O),
   assert_neo_new(KB, (dfi(node_prop_for(F, ST)))),
   assert_neo_new(KB, (dfi(node_prop(F)))),
   assert_neo_new(KB, (dfi(neo_node_prop(F, ST, S, O)):- Pred))))).

/*
edges_type_c(T, edges_table):- edges_table(T).
edges_type_c(T, nodes_table):- nodes_table(T).
edges_type_c(T, unknown_table):- unknown_table(T).
edges_type_c(T, rel_type(Val)):- analysis_column(T, 'rel_type', _, _, Val, _), !.
*/
%edges_type_c(T, labled_edge):- removed_columns(T, ['source_type', 'target_type', 'label']).
%edges_type_c(T, labled_edge_onto):- removed_columns(T, ['source_type', 'target_type', 'label', 'rel_type']).
edges_type_c(T, edge_no_prop):- predicate_schema(T, ['source_id', 'target_id']), !.
edges_type_c(T, edge_w_props([E|Extra])):- predicate_schema(T, ['source_id', 'target_id', E|Extra]), !.
edges_type_c(T, oddly_labled_uberon_edge):- removed_columns(T, ['source_type', 'label']), !.

table_type_c(T, unique(ID)):-  table_text_count(T, Count), analysis_column(T, ID, Count, _, _, _).
table_type_c(T, divider(ID)):- table_text_count(T, Count), analysis_column(T, ID, N, _, _, _), N>1, Count>N*4 , \+ source_id_f(ID).
table_type_c(T, per_id(ID)):-  table_text_count(T, Count), analysis_column(T, ID, N, _, _, _), N>1, Count>N*2, \+ (Count>N*4).
%analysis_column('cadd_nodes_snp', 'id', 756_256, 'str', 'rs1000014', 'rs999896')
table_type_c(T, has_float):- \+ \+ analysis_column(T, _, _, 'float', _, _).

nodes_type_c(T, node_no_id):- \+ analysis_column(T, id, _N, _, _, _).

any_type_c(T, TT):- edges_type_c(T, TT).
any_type_c(T, TT):- table_type_c(T, TT).
any_type_c(T, TT):- nodes_type_c(T, TT).

table_types(T, ET):-each_table(T), findall(TT, table_type(T, TT), TL), once(ET=TL;forall(member(E, ET), member(E, TL))).

'node_type'(T, TT):- nodes_table(T), table_type(T, TT).
edge_type(T, TT):- edges_table(T), table_type(T, TT).

table_type(T, TT):- var(T), !, each_table(T), table_type(T, TT).
table_type(T, TT):- var(TT), edges_table(T), !, ((each_edge_type(TT), any_type_c(T, TT))*->true;TT=unknown).
table_type(T, TT):- var(TT), nodes_table(T), !, ((each_node_type(TT), any_type_c(T, TT))*->true;TT=unknown).
table_type(T, TT):- var(TT), !, ((each_table_type(TT), any_type_c(T, TT))*->true;TT=unknown).
table_type(T, TT):- TT == unknown, !, \+ any_type_c(T, _).
table_type(T, TT):- any_type_c(T, TT).

unknown_table(T):- analysis_source_file(T, _, _), \+ edges_table(T), \+ nodes_table(T).

edges_table(T):- analysis_source_file(T, _, _), text_contains_text(T, "edges"), \+ text_contains_text(T, "nodes").
edges_label(L):- findall(Label, (edges_table(T), table_label(T, Label)), Labels), list_to_set(Labels, Set), member(L, Set).
edges_label(T, Label, Count):- edges_table(T), table_text_count(T, Count), table_label(T, Label).
edges_label(Label, Total):- edges_label(Label), findall(Count, edges_label(_, Label, Count), Counts), sumlist(Counts, Total).

nodes_table(T):- analysis_source_file(T, _, _), text_contains_text(T, "nodes"), \+ text_contains_text(T, "edges").
nodes_label(L):- findall(Label, (nodes_table(T), table_label(T, Label)), Labels), list_to_set(Labels, Set), member(L, Set).
nodes_label(T, Label, Count):- nodes_table(T), table_text_count(T, Count), table_label(T, Label).
nodes_label(Label, Total):- nodes_label(Label), findall(Count, nodes_label(_, Label, Count), Counts), sumlist(Counts, Total).


%(a1(5);a2(9)), (b1(23);b2(2);b3(16)), (c1(20);c2(30);c3(10))

%a1, (b1;b2;b3), (c1;c2;c3);a2, (b1;b2;b3), (c1;c2;c3)

inverses_table(X1, X2):- edges_label(X1, Y1, Z), edges_label(X2, Y2, Z), X1@>X2, Y1\==Y2.

table_text_count(T, Count):- analysis_source_file(T, _, Count).
table_label(T, Label):- analysis_column(T, 'label', _, _, Label, _).
each_table(T):- edges_table(T).
each_table(T):- nodes_table(T).
each_table(T):- unknown_table(T).
% Retrieve all distinct predicate heads for table_type/2
each_edge_type(TT):- clause(edges_type_c(_, TT), _).
each_edge_type(TT):- each_table_type(TT).
each_node_type(TT):- clause(nodes_type_c(_, TT), _).
each_node_type(TT):- each_table_type(TT).
each_table_type(TT):- clause(table_type_c(_, TT), _).


%mw_stats('metta-memory', Total):- statistics(memory, Total).

:- cmd_note(mw_stats).
mw_stats:-
  forall(mw_stats(X, Y),
     mw_write_src(X=Y)).


analysis_source_file(S, F, X):- mw_scheme_info(_, analysis_source_file(S, F, X)).
predicate_schema(S, F):- mw_scheme_info(_, predicate_schema(S, F)).
removed_columns(S, F):- mw_scheme_info(_, removed_columns(S, F)).
analysis_column(T, F, S, Type, VL, VH):- mw_scheme_info(_, analysis_column(T, F, S, Type, VL, VH)).
different(X, Y):- dif(X, Y).

:- cmd_note(sample_query).
% Predicate to run all sample queries using the defined sample_query predicates
sample_query:-
    sample_query(1,40).

sample_query(N):-
    sample_query(N,N).

sample_query(S,E):-
    if_t(var(S),S=1),if_t(var(E),E=40),
    forall((call_nth(sample_query(Name, Desc, VNs, Vars, Body), Nth), Nth>=S, Nth=<E),
           run_sample_query(Name, Desc, VNs, Vars, Body)).

mw_set_varname(V, N):- ignore('$VAR'(N)=V).

mw_set_varname_and_type(Body,Var, N):- directly_in_pred(Body,Var),Var=V:Type, ignore('$VAR'(N)=V),name_to_type(N,Type),!.
mw_set_varname_and_type(_Body,V, N):- ignore('$VAR'(N)=V).

directly_in_pred(Body,Var):- sub_term(Found,Body),compound(Found),arg(_,Found,V),V==Var, !, \+ functor(Found,':',_).

name_to_type(Name,_Type):- atom_concat('PropList',_,Name),!,fail.
name_to_type(Name,_Type):- atom_length(Name,Len), Len<5,!,fail.
name_to_type(Name,Type):-
    downcase_atom(Name,DC),atom_chars(DC,[C|Chars]),append(TypeChars,[N|_],Chars),char_type(N,digit),
    atom_chars(Type,[C|TypeChars]),!.
name_to_type(Name,Type):- downcase_atom(Name,Type),!.

mw_set_type(_:Type, N):-
        downcase_atom(N,DC),atom_chars(DC,[C|Chars]),append(TypeChars,[N|_],Chars),char_type(N,digit),!,
        atom_chars(Type,[C|TypeChars]).
mw_set_type(_:Type, N):- downcase_atom(N,Type).


% Predicate to execute and print results of a single sample query with a time limit

add_props(L1, L1):- var(L1),!.
add_props(L1, L2):- (atom(L1);string(L1)), atom_concat(L1, ' (With Properties)', L2), !.
add_props(N-L1, N-L2):- !, add_props(L1, L2).
add_props(L1, L2):- \+ compound(L1), !, L1=L2.
add_props(neo(S, P, O), neo_P(S, P, O, _)):-!.
add_props(L1, L1).

remove_eqs(Vars2, Vars, NewVars) :-
    exclude(var_in_list(Vars), Vars2, NewVars).

var_in_list(List, Var) :-
    member(ExistingVar, List),
    ExistingVar == Var.  % Strict equality check (no unification)


run_sample_query(Name, Desc, VNs, Vars, Body):-
  \+ \+ run_sample_query1(60,Name, Desc, VNs, Vars, Body),
  must_det_lls((maplist(add_props, [Name, Desc| Body], [Name2, Desc2| Body2]), !,
   term_variables(Body2, AllVars), remove_eqs(AllVars, Vars, NewVars),
   length(NewVars, N), generate_var_names(N, NewVNs),
   append(Vars, NewVars, Vars2), append(VNs, NewVNs, VNs2))),
  %nop
  (run_sample_query1(60,Name2, Desc2, VNs2, Vars2, Body2)), !.

generate_var_names(N, Names) :-
    findall(Prop, (between(1, N, I), atomic_list_concat(['PropList', I], Prop)), Names).

number_and_name(Num-Name,Num,Name):- nonvar(Num),!.
number_and_name(Name,unknown,Name).

run_sample_query1(Time, NName, Desc, VNs, Vars, Body):-
    nl, nl, nl, nl,
    number_and_name(NName,Num,Name),
    format('### ~w~n',[Name]),
    writeln('```no-wrap'),
    writeln('================================================================='),
    format('\t~w~n', [Desc]), % Prints the name and description of the query
    %maplist(mw_set_type, Vars, VNs),
    \+ \+
    ( copy_term(Vars,RVars),
      maplist(mw_set_varname_and_type(Body), Vars, VNs),
      maplist(mw_set_varname, RVars, VNs),
      Result =.. [result|Vars], nl,
      conj_as_comma(Body,CBody),
      mw_write_src(match('&neo4j_out_v3', CBody, Result)), nl), !,
    execute_query_tl(Body, VNs, Vars, Time),
    stats_about(Num),
    writeln('================================================================='),
    writeln('```').

conj_as_comma(Body,CBody):- is_list(Body),CBody =.. [','| Body],!.
conj_as_comma(Body,CBody):- conjuncts_to_list(Body,LBody),CBody =.. [','|LBody],!.

stats_about(QueryID):-
    forall(query_summary(QueryID, ExecutionTimeSeconds, ResultCount, NodeCount, EdgeCount),
    format('Neo4J Query ~w: Execution Time: ~w sec | Rows: ~w | Nodes: ~w | Edges: ~w~n',
           [QueryID, ExecutionTimeSeconds, ResultCount, NodeCount, EdgeCount])).

execute_query_tl(Body, VNs, Vars, Time):-
    time(   % Attempt to execute the query within a Time-second time limit
          catch(call_with_time_limit(Time, execute_query(Body, VNs, Vars)),
                time_limit_exceeded,
                format(' Time limit ~w seconds exceeded! ~n', [Time]))
      ;   % If no more results or after handling time limit exceeded
                format(' Failed ~w~n', [Body])), !,
    flag(result_count, Length, Length), % Get the number of results
    nb_current(last_result_at, ResultTimeTaken),
    format(' Last answer found: ~2f seconds~n', [ResultTimeTaken]),
    format(' Number of answers: ~D~n', [Length]), !.


% Helper predicate to execute the query, record and print execution time and number of results
execute_query(Body, _VNs, Vars):-
    flag(result_count, _, 0),
    clear_results_for(Body),
    statistics(cputime, StartTime), % Start timing
    nb_setval(last_result_at, -0.0),
    forall(call_each_unique(Body, Vars), % Execute the query and collect all results
       (statistics(cputime, ThisTime),
        ResultTimeTaken is ThisTime - StartTime,
        nb_setval(last_result_at, ResultTimeTaken),
        ignore((% new_result_or_fail(Body,Vars),
          flag(result_count, N, N+1),
          if_t(show_nth_answer(N), wm_write_vars(Vars)))),
        true)),
    statistics(cputime, EndTime), % End timing
    TimeTaken is EndTime - StartTime, % Calculate time taken
    format(' MeTTaLog Execution time: ~2f seconds~n', [TimeTaken]).


/*
clear_results_for(_Body):- forall(recorded(each_results_for, _Vars, Ref),erase(Ref)).
new_result_or_fail(Body,Vars):- \+ (numbervars(Vars,0,_,[attvar(bind)]),new_result_succeed(Body,Vars)).
new_result_succeed(_Body,Vars):- recorded(each_results_for,Vars),!,fail.
new_result_succeed(_Body,Vars):- recorda(each_results_for,Vars,_Ref),!.
*/
:- dynamic each_results_for/1.
% Clears all entries for each_results_for.
clear_results_for(_Body) :-  retractall(each_results_for(_)).
% Checks if a result already exists or fails otherwise.
new_result_or_fail(Body, Vars) :- \+ (numbervars(Vars, 0, _, [attvar(bind)]), new_result_succeed(Body, Vars)).
% Fails if the result already exists, simulating a cut.
new_result_succeed(_Body, Vars) :- each_results_for(Vars), !, fail.
% Adds a new result if it does not exist.
new_result_succeed(_Body, Vars) :- assertz(each_results_for(Vars)).


show_nth_answer(0). show_nth_answer(1). show_nth_answer(2).
show_nth_answer(100). show_nth_answer(101). show_nth_answer(103).
show_nth_answer(500). show_nth_answer(1000). show_nth_answer(10_000).
show_nth_answer(500_000). show_nth_answer(1_000_000). show_nth_answer(5_000_000).



best_with_rest( List, Compare, Best, Rest):- best_first_reorder(List, Compare, [Best|Rest]), !.
best_with_rest([H|T], Compare, Best, Rest) :-
    foldl(best_accumulate(Compare), T, H, Best), % Find the best element
    select_first(==(Best), [H|T], Rest).          % Remove the first occurrence

best_first_reorder([], _, []).
best_first_reorder(List, Compare, [Best|RestReordered]) :-
    best_with_rest(List, Compare, Best, Rest),
    best_first_reorder(Rest, Compare, RestReordered).

best_accumulate(Compare, X, CurrentBest, NewBest) :-
    call(Compare, Order, X, CurrentBest),
    ( Order == (<) -> NewBest = X ; NewBest = CurrentBest ).

select_first(Pred, [H|T], T) :- call(Pred, H), !.  % Remove first occurrence
select_first(Pred, [H|T], [H|Rest]) :- select_first(Pred, T, Rest).

match_template([]):-!.
match_template([Pred|Args]):- is_list(Args), atom(Pred), !, (Pred==',' -> match_template(Args);(Call=..[Pred|Args], match_template([Call]))).
match_template([Low]):- mw_write_call(Low), !, match_call(Low).
match_template(Body):- is_list(Body), !, do_first(Body, Low, Rest), mw_write_call(Low), !, match_call(Low), match_template(Rest).
match_template(Call):- match_call(Call).
%match_template(Body):- best_with_rest(Body, better_first, Low, Rest), mw_writeq(call(Low)), !, call(Low), match_template(Rest).

mw_write_call(_Call).

match_call([Pred|Args]):- atom(Pred), !, (Pred==',' -> match_template(Args);(Call=..[Pred|Args], match_call(Call))).
match_call(Call):- call(Call). % catch(Call, E, (wdmsg(E=Call), !, (atomic(E)->throw(E);(fail, rtrace(Call))))).

var_count(Item, Count) :-
    term_variables(Item, Vars),
    length(Vars, Count).

sols_count(Item, Count) :- arg(2, Item, Label), nonvar(Label), edges_label(Label, Count), !.
sols_count(Item, Count) :- arg(3, Item, Label), nonvar(Label), nodes_label(Label, Count), !.
sols_count(Item, Count) :- arg(1, Item, Label), nonvar(Label), nodes_label(Label, Count), !.
sols_count(_Item, Count) :- Count = 100_000_000.

do_first(Body, Low, Rest):- Body = [Low|Rest], !.
%do_first(Body, Low, Rest):- predsort(better_first, Body, [Low|Rest]), !.

better_first(R, Item1, Item2):- sols_count(Item1, Count1), sols_count(Item2, Count2), compare(R, Count1, Count2), R\== '='.
better_first(R, Item1, Item2):- var_count(Item1, Count1), var_count(Item2, Count2), compare(R, Count1, Count2), R\== '='.
better_first(R, Item1, Item2):- compare(R, Item1, Item2).

bi_triple(subclass_of, rel_type, subclass).
bi_triple(part_of, rel_type, part_of).
bi_triple(capable_of, rel_type, capable_of).








/*
remove_dupes_in_info:-
  clause(analysis_column(A, B, C, D, E, F), true, Ref),
  clause(analysis_column(A, B, C, D, E, F), true, Ref2),
  Ref\==Ref2, erase(Ref2).
  */
/*
previous_predicate_schema(T, [id, label, pathway_name]).
analysis_source_file(T, 'neo4j_out_v3_csv/reactome/nodes_pathway.csv', 2673).
removed_columns(T, ['label']).
analysis_column(T, 'id', 2673, 'str', 'r-hsa-1059683', 'r-hsa-997272').
analysis_column(T, 'label', 1, 'str', 'pathway', 'pathway').
analysis_column(T, 'pathway_name', 2656, 'str', '2-LTR circle formation', 'vRNP Assembly').
predicate_schema(T, [id, pathway_name]).
*/

pred_arg(Pred, F, S):-
   functor(Pred, T, _), predicate_schema(T, List),
   nth1(SubjNth, List, F), arg(SubjNth, Pred, S).

key_value_equals(N, V, N=V).
non_prop(label). % non_prop(label_type).
non_prop(id). non_prop(source_id). non_prop(source_type).
non_prop(target_id). non_prop(target_type).

edge_proplist(T, Proplist):- predicate_schema(T, List), exclude(non_prop, List, Proplist).

single_value(T, F, V):- analysis_column(T, F, 1, _, V, _).


kind_contains(List1, List2):- List1=List2, !.
kind_contains(List1, List2):- forall(member(E, List1), member(E, List2)).

combine_costs(KoC, Costs, Sum):- kind_contains(KoC, [unbound, disjuncts]), sum_list(Costs, Sum).
combine_costs(KoC, Costs, Sum):- kind_contains(KoC, [unbound, conjuncts]), min_list(Costs, Sum).
combine_costs(KoC, Costs, Sum):- kind_contains(KoC, [bound, conjuncts]), max_list(Costs, Sum).
combine_costs(KoC, Costs, Sum):- kind_contains(KoC, [bound, disjuncts]), max_list(Costs, Sum).
combine_costs(_KoC, Costs, Sum):- sum_list(Costs, Sum).

% try to fill in missing search_cost, combine_costs and costs_for

% ```prolog

analysis_nth_column(Functor, Named, Nth, Cost, DataType, LowVal, HighVal):-
   predicate_arg_names(Functor, List), nth1(Nth, List, Named),
   analysis_column(Functor, Named, Cost, DataType, LowVal, HighVal).


predicate_arg_names(Functor, List):-
   if_t(var(Functor), each_table(Functor)),
   (predicate_schema(Functor, List)->true
         ;List=['source_id', 'target_id']).

get_bound_unbound(Pred, Functor, BoundArgs, UnboundArgs) :-
    % For example, loves(joe, mara, Much) => Functor = loves, BoundArgs = [1, 2], UnboundArgs = [3]
    Pred =.. [Functor | Args],
    findall(Index, (nth1(Index, Args, Arg), nonvar(Arg)), BoundArgs),
    findall(Index, (nth1(Index, Args, Arg), var(Arg)), UnboundArgs).

bound_cost(_Functor, _BoundArg, 1).
unbound_cost(T, Nth, Cost):- analysis_nth_column(T, Nth, Nth, Cost, _, _, _).
costs_for(_KoC, BoundCosts, UnboundCosts, Cost) :-
   sum_list(BoundCosts, BoundSum),
   sum_list(UnboundCosts, UnboundSum),
   Cost is BoundSum + UnboundSum.

% make upa large cost for vars
search_cost(_Any, Var, Cost):-var(Var), !, Cost=100_000_000.
% DFI Rules
search_cost(KoC, dfi(Q), Sum):- findall(Body, clause(dfi(Q), Body), Bodies), !, maplist(search_cost(KoC), Bodies, Costs), combine_costs(KoC, Costs, Sum).
% Conj of bodies
search_cost(KoC, (Pred, _More), Cost):- !, search_cost(KoC, Pred, Cost).
% Pred with Known Arg Costs
search_cost(KoC, Pred, Cost):- get_bound_unbound(Pred, Functor, BoundArgs, UnboundArgs),
   maplist(bound_costs(Functor), BoundArgs, BoundCosts),
   maplist(unbound_costs(Functor), UnboundArgs, UnboundCosts),
   (BoundCosts\==[];UnboundCosts\==[]), !,
   costs_for(KoC, BoundCosts, UnboundCosts, Cost).
% Each Fact
search_cost(_KoC, Pred, Cost):- predicate_property(Pred, number_of_rules(0)), predicate_property(Pred, number_of_clauses(Cost)), Cost>0, !.
% Fact leading to a rule
search_cost(KoC, Pred, Sum):- predicate_property(Pred, number_of_rules(Rules)), Rules>0, predicate_property(Pred, number_of_clauses(Rules)), !,
    findall(Body, clause(Pred, Body), Bodies), !, maplist(search_cost(KoC), Bodies, Costs), combine_costs(KoC, Costs, Sum).
search_cost(_KoC, _Pred, 1_000).

register_linked(F/A):- assert_neo_new(_,is_registered_link(F, A)), dynamic(F/A).
:- register_linked(dfi/1).

:- ensure_loaded(library(metta_rt)).
:- load_metta('&self','neo4j_or_metta_queries1.metta').
:- time(load_mw_data(neo4j_out_v3_mw)).

:- time(link_mw_data(neo4j_out_v3_mw)).


rtq:- rtq(10).
:- cmd_note(rtq(120)).
rtq(DefaultTime):- forall(metta_atom(_,['isa-test-query',Y]),
  ignore(do_isa_test_query(Y, DefaultTime))).


do_isa_test_query(Y, DefaultTime):- nl,nl,write(' '),%write_src_wi(Y),
    must_det_lls((
    get_prop(Y,':metta',Z),
    get_prop(Y,':name',Name),
    get_prop(Y,':description',Desc),
    get_prop_else(Y,':minimum-time', Time, DefaultTime),
    assign_old_varnames(Z,ZZ),
    nl,nl,write(' '),write_src_wi(exec(ZZ)),nl,
    into_prolog_query(ZZ,Prolog),
    subst_vars(Prolog, NewTerm, NamedVarsList),
    mw_writeq(prolog=Prolog),
    %mw_writeq(term=NewTerm),
    %mw_writeq(vn=NamedVarsList),
    maplist(sep_var_name_vars,NamedVarsList, VNs, Vars),
    run_sample_query1(Time, Name, Desc, VNs, Vars, NewTerm))),!.
    %execute_query_tl(NewTerm, _VNs, NamedVarsList, 10).

sep_var_name_vars(N=V,N,V).

'each-different'(X):- nl,writeq('each-different'(X)),nl.

into_prolog_query(ZZ,Prolog):-
  sub_term(SubTerm,ZZ),is_list(SubTerm),member(E,SubTerm),E=[Neo,_|_],(Neo==neo;Neo==neo_P),!,
  neo_into_prolog_query(SubTerm,Prolog),!.

neo_into_prolog_query([Comma|SubTerm],Prolog):- Comma==',',!,neo_into_prolog_query(SubTerm,Prolog).
neo_into_prolog_query(Nil,true):- Nil==[],!.
neo_into_prolog_query(NL,NL):- \+ is_list(NL),!.
neo_into_prolog_query([A|B],F):- B\==[], is_list(B),atom(A),!,maplist(neo_into_prolog_query,B,BL),F=..[A|BL].
neo_into_prolog_query([A|B],(AA,BB)):- !, neo_into_prolog_query(A,AA), neo_into_prolog_query(B,BB),!.
neo_into_prolog_query(A,A).

get_prop_else(Y, Prop,V,_Else):- compound(Y), sub_term(Sub,Y),compound(Sub),Sub=[N,V|_],N==Prop,!.
get_prop_else(_, _Prp,V, Else):- V=Else,!.

get_prop(Y, Prop,V):- compound(Y), sub_term(Sub,Y),compound(Sub),Sub=[N,V|_],N==Prop,!.
get_prop(_, Prop,V):- sformat(V,"Unknown ~w",[Prop]).

assign_old_varnames(Z,Nat):-
  copy_term(Z,Nat,_Stuff),
  metta_file_buffer(_,_,_,D,E,_,_), D=@=Nat,D=Nat,maplist(name_m_vars,E).

%name_m_vars(N= ['$VAR'(Var),':',T]):- atomic_list_concat([Var,T],'_C_',N),!.
name_m_vars(N=('$VAR'(Var):T)):- atomic_list_concat([Var,T],'_C_',N),!.
name_m_vars(N=V):- ignore(V='$VAR'(N)).

end_of_file.

```


b=bound
u=unbound
i=ignored
q=query_var
s=shared

% should switch these around
neo(uq, b, uq), neo(b, qv, us).


% MW-NOTE: time(mw_stats).
% MW-NOTE: time(sample_query).


45 ?- m(neo(_A, _R1, B),
  neo(B, _R2, C),
  neo(C, _R3, D),
  neo(D, _R4, E),
  neo(E, _R5, _F)).
B = go_0019226:go,
C = go_0050877:go,
D = o76024:protein,
E = enst00000226760:transcript .

46 ?- m(neo(_A, _R1, B),
  neo(B, _R2, C),
  neo(C, _R3, D),
  neo(D, _R4, E),
  neo(E, _R5, _F)).
B = go_0019226:go,
C = go_0050877:go,
D = o76024:protein,
E = enst00000226760:transcript .



(neo cl_0000540:cl $R1 var1)
(neo var1 $R2 var2)
(neo var2 $R3 var3)
(neo var3 $R4 var4)
(neo var4 $R5 var5)
(neo var5 $R6 var6)
(neo var6 $R7 var7)
(neo var7 $R8 var8)
(neo var8 $R9 q13394)


 m(neo(cl_0000540:cl, R1, VAR1),
        neo(VAR1, R2, VAR2),
        neo(VAR2, R3, VAR3),
        neo(VAR3, R4, VAR4),
        neo(VAR4, R5, VAR5),
        neo(VAR5, R6, VAR6),
        neo(VAR6, R7, VAR7),
        neo(VAR7, R8, VAR8),
        neo(VAR8, R9, q13394)).


MATCH
  (c:Class {id: "cl_0000540"})-[r1]->(v1)-[r2]->(v2)-[r3]->(v3)-[r4]->(v4)
  -[r5]->(v5)-[r6]->(v6)-[r7]->(v7)-[r8]->(v8)-[r9]->(q:Protein {id: "q13394"})
RETURN c, v1, v2, v3, v4, v5, v6, v7, v8, q,
       type(r1) AS R1_Label, properties(r1) AS R1_Properties,
       type(r2) AS R2_Label, properties(r2) AS R2_Properties,
       type(r3) AS R3_Label, properties(r3) AS R3_Properties,
       type(r4) AS R4_Label, properties(r4) AS R4_Properties,
       type(r5) AS R5_Label, properties(r5) AS R5_Properties,
       type(r6) AS R6_Label, properties(r6) AS R6_Properties,
       type(r7) AS R7_Label, properties(r7) AS R7_Properties,
       type(r8) AS R8_Label, properties(r8) AS R8_Properties,
       type(r9) AS R9_Label, properties(r9) AS R9_Properties;


(neo cl_0000540:cl $capable_of $go_0019226_go)
(neo $go_0019226_go $subclass_of $go_0050877_go)
(neo $go_0050877_go $go_gene_product $o76024_protein)
(neo $o76024_protein $translation_of $enst00000226760_transcript)
(neo $enst00000226760_transcript $transcribed_from $ensg00000109501_gene)
(neo $ensg00000109501_gene $coexpressed_with $ensg00000123989_gene)
(neo $ensg00000123989_gene $transcribed_to $enst00000243776_transcript)
(neo $enst00000243776_transcript $translates_to $q8iz52_protein)
(neo $q8iz52_protein $interacts_with q13394)


46 ?- show_m2(5).
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene)
true .

47 ?- show_m2(10).
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q13394:protein),neo(q13394:protein,accessions,[Q6I9T5]:_28746)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q13394:protein),neo(q13394:protein,protein_name,MB211:_28746)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q13394:protein),neo(q13394:protein,synonyms,[AGR:HGNC:6757,AlphaFoldDB:Q13394,Antibodypedia:23045,Bgee:ENSG00000180660,BioGRID-ORCS:4081,BioGRID:110256,BioMuta:MAB21L1,CCDS:CCDS9353.1,ChiTaRS:MAB21L1,CTD:4081,DisGeNET:4081,DMDM:74739786,DNASU:4081,eggNOG:KOG3963,EMBL:AAB47576.1,EMBL:AAH28170.1,EMBL:AB073388,EMBL:AL390071,EMBL:BAE45718.1,EMBL:BC028170,EMBL:CAG33701.1,EMBL:CH471075,EMBL:CR457420,EMBL:EAX08547.1,EMBL:U38810,Ensembl:ENSG00000180660.9,Ensembl:ENSP00000369251.4,Ensembl:ENSP00000516753.1,Ensembl:ENST00000379919.6,Ensembl:ENST00000707125.1,ExpressionAtlas:Q13394,Gene3D:1.10.1410.40,Gene3D:3.30.460.90,GeneCards:MAB21L1,GeneID:4081,GeneTree:ENSGT01050000244827,Genevisible:Q13394,GenomeRNAi:4081,GlyGen:Q13394,GO:GO:0001654,GO:GO:0005524,GO:GO:0005525,GO:GO:0005634,GO:GO:0008283,GO:GO:0008284,GO:GO:0009653,GO:GO:0016779,GO:GO:0043010,GO:GO:0046872,HGNC:HGNC:6757,HOGENOM:CLU_045315_0_0_1,HPA:ENSG00000180660,InParanoid:Q13394,IntAct:Q13394,InterPro:IPR024810,InterPro:IPR046903,InterPro:IPR046906,iPTMnet:Q13394,KEGG:hsa:4081,MalaCards:MAB21L1,MANE-Select:ENSP00000369251.4,MANE-Select:ENST00000379919.6,MANE-Select:NM_005584.5,MANE-Select:NP_005575.1,MassIVE:Q13394,MIM:601280,MIM:618479,neXtProt:NX_Q13394,OMA:AVDKCKY,OpenTargets:ENSG00000180660,OrthoDB:5478899at2759,PANTHER:PTHR10656,PANTHER:PTHR10656:SF38,PathwayCommons:Q13394,PaxDb:9606-ENSP00000369251,PDB:5EOG,PDB:5EOM,PDBsum:5EOG,PDBsum:5EOM,PeptideAtlas:Q13394,Pfam:PF03281,Pfam:PF20266,PharmGKB:PA30516,Pharos:Q13394,PhosphoSitePlus:Q13394,PhylomeDB:Q13394,PIR:G02221,PRO:PR:Q13394,Proteomes:UP000005640,ProteomicsDB:59364,RefSeq:NM_005584.4,RefSeq:NP_005575.1,RNAct:Q13394,SignaLink:Q13394,SMART:SM01265,SMR:Q13394,STRING:9606.ENSP00000369251,TopDownProteomics:Q13394,TreeFam:TF315012,UCSC:uc032aca.2,VEuPathDB:HostDB:ENSG00000180660]:_28746)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q06828:protein),neo(q06828:protein,accessions,[Q15331,Q8IV47]:_41222)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q06828:protein),neo(q06828:protein,protein_name,FMOD:_41222)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q06828:protein),neo(q06828:protein,synonyms,[AGR:HGNC:3774,AlphaFoldDB:Q06828,Antibodypedia:34544,Bgee:ENSG00000122176,BioGRID-ORCS:2331,BioGRID:108618,BioMuta:FMOD,CCDS:CCDS30976.1,ChiTaRS:FMOD,CTD:2331,DisGeNET:2331,DMDM:223590208,DNASU:2331,eggNOG:KOG0619,EMBL:AAH35281.1,EMBL:AK291632,EMBL:AL359837,EMBL:BAF84321.1,EMBL:BC035281,EMBL:CAA51418.1,EMBL:CAA53233.1,EMBL:CH471067,EMBL:EAW91477.1,EMBL:X72913,EMBL:X75546,Ensembl:ENSG00000122176.12,Ensembl:ENSP00000347041.4,Ensembl:ENST00000354955.5,ExpressionAtlas:Q06828,Gene3D:3.80.10.10,GeneCards:FMOD,GeneID:2331,GeneTree:ENSGT00940000157007,Genevisible:Q06828,GeneWiki:FMOD_(gene),GenomeRNAi:2331,GlyConnect:1242,GlyCosmos:Q06828,GlyGen:Q06828,GO:GO:0005576,GO:GO:0005615,GO:GO:0005796,GO:GO:0007181,GO:GO:0030199,GO:GO:0031012,GO:GO:0043202,GO:GO:0062023,HGNC:HGNC:3774,HOGENOM:CLU_000288_186_4_1,HPA:ENSG00000122176,InParanoid:Q06828,IntAct:Q06828,InterPro:IPR000372,InterPro:IPR001611,InterPro:IPR003591,InterPro:IPR032675,iPTMnet:Q06828,jPOST:Q06828,KEGG:hsa:2331,MANE-Select:ENSP00000347041.4,MANE-Select:ENST00000354955.5,MANE-Select:NM_002023.5,MANE-Select:NP_002014.2,MassIVE:Q06828,MIM:600245,MINT:Q06828,neXtProt:NX_Q06828,OMA:KLLYVRM,OpenTargets:ENSG00000122176,OrthoDB:521898at2759,PANTHER:PTHR45712,PANTHER:PTHR45712:SF4,PathwayCommons:Q06828,PaxDb:9606-ENSP00000347041,PDB:5MX0,PDBsum:5MX0,PeptideAtlas:Q06828,Pfam:PF01462,Pfam:PF13855,PharmGKB:PA28190,Pharos:Q06828,PhosphoSitePlus:Q06828,PhylomeDB:Q06828,PIR:S55275,PRO:PR:Q06828,PROSITE:PS51450,Proteomes:UP000005640,ProteomicsDB:58485,Reactome:R-HSA-2022854,Reactome:R-HSA-2022857,Reactome:R-HSA-3000178,Reactome:R-HSA-3656225,Reactome:R-HSA-3656243,Reactome:R-HSA-3656244,RefSeq:NM_002023.4,RefSeq:NP_002014.2,RNAct:Q06828,SignaLink:Q06828,SMART:SM00013,SMART:SM00364,SMART:SM00369,SMR:Q06828,STRING:9606.ENSP00000347041,SUPFAM:SSF52058,TreeFam:TF334562,UCSC:uc001gzr.5,VEuPathDB:HostDB:ENSG00000122176]:_41222)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,o75897:protein),neo(o75897:protein,accessions,[Q069I8,Q08AS5,Q53S63]:_53950)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,o75897:protein),neo(o75897:protein,protein_name,ST1C4:_53950)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,o75897:protein),neo(o75897:protein,synonyms,[AGR:HGNC:11457,AlphaFoldDB:O75897,Antibodypedia:33063,Bgee:ENSG00000198075,BioGRID-ORCS:27233,BioGRID:118082,BioMuta:SULT1C4,BRENDA:2.8.2.1,CCDS:CCDS2077.1,CCDS:CCDS82492.1,ChEMBL:CHEMBL1743296,CTD:27233,DNASU:27233,DrugBank:DB00867,DrugBank:DB00871,DrugBank:DB00960,DrugBank:DB00968,DrugBank:DB12243,DrugBank:DB12471,DrugBank:DB14635,eggNOG:KOG1584,EMBL:AAC95519.1,EMBL:AAF72810.1,EMBL:AAI25044.1,EMBL:AAY14742.1,EMBL:ABI75348.1,EMBL:AC068941,EMBL:AF055584,EMBL:AF186263,EMBL:AK297851,EMBL:BAG60183.1,EMBL:BC125043,EMBL:CH471182,EMBL:DQ987914,EMBL:EAW53886.1,Ensembl:ENSG00000198075.10,Ensembl:ENSP00000272452.2,Ensembl:ENSP00000387225.3,Ensembl:ENST00000272452.7,Ensembl:ENST00000409309.3,EvolutionaryTrace:O75897,Gene3D:3.40.50.300,GeneCards:SULT1C4,GeneID:27233,GeneTree:ENSGT00940000157101,Genevisible:O75897,GeneWiki:SULT1C4,GenomeRNAi:27233,GO:GO:0004062,GO:GO:0005737,GO:GO:0005829,GO:GO:0006068,GO:GO:0006805,GO:GO:0008146,GO:GO:0009812,GO:GO:0044598,GO:GO:0050427,GO:GO:0051923,HGNC:HGNC:11457,HOGENOM:CLU_027239_1_2_1,HPA:ENSG00000198075,InParanoid:O75897,IntAct:O75897,InterPro:IPR000863,InterPro:IPR027417,iPTMnet:O75897,KEGG:hsa:27233,MANE-Select:ENSP00000272452.2,MANE-Select:ENST00000272452.7,MANE-Select:NM_006588.4,MANE-Select:NP_006579.2,MassIVE:O75897,MaxQB:O75897,MIM:608357,neXtProt:NX_O75897,OMA:VAPHAFF,OpenTargets:ENSG00000198075,OrthoDB:3083090at2759,PANTHER:PTHR11783,PANTHER:PTHR11783:SF44,PathwayCommons:O75897,PaxDb:9606-ENSP00000272452,PDB:2AD1,PDB:2GWH,PDBsum:2AD1,PDBsum:2GWH,PeptideAtlas:O75897,Pfam:PF00685,PharmGKB:PA162405070,Pharos:O75897,PhosphoSitePlus:O75897,PhylomeDB:O75897,PRO:PR:O75897,Proteomes:UP000005640,ProteomicsDB:50251,ProteomicsDB:58685,Reactome:R-HSA-156584,Reactome:R-HSA-9753281,RefSeq:NM_001321770.1,RefSeq:NM_006588.3,RefSeq:NP_001308699.1,RefSeq:NP_006579.2,RNAct:O75897,SignaLink:O75897,SMR:O75897,STRING:9606.ENSP00000272452,SUPFAM:SSF52540,TreeFam:TF321745,UCSC:uc002tea.2,VEuPathDB:HostDB:ENSG00000198075]:_53950)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q8nfl0:protein),neo(q8nfl0:protein,accessions,[B3KWY4,B7WNP0]:_21546)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q8nfl0:protein),neo(q8nfl0:protein,protein_name,B3GN7:_21546)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,q8nfl0:protein),neo(q8nfl0:protein,synonyms,[AGR:HGNC:18811,AlphaFoldDB:Q8NFL0,Antibodypedia:34416,Bgee:ENSG00000156966,BioGRID-ORCS:93010,BioGRID:124994,BioMuta:B3GNT7,BRENDA:2.4.1.149,CAZy:GT31,CCDS:CCDS46540.1,CTD:93010,DisGeNET:93010,DMDM:74723834,DNASU:93010,eggNOG:KOG2287,EMBL:AAM61770.1,EMBL:AAY24246.1,EMBL:AC017104,EMBL:AF502430,EMBL:AK126207,EMBL:BAG54296.1,Ensembl:ENSG00000156966.7,Ensembl:ENSP00000287590.5,Ensembl:ENST00000287590.6,EPD:Q8NFL0,Gene3D:3.90.550.50,GeneCards:B3GNT7,GeneID:93010,GeneTree:ENSGT00940000157606,Genevisible:Q8NFL0,GenomeRNAi:93010,GlyCosmos:Q8NFL0,GlyGen:Q8NFL0,GO:GO:0000139,GO:GO:0006493,GO:GO:0008194,GO:GO:0008375,GO:GO:0008499,GO:GO:0008532,GO:GO:0016020,GO:GO:0016266,GO:GO:0018146,HGNC:HGNC:18811,HOGENOM:CLU_036849_5_1_1,HPA:ENSG00000156966,InParanoid:Q8NFL0,IntAct:Q8NFL0,InterPro:IPR002659,iPTMnet:Q8NFL0,jPOST:Q8NFL0,KEGG:hsa:93010,MANE-Select:ENSP00000287590.5,MANE-Select:ENST00000287590.6,MANE-Select:NM_145236.3,MANE-Select:NP_660279.1,MassIVE:Q8NFL0,MIM:615313,neXtProt:NX_Q8NFL0,OMA:KWLNIYC,OpenTargets:ENSG00000156966,OrthoDB:532757at2759,PANTHER:PTHR11214,PANTHER:PTHR11214:SF93,PathwayCommons:Q8NFL0,PaxDb:9606-ENSP00000287590,PeptideAtlas:Q8NFL0,Pfam:PF01762,PharmGKB:PA38692,Pharos:Q8NFL0,PhosphoSitePlus:Q8NFL0,PhylomeDB:Q8NFL0,PRO:PR:Q8NFL0,Proteomes:UP000005640,ProteomicsDB:73322,Reactome:R-HSA-2022854,Reactome:R-HSA-913709,RefSeq:NM_145236.2,RefSeq:NP_660279.1,RNAct:Q8NFL0,SignaLink:Q8NFL0,SMR:Q8NFL0,STRING:9606.ENSP00000287590,TreeFam:TF318639,UCSC:uc002vrs.4,UniPathway:UPA00378,VEuPathDB:HostDB:ENSG00000156966]:_21546)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,o14594:protein),neo(o14594:protein,accessions,[Q9UPK6]:_33518)
true ;
neo(cl_0000540:cl,capable_of,go_0019226:go),neo(go_0019226:go,subclass_of,go_0050877:go),neo(go_0050877:go,go_gene_product,o76024:protein),neo(o76024:protein,translation_of,enst00000226760:transcript),neo(enst00000226760:transcript,transcribed_from,ensg00000109501:gene),neo(ensg00000109501:gene,coexpressed_with,ensg00000123989:gene),neo(ensg00000123989:gene,transcribed_to,enst00000243776:transcript),neo(enst00000243776:transcript,translates_to,q8iz52:protein),neo(q8iz52:protein,interacts_with,o14594:protein),neo(o14594:protein,protein_name,NCAN:_33518)
true





dfi(edge_prop(biological_context))
dfi(edge_prop(chr))
dfi(edge_prop(databases))
dfi(edge_prop(db_reference))
dfi(edge_prop(detection_method))
dfi(edge_prop(distance))
dfi(edge_prop(effect))
dfi(edge_prop(evidence))
dfi(edge_prop(evidence_type))
dfi(edge_prop(maf))
dfi(edge_prop(modification))
dfi(edge_prop(p_value))
dfi(edge_prop(pos))
dfi(edge_prop(qualifier))
dfi(edge_prop(score))
dfi(edge_prop(slope))
dfi(edge_prop(state))

dfi(edge_link(activity_by_contact))
dfi(edge_link(associated_with))
dfi(edge_link(belongs_to))
dfi(edge_link(binds_to))
dfi(edge_link(capable_of))
dfi(edge_link(child_pathway_of))
dfi(edge_link(chromatin_state))
dfi(edge_link(closest_gene))
dfi(edge_link(coexpressed_with))
dfi(edge_link(downstream_of))
dfi(edge_link(eqtl_association))
dfi(edge_link(expressed_in))
dfi(edge_link(genes_pathways))
dfi(edge_link(go_gene_product))
dfi(edge_link(histone_modification))
dfi(edge_link(in_dnase_i_hotspot))
dfi(edge_link(in_tad_region))
dfi(edge_link(includes))
dfi(edge_link(interacts_with))
dfi(edge_link(located_in))
dfi(edge_link(parent_pathway_of))
dfi(edge_link(part_of))
dfi(edge_link(regulates))
dfi(edge_link(subclass_of))
dfi(edge_link(tfbs_snp))
dfi(edge_link(transcribed_from))
dfi(edge_link(transcribed_to))
dfi(edge_link(translates_to))
dfi(edge_link(translation_of))
dfi(edge_link(upstream_of))


dfi(node_prop(accessions))
dfi(node_prop(alt))
dfi(node_prop(caf_alt))
dfi(node_prop(caf_ref))
dfi(node_prop(chr))
dfi(node_prop(data_source))
dfi(node_prop(end))
dfi(node_prop(enhancer_id))
dfi(node_prop(evidence))
dfi(node_prop(exon_id))
dfi(node_prop(exon_number))
dfi(node_prop(gene_id))
dfi(node_prop(gene_name))
dfi(node_prop(gene_type))
dfi(node_prop(genes))
dfi(node_prop(pathway_name))
dfi(node_prop(phred_score))
dfi(node_prop(protein_name))
dfi(node_prop(raw_cadd_score))
dfi(node_prop(ref))
dfi(node_prop(rna_type))
dfi(node_prop(se_id))
dfi(node_prop(start))
dfi(node_prop(subontology))
dfi(node_prop(synonyms))
dfi(node_prop(term_name))
dfi(node_prop(transcript_id))
dfi(node_prop(transcript_name))
dfi(node_prop(transcript_type))
dfi(node_prop(variant_accession))
dfi(node_prop(variant_type))






dfi(edge_prop_for(biological_context,activity_by_contact))
dfi(edge_prop_for(biological_context,associated_with))
dfi(edge_prop_for(biological_context,eqtl_association))
dfi(edge_prop_for(chr,closest_gene))
dfi(edge_prop_for(databases,regulates))
dfi(edge_prop_for(db_reference,go_gene_product))
dfi(edge_prop_for(detection_method,regulates))
dfi(edge_prop_for(distance,closest_gene))
dfi(edge_prop_for(distance,downstream_of))
dfi(edge_prop_for(distance,upstream_of))
dfi(edge_prop_for(effect,tfbs_snp))
dfi(edge_prop_for(evidence,go_gene_product))
dfi(edge_prop_for(evidence,regulates))
dfi(edge_prop_for(evidence_type,regulates))
dfi(edge_prop_for(maf,eqtl_association))
dfi(edge_prop_for(modification,histone_modification))
dfi(edge_prop_for(p_value,downstream_of))
dfi(edge_prop_for(p_value,eqtl_association))
dfi(edge_prop_for(p_value,expressed_in))
dfi(edge_prop_for(p_value,located_in))
dfi(edge_prop_for(p_value,upstream_of))
dfi(edge_prop_for(pos,closest_gene))
dfi(edge_prop_for(qualifier,go_gene_product))
dfi(edge_prop_for(score,activity_by_contact))
dfi(edge_prop_for(score,associated_with))
dfi(edge_prop_for(score,binds_to))
dfi(edge_prop_for(score,coexpressed_with))
dfi(edge_prop_for(score,expressed_in))
dfi(edge_prop_for(score,interacts_with))
dfi(edge_prop_for(score,tfbs_snp))
dfi(edge_prop_for(slope,eqtl_association))
dfi(edge_prop_for(state,chromatin_state))
dfi(neo_edge_link(activity_by_contact,A,B))                          :- activity_by_contact(A,B).
dfi(neo_edge_link(associated_with,A,B))                              :- associated_with(A,B).
dfi(neo_edge_link(belongs_to,A,B))                                   :- belongs_to(A,B).
dfi(neo_edge_link(binds_to,A,B))                                     :- binds_to(A,B).
dfi(neo_edge_link(capable_of,A,B))                                   :- capable_of(A,B).
dfi(neo_edge_link(child_pathway_of,A,B))                             :- child_pathway_of(A,B).
dfi(neo_edge_link(chromatin_state,A,B))                              :- chromatin_state(A,B).
dfi(neo_edge_link(closest_gene,A,B))                                 :- closest_gene(A,B).
dfi(neo_edge_link(coexpressed_with,A,B))                             :- coexpressed_with(A,B).
dfi(neo_edge_link(downstream_of,A,B))                                :- downstream_of(A,B).
dfi(neo_edge_link(eqtl_association,A,B))                             :- eqtl_association(A,B).
dfi(neo_edge_link(expressed_in,A,B))                                 :- expressed_in(A,B).
dfi(neo_edge_link(genes_pathways,A,B))                               :- genes_pathways(A,B).
dfi(neo_edge_link(go_gene_product,A,B))                              :- go_gene_product(A,B).
dfi(neo_edge_link(histone_modification,A,B))                         :- histone_modification(A,B).
dfi(neo_edge_link(in_dnase_i_hotspot,A,B))                           :- in_dnase_i_hotspot(A,B).
dfi(neo_edge_link(in_tad_region,A,B))                                :- in_tad_region(A,B).
dfi(neo_edge_link(includes,A,B))                                     :- includes(A,B).
dfi(neo_edge_link(interacts_with,A,B))                               :- interacts_with(A,B).
dfi(neo_edge_link(located_in,A,B))                                   :- located_in(A,B).
dfi(neo_edge_link(parent_pathway_of,A,B))                            :- parent_pathway_of(A,B).
dfi(neo_edge_link(part_of,A,B))                                      :- part_of(A,B).
dfi(neo_edge_link(regulates,A,B))                                    :- regulates(A,B).
dfi(neo_edge_link(subclass_of,A,B))                                  :- subclass_of(A,B).
dfi(neo_edge_link(tfbs_snp,A,B))                                     :- tfbs_snp(A,B).
dfi(neo_edge_link(transcribed_from,A,B))                             :- transcribed_from(A,B).
dfi(neo_edge_link(transcribed_to,A,B))                               :- transcribed_to(A,B).
dfi(neo_edge_link(translates_to,A,B))                                :- translates_to(A,B).
dfi(neo_edge_link(translation_of,A,B))                               :- translation_of(A,B).
dfi(neo_edge_link(upstream_of,A,B))                                  :- upstream_of(A,B).

dfi(neo_edge_prop(biological_context,activity_by_contact,A,B,C))     :- abc_edges_activity_by_contact(A,B,_,C).
dfi(neo_edge_prop(biological_context,associated_with,A,B,C))         :- dbsuper_edges_super_enhancer_gene(A,B,C).
dfi(neo_edge_prop(biological_context,associated_with,A,B,C))         :- enhancer_atlas_edges_enhancer_gene(A,B,C,_).
dfi(neo_edge_prop(biological_context,associated_with,A,B,C))         :- peregrine_edges_enhancer_gene(A,B,C).
dfi(neo_edge_prop(biological_context,eqtl_association,A,B,C))        :- eqtl_edges_gtex_variant_gene(A,B,_,_,_,C).
dfi(neo_edge_prop(chr,closest_gene,A,B,C))                           :- refseq_edges_closest_gene(A,B,C,_,_).
dfi(neo_edge_prop(databases,regulates,A,B,C))                        :- tflink_edges_tf_gene(A,B,_,C,_,_).
dfi(neo_edge_prop(db_reference,go_gene_product,A,B,C))               :- gaf_edges_go_gene_product(A,B,_,C,_).
dfi(neo_edge_prop(detection_method,regulates,A,B,C))                 :- tflink_edges_tf_gene(A,B,_,_,_,C).
dfi(neo_edge_prop(distance,closest_gene,A,B,C))                      :- refseq_edges_closest_gene(A,B,_,_,C).
dfi(neo_edge_prop(distance,downstream_of,A,B,C))                     :- gwas_edges_snp_downstream_gene(A,B,_,C).
dfi(neo_edge_prop(distance,upstream_of,A,B,C))                       :- gwas_edges_snp_upstream_gene(A,B,_,C).
dfi(neo_edge_prop(effect,tfbs_snp,A,B,C))                            :- fabian_edges_tfbs_snp(A,B,C,_).
dfi(neo_edge_prop(evidence,go_gene_product,A,B,C))                   :- gaf_edges_go_gene_product(A,B,_,_,C).
dfi(neo_edge_prop(evidence,regulates,A,B,C))                         :- tflink_edges_tf_gene(A,B,C,_,_,_).
dfi(neo_edge_prop(evidence_type,regulates,A,B,C))                    :- tflink_edges_tf_gene(A,B,_,_,C,_).
dfi(neo_edge_prop(maf,eqtl_association,A,B,C))                       :- eqtl_edges_gtex_variant_gene(A,B,C,_,_,_).
dfi(neo_edge_prop(modification,histone_modification,A,B,C))          :- roadmap_h3_mark_edges_histone_modification(A,_,B,C).
dfi(neo_edge_prop(p_value,downstream_of,A,B,C))                      :- gwas_edges_snp_downstream_gene(A,B,C,_).
dfi(neo_edge_prop(p_value,eqtl_association,A,B,C))                   :- eqtl_edges_gtex_variant_gene(A,B,_,_,C,_).
dfi(neo_edge_prop(p_value,expressed_in,A,B,C))                       :- bgee_edges_expressed_in(A,_,B,_,C).
dfi(neo_edge_prop(p_value,expressed_in,A,B,C))                       :- gtex_expression_edges_expressed_in(A,_,B,C).
dfi(neo_edge_prop(p_value,located_in,A,B,C))                         :- gwas_ingene_edges_snp_in_gene(A,B,C).
dfi(neo_edge_prop(p_value,upstream_of,A,B,C))                        :- gwas_edges_snp_upstream_gene(A,B,C,_).
dfi(neo_edge_prop(pos,closest_gene,A,B,C))                           :- refseq_edges_closest_gene(A,B,_,C,_).
dfi(neo_edge_prop(qualifier,go_gene_product,A,B,C))                  :- gaf_edges_go_gene_product(A,B,C,_,_).
dfi(neo_edge_prop(score,activity_by_contact,A,B,C))                  :- abc_edges_activity_by_contact(A,B,C,_).
dfi(neo_edge_prop(score,associated_with,A,B,C))                      :- enhancer_atlas_edges_enhancer_gene(A,B,_,C).
dfi(neo_edge_prop(score,binds_to,A,B,C))                             :- edges_gene_tfbs(A,B,C).
dfi(neo_edge_prop(score,coexpressed_with,A,B,C))                     :- coxpressdb_edges_coexpressed_with(A,B,C).
dfi(neo_edge_prop(score,expressed_in,A,B,C))                         :- bgee_edges_expressed_in(A,_,B,C,_).
dfi(neo_edge_prop(score,interacts_with,A,B,C))                       :- string_edges_interacts_with(A,B,C).
dfi(neo_edge_prop(score,tfbs_snp,A,B,C))                             :- fabian_edges_tfbs_snp(A,B,_,C).
dfi(neo_edge_prop(slope,eqtl_association,A,B,C))                     :- eqtl_edges_gtex_variant_gene(A,B,_,C,_,_).
dfi(neo_edge_prop(state,chromatin_state,A,B,C))                      :- roadmap_edges_chromatin_state(A,B,C).
dfi(neo_edge_proplist(_,_,[state=A],chromatin_state,B,C))            :- roadmap_edges_chromatin_state(C,B,A).
dfi(neo_edge_proplist(bto,bto,[],subclass_of,A,B))                   :- brenda_tissue_ontology_edges_bto_subclass_of(B,A).
dfi(neo_edge_proplist(cl,cl,[],subclass_of,A,B))                     :- cell_ontology_edges_cl_subclass_of(B,A).
dfi(neo_edge_proplist(cl,go,[],capable_of,A,B))                      :- cell_ontology_edges_cl_capable_of(B,A).
dfi(neo_edge_proplist(cl,uberon,[],part_of,A,B))                     :- cell_ontology_edges_cl_part_of(B,A).
dfi(neo_edge_proplist(clo,clo,[],subclass_of,A,B))                   :- cell_line_ontology_edges_clo_subclass_of(B,A).
dfi(neo_edge_proplist(efo,efo,[],subclass_of,A,B))                   :- experimental_factor_ontology_edges_efo_subclass_of(B,A).
dfi(neo_edge_proplist(enhancer,gene,[biological_context=A,score=B],associated_with,C,D)) :- enhancer_atlas_edges_enhancer_gene(D,C,A,B).
dfi(neo_edge_proplist(enhancer,gene,[biological_context=A],associated_with,B,C)) :- peregrine_edges_enhancer_gene(C,B,A).
dfi(neo_edge_proplist(gene,A,[p_value=B],expressed_in,C,D))          :- gtex_expression_edges_expressed_in(D,A,C,B).
dfi(neo_edge_proplist(gene,A,[score=B,p_value=C],expressed_in,D,E))  :- bgee_edges_expressed_in(E,A,D,B,C).
dfi(neo_edge_proplist(gene,gene,[evidence=A,databases=B,evidence_type=C,detection_method=D],regulates,E,F)) :- tflink_edges_tf_gene(F,E,A,B,C,D).
dfi(neo_edge_proplist(gene,gene,[score=A],coexpressed_with,B,C))     :- coxpressdb_edges_coexpressed_with(C,B,A).
dfi(neo_edge_proplist(gene,pathway,[],genes_pathways,A,B))           :- reactome_edges_genes_pathways(B,A).
dfi(neo_edge_proplist(gene,snp,[effect=A,score=B],tfbs_snp,C,D))     :- fabian_edges_tfbs_snp(D,C,A,B).
dfi(neo_edge_proplist(gene,tad,[],in_tad_region,A,B))                :- tadmap_edges_in_tad_region(B,A).
dfi(neo_edge_proplist(gene,tfbs,[score=A],binds_to,B,C))             :- edges_gene_tfbs(C,B,A).
dfi(neo_edge_proplist(gene,transcript,[],transcribed_to,A,B))        :- gencode_transcript_edges_transcribed_to(B,A).
dfi(neo_edge_proplist(go,go,[],subclass_of,A,B))                     :- gene_ontology_edges_go_subclass_of(B,A).
dfi(neo_edge_proplist(go,protein,[qualifier=A,db_reference=B,evidence=C],go_gene_product,D,E)) :- gaf_edges_go_gene_product(E,D,A,B,C).
dfi(neo_edge_proplist(non_coding_rna,go,[],belongs_to,A,B))          :- rna_central_edges_go_rna(B,A).
dfi(neo_edge_proplist(pathway,pathway,[],child_pathway_of,A,B))      :- reactome_edges_child_pathway_of(B,A).
dfi(neo_edge_proplist(pathway,pathway,[],parent_pathway_of,A,B))     :- reactome_edges_parent_pathway_of(B,A).
dfi(neo_edge_proplist(promoter,gene,[],associated_with,A,B))         :- epd_edges_promoter_gene(B,A).
dfi(neo_edge_proplist(protein,protein,[score=A],interacts_with,B,C)) :- string_edges_interacts_with(C,B,A).
dfi(neo_edge_proplist(protein,transcript,[],translation_of,A,B))     :- uniprot_edges_translation_of(B,A).
dfi(neo_edge_proplist(snp,A,[modification=B],histone_modification,C,D)) :- roadmap_h3_mark_edges_histone_modification(D,A,C,B).
dfi(neo_edge_proplist(snp,gene,[chr=A,pos=B,distance=C],closest_gene,D,E)) :- refseq_edges_closest_gene(E,D,A,B,C).
dfi(neo_edge_proplist(snp,gene,[maf=A,slope=B,p_value=C,biological_context=D],eqtl_association,E,F)) :- eqtl_edges_gtex_variant_gene(F,E,A,B,C,D).
dfi(neo_edge_proplist(snp,gene,[p_value=A,distance=B],downstream_of,C,D)) :- gwas_edges_snp_downstream_gene(D,C,A,B).
dfi(neo_edge_proplist(snp,gene,[p_value=A,distance=B],upstream_of,C,D)) :- gwas_edges_snp_upstream_gene(D,C,A,B).
dfi(neo_edge_proplist(snp,gene,[p_value=A],located_in,B,C))          :- gwas_ingene_edges_snp_in_gene(C,B,A).
dfi(neo_edge_proplist(snp,gene,[score=A,biological_context=B],activity_by_contact,C,D)) :- abc_edges_activity_by_contact(D,C,A,B).
dfi(neo_edge_proplist(snp,uberon,[],in_dnase_i_hotspot,A,B))         :- roadmap_dhs_edges_in_dnase_i_hotspot(B,A).
dfi(neo_edge_proplist(super_enhancer,gene,[biological_context=A],associated_with,B,C)) :- dbsuper_edges_super_enhancer_gene(C,B,A).
dfi(neo_edge_proplist(transcript,exon,[],includes,A,B))              :- gencode_exon_edges_includes(B,A).
dfi(neo_edge_proplist(transcript,gene,[],transcribed_from,A,B))      :- gencode_transcript_edges_transcribed_from(B,A).
dfi(neo_edge_proplist(transcript,protein,[],translates_to,A,B))      :- uniprot_edges_translates_to(B,A).
dfi(neo_edge_proplist(uberon,uberon,[],subclass_of,A,B))             :- edges_uberon_subclass_of(B,A).
dfi(neo_edge_typed(activity_by_contact,snp,gene,A,B))                :- abc_edges_activity_by_contact(B,A,_,_).
dfi(neo_edge_typed(associated_with,enhancer,gene,A,B))               :- enhancer_atlas_edges_enhancer_gene(B,A,_,_).
dfi(neo_edge_typed(associated_with,enhancer,gene,A,B))               :- peregrine_edges_enhancer_gene(B,A,_).
dfi(neo_edge_typed(associated_with,promoter,gene,A,B))               :- epd_edges_promoter_gene(B,A).
dfi(neo_edge_typed(associated_with,super_enhancer,gene,A,B))         :- dbsuper_edges_super_enhancer_gene(B,A,_).
dfi(neo_edge_typed(belongs_to,non_coding_rna,go,A,B))                :- rna_central_edges_go_rna(B,A).
dfi(neo_edge_typed(binds_to,gene,tfbs,A,B))                          :- edges_gene_tfbs(B,A,_).
dfi(neo_edge_typed(capable_of,cl,go,A,B))                            :- cell_ontology_edges_cl_capable_of(B,A).
dfi(neo_edge_typed(child_pathway_of,pathway,pathway,A,B))            :- reactome_edges_child_pathway_of(B,A).
dfi(neo_edge_typed(chromatin_state,_,_,A,B))                         :- roadmap_edges_chromatin_state(B,A,_).
dfi(neo_edge_typed(closest_gene,snp,gene,A,B))                       :- refseq_edges_closest_gene(B,A,_,_,_).
dfi(neo_edge_typed(coexpressed_with,gene,gene,A,B))                  :- coxpressdb_edges_coexpressed_with(B,A,_).
dfi(neo_edge_typed(downstream_of,snp,gene,A,B))                      :- gwas_edges_snp_downstream_gene(B,A,_,_).
dfi(neo_edge_typed(eqtl_association,snp,gene,A,B))                   :- eqtl_edges_gtex_variant_gene(B,A,_,_,_,_).
dfi(neo_edge_typed(expressed_in,gene,A,B,C))                         :- bgee_edges_expressed_in(C,A,B,_,_).
dfi(neo_edge_typed(expressed_in,gene,A,B,C))                         :- gtex_expression_edges_expressed_in(C,A,B,_).
dfi(neo_edge_typed(genes_pathways,gene,pathway,A,B))                 :- reactome_edges_genes_pathways(B,A).
dfi(neo_edge_typed(go_gene_product,go,protein,A,B))                  :- gaf_edges_go_gene_product(B,A,_,_,_).
dfi(neo_edge_typed(histone_modification,snp,A,B,C))                  :- roadmap_h3_mark_edges_histone_modification(C,A,B,_).
dfi(neo_edge_typed(in_dnase_i_hotspot,snp,uberon,A,B))               :- roadmap_dhs_edges_in_dnase_i_hotspot(B,A).
dfi(neo_edge_typed(in_tad_region,gene,tad,A,B))                      :- tadmap_edges_in_tad_region(B,A).
dfi(neo_edge_typed(includes,transcript,exon,A,B))                    :- gencode_exon_edges_includes(B,A).
dfi(neo_edge_typed(interacts_with,protein,protein,A,B))              :- string_edges_interacts_with(B,A,_).
dfi(neo_edge_typed(located_in,snp,gene,A,B))                         :- gwas_ingene_edges_snp_in_gene(B,A,_).
dfi(neo_edge_typed(parent_pathway_of,pathway,pathway,A,B))           :- reactome_edges_parent_pathway_of(B,A).
dfi(neo_edge_typed(part_of,cl,uberon,A,B))                           :- cell_ontology_edges_cl_part_of(B,A).
dfi(neo_edge_typed(regulates,gene,gene,A,B))                         :- tflink_edges_tf_gene(B,A,_,_,_,_).
dfi(neo_edge_typed(subclass_of,bto,bto,A,B))                         :- brenda_tissue_ontology_edges_bto_subclass_of(B,A).
dfi(neo_edge_typed(subclass_of,cl,cl,A,B))                           :- cell_ontology_edges_cl_subclass_of(B,A).
dfi(neo_edge_typed(subclass_of,clo,clo,A,B))                         :- cell_line_ontology_edges_clo_subclass_of(B,A).
dfi(neo_edge_typed(subclass_of,efo,efo,A,B))                         :- experimental_factor_ontology_edges_efo_subclass_of(B,A).
dfi(neo_edge_typed(subclass_of,go,go,A,B))                           :- gene_ontology_edges_go_subclass_of(B,A).
dfi(neo_edge_typed(subclass_of,uberon,uberon,A,B))                   :- edges_uberon_subclass_of(B,A).
dfi(neo_edge_typed(tfbs_snp,gene,snp,A,B))                           :- fabian_edges_tfbs_snp(B,A,_,_).
dfi(neo_edge_typed(transcribed_from,transcript,gene,A,B))            :- gencode_transcript_edges_transcribed_from(B,A).
dfi(neo_edge_typed(transcribed_to,gene,transcript,A,B))              :- gencode_transcript_edges_transcribed_to(B,A).
dfi(neo_edge_typed(translates_to,transcript,protein,A,B))            :- uniprot_edges_translates_to(B,A).
dfi(neo_edge_typed(translation_of,protein,transcript,A,B))           :- uniprot_edges_translation_of(B,A).
dfi(neo_edge_typed(upstream_of,snp,gene,A,B))                        :- gwas_edges_snp_upstream_gene(B,A,_,_).
dfi(neo_node_inst(bto,A))                                            :- brenda_tissue_ontology_nodes_bto(A,_,_).
dfi(neo_node_inst(cl,A))                                             :- cell_ontology_nodes_cl(A,_,_).
dfi(neo_node_inst(clo,A))                                            :- cell_line_ontology_nodes_clo(A,_,_).
dfi(neo_node_inst(efo,A))                                            :- experimental_factor_ontology_nodes_efo(A,_,_).
dfi(neo_node_inst(enhancer,A))                                       :- enhancer_atlas_nodes_enhancer(A,_,_,_).
dfi(neo_node_inst(enhancer,A))                                       :- peregrine_nodes_enhancer(A,_,_,_,_,_).
dfi(neo_node_inst(exon,A))                                           :- gencode_nodes_exon(A,_,_,_,_,_,_,_).
dfi(neo_node_inst(gene,A))                                           :- gencode_nodes_gene(A,_,_,_,_,_,_).
dfi(neo_node_inst(go,A))                                             :- gene_ontology_nodes_go(A,_,_,_).
dfi(neo_node_inst(non_coding_rna,A))                                 :- rna_central_nodes_non_coding_rna(A,_,_,_,_).
dfi(neo_node_inst(pathway,A))                                        :- reactome_nodes_pathway(A,_).
dfi(neo_node_inst(promoter,A))                                       :- epd_nodes_promoter(A,_,_,_).
dfi(neo_node_inst(protein,A))                                        :- uniprot_nodes_protein(A,_,_,_).
dfi(neo_node_inst(snp,A))                                            :- cadd_nodes_snp(A,_,_).
dfi(neo_node_inst(snp,A))                                            :- dbsnp_nodes_snp(A,_,_,_,_,_,_,_).
dfi(neo_node_inst(structural_variant,A))                             :- dbvar_nodes_structural_variant(A,_,_,_,_).
dfi(neo_node_inst(structural_variant,A))                             :- dgv_nodes_structural_variant(A,_,_,_,_,_,_).
dfi(neo_node_inst(super_enhancer,A))                                 :- dbsuper_nodes_super_enhancer(A,_,_,_,_).
dfi(neo_node_inst(tad,A))                                            :- tadmap_nodes_tad(A,_,_,_,_).
dfi(neo_node_inst(tfbs,A))                                           :- nodes_tfbs(A,_,_,_).
dfi(neo_node_inst(transcript,A))                                     :- gencode_nodes_transcript(A,_,_,_,_,_,_,_).
dfi(neo_node_inst(uberon,A))                                         :- nodes_uberon(A,_,_).
dfi(neo_node_prop(accessions,protein,A,B))                           :- uniprot_nodes_protein(A,B,_,_).
dfi(neo_node_prop(alt,snp,A,B))                                      :- dbsnp_nodes_snp(A,_,_,_,_,B,_,_).
dfi(neo_node_prop(caf_alt,snp,A,B))                                  :- dbsnp_nodes_snp(A,_,_,_,_,_,_,B).
dfi(neo_node_prop(caf_ref,snp,A,B))                                  :- dbsnp_nodes_snp(A,_,_,_,_,_,B,_).
dfi(neo_node_prop(chr,enhancer,A,B))                                 :- enhancer_atlas_nodes_enhancer(A,B,_,_).
dfi(neo_node_prop(chr,enhancer,A,B))                                 :- peregrine_nodes_enhancer(A,_,B,_,_,_).
dfi(neo_node_prop(chr,exon,A,B))                                     :- gencode_nodes_exon(A,_,_,B,_,_,_,_).
dfi(neo_node_prop(chr,gene,A,B))                                     :- gencode_nodes_gene(A,_,B,_,_,_,_).
dfi(neo_node_prop(chr,non_coding_rna,A,B))                           :- rna_central_nodes_non_coding_rna(A,B,_,_,_).
dfi(neo_node_prop(chr,promoter,A,B))                                 :- epd_nodes_promoter(A,B,_,_).
dfi(neo_node_prop(chr,snp,A,B))                                      :- dbsnp_nodes_snp(A,B,_,_,_,_,_,_).
dfi(neo_node_prop(chr,structural_variant,A,B))                       :- dbvar_nodes_structural_variant(A,B,_,_,_).
dfi(neo_node_prop(chr,structural_variant,A,B))                       :- dgv_nodes_structural_variant(A,_,B,_,_,_,_).
dfi(neo_node_prop(chr,super_enhancer,A,B))                           :- dbsuper_nodes_super_enhancer(A,_,B,_,_).
dfi(neo_node_prop(chr,tad,A,B))                                      :- tadmap_nodes_tad(A,B,_,_,_).
dfi(neo_node_prop(chr,tfbs,A,B))                                     :- nodes_tfbs(A,B,_,_).
dfi(neo_node_prop(chr,transcript,A,B))                               :- gencode_nodes_transcript(A,_,_,_,B,_,_,_).
dfi(neo_node_prop(data_source,enhancer,A,B))                         :- peregrine_nodes_enhancer(A,_,_,_,_,B).
dfi(neo_node_prop(end,enhancer,A,B))                                 :- enhancer_atlas_nodes_enhancer(A,_,_,B).
dfi(neo_node_prop(end,enhancer,A,B))                                 :- peregrine_nodes_enhancer(A,_,_,_,B,_).
dfi(neo_node_prop(end,exon,A,B))                                     :- gencode_nodes_exon(A,_,_,_,_,B,_,_).
dfi(neo_node_prop(end,gene,A,B))                                     :- gencode_nodes_gene(A,_,_,_,B,_,_).
dfi(neo_node_prop(end,non_coding_rna,A,B))                           :- rna_central_nodes_non_coding_rna(A,_,_,B,_).
dfi(neo_node_prop(end,promoter,A,B))                                 :- epd_nodes_promoter(A,_,_,B).
dfi(neo_node_prop(end,snp,A,B))                                      :- dbsnp_nodes_snp(A,_,_,B,_,_,_,_).
dfi(neo_node_prop(end,structural_variant,A,B))                       :- dbvar_nodes_structural_variant(A,_,_,B,_).
dfi(neo_node_prop(end,structural_variant,A,B))                       :- dgv_nodes_structural_variant(A,_,_,_,B,_,_).
dfi(neo_node_prop(end,super_enhancer,A,B))                           :- dbsuper_nodes_super_enhancer(A,_,_,_,B).
dfi(neo_node_prop(end,tad,A,B))                                      :- tadmap_nodes_tad(A,_,_,B,_).
dfi(neo_node_prop(end,tfbs,A,B))                                     :- nodes_tfbs(A,_,_,B).
dfi(neo_node_prop(end,transcript,A,B))                               :- gencode_nodes_transcript(A,_,_,_,_,_,B,_).
dfi(neo_node_prop(enhancer_id,enhancer,A,B))                         :- peregrine_nodes_enhancer(A,B,_,_,_,_).
dfi(neo_node_prop(evidence,structural_variant,A,B))                  :- dgv_nodes_structural_variant(A,_,_,_,_,_,B).
dfi(neo_node_prop(exon_id,exon,A,B))                                 :- gencode_nodes_exon(A,_,_,_,_,_,_,B).
dfi(neo_node_prop(exon_number,exon,A,B))                             :- gencode_nodes_exon(A,_,_,_,_,_,B,_).
dfi(neo_node_prop(gene_id,exon,A,B))                                 :- gencode_nodes_exon(A,B,_,_,_,_,_,_).
dfi(neo_node_prop(gene_name,gene,A,B))                               :- gencode_nodes_gene(A,_,_,_,_,B,_).
dfi(neo_node_prop(gene_name,transcript,A,B))                         :- gencode_nodes_transcript(A,_,_,_,_,_,_,B).
dfi(neo_node_prop(gene_type,gene,A,B))                               :- gencode_nodes_gene(A,B,_,_,_,_,_).
dfi(neo_node_prop(genes,tad,A,B))                                    :- tadmap_nodes_tad(A,_,_,_,B).
dfi(neo_node_prop(pathway_name,pathway,A,B))                         :- reactome_nodes_pathway(A,B).
dfi(neo_node_prop(phred_score,snp,A,B))                              :- cadd_nodes_snp(A,_,B).
dfi(neo_node_prop(protein_name,protein,A,B))                         :- uniprot_nodes_protein(A,_,B,_).
dfi(neo_node_prop(raw_cadd_score,snp,A,B))                           :- cadd_nodes_snp(A,B,_).
dfi(neo_node_prop(ref,snp,A,B))                                      :- dbsnp_nodes_snp(A,_,_,_,B,_,_,_).
dfi(neo_node_prop(rna_type,non_coding_rna,A,B))                      :- rna_central_nodes_non_coding_rna(A,_,_,_,B).
dfi(neo_node_prop(se_id,super_enhancer,A,B))                         :- dbsuper_nodes_super_enhancer(A,B,_,_,_).
dfi(neo_node_prop(start,enhancer,A,B))                               :- enhancer_atlas_nodes_enhancer(A,_,B,_).
dfi(neo_node_prop(start,enhancer,A,B))                               :- peregrine_nodes_enhancer(A,_,_,B,_,_).
dfi(neo_node_prop(start,exon,A,B))                                   :- gencode_nodes_exon(A,_,_,_,B,_,_,_).
dfi(neo_node_prop(start,gene,A,B))                                   :- gencode_nodes_gene(A,_,_,B,_,_,_).
dfi(neo_node_prop(start,non_coding_rna,A,B))                         :- rna_central_nodes_non_coding_rna(A,_,B,_,_).
dfi(neo_node_prop(start,promoter,A,B))                               :- epd_nodes_promoter(A,_,B,_).
dfi(neo_node_prop(start,snp,A,B))                                    :- dbsnp_nodes_snp(A,_,B,_,_,_,_,_).
dfi(neo_node_prop(start,structural_variant,A,B))                     :- dbvar_nodes_structural_variant(A,_,B,_,_).
dfi(neo_node_prop(start,structural_variant,A,B))                     :- dgv_nodes_structural_variant(A,_,_,B,_,_,_).
dfi(neo_node_prop(start,super_enhancer,A,B))                         :- dbsuper_nodes_super_enhancer(A,_,_,B,_).
dfi(neo_node_prop(start,tad,A,B))                                    :- tadmap_nodes_tad(A,_,B,_,_).
dfi(neo_node_prop(start,tfbs,A,B))                                   :- nodes_tfbs(A,_,B,_).
dfi(neo_node_prop(start,transcript,A,B))                             :- gencode_nodes_transcript(A,_,_,_,_,B,_,_).
dfi(neo_node_prop(subontology,go,A,B))                               :- gene_ontology_nodes_go(A,_,_,B).
dfi(neo_node_prop(synonyms,bto,A,B))                                 :- brenda_tissue_ontology_nodes_bto(A,_,B).
dfi(neo_node_prop(synonyms,cl,A,B))                                  :- cell_ontology_nodes_cl(A,_,B).
dfi(neo_node_prop(synonyms,clo,A,B))                                 :- cell_line_ontology_nodes_clo(A,_,B).
dfi(neo_node_prop(synonyms,efo,A,B))                                 :- experimental_factor_ontology_nodes_efo(A,_,B).
dfi(neo_node_prop(synonyms,gene,A,B))                                :- gencode_nodes_gene(A,_,_,_,_,_,B).
dfi(neo_node_prop(synonyms,go,A,B))                                  :- gene_ontology_nodes_go(A,_,B,_).
dfi(neo_node_prop(synonyms,protein,A,B))                             :- uniprot_nodes_protein(A,_,_,B).
dfi(neo_node_prop(synonyms,uberon,A,B))                              :- nodes_uberon(A,_,B).
dfi(neo_node_prop(term_name,bto,A,B))                                :- brenda_tissue_ontology_nodes_bto(A,B,_).
dfi(neo_node_prop(term_name,cl,A,B))                                 :- cell_ontology_nodes_cl(A,B,_).
dfi(neo_node_prop(term_name,clo,A,B))                                :- cell_line_ontology_nodes_clo(A,B,_).
dfi(neo_node_prop(term_name,efo,A,B))                                :- experimental_factor_ontology_nodes_efo(A,B,_).
dfi(neo_node_prop(term_name,go,A,B))                                 :- gene_ontology_nodes_go(A,B,_,_).
dfi(neo_node_prop(term_name,uberon,A,B))                             :- nodes_uberon(A,B,_).
dfi(neo_node_prop(transcript_id,exon,A,B))                           :- gencode_nodes_exon(A,_,B,_,_,_,_,_).
dfi(neo_node_prop(transcript_id,transcript,A,B))                     :- gencode_nodes_transcript(A,B,_,_,_,_,_,_).
dfi(neo_node_prop(transcript_name,transcript,A,B))                   :- gencode_nodes_transcript(A,_,B,_,_,_,_,_).
dfi(neo_node_prop(transcript_type,transcript,A,B))                   :- gencode_nodes_transcript(A,_,_,B,_,_,_,_).
dfi(neo_node_prop(variant_accession,structural_variant,A,B))         :- dgv_nodes_structural_variant(A,B,_,_,_,_,_).
dfi(neo_node_prop(variant_type,structural_variant,A,B))              :- dbvar_nodes_structural_variant(A,_,_,_,B).
dfi(neo_node_prop(variant_type,structural_variant,A,B))              :- dgv_nodes_structural_variant(A,_,_,_,_,B,_).
dfi(neo_node_proplist(bto,[term_name=A,synonyms=B],C))               :- brenda_tissue_ontology_nodes_bto(C,A,B).
dfi(neo_node_proplist(cl,[term_name=A,synonyms=B],C))                :- cell_ontology_nodes_cl(C,A,B).
dfi(neo_node_proplist(clo,[term_name=A,synonyms=B],C))               :- cell_line_ontology_nodes_clo(C,A,B).
dfi(neo_node_proplist(efo,[term_name=A,synonyms=B],C))               :- experimental_factor_ontology_nodes_efo(C,A,B).
dfi(neo_node_proplist(enhancer,[chr=A,start=B,end=C],D))             :- enhancer_atlas_nodes_enhancer(D,A,B,C).
dfi(neo_node_proplist(enhancer,[enhancer_id=A,chr=B,start=C,end=D,data_source=E],F)) :- peregrine_nodes_enhancer(F,A,B,C,D,E).
dfi(neo_node_proplist(exon,[gene_id=A,transcript_id=B,chr=C,start=D,end=E,exon_number=F,exon_id=G],H)) :- gencode_nodes_exon(H,A,B,C,D,E,F,G).
dfi(neo_node_proplist(gene,[gene_type=A,chr=B,start=C,end=D,gene_name=E,synonyms=F],G)) :- gencode_nodes_gene(G,A,B,C,D,E,F).
dfi(neo_node_proplist(go,[term_name=A,synonyms=B,subontology=C],D))  :- gene_ontology_nodes_go(D,A,B,C).
dfi(neo_node_proplist(non_coding_rna,[chr=A,start=B,end=C,rna_type=D],E)) :- rna_central_nodes_non_coding_rna(E,A,B,C,D).
dfi(neo_node_proplist(pathway,[pathway_name=A],B))                   :- reactome_nodes_pathway(B,A).
dfi(neo_node_proplist(promoter,[chr=A,start=B,end=C],D))             :- epd_nodes_promoter(D,A,B,C).
dfi(neo_node_proplist(protein,[accessions=A,protein_name=B,synonyms=C],D)) :- uniprot_nodes_protein(D,A,B,C).
dfi(neo_node_proplist(snp,[chr=A,start=B,end=C,ref=D,alt=E,caf_ref=F,caf_alt=G],H)) :- dbsnp_nodes_snp(H,A,B,C,D,E,F,G).
dfi(neo_node_proplist(snp,[raw_cadd_score=A,phred_score=B],C))       :- cadd_nodes_snp(C,A,B).
dfi(neo_node_proplist(structural_variant,[chr=A,start=B,end=C,variant_type=D],E)) :- dbvar_nodes_structural_variant(E,A,B,C,D).
dfi(neo_node_proplist(structural_variant,[variant_accession=A,chr=B,start=C,end=D,variant_type=E,evidence=F],G)) :- dgv_nodes_structural_variant(G,A,B,C,D,E,F).
dfi(neo_node_proplist(super_enhancer,[se_id=A,chr=B,start=C,end=D],E)) :- dbsuper_nodes_super_enhancer(E,A,B,C,D).
dfi(neo_node_proplist(tad,[chr=A,start=B,end=C,genes=D],E))          :- tadmap_nodes_tad(E,A,B,C,D).
dfi(neo_node_proplist(tfbs,[chr=A,start=B,end=C],D))                 :- nodes_tfbs(D,A,B,C).
dfi(neo_node_proplist(transcript,[transcript_id=A,transcript_name=B,transcript_type=C,chr=D,start=E,end=F,gene_name=G],H)) :- gencode_nodes_transcript(H,A,B,C,D,E,F,G).
dfi(neo_node_proplist(uberon,[term_name=A,synonyms=B],C))            :- nodes_uberon(C,A,B).
dfi(node_prop(accessions))
dfi(node_prop(alt))
dfi(node_prop(caf_alt))
dfi(node_prop(caf_ref))
dfi(node_prop(chr))
dfi(node_prop(data_source))
dfi(node_prop(end))
dfi(node_prop(enhancer_id))
dfi(node_prop(evidence))
dfi(node_prop(exon_id))
dfi(node_prop(exon_number))
dfi(node_prop(gene_id))
dfi(node_prop(gene_name))
dfi(node_prop(gene_type))
dfi(node_prop(genes))
dfi(node_prop(pathway_name))
dfi(node_prop(phred_score))
dfi(node_prop(protein_name))
dfi(node_prop(raw_cadd_score))
dfi(node_prop(ref))
dfi(node_prop(rna_type))
dfi(node_prop(se_id))
dfi(node_prop(start))
dfi(node_prop(subontology))
dfi(node_prop(synonyms))
dfi(node_prop(term_name))
dfi(node_prop(transcript_id))
dfi(node_prop(transcript_name))
dfi(node_prop(transcript_type))
dfi(node_prop(variant_accession))
dfi(node_prop(variant_type))
dfi(node_prop_for(accessions,protein))
dfi(node_prop_for(alt,snp))
dfi(node_prop_for(caf_alt,snp))
dfi(node_prop_for(caf_ref,snp))
dfi(node_prop_for(chr,enhancer))
dfi(node_prop_for(chr,exon))
dfi(node_prop_for(chr,gene))
dfi(node_prop_for(chr,non_coding_rna))
dfi(node_prop_for(chr,promoter))
dfi(node_prop_for(chr,snp))
dfi(node_prop_for(chr,structural_variant))
dfi(node_prop_for(chr,super_enhancer))
dfi(node_prop_for(chr,tad))
dfi(node_prop_for(chr,tfbs))
dfi(node_prop_for(chr,transcript))
dfi(node_prop_for(data_source,enhancer))
dfi(node_prop_for(end,enhancer))
dfi(node_prop_for(end,exon))
dfi(node_prop_for(end,gene))
dfi(node_prop_for(end,non_coding_rna))
dfi(node_prop_for(end,promoter))
dfi(node_prop_for(end,snp))
dfi(node_prop_for(end,structural_variant))
dfi(node_prop_for(end,super_enhancer))
dfi(node_prop_for(end,tad))
dfi(node_prop_for(end,tfbs))
dfi(node_prop_for(end,transcript))
dfi(node_prop_for(enhancer_id,enhancer))
dfi(node_prop_for(evidence,structural_variant))
dfi(node_prop_for(exon_id,exon))
dfi(node_prop_for(exon_number,exon))
dfi(node_prop_for(gene_id,exon))
dfi(node_prop_for(gene_name,gene))
dfi(node_prop_for(gene_name,transcript))
dfi(node_prop_for(gene_type,gene))
dfi(node_prop_for(genes,tad))
dfi(node_prop_for(pathway_name,pathway))
dfi(node_prop_for(phred_score,snp))
dfi(node_prop_for(protein_name,protein))
dfi(node_prop_for(raw_cadd_score,snp))
dfi(node_prop_for(ref,snp))
dfi(node_prop_for(rna_type,non_coding_rna))
dfi(node_prop_for(se_id,super_enhancer))
dfi(node_prop_for(start,enhancer))
dfi(node_prop_for(start,exon))
dfi(node_prop_for(start,gene))
dfi(node_prop_for(start,non_coding_rna))
dfi(node_prop_for(start,promoter))
dfi(node_prop_for(start,snp))
dfi(node_prop_for(start,structural_variant))
dfi(node_prop_for(start,super_enhancer))
dfi(node_prop_for(start,tad))
dfi(node_prop_for(start,tfbs))
dfi(node_prop_for(start,transcript))
dfi(node_prop_for(subontology,go))
dfi(node_prop_for(synonyms,bto))
dfi(node_prop_for(synonyms,cl))
dfi(node_prop_for(synonyms,clo))
dfi(node_prop_for(synonyms,efo))
dfi(node_prop_for(synonyms,gene))
dfi(node_prop_for(synonyms,go))
dfi(node_prop_for(synonyms,protein))
dfi(node_prop_for(synonyms,uberon))
dfi(node_prop_for(term_name,bto))
dfi(node_prop_for(term_name,cl))
dfi(node_prop_for(term_name,clo))
dfi(node_prop_for(term_name,efo))
dfi(node_prop_for(term_name,go))
dfi(node_prop_for(term_name,uberon))
dfi(node_prop_for(transcript_id,exon))
dfi(node_prop_for(transcript_id,transcript))
dfi(node_prop_for(transcript_name,transcript))
dfi(node_prop_for(transcript_type,transcript))
dfi(node_prop_for(variant_accession,structural_variant))
dfi(node_prop_for(variant_type,structural_variant))
neo_type_of(bto,s,A)                                                 :- brenda_tissue_ontology_edges_bto_subclass_of(A,_).
neo_type_of(bto,t,A)                                                 :- brenda_tissue_ontology_edges_bto_subclass_of(_,A).
neo_type_of(cl,s,A)                                                  :- cell_ontology_edges_cl_capable_of(A,_).
neo_type_of(cl,s,A)                                                  :- cell_ontology_edges_cl_part_of(A,_).
neo_type_of(cl,s,A)                                                  :- cell_ontology_edges_cl_subclass_of(A,_).
neo_type_of(cl,t,A)                                                  :- cell_ontology_edges_cl_subclass_of(_,A).
neo_type_of(clo,s,A)                                                 :- cell_line_ontology_edges_clo_subclass_of(A,_).
neo_type_of(clo,t,A)                                                 :- cell_line_ontology_edges_clo_subclass_of(_,A).
neo_type_of(efo,s,A)                                                 :- experimental_factor_ontology_edges_efo_subclass_of(A,_).
neo_type_of(efo,t,A)                                                 :- experimental_factor_ontology_edges_efo_subclass_of(_,A).
neo_type_of(enhancer,s,A)                                            :- enhancer_atlas_edges_enhancer_gene(A,_,_,_).
neo_type_of(enhancer,s,A)                                            :- peregrine_edges_enhancer_gene(A,_,_).
neo_type_of(exon,t,A)                                                :- gencode_exon_edges_includes(_,A).
neo_type_of(gene,s,A)                                                :- bgee_edges_expressed_in(A,_,_,_,_).
neo_type_of(gene,s,A)                                                :- coxpressdb_edges_coexpressed_with(A,_,_).
neo_type_of(gene,s,A)                                                :- edges_gene_tfbs(A,_,_).
neo_type_of(gene,s,A)                                                :- fabian_edges_tfbs_snp(A,_,_,_).
neo_type_of(gene,s,A)                                                :- gencode_transcript_edges_transcribed_to(A,_).
neo_type_of(gene,s,A)                                                :- gtex_expression_edges_expressed_in(A,_,_,_).
neo_type_of(gene,s,A)                                                :- reactome_edges_genes_pathways(A,_).
neo_type_of(gene,s,A)                                                :- tadmap_edges_in_tad_region(A,_).
neo_type_of(gene,s,A)                                                :- tflink_edges_tf_gene(A,_,_,_,_,_).
neo_type_of(gene,t,A)                                                :- abc_edges_activity_by_contact(_,A,_,_).
neo_type_of(gene,t,A)                                                :- coxpressdb_edges_coexpressed_with(_,A,_).
neo_type_of(gene,t,A)                                                :- dbsuper_edges_super_enhancer_gene(_,A,_).
neo_type_of(gene,t,A)                                                :- enhancer_atlas_edges_enhancer_gene(_,A,_,_).
neo_type_of(gene,t,A)                                                :- epd_edges_promoter_gene(_,A).
neo_type_of(gene,t,A)                                                :- eqtl_edges_gtex_variant_gene(_,A,_,_,_,_).
neo_type_of(gene,t,A)                                                :- gencode_transcript_edges_transcribed_from(_,A).
neo_type_of(gene,t,A)                                                :- gwas_edges_snp_downstream_gene(_,A,_,_).
neo_type_of(gene,t,A)                                                :- gwas_edges_snp_upstream_gene(_,A,_,_).
neo_type_of(gene,t,A)                                                :- gwas_ingene_edges_snp_in_gene(_,A,_).
neo_type_of(gene,t,A)                                                :- peregrine_edges_enhancer_gene(_,A,_).
neo_type_of(gene,t,A)                                                :- refseq_edges_closest_gene(_,A,_,_,_).
neo_type_of(gene,t,A)                                                :- tflink_edges_tf_gene(_,A,_,_,_,_).
neo_type_of(go,s,A)                                                  :- gaf_edges_go_gene_product(A,_,_,_,_).
neo_type_of(go,s,A)                                                  :- gene_ontology_edges_go_subclass_of(A,_).
neo_type_of(go,t,A)                                                  :- cell_ontology_edges_cl_capable_of(_,A).
neo_type_of(go,t,A)                                                  :- gene_ontology_edges_go_subclass_of(_,A).
neo_type_of(go,t,A)                                                  :- rna_central_edges_go_rna(_,A).
neo_type_of(non_coding_rna,s,A)                                      :- rna_central_edges_go_rna(A,_).
neo_type_of(pathway,s,A)                                             :- reactome_edges_child_pathway_of(A,_).
neo_type_of(pathway,s,A)                                             :- reactome_edges_parent_pathway_of(A,_).
neo_type_of(pathway,t,A)                                             :- reactome_edges_child_pathway_of(_,A).
neo_type_of(pathway,t,A)                                             :- reactome_edges_genes_pathways(_,A).
neo_type_of(pathway,t,A)                                             :- reactome_edges_parent_pathway_of(_,A).
neo_type_of(promoter,s,A)                                            :- epd_edges_promoter_gene(A,_).
neo_type_of(protein,s,A)                                             :- string_edges_interacts_with(A,_,_).
neo_type_of(protein,s,A)                                             :- uniprot_edges_translation_of(A,_).
neo_type_of(protein,t,A)                                             :- gaf_edges_go_gene_product(_,A,_,_,_).
neo_type_of(protein,t,A)                                             :- string_edges_interacts_with(_,A,_).
neo_type_of(protein,t,A)                                             :- uniprot_edges_translates_to(_,A).
neo_type_of(snp,s,A)                                                 :- abc_edges_activity_by_contact(A,_,_,_).
neo_type_of(snp,s,A)                                                 :- eqtl_edges_gtex_variant_gene(A,_,_,_,_,_).
neo_type_of(snp,s,A)                                                 :- gwas_edges_snp_downstream_gene(A,_,_,_).
neo_type_of(snp,s,A)                                                 :- gwas_edges_snp_upstream_gene(A,_,_,_).
neo_type_of(snp,s,A)                                                 :- gwas_ingene_edges_snp_in_gene(A,_,_).
neo_type_of(snp,s,A)                                                 :- refseq_edges_closest_gene(A,_,_,_,_).
neo_type_of(snp,s,A)                                                 :- roadmap_dhs_edges_in_dnase_i_hotspot(A,_).
neo_type_of(snp,s,A)                                                 :- roadmap_h3_mark_edges_histone_modification(A,_,_,_).
neo_type_of(snp,t,A)                                                 :- fabian_edges_tfbs_snp(_,A,_,_).
neo_type_of(super_enhancer,s,A)                                      :- dbsuper_edges_super_enhancer_gene(A,_,_).
neo_type_of(tad,t,A)                                                 :- tadmap_edges_in_tad_region(_,A).
neo_type_of(tfbs,t,A)                                                :- edges_gene_tfbs(_,A,_).
neo_type_of(transcript,s,A)                                          :- gencode_exon_edges_includes(A,_).
neo_type_of(transcript,s,A)                                          :- gencode_transcript_edges_transcribed_from(A,_).
neo_type_of(transcript,s,A)                                          :- uniprot_edges_translates_to(A,_).
neo_type_of(transcript,t,A)                                          :- gencode_transcript_edges_transcribed_to(_,A).
neo_type_of(transcript,t,A)                                          :- uniprot_edges_translation_of(_,A).
neo_type_of(uberon,s,A)                                              :- edges_uberon_subclass_of(A,_).
neo_type_of(uberon,t,A)                                              :- cell_ontology_edges_cl_part_of(_,A).
neo_type_of(uberon,t,A)                                              :- edges_uberon_subclass_of(_,A).
neo_type_of(uberon,t,A)                                              :- roadmap_dhs_edges_in_dnase_i_hotspot(_,A).








