:- use_module(library(csv)).
:- use_module(library(filesex)). % For recursive file searching
:- use_module(library(apply)).   % For maplist/2

:- ensure_loaded(library(metta_rt)).

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

:- dynamic(mw_file/2).
load_mw_file(KB, File):- mw_file(KB, File), !.
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
assert_neo_new(_KB, Term):- clause_asserted(Term), !.
assert_neo_new(_KB, Term):- assert(Term).

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

% Example usage:
% ?- use_mw_directory(neo4j_out_v4_mw).

sample_query("Saulo - returns 2 answers ",
    "MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.id, e.id",
    ['T', 'E'],
    [T, E], [
    neo(T:transcript,transcript_id,'ENST00000472835.1'),
    neo(T:transcript,includes,E:exon)]).

sample_query("Saulo - returns 2 answers ",
    "MATCH ((t:transcript {transcript_id: 'ENST00000472835.1'})-[:includes]->(e:exon))
    RETURN t.transcript_name, e.exon_id",
    ['TranscriptName', 'ExonID'],
    [TranscriptName, ExonID], [
    neo(T:transcript,transcript_id,'ENST00000472835.1'),
    neo(T:transcript,includes,E:exon),
    neo(T:transcript,transcript_name,TranscriptName),
    neo(E:exon,exon_id,ExonID)]).



sample_query("Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc",
    "MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target",
    ['Gene1', 'Gene2'],
    [Gene1, Gene2], [
    neo(Gene1,gene_name,"FTO"),
    neo(Gene2,regulates,Gene1)]).

sample_query("Saulo - 502 https://chat.singularitynet.io/snet/pl/ypbc7org4p8odqpubddnogf6cc",
    "MATCH path = (x:gene)-[:regulates*1..1]->(g:gene {gene_name: 'FTO'})
    UNWIND relationships(path) AS rel
    RETURN DISTINCT startNode(rel).gene_name AS regulator, endNode(rel).gene_name AS target",
    ['Regulator', 'Target'],
    [Regulator, Target], [
    neo(Gene1,gene_name,"FTO"),
    neo(Gene2,regulates,Gene1),
    neo(Gene1,gene_name,Target),
    neo(Gene2,gene_name,Regulator)]).

sample_query("1. Find Interactions of BRCA2 Gene",
    "Find all interactions involving the BRCA2 gene, including transcripts, proteins, and pathways.",
    ['Gene', 'Transcript', 'Protein1', 'Protein2', 'Pathway'],
    [Gene, Transcript, Protein1, Protein2, Pathway], [
    neo(Gene, gene_name, startsWith("BRCA2")),
    neo(Gene, transcribed_to, Transcript),
    neo(Transcript, translates_to, Protein1),
    neo(Protein1, interacts_with, Protein2),
    neo(Gene, genes_pathways, Pathway)]).


sample_query("2. Find Components Associated with IGF2",
    "Find promoters, enhancers, pathways, and child pathways associated with the IGF2 gene.",
    ['Promoter', 'Gene', 'Enhancer', 'Pathway', 'Pathway1Child'],
    [Promoter, Gene, Enhancer, Pathway, Pathway1Child], [
    neo(Gene:gene, gene_name, startsWith("IGF2")),
    neo(Promoter:promoter, associated_with, Gene:gene),
    neo(Enhancer:enhancer, associated_with, Gene:gene),
    neo(Gene:gene, genes_pathways, Pathway:pathway),
    neo(Pathway1Child:pathway, child_pathway_of, Pathway:pathway)]).

sample_query("3. Gene Interactions and GO Terms",
    "Find gene interactions and associated GO terms including proteins and transcripts.",
    ['Gene', 'Transcript', 'Exon', 'Protein1', 'Protein2', 'GO1Term'],
    [Gene, Transcript, Exon, Protein1, Protein2, GO1Term], [
    neo(Gene, transcribed_to, Transcript),
    neo(Transcript, includes, Exon),
    neo(Protein1, translation_of, Transcript),
    neo(Protein1, interacts_with, Protein2),
    neo(GO1Term, go_gene_product, Protein1)]).

sample_query("4. Interactions Involving 1433B Protein",
    "Find interactions involving 1433B protein including transcripts, exons, and GO terms.",
    ['Gene', 'Transcript', 'Exon', 'Protein1', 'Protein2', 'GO1Term'],
    [Gene, Transcript, Exon, Protein1, Protein2, GO1Term], [
    neo(Protein1, protein_name, startsWith("1433B")),
    neo(Gene, transcribed_to, Transcript),
    neo(Transcript, includes, Exon),
    neo(Protein1, translation_of, Transcript),
    neo(Protein1, interacts_with, Protein2),
    neo(GO1Term, go_gene_product, Protein1)]).

sample_query("5. Components Associated with IGF1",
    "Find enhancers, pathways, and transcripts associated with the IGF1 gene.",
    ['Gene', 'Pathway', 'Enhancer', 'Transcript', 'Protein'],
    [Gene, Pathway, Enhancer, Transcript, Protein], [
    neo(Gene:gene, gene_name, "IGF1"),
    neo(Gene:gene, genes_pathways, Pathway),
    neo(Enhancer:enhancer, associated_with, Gene:gene),
    neo(Transcript, transcribed_from, Gene:gene),
    neo(Transcript, translates_to, Protein:protein)]).

sample_query("6. Pathways and Protein Interactions for IGF1",
    "Find pathways and interacting proteins for the IGF1 gene including all associated components.",
    ['Gene', 'Pathway', 'Enhancer', 'Transcript', 'Protein1', 'Protein2'],
    [Gene, Pathway, Enhancer, Transcript, Protein1, Protein2], [
    neo(Gene:gene, gene_name, "IGF1"),
    neo(Gene:gene, genes_pathways, Pathway),
    neo(Enhancer:enhancer, associated_with, Gene:gene),
    neo(Transcript, transcribed_from, Gene:gene),
    neo(Transcript, translates_to, Protein1:protein),
    neo(Protein1:protein, interacts_with, Protein2:protein)]).

sample_query("7. Transcripts and Exons for TP73-AS1",
    "Find transcripts and exons associated with the TP73-AS1 gene.",
    ['Transcript', 'Exon', 'Gene'],
    [Transcript, Exon, Gene], [
    neo(Transcript, includes, Exon),
    neo(Transcript, transcribed_from, Gene),
    neo(Gene, gene_name, endsWith("TP73-AS1"))]).

sample_query("8. Interactions Involving 1433S Protein",
    "Find proteins interacting with 1433S and associated GO terms.",
    ['GO1Term', 'Protein1', 'Protein2'],
    [GO1Term, Protein1, Protein2], [
    neo(Protein1, protein_name, stringEqual("1433S")),
    neo(GO1Term, go_gene_product, Protein1),
    neo(Protein1, interacts_with, Protein2)]).

sample_query("9. IGF1 Expression in Tissues and Transcripts",
    "Find IGF1 expression in tissues and related transcripts.",
    ['Gene', 'Uberon', 'Transcript'],
    [Gene, Uberon, Transcript], [
    neo(Gene, gene_name, startsWith("IGF1")),
    neo(Gene, expressed_in, Uberon),
    neo(Gene, transcribed_to, Transcript)]).

sample_query("10. Transcripts and Exons on Chromosome 1",
    "Find transcripts, exons, and interacting proteins located on chromosome 1.",
    ['Transcript', 'Exon', 'Protein1', 'Protein2'],
    [Transcript, Exon, Protein1, Protein2], [
    neo(Transcript, includes, Exon),
    neo(Exon, chr, chr1),
    neo(Transcript, translates_to, Protein1),
    neo(Protein2, interacts_with, Protein1)]).

sample_query("11. IGF1 Gene Expression in Cell Lines",
    "Find IGF1 gene expression in cell lines and related subclass relationships.",
    ['Gene', 'CL1', 'CL2'],
    [Gene, CL1, CL2], [
    neo(Gene, gene_name, "IGF1"),
    neo(Gene, expressed_in, CL1),
    neo(CL2, subclass_of, CL1)]).

sample_query("12. IGF1 Gene Regulation by SNP Activity",
    "Find regulation of the IGF1 gene by SNP activity.",
    ['SNP', 'Gene'],
    [SNP, Gene], [
    neo(Gene, gene_name, startsWith("IGF1")),
    neo(SNP, activity_by_contact, Gene)]).

sample_query("13. IGF1 Gene Interactions and Regulations",
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

sample_query("14. Pathway Associations for SNAP25",
    "Locate SNAP25 in pathways with other genes, including cases where the other gene may be SNAP25 itself.",
    ['Gene1', 'Pathway', 'Gene2'],
    [Gene1, Pathway, Gene2], [
    neo(Gene1, gene_name, startsWith("SNAP25")),
    neo(Gene1, genes_pathways, Pathway),
    neo(Gene2, genes_pathways, Pathway)]).

sample_query("14a. Pathway Associations for SNAP25 - Distinct Genes",
    "Locate SNAP25 in pathways with other genes, ensuring that SNAP25 and other genes are distinct.",
    ['Gene1', 'Pathway', 'Gene2'],
    [Gene1, Pathway, Gene2], [
    neo(Gene1, gene_name, startsWith("SNAP25")),
    neo(Gene1, genes_pathways, Pathway),
    neo(Gene2, genes_pathways, Pathway),
    different(Gene1, Gene2)]).


register_linked(F/A):- assert_if_new(is_registered_link(F, A)), dynamic(F/A).

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



:- register_linked(neo_triple/3).
:- register_linked(neo_triple_l1/3).
:- register_linked(neo_triple_l2/3).
:- register_linked(dfi/1).

:- forall(between(3, 10, A), register_linked(neo_table_label/A)).
:- forall(between(3, 10, A), register_linked(neo_table/A)).

:- cmd_note((link_mw_data(neo4j_out_v3_mw))).
link_mw_data(KB):-
    %abolish(neo_triple/3),
    clear_linked, dyn_linked, !,
    forall(mw_scheme_info(KB, Term),
         must_link_mw_term(KB, Term)),
    show_linked.

mw_note_xtreme(_).


show_m2(N):- m2(N,Q),m(Q),writeln(Q).
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

call_each(_M, List):-
  call_each_unique(List, _Vars).


call_each_unique(List, Vars):-
  must_det_lls((
   mark_non_difs(List),
   term_variables(List, IVars),
   all_dif(IVars),
   if_t(var(Vars), Vars=IVars))),
   term_variables(List+Vars, RmDifs),
   nop(no_repeats_var(NRV)), !,
   match_template(List),
   (NRV=Vars->true;(nop(wdmsg(repeated(Vars))), fail)),
   maplist(remove_dif2, RmDifs).

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

    */

all_dif([]).     % Base case: empty list is trivially distinct
all_dif([_]).    % Base case: a single element is trivially distinct
all_dif([X | Xs]) :-
    dif_all(X, Xs), % Ensure X is different from every element in Xs
    all_dif(Xs).     % Recursively check the rest of the list

% Ensure X is different from every element in the list
dif_all(_, []).
dif_all(X, [Y | Ys]) :-
    ignore(( \+ is_ldif(X), \+ is_ldif(Y), dif(X, Y))), % Constraint: X must be different from Y
    dif_all(X, Ys). % Continue checking with the rest

is_ldif(Y):- attvar(Y), get_attr(Y, ldif, _).

produce_list_call(M):-
 forall(between(1, 20, N),
    ( functor(Head, M, N),
      Head =.. [M | Args],
      retractall(Head),
      Body =.. [call_each, M, Args],
      assert_if_new((Head :- Body))
    )).

:- produce_list_call(m).


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

dfq([Neo|Args]):- is_list(Args), Neo\=='=', atom(Neo), !, match_template([[Neo|Args]]).


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


neo3(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_edge_typed(Label, ST, TT, O, S)).
%neo3(S2, Label, O2):- s2_s_st(S2, S, ST), s2_s_st(O2, O, TT), dfi(neo_edge_link(Label, O, S)), maybe_typed(ST, S), maybe_typed(TT, O).
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
link_mw_term(KB, analysis_source_file(T, GL, _Size)):- text_contains_text(GL, '/global_links/'), unknown_table(T),
    Pred=..[T, S, O], !,
    assert_neo_new(KB, (dfi(neo_edge_link(T, O, S)):- Pred)), !.

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
       assert_neo_new(KB, (neo_type_of(ST, s, S):- Pred))),
    if_t(single_value(T, 'target_type', TT),
       assert_neo_new(KB, (neo_type_of(TT, t, O):- Pred))),
    !.

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

% Predicate to execute and print results of a single sample query with a time limit

add_props(L1, L2):- (atom(L1);string(L1)), atom_concat(L1, ' (With Properties)', L2), !.
add_props(L1, L2):- \+ compound(L1), !, L1=L2.
add_props(neo(S, P, O), neo_P(S, P, O, _)):-!.
add_props(L1, L1).

remove_eqs(Vars2, Vars, NewVars) :-
    exclude(var_in_list(Vars), Vars2, NewVars).

var_in_list(List, Var) :-
    member(ExistingVar, List),
    ExistingVar == Var.  % Strict equality check (no unification)


run_sample_query(Name, Desc, VNs, Vars, Body):-
  run_sample_query1(Name, Desc, VNs, Vars, Body),
  must_det_lls((maplist(add_props, [Name, Desc| Body], [Name2, Desc2| Body2]), !,
   term_variables(Body2, AllVars), remove_eqs(AllVars, Vars, NewVars),
   length(NewVars, N), generate_var_names(N, NewVNs),
   append(Vars, NewVars, Vars2), append(VNs, NewVNs, VNs2))),
  %nop
  (run_sample_query1(Name2, Desc2, VNs2, Vars2, Body2)), !.

generate_var_names(N, Names) :-
    findall(Prop, (between(1, N, I), atomic_list_concat(['PropList', I], Prop)), Names).

run_sample_query1(Name, Desc, VNs, Vars, Body):- nl, nl, nl, nl,
    format('### ~w~n',[Name]),
    writeln('```no-wrap'),
    writeln('================================================================='),
    format('\t~w~n', [Desc]), % Prints the name and description of the query
    Time = 60,
    \+ \+
    ( maplist(mw_set_varname, Vars, VNs),
      Result =.. [result|Vars], nl,
      CBody =.. [','| Body],
      mw_write_src(match('&neo4j_out_v3', CBody, Result)), nl), !,

    time(   % Attempt to execute the query within a Time-second time limit
        catch(call_with_time_limit(Time, execute_query(Body, VNs, Vars)),
              time_limit_exceeded,
              format('Time limit ~w exceeded for ~w~n', [Time, Name]))
    ;   % If no more results or after handling time limit exceeded
        true
    ), !,
    flag(result_count, Length, Length), % Get the number of results
    nb_current(last_result_at, ResultTimeTaken),
    format(' Last answer found: ~2f seconds~n', [ResultTimeTaken]),
    format(' Number of answers: ~D~n', [Length]), !,
    writeln('================================================================='),
    writeln('```').

% Helper predicate to execute the query, record and print execution time and number of results
execute_query(Body, _VNs, Vars):-
    flag(result_count, _, 0),
    clear_results_for(Body),
    statistics(cputime, StartTime), % Start timing
    nb_setval(last_result_at, StartTime),
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

%```



end_of_file.

b=bound
u=unbound
i=ignored
q=query_var
s=shared

% should switch these around
neo(uq, b, uq), neo(b, qv, us).


