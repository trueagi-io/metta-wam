:- use_module(library(csv)).
:- use_module(library(filesex)). % For recursive file searching
:- use_module(library(apply)).   % For maplist/2

:- ensure_loaded(library(metta_rt)).

mwnv(PB) :-  numbervars(PB,0,_,[singletons(true),attvar(bind)]).
mw_writeq(PB):- ignore( \+  ( mwnv(PB),format('~N~q.~n',[PB]))).
mw_note(Note):- ignore(format(user_error,'~N% MW-NOTE: ~q. ~N',[Note])).
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
    directory_files_ending_with(KBExpanded, '_mw.qlf', Files),
    length(Files,Len),
    mw_note(KBExpanded=Len),
    maplist(load_mw_file(KB), Files),!.

:- dynamic(mw_file/2).
load_mw_file(KB, File):- mw_file(KB, File),!.
load_mw_file(KB, File):-
    assert(mw_file(KB, File)),
    mw_note(start(load_mw_file(KB, File))),
    ensure_loaded(File),
    mw_note(ended(load_mw_file(KB, File))),!.

:- cmd_note((load_mw_info(neo4j_out_v3_mw))).
load_mw_info(KB):- expand_file_name(KB, [KBExpanded]), % Expand '~' or other paths
    KBExpanded\==KB,!,load_mw_info(KBExpanded).
load_mw_info(KB):-
    exists_directory(KB),!,
    absolute_file_name(column_analysis_log,InfoFile,[relative_to(KB),file_type(prolog)]),
    load_mw_info(KB, InfoFile),!.

load_mw_info(KB, File) :-
    setup_call_cleanup(
        open(File, read, Stream),
        stream_mw_info(KB, Stream),
        close(Stream)
    ).

:- dynamic(mw_scheme_info/2).
stream_mw_info(KB, Stream) :-
   repeat,
    read(Stream, Term),
    ( Term == end_of_file
      -> !
    ; ( assert(mw_scheme_info(KB, Term)),
        fail)
    ).

assert_neo_new(_KB,Term):- mw_writeq(Term),fail.
assert_neo_new(_KB,Term):- clause_asserted(Term),!.
assert_neo_new(_KB,Term):- assert(Term).

select_id_field(List,Subj,Rest):- priority_id_name(Subj),select(Subj,List,Rest).
priority_id_name('id').
priority_id_name('source_id').
priority_id_name(_).

% Recursively find and use all CSV files in a given base directory
use_csv_directory(KB) :-
    expand_file_name(KB, [KBExpanded]), % Expand '~' or other paths
    %absolute_file_name(column_analysis_log,InfoFile,[relative_to(KBExpanded),file_type(prolog)]),
    %load_mw_info(KB,InfoFile),
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
    maplist({Dir}/[Entry,Path]>>atomic_list_concat([Dir, '/', Entry], Path), CleanEntries, Paths),
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
    file_name_extension(Base, _, CsvFile),  % Extract base name
    atom_string(Predicate, Base),           % Convert to atom for predicate name
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


register_linked(F/A):- assert_if_new(is_registered_link(F,A)), dynamic(F/A).

clear_linked:-
  forall(is_registered_link(F,A),clear_linked(F,A)).
clear_linked(F,A):- dynamic(F/A),functor(P,F,A),retractall(P).

dyn_linked:-
  forall(is_registered_link(F,A),dyn_linked(F,A)).
dyn_linked(F,A):- dynamic(F/A).


:- cmd_note(show_linked).
show_linked:-  show_linked(3).
:- cmd_note(show_linked(1)).
show_linked(Max) :- forall(is_registered_link(F, A), show_linked(F, A, Max)).
% Predicate to display the rules and up to Max facts of a predicate P
show_linked(F, A, Max) :- functor(P, F, A), predicate_property(P, number_of_clauses(N)), N>0, !,
   ignore((N<100, findall((P:-B),(clause(P, B), B\==true),L),sort(L,S),forall(member((PB),S),mw_writeq(PB)))),
   ignore((predicate_property(P, number_of_rules(0))->forall(limit(Max, P), mw_writeq(P));forall(limit(Max, clause(P, true)), mw_writeq(P)))).
show_linked(F, A, _Max):- nop(format('No Facts/Rules: ~q~n',[F/A])),!.


:- cmd_note(show_three).
show_three:-
   forall(mw_scheme_info(_,predicate_schema(Table,List)),ignore(show_three(Table,List))).

show_three(Table,List):-
   length(List,A),
   format('~N%~n%~q(~w).~n',[Table,List]),
   show_linked(Table, A, 3),!,
   forall(mw_scheme_info(_,analysis_source_file(Table,_,Size)),      mw_writeq(analysis_source_file(Table,Size))),
   forall(mw_scheme_info(_,analysis_column(Table, F, S, T, VL, VH)), mw_writeq(analysis_column(Table, F, S, T, VL, VH))),

   %A\==2,
   link_mw_term(_KB, predicate_schema(Table, List)).



:- register_linked(neo_triple/3).
:- register_linked(neo_triple_l1/3).
:- register_linked(neo_triple_l2/3).
:- forall(between(3,10,A),register_linked(neo_table_label/A)).
:- forall(between(3,10,A),register_linked(neo_table/A)).

:- cmd_note((link_mw_data(neo4j_out_v3_mw))).
link_mw_data(KB):-
    %abolish(neo_triple/3),
    clear_linked, dyn_linked,!,
    forall(mw_scheme_info(KB, Term),
         must_link_mw_term(KB, Term)),
    show_linked.

mw_note_xtreme(_).

must_link_mw_term( KB, Term):- link_mw_term(KB, Term),!, mw_note_xtreme(confirmed(do_link_mw_term(KB, Term))).
must_link_mw_term(_KB, Term):- \+ \+ unused_link_mw_term(Term),!.
must_link_mw_term( KB, Term):- mw_note(skipped(must_link_mw_term(KB, Term))),!.

unused_link_mw_term(predicate_schema(_,_)):-!, fail.
unused_link_mw_term(_).

/*
previous_predicate_schema('reactome_nodes_pathway', [id, label, pathway_name]).
analysis_source_file('reactome_nodes_pathway', 'neo4j_out_v3_csv/reactome/nodes_pathway.csv', 2673).
original_columns('reactome_nodes_pathway', ['id', 'label', 'pathway_name']).
removed_columns('reactome_nodes_pathway', ['label']).
final_columns('reactome_nodes_pathway', ['id', 'pathway_name']).
analysis_column('reactome_nodes_pathway', 'id', 2673, 'str', 'r-hsa-1059683', 'r-hsa-997272').
analysis_column('reactome_nodes_pathway', 'label', 1, 'str', 'pathway', 'pathway').
analysis_column('reactome_nodes_pathway', 'pathway_name', 2656, 'str', '2-LTR circle formation', 'vRNP Assembly').
predicate_schema('reactome_nodes_pathway', [id, pathway_name]).
*/
link_mw_term(KB, predicate_schema(Table, List)):-
  A = 2, length(List,A), functor(Pred,Table,A),
  mw_scheme_info(KB, analysis_column(Table, 'label', 1, 'str', Label, _)),
  select_id_field(List,SubjF,[ObjF]),
  nth1(SubjNth,List,SubjF),nth1(ObjNth,List,ObjF),
  arg(SubjNth,Pred,S),arg(ObjNth,Pred,O),!,
  assert_neo_new(KB, (neo_triple(S,Label,O):- Pred)).

link_mw_term(KB, predicate_schema(Table, List)):-
  select_id_field(List,SubjF,More),
  nth1(SubjNth,List,SubjF),
  length(List,A), functor(Pred,Table,A), arg(SubjNth,Pred,S),
  forall(member(ObjF,More),
  (nth1(ObjNth,List,ObjF),
   %mw_scheme_info(KB, analysis_column(Table, 'label', 1, 'str', Label, _)),
   arg(ObjNth,Pred,O),
   assert_neo_new(KB, (neo_triple_l1(S,ObjF,O):- Pred)))),
 nop((forall(
    (mw_scheme_info(KB, analysis_column(Table, Label, 1, _, Value, _)),
     \+ member(Label,List)),
     assert_neo_new(KB, (neo_triple_l2(S,Label,Value):- Pred))))).

neo2p(Q,A):- string(Q),atom_string(A,Q).
neo2p(Q,A):- \+ compound(Q),!,Q=A.
neo2p(fnR(P,Q,A),A):- freeze(P,freeze(Q,freeze(A,call(P,Q,A)))).
neo2p(fnL(P,Q,A),Q):- freeze(P,freeze(Q,freeze(A,call(P,Q,A)))).
neo2p(Q,A):- arg(2,Q,M),nonvar(M),neo2p(M,A),!.
neo2p(Q,A):- arg(1,Q,M),nonvar(M),neo2p(M,A),!.

neo(S,Label,O):- neo2p(S,S2),neo2p(S,S2),neo2p(S,S2),neo_triple(S,Label,O).

:- cmd_note((sample_query(_Results))).
sample_query([Promoter, Gene, Enhancer, Pathway, ChildPathway]):-
    neo(Gene, ex:gene_name, fnL(substring,_,"IGF2")),
    neo(Promoter, ex:associated_with, Gene),
    neo(Enhancer, ex:associated_with, Gene),
    neo(Gene, ex:genes_pathways, Pathway),
    neo(ChildPathway, ex:child_pathway_of, Pathway).


