:- use_module(library(csv)).
:- use_module(library(filesex)). % For recursive file searching
:- use_module(library(apply)).   % For maplist/2

:- ensure_loaded(library(metta_rt)).

% Recursively find and process all _mw.pl files in a given base directory
process_mw_directory(DataSlug) :-
    expand_file_name(DataSlug, [DataSlugExpanded]), % Expand '~' or other paths
    absolute_file_name(column_analysis_log,InfoFile,[relative_to(DataSlugExpanded),file_type(prolog)]),
    assert_file_contents(DataSlug,InfoFile),
    directory_files_ending_with(DataSlugExpanded, '_mw.qlf', Files),
    maplist(process_mw_file(DataSlug), Files),
    confirm_mw_data(DataSlug).

:- dynamic(mw_file/2).
process_mw_file(DataSlug, File):-
    assert(mw_file(DataSlug, File)),
    ensure_loaded(File).

assert_file_contents(DataSlug, File) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_and_assert_mw_directory_data(DataSlug, Stream),
        close(Stream)
    ).

:- dynamic(mw_directory_data/2).
read_and_assert_mw_directory_data(DataSlug, Stream) :-
   repeat,
    read(Stream, Term),
    ( Term == end_of_file
      -> !
    ; ( assert(mw_directory_data(DataSlug, Term)),
        fail)
    ).


confirm_mw_data(DataSlug):-
    forall(mw_directory_data(DataSlug, Term),
         must_process_mw_term(DataSlug, Term)).


must_process_mw_term(DataSlug, Term):- process_mw_term(DataSlug, Term),!.
must_process_mw_term(DataSlug, Term):- write_src_nl(skip(must_process_mw_term(DataSlug, Term))),!.

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
process_mw_term(DataSlug, predicate_schema(Table, List)):-
  select_id_field(List,SubjF,[ObjF]),
  nth1(SubjNth,List,SubjF),nth1(ObjNth,List,ObjF),!,
  mw_directory_data(DataSlug, analysis_column(Table, 'label', 1, 'str', Label, _)),
  length(List,A),
  functor(Pred,Label,A),
  arg(SubjNth,Pred,S),arg(ObjNth,Pred,O),
  assert_if_new((neo_triple(S,Label,O):- Pred)).

select_id_field(List,Subj,Rest):- priority_id_name(Subj),select(Subj,List,Rest).
priority_id_name('id').
priority_id_name('source_id').
priority_id_name(_).

% Recursively find and process all CSV files in a given base directory
process_csv_directory(DataSlug) :-
    expand_file_name(DataSlug, [DataSlugExpanded]), % Expand '~' or other paths
    %absolute_file_name(column_analysis_log,InfoFile,[relative_to(DataSlugExpanded),file_type(prolog)]),
    %assert_file_contents(DataSlug,InfoFile),
    directory_files_ending_with(DataSlugExpanded, '.csv', CsvFiles),
    maplist(process_csv_file, CsvFiles).

% Recursively find files matching a given extension
directory_files_ending_with(DataSlugExpanded, Pattern, Files) :-
    directory_files_ending_with(DataSlugExpanded, Pattern, [], Files).

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
process_csv_file(CsvFile) :-
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
% ?- process_csv_directory('/path/to/csv_directory').




