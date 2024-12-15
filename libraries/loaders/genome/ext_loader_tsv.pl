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
% PROGRAM FUNCTION: processes, cleans, and structures FlyBase dataset tables for efficient 
% querying and analysis.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  fix_header_names(+Fn, +Header, -GNames) is det.
%
%   Processes a list of header names and generates a list of argument types 
%   (`GNames`) based on the provided function (`Fn`) and the headers.
%
%   This predicate applies the function `Fn` to each element in the `Header` list
%   to generate corresponding argument types. Only non-empty argument types are
%   included in the resulting `GNames` list.
%
%   @arg Fn      A callable that is applied to each header to determine its argument type.
%   @arg Header  A list of header names (e.g., strings or atoms) to be processed.
%   @arg GNames  A list of generated names (argument types) after filtering out empty values.
%
%   @example
%     % Define a function that returns the length of a header name.
%     ?- Fn = (Header, Result) :- atom_length(Header, Result),
%        fix_header_names(Fn, [name, id, code], GNames).
%     GNames = [4, 2, 4].
%
fix_header_names(Fn, Header, GNames) :-
    % Apply the given function `Fn` to each element of `Header` and store results in `ArgTypes`.
    maplist(fix_header_names(Header, Fn), Header, ArgTypes),
    % Filter out empty results from `ArgTypes` to produce the final list of `GNames`.
    include(\=(''), ArgTypes, GNames).

%!  fix_header_names(+FL, +Fn, +ID, -Out) is det.
%
%   Standardizes and processes header identifiers (`ID`) into a desired format (`Out`).
%   This includes removing prefixes and suffixes, converting plural forms to singular, 
%   and handling list-related terms.
%
%   This predicate supports a variety of naming conventions and transformations, 
%   ensuring consistent and usable header names.
%
%   @arg FL   A list of fields used for validation or transformation rules.
%   @arg Fn   A transformation function or naming convention applied to headers.
%   @arg ID   The input header identifier to be processed.
%   @arg Out  The resulting standardized identifier.
%
%   @example
%     % Convert plural forms to list types:
%     ?- fix_header_names([], _, 'user_ids', Out).
%     Out = listOf(user_id).
%
%     % Retain generic names unchanged:
%     ?- fix_header_names([], _, 'username', Out).
%     Out = username.
%

%fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_','_id','_ID']),symbol_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
% Numeric IDs are returned as-is since they require no transformation.
fix_header_names(_FL, _Fn, ID, Out) :- 
    % Check if the input ID is a number.
    number(ID), !, Out = ID.
% Handle list types like `listOf(ID)` by recursively processing the inner `ID`.
fix_header_names(FL, Fn, listOf(ID), listOf(Out)) :- 
    % Recursively process the `ID` inside the `listOf` structure.
    fix_header_names(FL, Fn, ID, Out), !.
% Process list types with a separator, such as `listOf(ID, Sep)`.
fix_header_names(FL, Fn, listOf(ID, Sep), listOf(Out, Sep)) :- 
    % Process the `ID` recursively, maintaining the separator.
    fix_header_names(FL, Fn, ID, Out), !.
% Remove suffixes or unwanted characters like `#`, space, or `_` at the end.
fix_header_names(FL, Fn, ID, Out) :- 
    % Identify and remove specific suffixes.
    member(RF, ['#', ' ', '_']), 
    % Strip the suffix from the identifier.
    symbol_concat(MID, RF, ID), 
    % Process the stripped identifier.
    !,fix_header_names(FL, Fn, MID, Out).
% Remove unwanted prefixes like `#`, space, or `_` at the beginning.
fix_header_names(FL, Fn, ID, Out) :- 
    % Identify and remove specific prefixes.
    member(RF, ['#', ' ', '_']), 
    % Strip the prefix from the identifier.
    symbol_concat(RF, MID, ID), 
    % Process the stripped identifier.
    !,fix_header_names(FL, Fn, MID, Out).
% Standardize compound names separated by `__` or spaces.
fix_header_names(FL, Fn, ID, Out) :- 
    % Handle names with double underscores or spaces.
    member(RF, ['__', ' ']), 
    % Split the identifier into multiple parts.
    symbolic_list_concat(MIDL, RF, ID), 
    % Ensure the identifier has multiple parts.
    MIDL \= [_], 
    % Rejoin the parts using underscores.
    symbolic_list_concat(MIDL, '_', MID), 
    % Process the standardized name.
    !,fix_header_names(FL, Fn, MID, Out).
% Handle plural or list-related terms like `(es)`, `(s)`, or `ids`.
fix_header_names(FL, Fn, ID, listOf(AOut)) :- 
    % Identify suffixes related to plurals or lists.
    member(RF, ['(es)', '(s)', 'ids']), 
    % Split the identifier by the suffix.
    symbolic_list_concat([Left, Right], RF, ID), 
    % Rejoin the parts with an underscore.
    symbolic_list_concat([Left, Right], '_', MID), 
    % Process the standardized name recursively.
    !,fix_header_names(FL, Fn, MID, AOut), !.
% Process specific mappings like `IDs` to `ID` to create a `listOf` format.
fix_header_names(FL, Fn, TT, listOf(AOut)) :- 
    % Map specific patterns like `IDs` to `ID`.
    member(IDs = ID, ['IDs' = 'ID']), 
    % Concatenate the type with `IDs` to form the original term.
    symbol_concat(Type, IDs, TT), 
    % Replace `IDs` with `ID` to standardize the term.
    symbol_concat(Type, ID, MID), 
    % Process the updated name recursively.
    fix_header_names(FL, Fn, MID, AOut), !.
% Handle `_IDs` to `_ID` and convert to `listOf` format.
fix_header_names(FL, Fn, ID, listOf(AOut)) :- 
    % Map `_IDs` patterns to `_ID`.
    member(RFS = RF, ['_IDs' = '_ID', 'IDs' = 'ID']), 
    % Split the identifier by the pattern.
    symbolic_list_concat([Left, Right], RFS, ID), 
    % Rejoin the parts with the updated term.
    symbolic_list_concat([Left, Right], RF, MID), 
    % Process the updated name recursively.
    !,fix_header_names(FL, Fn, MID, AOut), !.
% Retain non-generic names unchanged.
fix_header_names(_, _, Name, Name) :- 
    % Ensure the name is specific and not too generic.
    \+ too_generic(Name), !.
% Retain compound names with multiple underscores unchanged.
fix_header_names(_, _, Name, Name) :- 
    % Ensure the name has multiple underscore-separated parts.
    symbolic_list_concat([_, _|_], '_', Name), !.
% fix_header_names(_, Fn, ID, Out):- symbolic_list_concat([Fn, ID], '_column_', Out).
% fix_header_names(FieldList, Fn, ID, Out):- 
%     symbolic_list_concat([Fn, ID], '_', Out), 
%     \+ member(Out, FieldList).
% Default case: retain the name unchanged.
fix_header_names(_, _, Name, Name).

%!  pmt is det.
%
%   Processes all FlyBase tables and generates missing column data where applicable.
%   For each table in `flybase_tables/1`, if column data (`flybase_cols/2`) is 
%   missing, this predicate generates the column table entry by invoking `get_fbt/1`.
%
%   This predicate is a utility to ensure all FlyBase tables have their 
%   associated column data registered in the database.
%
%   @example
%     % Run the predicate to process all FlyBase tables:
%     ?- pmt.
%
%     % For tables missing column data, it will print `get_fbt/1` terms for those tables.
%
pmt :-
    % Retrieve the list of FlyBase tables.
    flybase_tables(FBT),
    % Iterate over each table in the list.
    for_all(
        member(T, FBT),
        (
            % Check if the table has no associated column data.
            '\\+'(flybase_cols(T, _)) ->
                % If missing, print a `get_fbt/1` term for the table.
                format('~N~q.~n', [get_fbt(T)]);
            % Otherwise, do nothing.
            true)).

%!  use_flybase_cols(+Table, +Columns) is det.
%
%   Associates column information with a FlyBase table and registers its arity.
%   This predicate processes the provided column names (`Columns`) for the 
%   specified table (`Table`), applies header fixes, and stores the resulting
%   information for use in the database.
%
%   - Standardizes column names using `fix_header_names/3`.
%   - Asserts the column information into the knowledge base as 
%     `flybase_col_names(Table, Columns, ArgTypes)`.
%   - Registers the arity of the table using `do_arity_2_names/2`.
%
%   @arg Table   The FlyBase table name for which column information is processed.
%   @arg Columns A list of column names to be associated with the table.
%
%   @example
%     % Use standardized columns for a FlyBase table:
%     ?- use_flybase_cols(my_table, ['col1', 'col2', 'col3']).
%
%     % The column data will be processed, standardized, and registered.
%
use_flybase_cols(Table, Columns) :-
    must_det_ll((
        % Apply `fix_header_names/3` to standardize column names.
        maplist(fix_header_names(Columns, Table), Columns, ArgTypes),
        % Assert the column information into the knowledge base.
        assert(flybase_col_names(Table, Columns, ArgTypes)),
        % Register the table's arity using the standardized argument types.
        do_arity_2_names(Table, ArgTypes)
    )).

%!  do_arity_2_names(+Table, +ArgTypes) is det.
%
%   Processes a table and its argument types to generate and assert arity-2 predicates.
%   For a given table and its argument types, this predicate:
%   - Constructs a functor for the table with the appropriate arity.
%   - Iteratively generates arity-2 predicates for each argument.
%
%   @arg Table     The name of the table being processed.
%   @arg ArgTypes  A list of argument types for the table.
%
%   @example
%     % Generate and assert arity-2 predicates for a table:
%     ?- do_arity_2_names(my_table, ['id', 'name', 'value']).
%
%     % This will create and assert arity-2 predicates like:
%     % my_table_name(DataID, NameValue) :- data_my_table(DataID, NameValue, _).
%
do_arity_2_names(Table, [ID | ArgTypes]) :-
    must_det_ll((
        % Create the functor for the table.
        symbol_concat('data_', Table, F),
        % Determine the arity of the table.
        length([ID | ArgTypes], Arity),
        % Create a list of arguments matching the table's arity.
        length(Args, Arity),
        % Construct the table call.
        DataCall =.. [F | Args],
        % Process each argument to generate arity-2 predicates.
        do_arity_2_names_dc(Table, DataCall, 2, ArgTypes)
    )).

%!  do_arity_2_names_dc(+Table, +DataCall, +N, +ArgTypes) is det.
%
%   Iteratively generates arity-2 predicates for each argument of a table.
%   For each argument, this predicate calls `do_arity_2_names_dc1/4` to handle
%   the generation and assertion of the corresponding arity-2 clause.
%
%   @arg Table     The name of the table being processed.
%   @arg DataCall  The constructed functor call for the table.
%   @arg N         The position of the current argument in the list.
%   @arg ArgTypes  The remaining argument types to process.
%
do_arity_2_names_dc(Table, DataCall, N, [Nth | ArgTypes]) :-
    % Process the current argument to create its arity-2 predicate.
    do_arity_2_names_dc1(Table, DataCall, N, Nth),
    !,
    % Increment the argument position and process the remaining arguments.
    N2 is N + 1,
    do_arity_2_names_dc(Table, DataCall, N2, ArgTypes).
% Base case: Stop when there are no more arguments to process.
do_arity_2_names_dc(_Table, _DataCall, _N, []).

%!  do_arity_2_names_dc1(+Table, +DataCall, +N, +Nth) is det.
%
%   Generates and asserts an arity-2 predicate for the Nth argument of a table.
%   This predicate constructs the clause for the Nth argument and asserts it
%   into the knowledge base.
%
%   @arg Table     The name of the table being processed.
%   @arg DataCall  The constructed functor call for the table.
%   @arg N         The position of the argument in the list.
%   @arg Nth       The name of the Nth argument being processed.
%
do_arity_2_names_dc1(Table, DataCall, N, Nth) :-
    must_det_ll((
        % Extract the first argument (e.g., DataID).
        arg(1, DataCall, Arg1Data),
        % Extract the Nth argument value.
        arg(N, DataCall, Arg2Data),
        % Create the name for the arity-2 predicate.
        make_arity_2_name(Table, Nth, Arity2),
        % Construct the first argument of the arity-2 predicate.
        Arg1 =.. [Table, Arg1Data],
        % Clip '_id' suffix from the argument name, if present.
        clip_id(Nth, NthNoID),
        % Construct the second argument.
        (Nth == NthNoID -> Arg2 = Arg2Data ; Arg2 =.. [NthNoID, Arg2Data]),
        % Construct the arity-2 predicate clause.
        Arity2Call =.. [Arity2, Arg1, Arg2],
        % Debugging/logging output for the clause being asserted.
        fbug((Arity2Call :- DataCall)),
        % Assert the clause into the knowledge base.
        fb_assert((Arity2Call :- DataCall))
    )).

%!  make_arity_2_name(+Table, +Nth, -Arity2) is det.
%
%   Generates the name for an arity-2 predicate based on the table and argument.
%   If the argument name contains the table name, it is returned as-is.
%   Otherwise, a new name is generated by concatenating the table and argument names.
%
%   @arg Table     The name of the table.
%   @arg Nth       The name of the argument being processed.
%   @arg Arity2    The generated name for the arity-2 predicate.
%
make_arity_2_name(Table, Nth, Arity2) :-
    % Clip '_id' suffix from the argument name.
    clip_id(Nth, NthNoID),
    % Check if the argument already contains the table name.
    (symbol_concat(Table, _, Nth)
        % Use the argument name directly if it contains the table name.
        -> Arity2 = Nth
        % Otherwise, concatenate the table and argument names.
        ; symbolic_list_concat([Table, NthNoID], '_', Arity2)).

%!  clip_id(+Nth, -ID) is det.
%
%   Removes the '_id' suffix from an argument name, if present.
%   If the suffix is not present, the argument name is returned unchanged.
%
%   @arg Nth   The original argument name.
%   @arg ID    The argument name with '_id' clipped, if applicable.
%
clip_id(Nth, ID) :-
    % Check if the name ends with '_id' and remove it.
    (symbol_concat(ID, '_id', Nth) -> true ; Nth = ID),
    !.

%!  setup_flybase_cols is det.
%
%   Processes all FlyBase tables with their columns to ensure proper setup.
%   This predicate iterates over all entries in `flybase_cols/2`, retrieving 
%   each table and its associated columns, and calls `use_flybase_cols/2` 
%   to process and register the column information for each table.
%
%   This predicate ensures that all column data for FlyBase tables is standardized 
%   and properly asserted into the knowledge base.
%
%   @example
%     % Set up column information for all FlyBase tables:
%     ?- setup_flybase_cols.
%
%     % Each table in `flybase_cols/2` will be processed using `use_flybase_cols/2`.
%
setup_flybase_cols :-
    % Iterate over all entries in `flybase_cols/2`.
    for_all(
        flybase_cols(Table, Columns),
        % Process each table and its associated columns.
        use_flybase_cols(Table, Columns)
    ).

%:- load_flybase("das_precomputed/allele_genetic_interactions_fb_2022_06.tsv").

:- use_module(library(csv)).

%!  write_flybase_data(+ArgTypes, +Fn, +DataL) is det.
%
%   Asserts FlyBase data into the knowledge base after processing it.
%
%   This predicate takes a list of data (`DataL`) and processes it according 
%   to the given function name (`Fn`) and argument types (`ArgTypes`).
%   - Empty data lists and single-element lists are ignored.
%   - The data is transformed using `fast_column/2`, combined into a term using
%     the function name `Fn`, and asserted into the knowledge base.
%
%   @arg ArgTypes  The expected argument types (currently unused in this predicate).
%   @arg Fn        The function name used to construct the data term.
%   @arg DataL     The list of data to process and assert.
%
%   @example
%     % Process and assert FlyBase data:
%     ?- write_flybase_data([arg1, arg2], my_function, [data1, data2, data3]).
%
%     % This will construct and assert:
%     % my_function(data1, data2, data3).
%

% Base case: Ignore empty data lists.
write_flybase_data(_ArgTypes, _Fn, []) :- !.
% Ignore data lists containing a single empty string.
write_flybase_data(_ArgTypes, _Fn, ['']) :- !.
% Ignore data lists containing a single element.
write_flybase_data(_ArgTypes, _Fn, [_]) :- !.
% Process and assert non-trivial data lists.
write_flybase_data(_ArgTypes, Fn, DataL0) :-
    % Apply `fast_column/2` to transform the data list.
    maplist(fast_column, DataL0, DataL),
    % Combine the transformed data into a term using `Fn`.
    !,Data =.. [Fn | DataL],
    % Assert the constructed term into the knowledge base.
    assert_MeTTa(Data).
% write_flybase_data(_ArgTypes, Fn, DataL):- into_datum(Fn, DataL, Data), assert_OBO(Data).


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

%!  make_assertion(+Fn, +Cols, +NewData, +OldData) is det.
%
%   Creates or processes assertions based on the provided function, columns, 
%   and data. Delegates to `make_assertion4/4` if called with this signature.
%
%   @arg Fn       The function name for the assertion.
%   @arg Cols     The list of columns associated with the data.
%   @arg NewData  The new data to be asserted.
%   @arg OldData  The existing data being compared or replaced.
%
make_assertion(Fn, Cols, NewData, OldData) :-
    % Delegate to `make_assertion4/4` for this signature.
    !,
    make_assertion4(Fn, Cols, NewData, OldData).
make_assertion(Fn, DataL0, Data, DataL0) :-
    must_det_ll((
        % Convert the raw data into a term (`Data0`) using the function name.
        into_datum(Fn, DataL0, Data0),
        % Deconstruct the term into its functor (`F`) and arguments (`Args`).
        Data0 =.. [F | Args],
        % Assign the arguments to the processed data list.
        Args = DataL,
        % Construct the resulting term using the original functor and processed data list.
        Data =.. [F | DataL]
    )).

%!  make_assertion(+ArgTypes, +Fn, +DataL0, -Data, -DataL0) is det.
%
%   Processes raw data (`DataL0`) and argument types to construct a term for assertion.
%   This predicate includes type-checking and argument validation, ensuring that
%   the data conforms to the specified argument types (`ArgTypes`).
%
%   @arg ArgTypes The list of expected argument types for the function.
%   @arg Fn       The function name used to construct the term.
%   @arg DataL0   The raw data list to be processed.
%   @arg Data     The resulting term constructed from the data.
%   @arg DataL0   The unchanged raw data list (also returned as the last argument).
%
make_assertion(ArgTypes, Fn, DataL0, Data, DataL0) :-
    must_det_ll((
        % Convert the raw data into a term (`Data0`) using the function name.
        into_datum(Fn, DataL0, Data0),
        % Deconstruct the term into its functor (`F`) and arguments (`Args`).
        Data0 =.. [F | Args],
        % Perform type-checking and argument validation if argument types are unbound.
        skip(if_t(var(ArgTypes),
            must_det_ll((
                once((
                    % Ensure the length of arguments matches the length of argument types.
                    length(Args, Len),
                    length(ArgTypes, Len),
                    % Attempt to fetch table columns from the function or its functor.
                    once((table_columns(Fn, ArgTypes); table_columns(F, ArgTypes)))
                ))
            ))
        )),
        % Fix arguments based on argument types and prepare the processed data list.
        fix_list_args(Fn, ArgTypes, Args, DataL),
        % Construct the resulting term using the original functor and processed data list.
        Data =.. [F | DataL])).

% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743

%!  reprefix(+Prefixes, +Replacement) is det.
%
%   Defines mappings from multiple prefixes to a single standardized replacement.
%   This predicate is used to unify or simplify the prefixes of identifiers 
%   by replacing any of the given prefixes (`Prefixes`) with the desired 
%   replacement (`Replacement`).
%
%   The predicate itself defines static mappings and is not executed directly.
%   It serves as a knowledge base for prefix standardization.
%
%   @arg Prefixes     A list of prefixes to be replaced.
%   @arg Replacement  The replacement prefix to use for standardization.
%
%   @example
%     % Standardize prefixes for GO terms:
%     ?- reprefix(['GO_', 'GO--', 'BiologicalProcess:GO:'], 'GO:').
%
%     % Remove prefixes for FlyBase comments:
%     ?- reprefix(['flybase:', 'FLYBASE:', 'comment:'], '').
%

%reprefix(['GO_','GO--','FBgn','BiologicalProcess:GO:'],'GO:').
% Map prefixes related to GO terms to the standardized prefix "GO:".
reprefix(['GO_', 'GO--', 'BiologicalProcess:GO:'], 'GO:').
% Remove prefixes related to FlyBase comments.
reprefix(['flybase:', 'FLYBASE:', 'comment:'], '').
% Standardize the prefix "FBpp:" without modification.
reprefix(['FBpp:'], 'FBpp').
% Standardize the prefix "FBgn:" without modification.
reprefix(['FBgn:'], 'FBgn').
% Replace "FB:FB" with the standard prefix "FB".
reprefix(['FB:FB'], 'FB').

%./KBs/SUMO-OBO/gene-merged-SUMO.kif
%#
%FBbt_00051628=

%!  as_list(+Input, -Output) is det.
%
%   Converts the given input (`Input`) into a list (`Output`).
%
%   This predicate normalizes various types of input into list form:
%   - If the input is already a list, it remains unchanged.
%   - Certain symbols and empty strings are converted to empty lists.
%   - Strings or atoms separated by delimiters are split into lists.
%   - Single non-symbolic, non-string inputs are wrapped into a list.
%
%   @arg Input   The input value to be normalized.
%   @arg Output  The resulting list.
%
%   @example
%     % Convert a single number into a list:
%     ?- as_list(42, List).
%     List = [42].
%
%     % Split a delimited string into a list:
%     ?- as_list(",", "apple,banana,pear", List).
%     List = ["apple", "banana", "pear"].
%
%     % Handle empty strings and symbols:
%     ?- as_list("", List).
%     List = [].
%

% Base case: If the input is already a list, return it as-is.
as_list(A, New) :- 
    is_list(A), 
    !, 
    A = New.
% Wrap single non-symbolic, non-string inputs into a list.
as_list(N, [N]) :- 
    \+ symbol(N), 
    \+ string(N), 
    !.
% Commented-out case: Handle unbound variables.
% as_list(A, New) :- 
%     var(A), 
%     !, 
%     New = [A].
% Map specific symbols or strings to empty lists.
as_list('-', []).
as_list("-", []).
as_list('', []).
as_list("", []).
as_list(' ', []).
as_list(" ", []).
% Commented-out case: Wrap any input into a list.
% as_list(N, [N]) :- 
%     !.
% Recursively process nested inputs.
as_list(_, S, O) :- 
    as_list(S, O), 
    !.
% Split the input into a list using a member of the given separator list.
as_list(SepL, A, List) :- 
    member(Sep, SepL), 
    catch_ignore(symbolic_list_concat(List, Sep, A)), 
    List \= [_], 
    !.
% Split input using default separators and fix individual elements.
as_list([], A, ListO) :- 
    member(Sep, ['|', ',', ';']), 
    catch_ignore(symbolic_list_concat(List, Sep, A)), 
    List \= [_], 
    !, 
    maplist(fix_concept, List, ListO).
% Default case: Wrap the input into a list if no other rule matches.
as_list(_Sep, A, [A]).

%!  has_list(+Header) is nondet.
%
%   Checks if a given header contains a `listOf(_)` term.
%
%   This predicate succeeds if `Header` is a list and contains at least one element 
%   of the form `listOf(_)`.
%
%   @arg Header  The input to be checked. It is expected to be a list of terms.
%
%   @example
%     % Check a header with `listOf(_)`:
%     ?- has_list([int, listOf(string), atom]).
%     true.
%
has_list(Header) :-
    % Verify the input is a list.
    is_list(Header),
    % Check if the list contains a `listOf(_)` term.
    member(listOf(_), Header).


% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743

% =======================================
% Fix Concept1
% =======================================

%!  fix_concept1(+Input, -Output) is det.
%
%   Processes an input concept (`Input`) and converts it into a standardized form (`Output`).
%   This predicate handles various transformations:
%   - Splits delimited strings into lists.
%   - Converts symbols to numbers when applicable.
%   - Handles special cases like prefixed strings and quoted identifiers.
%
%   Commented-out code sections suggest additional transformations or cases that 
%   might be applied if uncommented.
%
%   @arg Input   The input concept to process (e.g., a string, symbol, or list).
%   @arg Output  The resulting standardized concept.
%
%   @example
%     % Convert a delimited string to a list:
%     ?- fix_concept1("apple|banana", Output).
%     Output = ["apple", "banana"].
%
%     % Handle quoted identifiers:
%     ?- fix_concept1('"gene_name"', Output).
%     Output = gene_name.
%
%     % Handle gene names:
%     ?- fix_concept1('BRCA1(gene name)', Output).
%     Output = BRCA1.
%
fix_concept1(A, L) :- 
    % Split the input using the '|' delimiter into a list.
    as_list(['|'], A, L), 
    % Ensure the result differs from the original input.
    (L\=@=[A], A\=@=L).
% Convert symbols that represent numbers into numeric form.
fix_concept1(A, N) :-  
    symbol_number(A, N), 
    !.
% Commented-out transformation: Handle prefixed strings.
% fix_concept1(A, AO) :- 
%     reprefix(List, To), 
%     member(E, List), 
%     symbol_concat(E, AM, A), 
%     symbol_concat(To, AM, AO).
% Commented-out transformation: Handle FlyBase identifiers.
% fix_concept1(A, AO) :- 
%     symbol_concat('FB', _, A), 
%     symbolic_list_concat([Type, Number], ':', A), 
%     !, 
%     symbol_concat(Type, Number, AO).
% Process quoted identifiers (e.g., `"gene_name"`) and convert to strings.
fix_concept1(A, AO) :- 
    symbol_concat('"', Mid, A), 
    symbol_concat(AS, '"', Mid), 
    symbol_string(AS, AO).
% Handle gene names by removing the `(gene name)` suffix.
fix_concept1(A, AO) :- 
    symbol_concat(AO, '(gene name)', A), 
    AO \== ''.
% Return the input as-is if it is already a symbol.
fix_concept1(A, N) :- 
    symbol(A), 
    !, 
    N = A.
% Commented-out transformation: Handle numbers represented as strings.
% fix_concept(S, A) :- 
%     number_string(A, S), 
%     !.

% =======================================
% Fix Concept
% =======================================

%!  fix_concept(+Input, -Output) is det.
%
%   Standardizes a given concept (`Input`) into a normalized form (`Output`).
%   This predicate applies various transformations recursively to handle lists, 
%   non-symbolic inputs, and other cases defined by `fix_concept1/2`.
%
%   @arg Input   The input concept to standardize (e.g., a symbol, string, or list).
%   @arg Output  The resulting standardized concept.
%
%   @example
%     % Normalize a list of concepts:
%     ?- fix_concept(['gene_name', "123", 'symbol'], Output).
%     Output = [gene_name, "123", symbol].
%
%     % Normalize a single symbol:
%     ?- fix_concept('symbol', Output).
%     Output = symbol.
%
%     % Normalize a nested concept:
%     ?- fix_concept("123|456", Output).
%     Output = ["123", "456"].
%

% Case 1: If the input is a list, recursively apply `fix_concept` to each element.
fix_concept(A, New) :- 
    is_list(A), !, 
    % Recursively process each element of the list.
    maplist(fix_concept, A, L), !, New = L.
% Case 2: If the input is not a symbol, return it as-is.
fix_concept(A, New) :- 
    \+ symbol(A), !, New = A.
% Case 3: Process the input using `fix_concept1/2` and handle intermediate results.
fix_concept(S, O) :- 
    % Apply `fix_concept1` once to transform the input.
    once(fix_concept1(S, M)), 
    % Check if the transformed result differs from the original input.
    S \=@= M, !, 
    % Recursively process the transformed result.
    fix_concept(M, O).
% Case 4: Default case: unify the input with the output.
fix_concept(A, New) :- 
    % Directly unify the input and output.
    =(A, New), !.

%!  fix_columns_nth(+Table, +ColumnIndex) is det.
%
%   Specifies columns that require special handling for a given table.
%   This predicate acts as a knowledge base for column indices that need 
%   adjustments or transformations in various FlyBase tables.
%
%   @arg Table        The name of the table (e.g., `'genome-cyto-seq'`).
%   @arg ColumnIndex  The index of the column in the table requiring handling.
%                     An underscore (`_`) indicates all columns in the table.
%
%   @example
%     % Query all columns requiring fixes in a specific table:
%     ?- fix_columns_nth('genome-cyto-seq', Index).
%     Index = 1 ;
%     Index = 2 ;
%     Index = 3.
%
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

% discontiguous allows for these predicates to be out of sequence.
:- discontiguous column_description/4.
:- discontiguous primary_column/2.
:- discontiguous column_names/2.
:- discontiguous file_location/2.

% 466_896_429
% Descriptions for allele_genetic_interactions columns
% Descriptions for genotype_phenotype_data columns
% For the file allele_genetic_interactions_*.tsv
% For the file genotype_phenotype_data_*.tsv

%!  file_to_sep(+FileOrExtension, -Separator) is det.
%
%   Determines the field separator for a given file or file extension.
%   This predicate maps common file formats (e.g., CSV, TSV) or custom extensions
%   (e.g., `metta_x`) to their respective field separators. If no specific mapping
%   exists, the default separator is a tab character (`'\t'`).
%
%   @arg FileOrExtension  The input file name or file extension.
%   @arg Separator         The determined field separator.
%
%   @example
%     % Retrieve the separator for a CSV file:
%     ?- file_to_sep('example.csv', Sep).
%     Sep = ','.
%
%     % Retrieve the separator for a TSV file:
%     ?- file_to_sep('example.tsv', Sep).
%     Sep = '\t'.
%
%     % Use a custom extension:
%     ?- file_to_sep('data.metta_x', Sep).
%     Sep = '\t'.
%
%     % Fallback to the default separator:
%     ?- file_to_sep('unknown_file_type.ext', Sep).
%     Sep = '\t'.
%

%file_to_sep(_File,9).
% Directly map extensions or specific formats to their separators.
file_to_sep(csv, ',').
file_to_sep(tsv, '\t').
file_to_sep(metta_x, '\t').
% Determine separator by extracting file extension and checking mappings.
file_to_sep(File, Sep) :-
    % Extract the extension from the file name.
    file_name_extension(_, Ext, File),
    % Check if the extension has a predefined separator.
    clause(file_to_sep(Ext, Sep), true),
    % Stop further processing once a match is found.
    !.
% Fallback case: Use tab as the default separator.
file_to_sep(_, '\t').

%!  load_flybase(+Sep, +File, +Stream, +Fn) is det.
%
%   Loads FlyBase data from a stream, processes it, and associates it with a given predicate.
%   This predicate initializes the process of loading FlyBase data from a file 
%   or stream, creating a `data_pred/2` association and invoking `load_flybase_sv/4`.
%
%   @arg Sep     The field separator used in the file (e.g., `','`, `'\t'`).
%   @arg File    The name of the file being loaded.
%   @arg Stream  The input stream from which data is read.
%   @arg Fn      The predicate name to associate the loaded data with.
%
%   @example
%     % Load FlyBase data from a CSV file:
%     ?- load_flybase(',', 'flybase_data.csv', Stream, flybase_predicate).
%
%     % The data will be associated with the predicate `flybase_predicate`.
%
load_flybase(Sep, File, Stream, Fn) :-
    must_det_ll((
        % Commented out: Ignore encoding-specific instructions for SWI-Prolog.
        % ignore(swi_only(format(":- ~q.\n",[encoding(utf8)]))),
        % Create a predicate name by concatenating "data" with `Fn`.
        symbolic_list_concat([data, Fn], '_', Fn0),
        % Associate the generated predicate name with `Fn`.
        data_pred(Fn0, Fn),
        % Load the FlyBase data using the specified separator and stream.
        load_flybase_sv(Sep, File, Stream, Fn)
    )).

% Sep,File,Stream,OutputStream,Fn

%!  load_flybase_sv(+Sep, +File, +Stream, +Fn) is det.
%
%   Loads FlyBase data from a stream and processes it line by line.
%   This predicate manages the reading and processing of FlyBase data until 
%   the stream reaches its end. It handles header information, argument types, 
%   and line-by-line parsing, and invokes `load_fb_data/6` to finalize processing.
%
%   @arg Sep     The field separator used in the file (e.g., `','`, `'\t'`).
%   @arg File    The name of the file being loaded.
%   @arg Stream  The input stream from which data is read.
%   @arg Fn      The predicate name to associate the loaded data with.
%
%   @example
%     % Load FlyBase data from a file:
%     ?- load_flybase_sv(',', 'flybase_data.csv', Stream, flybase_predicate).
%
%     % The predicate `flybase_predicate` will handle data parsing and processing.
%

% Base case: If the stream is at its end, conclude the loading process.
load_flybase_sv(Sep, File, Stream, Fn) :-
    % If the stream is at its end, finalize processing.
    at_end_of_stream(Stream),
    !,
    % Process the end-of-file using `load_fb_data/6`.
    once(load_fb_data(_ArgTypes, File, Stream, Fn, Sep, end_of_file)).
% Main case: Read and process data line by line from the stream.
load_flybase_sv(Sep, File, Stream, Fn) :-
    must_det_ll((
        % Initialize a counter for loaded lines.
        flag(loaded_from_file_count, _, 0),
        % Attempt to determine header names and argument types.
        notrace(ignore(attempt_header_names(Sep, File, Stream, Fn, ArgTypes))),
        % Ensure `ArgTypes` is a list starting with `N`.
        ArgTypes = [N | _],
        ignore(N = 1),
        % If `ArgTypes` is a list, declare the predicate with its arity.
        if_t(is_list(ArgTypes),
             ignore((length(ArgTypes, A), decl_fb_pred(Fn, A)))),
        % Read the first line of the stream into `Chars0`.
        read_line_to_chars(Stream, Chars0),
        % Log the first line for debugging purposes.
        wdmsg(Chars0),
        % Process the first line of characters.
        once(load_flybase_chars(ArgTypes, File, Stream, Fn, Sep, Chars0)),
        !,
        %\+ reached_file_max,
        % \+ done_reading(File),
        %\+ at_end_of_stream(Stream),
        % Repeat reading and processing lines until a termination condition is met.
        time((repeat,
              % Read the next line from the stream.
              read_line_to_chars(Stream, Chars),
              % Log the current line for debugging purposes.
              wdmsg(Chars),
              % Process the current line of characters.
              once(load_flybase_chars(ArgTypes, File, Stream, Fn, Sep, Chars)),
              % Stop processing if a termination condition is met.
              once(reached_file_max;
                   % done_reading(File);
                   at_end_of_stream(Stream)),
              !,
              % Finalize processing at the end of the file.
              once(load_fb_data(ArgTypes, File, Stream, Fn, Sep, end_of_file)))),
        % Retrieve and log the count of loaded lines.
        loaded_from_file_count(X),
        !,
        % Log statistical information about the processed data.
        metta_stats(Fn),
        pl_stats(File, X))),!.

%!  attempt_header_names(+Sep, +File, +Stream, +Fn, -NArgTypes) is nondet.
%
%   Attempts to determine and fix the header names for a FlyBase file.
%
%   This predicate tries to extract header names (`Header`) from the given file 
%   (`File`) or associated predicate (`Fn`) and standardizes them into argument 
%   types (`ArgTypes`). If successful, it computes the primary column index (`N`) 
%   and constructs `NArgTypes` as `[N|ArgTypes]`.
%
%   If headers cannot be determined, it processes the first line of the stream to 
%   derive the argument types using `load_flybase_chars/6`.
%
%   @arg Sep         The field separator used in the file (e.g., `','`, `'\t'`).
%   @arg File        The name of the file being processed.
%   @arg Stream      The input stream from which data is read.
%   @arg Fn          The predicate associated with the table.
%   @arg NArgTypes   The resulting list of argument types, prefixed with the primary column index.
%
%   @example
%     % Attempt to derive header names for a file:
%     ?- attempt_header_names(',', 'flybase_data.csv', Stream, my_predicate, NArgTypes).
%
%     % This will compute NArgTypes as `[N|ArgTypes]` if successful.
%

% Case 1: Successfully determine and fix header names.
attempt_header_names(Sep, File, _Stream, Fn, NArgTypes) :-
    ((
        % Attempt to retrieve header names from the table's metadata.
        ignore(once((table_columns(File, Header); table_columns(Fn, Header)))),
        % If headers are available, fix them into argument types.
        if_t(nonvar(Header), fix_header_names(Fn, Header, ArgTypes)),
        % Log any discrepancies in table column definitions.
        forall(
            (table_columns(File, ColInfo), ArgTypes \== ColInfo),
            pp_fb(odd_table_columns(File, ColInfo))
        ),
        forall(
            (table_columns(Fn, ColInfo), ArgTypes \== ColInfo),
            pp_fb(odd_table_columns(Fn, ColInfo))
        ),
        % Compute the primary column index (N) and construct `NArgTypes`.
        ((primary_column(Fn, Name), nth1(N, ArgTypes, Name))
         -> NArgTypes = [N | ArgTypes]
         ;  NArgTypes = [1 | ArgTypes]),
        % If argument types are a list, add table type metadata.
        if_t(is_list(ArgTypes), add_table_n_types(Fn, 1, ArgTypes))
    )),
    % Ensure `NArgTypes` is grounded and log the result.
    ground(NArgTypes),
    wdmsg(fix_header_names(Sep, File, Fn, Header, ArgTypes, N, NArgTypes, A)),
    % Validate the result with an additional condition on `A`.
    nonvar(A), A > 0, !.
% Case 2: Fallback to processing the first line of the stream.
attempt_header_names(Sep, File, Stream, Fn, NArgTypes) :-
    % Explicitly fail if headers cannot be determined.
    fail,
    % Read the first line of the stream into `Chars`.
    read_line_to_chars(Stream, Chars),
    % Process the characters to derive argument types.
    once(load_flybase_chars(NArgTypes, File, Stream, Fn, Sep, Chars)),
    % Log the processing result.
    wdmsg(load_flybase_chars(NArgTypes, File, Stream, Fn, Sep, Chars)).

%save_conversion_data(ArgTypes,Fn,OutputStream,Data):- maplist(write_flybase_data(ArgTypes,ArgTypes,Fn,OutputStream),Data).

%!  is_really_header_row(+Row, +Names) is nondet.
%
%   Determines if the given `Row` is a valid header row.
%
%   This predicate checks the first element of the row to see if it matches
%   the pattern of a header name. It uses `symbol_concat/2` to test whether
%   the first element (`H`) can be concatenated with an empty string, which
%   indicates it may be a valid header.
%
%   @arg Row     The list representing a row of data.
%   @arg Names   An unused argument in this predicate, possibly for further checks.
%
%   @example
%     % Check if a row is a header row:
%     ?- is_really_header_row(['Column1', 'Column2'], _).
%     true.
%
%     % Check if a row is not a header row:
%     ?- is_really_header_row([123, 456], _).
%     false.
%
is_really_header_row([H|_], _Names) :-
    % Check if the first element (`H`) matches a header pattern.
    symbol_concat('', _, H),!.

%!  read_csv_stream(+Sep, +CharsStream, -Header) is det.
%
%   Reads a line from a character stream and processes it into a header.
%
%   This predicate handles different configurations and options for processing
%   the header row of a CSV-like stream. Depending on the `option_value/2` settings,
%   it parses the input line using various strategies:
%   - If `full_canon` is disabled, the line is split using a symbolic or string-based separator.
%   - Otherwise, it uses `csv_read_row/3` for more structured parsing with separator options.
%
%   @arg Sep         The field separator used in the CSV (e.g., `','`, `'\t'`).
%   @arg CharsStream The input stream from which the line is read.
%   @arg Header      The resulting header row as a list of terms.
%
%   @example
%     % Read a CSV header with a comma separator:
%     ?- open('file.csv', read, Stream), read_csv_stream(',', Stream, Header).
%     Header = ["Column1", "Column2", "Column3"].
%
%     % Read a TSV header with a tab separator:
%     ?- open('file.tsv', read, Stream), read_csv_stream('\t', Stream, Header).
%     Header = ["ColumnA", "ColumnB"].
%

%read_csv_stream(Sep,CharsStream,Header):- read_string(CharsStream, "\n", "\r\t ",_,)
% Case 1: Split the header using a symbolic separator and `symbolic_list_concat/3`.
read_csv_stream(Sep, CharsStream, Header) :-
    % Read the next line from the stream as a string.
    read_line_to_string(CharsStream, Chars),
    % If the line is the end of file, unify Header with `end_of_file`.
    (Chars == end_of_file -> Header = Chars
    % Otherwise, split the line into a header list using the separator.
    ; symbolic_list_concat(Header, Sep, Chars)).
% Case 2: Split the header using a string-based separator if `full_canon` is disabled.
read_csv_stream(Sep, CharsStream, Header) :-
    % Check that the `full_canon` option is not enabled.
    \+ option_value(full_canon, []),
    !,
    % Read the next line from the stream as a string.
    read_line_to_string(CharsStream, Chars),
    % If the line is the end of file, unify Header with `end_of_file`.
    (Chars == end_of_file -> Header = Chars
    % Otherwise, split the line into a header list using `split_string/4`.
    ; split_string(Chars, Sep, "\s\t\n", Header)).
% Case 3: Parse the header using `csv_read_row/3` for more structured handling.
read_csv_stream(Sep, CharsStream, Header) :-
    % Convert the separator into its ASCII code representation.
    name(Sep, [SepCode]),
    % Prepare options for `csv_read_row/3` with the specified separator.
    csv_options(CompiledHeaderOptions, [separator(SepCode)]),
    % Read the header row as a structured term using the CSV options.
    csv_read_row(CharsStream, HeaderRow, CompiledHeaderOptions),
    % Extract the header list from the term.
    HeaderRow =.. [_ | Header],
    !.

%!  read_csv(+Sep, +Chars, -Header) is det.
%
%   Parses a line of CSV data into a header row.
%
%   This predicate processes a line of CSV data (`Chars`) using the specified 
%   separator (`Sep`). It opens the string as a stream and uses `read_csv_stream/3` 
%   to parse the header row into a list of terms.
%
%   @arg Sep     The field separator used in the CSV (e.g., `','`, `'\t'`).
%   @arg Chars   The string representing a single line of CSV data.
%   @arg Header  The resulting header row as a list of terms.
%
%   @example
%     % Parse a CSV header:
%     ?- read_csv(',', "Column1,Column2,Column3", Header).
%     Header = ["Column1", "Column2", "Column3"].
%
%     % Parse a TSV header:
%     ?- read_csv('\t', "ColumnA\tColumnB", Header).
%     Header = ["ColumnA", "ColumnB"].
%

% Commented-out code: Direct parsing using `split_string/4`.
% read_csv(Sep, Chars, Header) :-
%     \+ option_value(full_canon, []),
%     !,
%     split_string(Chars, Sep, "\s\t\n", Header).
% Main case: Open the string as a stream and parse it.
read_csv(Sep, Chars, Header) :-
    % Open the string as a character stream.
    open_string(Chars, CharsStream),
    % Use `read_csv_stream/3` to parse the header row.
    read_csv_stream(Sep, CharsStream, Header).

%!  attempt_header_row(+Sep, +Chars, +Fn, -Header, -ArgTypes) is det.
%
%   Attempts to process the header row of a CSV-like data line.
%
%   This predicate reads a header row (`Chars`) using the specified separator (`Sep`),
%   fixes the header names to generate standardized argument types (`ArgTypes`),
%   and associates them with a given function or predicate (`Fn`).
%
%   @arg Sep      The field separator used in the CSV (e.g., `','`, `'\t'`).
%   @arg Chars    The string representing the header row.
%   @arg Fn       The predicate associated with the header and argument types.
%   @arg Header   The resulting header row as a list of terms.
%   @arg ArgTypes The standardized argument types derived from the header row.
%
%   @example
%     % Attempt to process a header row:
%     ?- attempt_header_row(',', "Column1,Column2,Column3", my_predicate, Header, ArgTypes).
%     Header = ["Column1", "Column2", "Column3"],
%     ArgTypes = ["col1", "col2", "col3"].
%
attempt_header_row(Sep, Chars, Fn, Header, ArgTypes) :-
    % Read the header row from the given string.
    read_csv(Sep, Chars, Header),
    % Fix and standardize the header names to derive argument types.
    fix_header_names(Fn, Header, ArgTypes),
    % Cut to prevent backtracking.
    !.

:- dynamic(t_h_n/3).

%!  load_flybase_chars(+ArgTypes, +File, +Stream, +Fn, +Sep, +Chars) is det.
%
%   Processes a line of FlyBase data (`Chars`) based on its content and context.
%
%   This predicate handles various cases of input lines:
%   - Lines that do not match the expected format are logged as comments.
%   - Header rows are detected and processed to determine `ArgTypes`.
%   - Regular data rows are processed and asserted into the knowledge base.
%
%   @arg ArgTypes The list of argument types for the data.
%   @arg File     The file being processed.
%   @arg Stream   The input stream (unused in some clauses).
%   @arg Fn       The predicate name associated with the data.
%   @arg Sep      The field separator used in the file (e.g., `','`, `'\t'`).
%   @arg Chars    The string representing a single line of input.
%
%   @example
%     % Process a comment line:
%     ?- load_flybase_chars([1, "col1", "col2"], 'file.csv', _, my_predicate, ',', "# This is a comment").
%
%     % Detect and process a header row:
%     ?- load_flybase_chars([1, "col1", "col2"], 'file.csv', _, my_predicate, ',', "col1,col2,col3").
%
%     % Process a data row:
%     ?- load_flybase_chars([1, "col1", "col2"], 'file.csv', _, my_predicate, ',', "val1,val2,val3").
%

% Case 1: Handle comment or invalid lines.
load_flybase_chars(ArgTypes, File, _Stream, _Fn, Sep, Chars) :-
    % Skip lines that are comments or do not match the expected format.
    (\+ member(Sep, Chars) ; 
     (['#', '#', ' ' | _] = Chars) ; 
     (ground(ArgTypes), ['#' | _] = Chars)),
    % Log the comment or skipped line.
    % writeln(comment(Sep) = Chars),  % Commented-out logging line.
    (format("~n ; ~s", [Chars])),
    % If too many rows are processed, stop reading further.
    ignore((loaded_from_file_count(X), X > 2000, !, assert(done_reading(File)))).
% Case 2: Detect and process a header row.
load_flybase_chars([N | ArgTypes], File, Stream, Fn, Sep, Chars) :-
    % If `ArgTypes` is unbound and the line contains the separator and starts with `#`.
    var(ArgTypes), 
    member(Sep, Chars), 
    ['#' | _] = Chars,
    % Log potential header lines.
    (format("~n ; Maybe Header: ~s", [Chars])),
    % Attempt to process the header row.
    attempt_header_row(Sep, Chars, Fn, Header, ArgTypes),
    % Verify if the header row is valid.
    is_really_header_row(Header, ArgTypes),
    % Log and assert the header and argument types.
    (fbug(t_h_n(Fn, Header, ArgTypes)),
    fb_assert(t_h_n(Fn, Header, ArgTypes))),
    % Load subsequent data rows.
    !,
    load_fb_data([N | ArgTypes], File, Stream, Fn, Sep, is_swipl).
% Case 3: Process a data row.
load_flybase_chars([N | ArgTypes], File, Stream, Fn, Sep, Chars) :-
    % Process rows in SWI-Prolog mode.
    is_swipl,
    % Attempt to process the header row.
    attempt_header_row(Sep, Chars, Fn, Header, _),
    % Write the processed data to the knowledge base.
    write_flybase_data([N | ArgTypes], Fn, Header),
    % Load subsequent data rows.
    !,
    load_fb_data([N | ArgTypes], File, Stream, Fn, Sep, is_swipl).

%!  load_fb_data(+ArgTypes, +File, +Stream, +Fn, +Sep, +DataMode) is det.
%
%   Processes FlyBase data from a file or stream based on the specified mode.
%
%   This predicate handles FlyBase data in multiple cases:
%   - Stops processing if the data or file has reached its end (`end_of_file`) 
%     or if the `done_reading/1` flag is asserted.
%   - Processes data row by row, either using `read_csv_stream/3` or 
%     `csv_read_row/3`, depending on the mode (`is_swipl`).
%   - Handles large data sets with `repeat` and termination conditions based on 
%     the maximum rows per file or reaching the end of the file.
%
%   @arg ArgTypes  The argument types of the data being processed.
%   @arg File      The name of the file being processed.
%   @arg Stream    The input stream to read data from.
%   @arg Fn        The predicate name associated with the data.
%   @arg Sep       The field separator used in the data.
%   @arg DataMode  The mode of processing, such as `is_swipl`.
%
%   @example
%     % Process data until the end of a file:
%     ?- load_fb_data(["col1", "col2"], 'data.csv', Stream, my_predicate, ',', is_swipl).
%
%     % Process a CSV file with custom options:
%     ?- load_fb_data(["id", "value"], 'data.tsv', Stream, another_pred, '\t', is_swipl).
%

% Case 1: Stop processing if the data is at the end of the file or the file is done.
load_fb_data(_ArgTypes, File, _Stream, _Fn, _Sep, Data) :-
    (Data == end_of_file; done_reading(File)),
    !.
% Case 2: Process data rows using `read_csv_stream/3` in SWI-Prolog mode.
load_fb_data(ArgTypes, File, Stream, Fn, Sep, is_swipl) :-
    % Optionally retrieve the maximum rows per file; default is infinite.
    (option_value(max_per_file, Max) -> true ; Max = inf),
    % Log the start of data loading.
    fbug(load_fb_data(ArgTypes, File, Max, Fn, Sep)),
    % Register the table with its argument types.
    add_table_n_types(Fn, 1, ArgTypes),
    !, % Prevent backtracking here.
    repeat,
    % Read a row of data using `read_csv_stream/3`.
    once(read_csv_stream(Sep, Stream, Data)),
    % Retrieve the number of rows processed so far.
    loaded_from_file_count(X),
    % Terminate if the end of the file is reached, the file max is reached, or max rows exceeded.
    (((Data == end_of_file); reached_file_max; (X > Max))
     -> assert(done_reading(File))
     % Otherwise, process the row and continue.
     ; (once(write_flybase_data(ArgTypes, Fn, Data)), fail)),
    !.
% Case 3: Process data rows using `csv_read_row/3` with compiled CSV options.
load_fb_data(ArgTypes, File, Stream, Fn, Sep, is_swipl) :-
    !,
    % Convert the separator into its ASCII code representation.
    name(Sep, [SepCode]),
    % Compile CSV options with the separator.
    csv_options(CompiledOptions, [separator(SepCode)]),
    % Optionally retrieve the maximum rows per file; default is infinite.
    (option_value(max_per_file, Max) -> true ; Max = inf),
    % Log the start of data loading.
    fbug(load_fb_data(ArgTypes, File, Max, Fn, Sep)),
    % Register the table with its argument types.
    add_table_n_types(Fn, 1, ArgTypes),
    !, % Prevent backtracking here.
    repeat,
    % Read a row of data using `csv_read_row/3`.
    once((csv_read_row(Stream, RData, CompiledOptions))),
    % Retrieve the number of rows processed so far.
    loaded_from_file_count(X),
    % Terminate if the end of the file is reached, the file max is reached, or max rows exceeded.
    (((RData == end_of_file); reached_file_max; (X > Max))
     -> assert(done_reading(File))
     % Otherwise, process the row data and continue.
     ; (RData =.. [_ | Data],
        once(write_flybase_data(ArgTypes, Fn, Data)), fail)),
    !.
% Case 4: Recursive processing of data rows with advanced CSV options.
% recursion depth 16 million rows
load_fb_data(ArgTypes, File, Stream, Fn, Sep, is_swipl) :-
    % Convert the separator into its ASCII code representation.
    name(Sep, [SepCode]),
    % Compile advanced CSV options with stripping and conversion enabled.
    csv_options(CompiledOptions, [strip(true), convert(true), separator(SepCode)]),
    % Optionally retrieve the maximum rows per file; default is infinite.
    (option_value(max_per_file, Max) -> true ; Max = inf),
    % Read a row of data using `csv_read_row/3`.
    once((csv_read_row(Stream, RData, CompiledOptions))),
    % Retrieve the number of rows processed so far.
    loaded_from_file_count(X),
    % Terminate if the end of the file is reached or max rows exceeded.
    (((RData == end_of_file); (X > Max))
     -> assert(done_reading(File))
     % Otherwise, process the row data and continue recursively.
     ; (RData =.. [_ | Data],
        once(write_flybase_data(ArgTypes, Fn, Data)),
        load_fb_data(ArgTypes, File, Stream, Fn, Sep, is_swipl))),
    !.