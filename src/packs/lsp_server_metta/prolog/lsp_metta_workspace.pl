%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_metta_workspace.pl
%
% Purpose:
% This module is responsible for managing workspace references and indexing files 
% within the context of a Language Server Protocol (LSP) server for the MeTTa language. 
% It handles tasks related to tracking open workspaces, managing project files, and 
% maintaining an up-to-date index of files for efficient code navigation, refactoring, 
% and code actions.
%
% Key Responsibilities:
% - **Workspace Management**: Tracks references to workspaces that are opened by the 
%   user in the editor. It ensures that the correct context (set of files, dependencies, 
%   etc.) is available for language features such as diagnostics, code actions, and 
%   autocompletion.
%
% - **File Indexing**: Manages the indexing of files in the workspace. The indexing 
%   includes parsing files to gather symbols, functions, variables, and other relevant 
%   elements to make them available for lookup, navigation, and other code-based features.
%
% - **Efficient File Access**: Provides mechanisms for efficiently accessing file 
%   contents and their metadata. This includes handling changes to files (e.g., edits 
%   or saves), ensuring that the workspace index remains accurate and responsive to 
%   modifications.
%
% - **Code Navigation and Refactoring Support**: Maintains references to allow for 
%   features like go-to-definition, find-references, and automated refactorings. The 
%   file index is constantly updated to provide real-time code navigation and reference 
%   lookups.
%
% How It Works:
% - **Tracking Open Workspaces**: When a workspace is opened, the module loads and 
%   indexes all relevant files, keeping track of changes and updating the index as 
%   necessary.
% - **File Change Handling**: The module listens for file system events such as file 
%   saves, modifications, or deletions, and adjusts the workspace index accordingly.
% - **Efficient Lookup**: Provides APIs to search for symbols, definitions, and 
%   references across the workspace to support navigation and refactoring tasks.
%
% Dependencies:
% This module may depend on other LSP components such as:
% - `lsp_metta_utils.pl` for general utility functions.
% - `lsp_metta_changes.pl` for handling file changes and ensuring the index remains 
%   up-to-date.
%
% Author: Douglas Miles   
% Date: 10-21-2024
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(lsp_metta_workspace, [
%                        xref_metta_source/1]).

%!  metta_atom_xref(+Atom) is semidet.
%
%   Attempts to cross-reference a given atom from a Metta buffer or already
%   asserted facts. This is used for finding known atoms.
%
%   @arg Atom The atom to be cross-referenced.
:- dynamic(metta_atom_asserted/2).
:- multifile(metta_atom_asserted/2).
:- dynamic(metta_atom_asserted_deduced/2).
:- multifile(metta_atom_asserted_deduced/2).

metta_atom_xref(Atom):- metta_atom_xref(Atom, _Path, _Loc).

metta_atom_xref(Atom, Path, Loc):- 
    metta_file_buffer(0,_Ord,_Kind, Atom, NamedVarsList, Path, Loc),  % Retrieve the atom from a Metta file buffer.
    \+ clause(metta_atom_asserted(_, Atom), true),  % Ensure the atom has not been asserted already.
   ignore(maybe_name_vars(NamedVarsList)).  % Set variable names based on the named variables list.
metta_atom_xref(Atom, Path, Loc):- 
    clause(metta_atom_asserted(_, Atom), true),  % Check if the atom has been asserted in the knowledge base.
    copy_term(Atom,Copy),
    ignore(metta_file_buffer(0,_Ord,_Kind, Atom, NamedVarsList, Path, Loc)),
    Atom =@= Copy,
    ignore(maybe_name_vars(NamedVarsList)).


%!  predicate_help_hook(+HookType, +Path, +Term, +Arity, -S) is semidet.
%
%   Provides help documentation for predicates in the form of hooks. If the predicate 
%   is a symbol and is documented, it retrieves the help text.
%
%   @arg HookType  The stage of hook invocation (e.g., first, last).
%   @arg Path      The file or path associated with the predicate.
%   @arg Term      The term (predicate) for which help is sought.
%   @arg Arity     The arity of the predicate.
%   @arg S         The help content to return.
:- multifile(user:predicate_help_hook/5).
:- dynamic(user:predicate_help_hook/5).
predicate_help_hook(_, _, Term, _, _) :- 
    \+ symbol(Term),  % Fail if the term is not a valid symbol.
    !, fail.
predicate_help_hook(first, Path, Term, Arity, S):-     
    Path \= recurse(_),  % Avoid recursion for already processed paths.
    \+ is_documented(Term),  % Check if the term is undocumented.
    symbol_resolve(Term, Resolved),  % Try to resolve the symbol to a documented one.
    is_documented(Resolved),  % Ensure the resolved symbol is documented.
    predicate_help_hook(first, recurse(Path), Resolved, Arity, S),  % Recurse to retrieve help for the resolved symbol.
    !.
predicate_help_hook(last, Path, Term, Arity, S):- 
    Path \= recurse(_),  % Avoid recursion for already processed paths.
    \+ is_documented(Term),  % Check if the term is undocumented.
    symbol_resolve(Term, Resolved),  % Try to resolve the symbol to a documented one.
    is_documented(Resolved),  % Ensure the resolved symbol is documented.
    lsp_metta_utils:predicate_help(recurse(Path), Resolved, Arity, S),  % Retrieve help content for the resolved symbol.
    !.
predicate_help_hook(first, Path, _Term, _Arity, _S):- 
    atomic(Path),  % Ensure the path is an atomic value (not a variable or complex term).
    make,  % Trigger the make command to compile the knowledge base. (For real-time editing)
    xref_metta_source(Path),  % Possibly cross-reference the file.
    fail.

predicate_help_hook(first, _, Term, Arity, S):-     
    all_info_string(Term, Arity, Str),  % Generate a term definition string based on its arity.
    Str \= "",  % Ensure the definition string is not empty.
    atom_length(Str, Len), 
    Len > 10,  % Ensure the definition string has a minimum length.    
    maybe_markdown(Str, S).

predicate_help_hook(last, _, Term, Arity, S):- 
   (get_type(Term, Type) -> true; Type = unknownType),  % Get the type of the term or default to 'unknownType'.
    all_info_string(Term, Arity, DS),  % Generate a term definition string.
    format(string(Str), "*Type ~w*: ~w (~w)~n~w", [Type, Term, Arity, DS]),  % Format the output as a help string.
    maybe_markdown(Str, S),!.


maybe_markdown(Str, S):- 
    S = _{ contents: _{
            kind: 'markdown',  % Define the help content format as markdown text.
            value: Str  % Store the formatted help string.
        } }, !.


%!  is_documented(+Symbol) is semidet.
%
%   Checks if a given symbol is documented by querying for an '@doc' tag in the Metta
%   knowledge base. This predicate succeeds if documentation is found.
%
%   @arg Symbol The symbol to check for documentation.
is_documented(Symbol):- 
    nonvar(Symbol),  % Ensure that the symbol is instantiated (not a variable).
    metta_atom_xref(['@doc', Symbol | _]).  % Look for a documentation entry in the knowledge base.
is_documented(Symbol):- 
    metta_atom_xref([_, Head | _]),  % Search for a matching atom in the knowledge base.
    sub_var(Symbol, Head).  % Check if the symbol appears as a subterm of Atom.

%!  is_documented_arity(+Symbol, +Arity) is semidet.
%
%   Checks if a given symbol is documented for a specific arity by inspecting its
%   parameter list or type definition in the knowledge base.
%
%   @arg Symbol The symbol to check for arity-based documentation.
%   @arg Arity  The arity (number of arguments) to check for.
is_documented_arity(Symbol, Arity):- 
    nonvar(Symbol),  % Ensure that the symbol is instantiated.
    metta_atom(_KB, ['@doc', Symbol | Docs]),  % Retrieve documentation for the symbol.
    once((member(List, Docs),  % Find a '@params' entry in the documentation.
          is_list(List), List = [Sub, Params], Sub == '@params',  % Check if the entry is a parameter list.
          is_list(Params), length(Params, Arity))).  % Verify if the arity matches the length of the parameter list.
is_documented_arity(Symbol, Arity):- 
    nonvar(Symbol),  % Ensure that the symbol is instantiated.
    get_operator_typedef1('&xref', Symbol, Arity, ParamTypes, _RetType),  % Retrieve the operator's type definition.
    is_list(ParamTypes),  % Ensure that the parameter types are in a list.
    length(ParamTypes, Arity).  % Verify if the arity matches the length of the parameter types.


%!  symbol_resolve(+Term, -Resolved) is semidet.
%
%   Attempts to resolve a symbol to its counterpart with or without an exclamation mark (!).
%
%   @arg Term The term (symbol) to resolve.
%   @arg Resolved The resolved version of the term (with or without '!').
symbol_resolve(Term, Resolved):- 
    symbol(Term),  % Ensure the term is a valid symbol.
    symbol_concat(Resolved, '!', Term), !.  % Try to resolve the symbol by removing the '!' if present.
symbol_resolve(Term, Resolved):- 
    symbol(Term),  % Ensure the term is a valid symbol.
    symbol_concat(Term, '!', Resolved).  % Try to resolve the symbol by adding a '!' if missing.

%!  all_info_string(+Term, +Arity, -S) is det.
%
%   Generates a formatted string describing the term and its arity.
%
%   @arg Term  The term (predicate) for which the description is generated.
%   @arg Arity The arity of the predicate.
%   @arg S     The resulting string description.
all_info_string(Term, Arity, Str):- 
    with_output_to(string(S), grovel_all_info(Term, Arity)),  % Generate a string output for the term's arity help.
    string(S),  % Ensure that the output is a valid string.
    S \= "",  % Ensure that the string is not empty.
    atom_length(S, Len), 
    Len > 1, % Ensure the string has a minimum length.
    format(string(Str), "```lisp~n~w~n```", [S]). 

%!  grovel_all_info(+Term, +Arity) is det.
%
%   Outputs help information for a term and its associated arity.
%
%   @arg Term  The term (predicate) for which help is displayed.
%   @arg Arity The arity to check for.
grovel_all_info(Target, Arity):- 
    forall(grovel_some_help(Target, Arity), true).  % Execute the help logic for each possible instance of the term's arity.

banner_for(Type,Target):- format('~N```~n---~n ## ~w: ~w ~n```lisp~n',[Type, Target]).
lsp_separator():- format('~N```~n---~n```lisp~n',[]).

show_checked(Name, Value, Caption) :- fail,
    format("[~w](file:command:myExtension.toggleValue?{\"name\":\"~w\",\"value\":\"~w\"}) ~w ", [Value, Name, Value, Caption]).
show_checked(Name, Value, Caption) :- format("[~w](file://toggleValue_~w.metta) ~w ", [Value, Name, Caption]).



%!  grovel_some_help(+Target, +Arity) is det.
%
%   Helper predicate to display information about each instance of a term's arity.
%
%   @arg Target The term (predicate) for which help is displayed.
%   @arg Arity The arity to check for.
grovel_some_help(Target, _) :- fail, % (for debugging) commenting out fail will let the hover show the cross-ref index
  each_type_at_sorted(Target, Sort),
  forall(member(RefType,[definition,declaration,typeDefinition,implementation,references]),
  (banner_for(RefType,Target),
    %ignore((metta_defined_at(Type, HintPath, Target, Clause,Path,Loc),
    forall(member(each_type_at(Target,Clause,Path,Loc,Type),Sort),
      ignore((
          once(type_expand(RefType,Type)),
          write_src_xref(Clause,Type,Path,Loc),nl))))),
   banner_for('rest-of',Target).

grovel_some_help(Target, _) :- 
    xref_call(eval(['help!', Target], _)), lsp_separator().  % Evaluate the help command for the term.
grovel_some_help(Target, Arity):- number(Arity), Arity > 1,
    findall(A, is_documented_arity(Target, A), ArityDoc),  % Retrieve documented arities for the term.
    ArityDoc \== [],  % Ensure the documentation is not empty.
    \+ memberchk(Arity, ArityDoc),  % Verify if the term's arity DOES NOT matches the documented arity.    
    format('Arity expected: ~w vs ~w~n', [ArityDoc, Arity]),lsp_separator() .  % Output a message if there's an arity mismatch.

grovel_some_help(Target, _) :- 
    format("~n```~n",[]),
    show_checked("show_docs","(-)","Show Docs "),
    show_checked("show_refs","(+)","Show Refs "),
    show_checked("show_menu","(+)","Show Menu "),
    format("for: ~w", [Target]),
    format("~n```~n",[]).  

grovel_some_help(Target, _) :- 
    each_type_at_sorted(Target,Clause,Path,Loc,Type),
    write_src_xref(Clause,Type,Path,Loc).  % Write the source cross-reference for the atom.
   
%xref_call(G):- catch(G,E,debug(lsp(high), "xref_call ~w", [G])).
%xref_call(G):- catch(with_no_debug(G),E,debug(lsp(high), "xref_call ~w", [G->E])).
xref_call(G):- with_no_debug(G).
%xref_call(G):- call(G). 

each_type_at_sorted(Target,Clause,Path,Loc,Type):-
    each_type_at_sorted(Target, Sort),
    member(each_type_at(Target,Clause,Path,Loc,Type),Sort).

each_type_at_sorted(Target, Sort):-
      findall(each_type_at(Target,Clause,Path,Loc,Type),
              each_type_at(Target,Clause,Path,Loc,Type),
              List), 
       group_by_last_arg(List,Sort).

%% group_by_last_arg(+Terms, -SortedList) is det.
% This predicate groups terms by their last argument, sorts the groups by the last argument,
% and preserves the original order of terms within each group.
group_by_last_arg(TermL, SortedList) :-
    list_to_set(TermL, Terms),
    % Find all unique group keys (last argument of each term)
    findall(GroupKey, (member(Term, Terms), functor(Term, _, Arity), arg(Arity, Term, GroupKey)), GroupKeysUnsorted),
    % Sort group keys lexicographically (to define group order)
    sort(GroupKeysUnsorted, GroupKeys),
    % Group terms by their last argument (group key) without sorting the terms inside each group
    findall(Group,
            (member(GroupKey, GroupKeys),
             include(is_in_group(GroupKey), Terms, Group)),
            SortedGroups),
    % Flatten the sorted groups into a single list
    append(SortedGroups, SortedList).
% is_in_group(+GroupKey, +Term)
% Helper predicate to check if Term belongs to the specified GroupKey
is_in_group(GroupKey, Term) :-
    functor(Term, _, Arity),
    arg(Arity, Term, GroupKey).



each_type_at(Target,Clause,Path,Loc,Type):-
    no_repeats_var(ClauseV),    
    metta_atom_xref(Clause, Path, Loc), ClauseV = Clause,  % Cross-reference the term with known atoms.
    about_term(Clause, Target),  % Determine if the atom is related to the term.
    \+ skip_xref_atom(Clause),  % Skip atoms that are not relevant for cross-referencing.
    type_symbol_clause(Type,Target,Clause).
    %format('~@', [write_src_xref(Clause,Type,Path,Loc)]).

%!  about_term(+Atom, +Term) is semidet.
%
%   Determines if the Atom is about the given Term.
%
%   @arg Atom The atom to check.
%   @arg Term The term to match with the atom.
about_term([Op, Atom | _], Term):- fail, Op==':', is_documented(Term), !, Atom\==Term, % Dont reshow types
    sub_var(Term, Atom),  !. % Check if the term is a subterm of the atom.
about_term([_,[Atom|_]|_],Term):- sub_var(Term,Atom),!.
about_term([_,Atom|_],Term):- ==(Term,Atom),!.
about_term([Atom|_],Term):- \+ promiscuous_symbol(Term), sub_var(Term,Atom),!.
about_term(exec(Atom),Term):-!, sub_var(Term, Atom).


:- multifile(user:handle_msg_hook/3).
:- dynamic(user:handle_msg_hook/3).
% Save the last Msg.body Object for each method
user:handle_msg_hook(MethodStr, MsgBody, _) :- fail,
   atom_string(Method,MethodStr),
   nb_setval(Method, MsgBody),
   fail.

%!  write_src_xref(+Src) is det.
%
%   Outputs source code or its reference based on the nesting of the source.
%
%   @arg Src The source code or reference to output.
write_src_xref(Src):- % fail, 
    very_nested_src(Src),  % Check if the source is complex.
    write_src_wi(Src), !.  % Write the full source content if it's complex.
write_src_xref(Src):- 
    write_src_woi(Src).  % Otherwise, write the source content without additional information.


write_src_xref(Clause,Type,Path,Loc):-
   catch_skip((write_src_xref(Clause),
     ignore(write_file_link(Type,Path,Loc)))).

write_src_xref(Clause,Path,Loc):-
   catch_skip((write_src_xref(Clause),
     ignore(write_file_link(Path,Loc)))). 

catch_skip(G):- ignore(catch(G,_,true)).

% Check for deeply nested lists
very_nested_src([_, _ | Src]):- is_list(Src), 
    member(M, Src), is_list(M), 
    member(E, M), is_list(E),
    member(I, E), is_list(I), !.  

maybe_link_xref(What):- 
  ignore(once((
     metta_file_buffer(0,_Ord,_Kind,Atom,_,Path,Pos),
     %symbolic(Path), \+ symbol_contains(Path,'stdlib_mettalog'),
     once((alpha_unify(What,Atom); \+ (What \= Atom))),
     %next_clause(Ref, metta_file_buffer(0,_Ord,_Kind,_,_,Path,Pos)),     
     write_file_link(Path,Pos)))).

% next_clause(Ref, NextClause)
%   - Ref is the reference of the current clause
%   - NextClause is the next clause's reference, if it exists
next_clause(Ref, NextClause) :-
     nth_clause(Pred, Nth, Ref),
     NextIndex is Nth + 1,
     nth_clause(Pred, NextIndex, NextRef),!,
     clause(NextClause, _, NextRef).  % Get the clause at this reference

%     ~n```~n*~w*~n```lisp~n
write_file_link(Type, Path,Position):-   
  must_succeed1(position_line(Position, Line)),
  format('~n```~n[~w:~w](file://~w#L~w) _(~w)_~n```lisp~n',[Path,Line,Path,Line,Type]).
write_file_link(Path,Position):-   
  must_succeed1(position_line(Position, Line)),
  format('~n```~n[~w:~w](file://~w#L~w)~n```lisp~n',[Path,Line,Path,Line]).



position_line(Position, Line2):-
   into_line_char(Position, line_char(Line1, _)), succl(Line1, Line2).

% Converts a position or range into a line_char range.
% Pos: Single position or a range (from LSP format).
% Start and End are line_char pairs representing the start and end of the range.
into_line_char_range(Pos, range(Start, End)):-
   _{line: Line0, character: Char0} :< Pos,  % Match LSP position format (line and character).
   %into_line_char(Pos, Start).
   succl(Line0, Line1), succl(Char0, Char1), % succl(Line1, Line2), 
   Start = line_char(Line1, Char0),  % Set Start to the position's line and character.
   End = line_char(Line1, Char1), !.  % If a single position, start and end are the same.
into_line_char_range(Range, range(Start, End)):-   
   _{start: RStart, end: REnd} :< Range,  % Match LSP range format (start and end positions).
   into_line_char(RStart, Start),  % Convert start position into line_char.
   into_line_char(REnd, End), !.  % Convert end position into line_char.


%succl(X,Y):- succ(X,Y).
succl(X,X).

into_json_range(Line, Location) :- var(Line),!, freeze(into_json_range(Line, Location)).
into_json_range(Number, Location):- integer(Number),!, into_json_range(local(Number), Location).
into_json_range(Line, Location) :- \+ compound(Line),!,into_json_range(line(1), Location).
into_json_range(range(line_char(Line0,Char0),line_char(Line1,Char1)), Loc) :- !, number(Line0),number(Line1), !, 
  succl(Line0M1,Line0),succl(Line1M1,Line1),  Loc = _{start: _{line: Line0M1, character: Char0}, end: _{line: Line1M1, character: Char1}}.
into_json_range(position(Position), Location) :- !,into_json_range(Position, Location).
into_json_range('$stream_position'(A,B,C,D), Location) :- !, line_col('$stream_position'(A,B,C,D), line_char(Line0,Char0)), into_json_range(position(Line0, Char0), Location).
into_json_range(position(Line0, Char0), Location) :- !, into_json_range(line_char(Line0, Char0),Location).
into_json_range(line_char(Line0, Char0), Location) :- !, into_json_range(range(line_char(Line0, Char0),line_char(Line0, Char0)),Location).
into_json_range(Line0: Char0, Location) :- number(Line0), !, into_json_range(line_char(Line0, Char0), Location).
into_json_range(_File:Line0, Location) :- number(Line0),!,into_json_range(line(Line0), Location).
into_json_range(line(Line0), Location):- !, into_json_range(line_char(Line0, 1), Location).
into_json_range(local(Line0), Location):- !, into_json_range(line(Line0), Location).
into_json_range(imported(_), Location):- !, into_json_range(line_char(1, 1), Location).
into_json_range(Cmpd, Location):- \+ is_dict(Cmpd), !, into_json_range(local(1), Location).
into_json_range(Line, Location) :- _{range: Location} :< Line, !.
into_json_range(Line, Location) :- _{start: _{line: _, character: _}, end: _{line: _, character: _}} :< Line, !, Location = Line.
into_json_range(Line, Location) :- _{line: Line0M1, character: Char0} :< Line, !, succl(Line0M1,Line0), into_json_range(line_char(Line0, Char0), Location).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
into_json_range(_, Location):- into_json_range(local(1), Location).


into_line_char(Line1, Location) :- var(Line1),!, freeze(into_line_char(Line1, Location)).
into_line_char(Number, Location):- integer(Number),!, into_line_char(local(Number), Location).
into_line_char(Line1, Location) :- \+ compound(Line1),!,into_line_char(line(1), Location).
into_line_char(line_char(Line1, Char), Location) :- !, Location = line_char(Line1, Char).
into_line_char(range(Start,_), Loc) :- !, into_line_char(Start, Loc).
into_line_char(line(Line1), Location):- !, into_line_char(line_char(Line1, 1), Location).
into_line_char(position(Position), Location) :- !,into_line_char(Position, Location).
into_line_char('$stream_position'(A,B,C,D), Location) :- !, line_col('$stream_position'(A,B,C,D), line_char(Line1,Char)), into_line_char(position(Line1, Char), Location).
into_line_char(position(Line1, Char), Location) :- !, into_line_char(line_char(Line1, Char),Location).
into_line_char(Line1: Char, Location) :- number(Line1), !, into_line_char(line_char(Line1, Char), Location).
into_line_char(_File:Line1, Location) :- number(Line1),!,into_line_char(line(Line1), Location).
into_line_char(local(Line1), Location):- !, into_line_char(line(Line1), Location).
into_line_char(imported(_), Location):- !, into_line_char(line_char(1, 1), Location).
into_line_char(Cmpd, Location):- \+ is_dict(Cmpd), !, into_line_char(local(1), Location).
into_line_char(ORange, Location) :- _{range: Range} :< ORange, !, into_line_char(Range, Location).
into_line_char(Range, Location) :- _{start: Start} :< Range, !, into_line_char(Start, Location).
into_line_char(Line, Location) :- _{line: Line0, character: Char} :< Line, !, succl(Line0,Line1), !, Location = line_char(Line1, Char).
into_line_char(Line, Location) :- _{line: Line0} :< Line, !, succl(Line0,Line1), !, Location = line_char(Line1, 1).
into_line_char(_, Location):- into_line_char(local(1), Location).


%!  skip_xref_atom(+Atom) is semidet.
%
%   Checks whether the given Atom should be skipped during cross-referencing.
%
%   @arg Atom The atom to check.

%skip_xref_atom(Atom):- sub_var('@doc', Atom), !. % Skip documentation (already shown)
%skip_xref_atom(exec(_)).  % Skip execution commands.
skip_xref_atom([Pred | _]):- 
    symbol(Pred),  % Check if the predicate is a symbol.
    symbol_concat('assert', _, Pred).  % Skip 'assert*' predicates.

%! xref_maybe(+Path, +FullText) is det.
%
%   Checks if a file's content has changed. If it has, interrupts any ongoing processing
%   of that file and requeues it for reprocessing. If not, processing continues without
%   interruption.
%
%   @arg Path The file path of the Metta file.
%   @arg NewText The current text content to compare.

xref_maybe(Path, NewText) :- user:next_text(Path, OldText), OldText = NewText, !,
    debug(lsp(high), 'NewText for "~w" has not changed, skipping reload.~n', [Path]).
xref_maybe(Path, NewText) :- user:full_text(Path, OldText), OldText = NewText, !,
    debug(lsp(high), 'FullText for "~w" has not changed, skipping reload.~n', [Path]).
xref_maybe(Path, NewText) :- retractall(user:full_text(Path, _)), assertz(user:full_text(Path, NewText)),!.
xref_maybe(Path, NewText) :-
    debug(lsp(high), 'Text for "~w" has changed, reprocessing buffer.~n', [Path]),
    retractall(user:full_text(Path, _)),
    xref_source_expired(Path),
    asserta(user:next_text(Path, NewText)),
    xref_enqueue_file(Path).


xref_source_dir(Dirs) :- exists_directory(Dirs), enumerate_directory_files(Dirs, FileList), !, maplist(xref_metta_source, FileList).



/*
       , RootPath), directory_source_files(RootPath, Files, [recursive(true)]),
       maplist(xref_metta_file,Files) )

xref_metta_file(File):- assert(in_editor(File)).
*/


%!  xref_metta_source(+Path) is semidet.
%
%   Checks if a Metta file needs to be reprocessed by comparing its current content 
%   with the last recorded content, ensuring that at least 20 seconds have passed since 
%   the last check.
%
%   @arg Path The file path of the Metta file.
xref_metta_source(Path) :- ignore(xref_source_path(Path)).

% Check if the file or directory should be processed
xref_source_path(Doc) :- var(Doc),!.
xref_source_path(List):- is_list(List),!,maplist(xref_source_path,List).
xref_source_path(Doc) :- maybe_doc_path(Doc, Path), !, xref_source_path(Path).
xref_source_path(Dirs) :- exists_directory(Dirs), !,nop( xref_source_dir(Dirs)).
xref_source_path(Path):- \+ check_time_elapsed(Path), !, debug(lsp(xrefTime), 'Skipping check for "~w" as 20 seconds have not passed.~n', [Path]), !.
xref_source_path(Path):- \+ file_name_extension(_, metta, Path),!.  % Ensure the file has a .metta extension.
xref_source_path(Path) :- xref_enqueue_file(Path).

:- dynamic
   xref_file_state/2,
   xref_thread_control/1,
   xref_file_queue/1.

% Main predicate to submit a file and ensure it's processed
xref_submit_and_wait(File) :- xref_file_state(File, done), !.
xref_submit_and_wait(File) :-
    xref_enqueue_file(File),
    xref_wait_for_file(File).

disable_thread_system.

% Send an interrupt to the worker thread
xref_interrupt_worker(_File) :- disable_thread_system, !.
xref_interrupt_worker(File) :-
    ignore((xref_update_file_state(File, processing),
            xref_thread_control(ThreadID),
            thread_signal(ThreadID, throw(interrupted)))).

% Wait for a specific file to be processed completely
xref_wait_for_file(File) :-
    repeat,
    xref_file_state(File, State),
    (State \== submitted, State \== processing),
    sleep(0.1),
    fail.
xref_wait_for_file(File) :-
    xref_file_state(File, State),
    debug(lsp(xref), "File ~w has been processed and is now ~w.~n", [File, State]).


% Predicate to recursively enumerate files in directories, resolving symlinks.
enumerate_directory_files(Dirs, FileList) :-
    (is_list(Dirs) -> ProcessDirs = Dirs ; ProcessDirs = [Dirs]),
    findall(File,
            (member(Dir, ProcessDirs),
             absolute_file_name(Dir, AbsDir, [file_type(directory), access(read), solutions(all), file_errors(fail)]),
             enumerate_files_in_directory(AbsDir, File)),
            Files),
    list_to_set(Files, FileList).

% Recursive predicate to enumerate files within a directory
enumerate_files_in_directory(Path, Files) :-
    exists_directory(Path), !,
    directory_files(Path, Entries),
    findall(File,
            (member(Entry, Entries),
             \+ member(Entry, ['.', '..']),
             absolute_file_name(Entry, EntryPath, [relative_to(Path), solutions(all), file_errors(fail)]),
             (exists_file(EntryPath) -> File = EntryPath ; exists_directory(EntryPath), enumerate_files_in_directory(EntryPath, File))),
            Files).
enumerate_files_in_directory(_, []).  % Handle non-directory paths by returning an empty list

% xref_enqueue_file(+File) is det.
%
%   Adds a file to the processing queue and ensures the worker thread is running.
xref_enqueue_file(File) :- xref_file_queue(File),!.
xref_enqueue_file(File) :- made_metta_file_buffer(File),!.
xref_enqueue_file(Path):- disable_thread_system, !,
      get_current_text(Path, NewText),  % Get the current content of the file.
      compare_and_update_string(Path, NewText),  % Compare with the last stored content.
      call(debug_buffer_info).  % Save the current state.
xref_enqueue_file(File) :-
    xref_ensure_worker_thread_running(),
    xref_update_file_state(File, submitted),
    ( xref_file_queue(File) ->
        true  % File is already in the queue; do nothing
    ;   assertz(xref_file_queue(File))
    ).


%!  check_time_elapsed(+Path) is semidet.
%
%   Checks if at least 20 seconds have passed since the last file check for the given path.
%
%   @arg Path The file path to check.
:- dynamic(last_check_time/2).

check_time_elapsed(Path) :-
    get_time(CurrentTime),  % Get the current system time.
    (   last_check_time(Path, LastCheck),  % Retrieve the last check time for the file.
        ElapsedTime is CurrentTime - LastCheck,  % Calculate the time difference.
        ElapsedTime > 20  % Ensure at least 20 seconds have passed.
    ->  retractall(last_check_time(Path, _)),  % Remove the old time entry.
        asserta(last_check_time(Path, CurrentTime))  % Update with the new time.
    ;   \+ last_check_time(Path, _),  % If no previous time is recorded, store the current time.
        asserta(last_check_time(Path, CurrentTime))
    ).

%!  get_current_text(+Path, -NewText) is det.
%
%   Retrieves the current content of a file either from fallback documentation 
%   or by reading the file itself.
%
%   @arg Path    The file path to read.
%   @arg NewText The text content of the file.
get_current_text(Path, NewText) :-
    (   source_file_text(Path, NewText)  % Attempt to get the file's content as text.
    *->  true
    ;   NewText = ""  % Default to an empty string if the file cannot be read.
    ).

%!  compare_and_update_string(+Path, +NewText) is det.
%
%   Compares the current file content with the last recorded content. If different, 
%   the buffer is reprocessed.
%
%   @arg Path    The file path.
%   @arg NewText The current text content to compare.
:- dynamic(last_retrieved_string/2).
compare_and_update_string(Path, NewText) :-
    (   last_retrieved_string(Path, OldText),  % Retrieve the last known content.
        OldText \= NewText  % Check if the content has changed.
    -> (debug(lsp(xref), 'Text for "~w" has changed, reprocessing buffer.~n', [Path]),  % Log the change.
        retractall(last_retrieved_string(Path, _)),  % Remove the old content entry.
        asserta(last_retrieved_string(Path, NewText)),  % Update with the new content.
        xref_source_expired(Path),
        xref_metta_file_text('&xref', Path, NewText))   % Reprocess the file with the new content.
    ;   (debug(lsp(xref), 'Text for "~w" has not changed, skipping reload.~n', [Path]),  % Log if no change is detected.
        xref_metta_file_text('&xref', Path, NewText))  % Still cross-reference the file for consistency.
    ).

   
   
   
    
% Update the processing state of a file
xref_update_file_state(File, State) :-
    retractall(xref_file_state(File, _)),
    assertz(xref_file_state(File, State)).

% Worker thread for processing files
xref_ensure_worker_thread_running() :-
    ( xref_thread_control(ThreadID) ->
        ( thread_property(ThreadID, status(running)) ->
            true  % The thread is running; do nothing
        ;   % Else: Thread is not running
            ( retractall(xref_thread_control(ThreadID)),
              thread_create(xref_process_files, NewThreadID, []),
              assertz(xref_thread_control(NewThreadID))
            )
        )
    ;   % Else: No thread ID stored
        ( thread_create(xref_process_files, ThreadID, []),
          assertz(xref_thread_control(ThreadID))
        )
    ).

% Worker thread that processes files from the queue
xref_process_files :-
    repeat,
    (xref_file_queue(File) ->
        retract(xref_file_queue(File)),
        once(xref_handle_file(File)),
        fail  % Continue processing files
    ; sleep(0.1),
      fail
    ).

% Handle individual files and catch interruptions
xref_handle_file(File) :-
    catch((debug(lsp(xref), "Processing file: ~w~n", [File]),
           setup_call_cleanup(xref_update_file_state(File, processing),
                              xref_source_now(File),
                              xref_update_file_state(File, done)),
           debug(lsp(xref), "Processing complete: ~w~n", [File])),
          interrupted,
          (debug(lsp(xref), "Processing of file ~w was interrupted, resuming...~n", [File]),
           xref_update_file_state(File, interrupted))).

xref_source_now(Path) :-
    source_file_text(Path, NewText),
    xref_metta_file_text('&xref',Path, NewText),
    call(debug_buffer_info).  % Save the current state

xref_source_expired(Path):- var(Path),!.
xref_source_expired(Doc):- maybe_doc_path(Doc,Path),!,xref_source_expired(Path).
xref_source_expired(Path):-
   doc_path(Doc,Path),
   retractall(made_metta_file_buffer(Doc)),
   retractall(gave_document_symbols(Doc,_)),
   retractall(made_metta_file_buffer(Path)),
   retractall(gave_document_symbols(Path,_)),
  %retractall(metta_file_buffer(0,_Ord,_Kind,Mode, _Term, _NamedVarsList, Path, _Pos)),
   xref_interrupt_worker(Path).

    

:- debug(lsp(xref)).

%!  debug_buffer_info is det.
%
%   Saves the current state of the Metta file buffer to 'last_file.txt'.
debug_buffer_info:- 
    ignore((debugging(server(xref)),
            open('last_file.txt', write, Stream),
            with_output_to(Stream, listing(metta_file_buffer)),
            close(Stream))).


%!  xref_metta_file_text(+Self, +Path, +Text) is det.
%
%   Cross-references a Metta file's content and processes it in the correct context.
%
%   @arg Self The context (usually '&self') for the cross-reference.
%   @arg Path The file path to cross-reference.
%   @arg Text The content of the file as text.
:- dynamic(made_metta_file_buffer/1).
xref_metta_file_text(_Self, Path, _Text) :- made_metta_file_buffer(Path),!.
xref_metta_file_text(_Self, Path, _Text) :- metta_file_buffer(0,_Ord,_Kind, _Term, _NamedVarsList, Path, _Pos), !.
xref_metta_file_text(Self, Path, Text):- fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, % so we notice we are not using this clause
    % this one calls the compiler and makes sense to be the default for xref-ing
    % (var(Text); Text==""),!,
    nop(debug(lsp(high), "xref_metta_file_text ~w", [Text])), 
    absolute_file_name(Path, Filename),  % Convert the file path to an absolute path.
    directory_file_path(Directory, _, Filename),  % Extract the directory path from the file path.
    debug(lsp(xref), "xref_metta_file_text Path ~w", [Path]),  % Log the cross-referencing process.
    with_option(exec, skip,  % Use options for processing in the correct context.
  locally(nb_setval(may_use_fast_buffer,t),
   locally(nb_setval(suspend_answers,true),
     with_output_to(string(_),
       include_metta_directory_file(Self,Directory,Filename))))),!.
%   This variant of `xref_metta_file_text/3` handles file inclusion by converting a 
%   file path to an absolute path, opening the file, and processing its content in 
%   the correct working directory context. It handles both string content and file 
%   reading.
xref_metta_file_text(_Self, Path, Text) :-
    asserta(made_metta_file_buffer(Path)),!,
    debug(lsp(xref), "xref_metta_file_text ~w", [Path]),  % Log the file path being processed.
    must_det_ll((
        % Convert the file path to an absolute path
        absolute_file_name(Path, Filename),
        % Extract the directory path from the absolute file name
        directory_file_path(Directory, _, Filename),
        % Process the file content in the specified directory context
       locally(nb_setval(may_use_fast_buffer,t),
         with_option(exec, skip,
          with_option(suspend_answers, true,
            with_cwd(Directory,
                setup_call_cleanup(
                        % Open the file or string content
                    (atomic(Text)->open_string(Text,In);open(Path, read, In, [encoding(utf8)])),
                        % Process the buffer content from the file
                      must_det_ll(xref_metta_file_text_buffer(false, Path, In)),
                        % Ensure the file is closed even in case of errors
                        catch(close(In), _, true)))))))).  

%!  xref_metta_file_text_buffer(+TFMakeFile, +Filename, +In) is det.
%
%   Processes the content of a Metta file buffer, optionally writing it to a buffer file.
%   The predicate reads lines from the input stream `In`, and asserts the parsed content 
%   into the buffer. It optionally writes the buffer content to an output file for later use.
%
%   @arg TFMakeFile  If true, also create a buffer file.
%   @arg Filename    The name of the file being processed.
%   @arg In          The input stream (either from the file or string).
xref_metta_file_text_buffer(TFMakeFile, FileName, InStream) :-
  make_metta_file_buffer(TFMakeFile, FileName, InStream).

maybe_process_directives(+, exec([Op|List])):-
  op_execkind(Op,import),
  last(List,Path),!,
  absolute_file_name(Path, File, [access(read), extensions(['','.metta'])]),
  xref_metta_source(File).


%!  source_file_text(+Path, -String) is det.
%
%   Retrieves the content of the specified file or fallback text, ensuring the content
%   is read as a string. It first tries to obtain the text using `doc_text_fallback/2`,
%   and if that fails, it attempts to read the file directly.
%
%   @arg Path   The file path from which the content is to be read.
%   @arg String The file content represented as a string.
%
%   @example Example usage:
%       ?- source_file_text('file.txt', Text).
%       Text = "File content here".
%
:- dynamic(user:next_text/2).
:- dynamic(user:full_text/2).
source_file_text(Doc, FullText) :- maybe_doc_path(Doc,Path), !, source_file_text(Path, FullText).
source_file_text(Path, FullText) :- user:next_text(Path, FullText),!.
source_file_text(Path, FullText) :- user:full_text(Path, FullText),!.
source_file_text(Path, String) :-
    % Tries to retrieve text using doc_text_fallback/2 first.
    findall(Str, 
        (lsp_metta_changes:doc_text_fallback(Path, D4s),
         sub_term(d(_, Str, _, _), D4s), 
         string(Str)), 
    List),
    % Combines the fallback text into a single string.
    atomics_to_string(List, String), !.

source_file_text(Path, String) :-
    % If no fallback text was found, check if the file exists.
    exists_file(Path),
    % Read the file content as a string with UTF-8 encoding.
    read_file_to_string(Path, String, [encoding(utf8)]), !.


file_doc(Path, Doc):- atom_concat( 'file://', Path, Doc).
path_doc(Path, Doc):- file_doc(Path, Doc).
 
doc_path(Doc,Path):- atomic(Doc),file_doc(Path, Doc),!.
doc_path(Doc,Path):- atomic(Path), assertion( \+ file_doc(_, Path)), file_doc(Path, Doc),!.
doc_path(Doc,Path):- nonvar(Doc),!,Path=Doc.
doc_path(Doc,_Path):- frozen(Doc,Ice),Ice=freeze(_,_),!.
doc_path(Doc,Path):- freeze(Path,file_doc(Path, Doc)).

doc_uri(Doc,DocUri):- nonvar(Doc), file_doc(_, Doc), !, DocUri = Doc. 
doc_uri(Doc,DocUri):- nonvar(Doc), file_doc(Doc, DocUri).

maybe_doc_path(Doc,Path):- atomic(Doc),file_doc(Path, Doc),!.





/*
Dict = _{
  capabilities: _{
    textDocumentSync: _{
      openClose: true,
      change: 2,  % Full sync (incremental changes)
      willSave: true,
      willSaveWaitUntil: true,
      save: _{ includeText: true }
    },
    hoverProvider: true,
    completionProvider: _{
      resolveProvider: true,
      triggerCharacters: [".", ":", "(", "<"]
    },
    signatureHelpProvider: _{
      triggerCharacters: ["(", ","],
      retriggerCharacters: [")", ","]
    },
    definitionProvider: true,
    typeDefinitionProvider: true,
    implementationProvider: true,
    declarationProvider: true,
    referencesProvider: true,
    documentHighlightProvider: true,
    documentSymbolProvider: _{
      labelSupport: true,
      hierarchicalDocumentSymbolSupport: true
    },
    workspaceSymbolProvider: true,
    codeActionProvider: _{
      codeActionKinds: [
        "quickfix", "refactor", "refactor.extract", "refactor.inline",
        "refactor.rewrite", "source.organizeImports", "source.fixAll"
      ],
      resolveProvider: true
    },
    codeLensProvider: _{ resolveProvider: true },
    documentFormattingProvider: true,
    documentRangeFormattingProvider: true,
    documentOnTypeFormattingProvider: _{
      firstTriggerCharacter: ";",
      moreTriggerCharacter: ["}", "\n"]
    },
    renameProvider: _{ prepareProvider: true },
    foldingRangeProvider: _{
      rangeLimit: 5000,
      lineFoldingOnly: true
    },
    executeCommandProvider: _{
      commands: ["command.id", "other.custom.command"]
    },
    workspace: _{
      workspaceFolders: true,
      applyEdit: true,
      didChangeWatchedFiles: _{ dynamicRegistration: true },
      configuration: true,
      symbol: _{
        symbolKind: _{
          valueSet: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]
        }
      }
    },
    semanticTokensProvider: _{
      legend: _{
        tokenTypes: [
          "namespace", "class", "enum", "interface", "struct", "typeParameter",
          "type", "parameter", "variable", "property", "enumMember", "event",
          "function", "method", "macro", "keyword", "modifier", "comment",
          "string", "number", "regexp", "operator"
        ],
        tokenModifiers: [
          "declaration", "definition", "readonly", "static", "deprecated", 
          "abstract", "async", "modification", "documentation", "defaultLibrary"
        ]
      },
      full: _{ delta: true },
      range: true
    },
    monikerProvider: true,
    documentLinkProvider: _{ resolveProvider: true },
    colorProvider: true,
    inlayHintProvider: _{ resolveProvider: true },
    callHierarchyProvider: true,
    typeHierarchyProvider: true,
    linkedEditingRangeProvider: true,
    selectionRangeProvider: true,
    diagnosticProvider: _{
      interFileDependencies: true,
      workspaceDiagnostics: true
    },
    notebookDocumentSync: _{
      notebookSelector: [
        _{
          notebook: _{
            type: "jupyter-notebook",
            scheme: "file"
          },
          cells: [_{ language: "python" }]
        }
      ]
    },
    window: _{
      workDoneProgress: true,
      showMessage: _{
        messageActionItem: _{ additionalPropertiesSupport: true }
      },
      showDocument: _{ support: true }
    },
    experimental: _{ experimentalFeature: true }  % Placeholder for any experimental features
  }
}.
*/
