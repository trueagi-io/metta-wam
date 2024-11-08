nop_mod_lsp_metta_hover
:- module(lsp_metta_hover,
           [metta_called_at/4,
            metta_defined_at/4,
            defined_at/4,
            matta_name_callable/2,
            metta_relative_ref_location/4
            %help_at_position/4,
            %clause_in_file_at_position/3,
            %clause_variable_positions/3,
            %seek_to_line/2,
            %linechar_offset/3
           ]).
/** <module> LSP Utils

Module with a bunch of helper predicates for looking through prolog
source and stuff.

@author James Cash
@author Roy Ward
@author Douglas Miles

*/
    :- include(lsp_metta_include).


%! hover_at_position(+Path:atom, +Line:integer, +Char:integer, -Help:term) is det.
%
%  =Help= is the documentation for the term under the cursor at line
%  =Line=, character =Char= in the file =Path=.

hover_at_position(Doc, Line0, Char0, S) :- maybe_doc_path(Doc, Path), !, hover_at_position(Path, Line0, Char0, S).
hover_at_position(Path, Line0, Char0, S) :-
  Loc = line_char(Line0, Char0),
  %debug(lsp(low), "hover_at_position", []),
  clause_with_arity_in_file_at_position(Term, Arity, Path, Loc),
  % TODO - add this in when I can import eval_args
  %debug(lsp(low), "Term=~w", [Term]),
  findall(S, lsp_hooks:hover_hook(Path, Loc, Term, Arity, S), SS),
  combine_hover(Term, SS, S).

combine_hover(Term, [], _{contents: _{kind: plaintext, value: S}}):- !,
   format(string(S), "Unknown: ~w", [Term]).

combine_hover(_Term, SS, _{contents: _{kind: markdown, value: S}}):- !,
  list_to_set(SS, Set),
  maplist(into_markdown, Set, Strings),
  atomics_to_string(Strings, S1),
  string_replace_each(S1,
 ["```lisp\n\n"="```lisp\n",
  "```\n\n"="```\n",
  "\n\n```"="\n```",
  %"```lisp\n```\n```lisp\n```\n"="```lisp\n```\n",
  "```lisp\n```\n"="\n",
  %"```lisp\n```\n--\n```lisp\n```\n"="--\n",
  % "\n\n\n"="\n\n",
  "fooooo"="barrrrrr"],S), !.

string_replace_each(S1,[],S1):-!.
string_replace_each(S1,[F=R|List],S):-
      string_replace(S1,F,R,S2),
      string_replace(S2,F,R,S3),
 string_replace_each(S3,List,S).


into_markdown(Ans, S):- \+ is_dict(Ans), sformat(Str, '~w', [Ans]),
  into_markdown_s(Str, S).
into_markdown(Ans, S):-
   _{contents: _{kind: plaintext, value: Help}} :< Ans, !,
   into_markdown_s(Help, S).
into_markdown(Ans, S):-
   _{contents: _{kind: markdown, value: S}} :< Ans, !.
into_markdown(Ans, S):- sformat(Str, '~q', [Ans]),
   into_markdown_s(Str, S).

into_markdown_s(Ans, Final) :-
  trim_white_lines(Ans, Stripped), % removes leading and trailing newlines
  atomics_to_string(['```lisp\n', Stripped, '\n```\n'], Final).

% trim/2 predicate that trims newlines and carriage returns from the front and back of strings and atoms
trim_white_lines(Input, Trimmed) :-
  % Convert Input to a string if it’s an atom
  (atom(Input) -> atom_string(Input, Str); Str = Input),
  % Trim leading and trailing whitespace, newlines, and carriage returns
  trim_leading(Str, TrimmedStr1),
  trim_trailing(TrimmedStr1, TrimmedStr),
  % Convert back to atom if original input was an atom
  (atom(Input) -> atom_string(Trimmed, TrimmedStr); Trimmed = TrimmedStr).

% Helper predicate to trim leading whitespace, newlines, and carriage returns
trim_leading(Str, TrimmedStr) :- sub_string(Str, 0, 1, _, Start), memberchk(Start, ["\n", "\r"]), !,
  sub_string(Str, 1, _, 0, Str1), !, trim_leading(Str1, TrimmedStr).
trim_leading(Str, Str).  % Base case: no leading newline, return unchanged

% Helper predicate to trim trailing whitespace, newlines, and carriage returns
trim_trailing(Str, TrimmedStr) :-
  sub_string(Str, _, 1, 0, LastChar),
  ( LastChar = '\n' ; LastChar = '\r' ; LastChar = ' ' ), !,
  sub_string(Str, 0, _, 1, Str1),
  trim_trailing(Str1, TrimmedStr).
trim_trailing(Str, Str).  % Base case: no trailing newline, return unchanged


% string_contains(String, Substring): true if Substring is contained within String
string_contains(String, Substring) :-
  sub_string(String, _, _, _, Substring).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Roy's initial impl of Hover
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use_vitalys_help :- true.

lsp_hooks:hover_hook(Path, _Loc, Term, Arity, S):- \+ use_vitalys_help,
  metta_predicate_help(Path, Term, Arity, S).

metta_predicate_help(_, Var, _, S) :- var(Var), !, format(string(S), "Var: ~w", [Var]).
metta_predicate_help(_, '', _, "") :- !.
metta_predicate_help(_, '$VAR'(Term), _, S) :- !, format(string(S), "Variable: ~w", [Term]).
metta_predicate_help(_, Term, _, "") :- number(Term), !.
metta_predicate_help(_, ')', _, "") :- !.
metta_predicate_help(_, ']', _, "") :- !.
metta_predicate_help(_, '}', _, "") :- !.
metta_predicate_help(_, Term, _, S) :- find_at_doc(Term, S), !.
metta_predicate_help(_, Term, Arity, S) :- fail, metta_atom(_KB, ['@doc', Term|Help]),
  format_metta_doc(Term, Arity, Help, S), !.

format_metta_doc(Term, Arity, [['@desc', Description], ['@params', Params], ['@return', Return]], String) :-
  maplist(format_metta_Param, Params, Params_formatted),
  atomic_list_concat(Params_formatted, '\n', Params_formattednl),
  length(Params, LP),
  ((Arity=unknown;Arity=LP) -> Warning="" ; format(string(Warning), "\n Arity warning, found ~w, expected ~w", [Arity, LP])),
  format(string(String), "~w: ~w\n~w\nReturns: ~w~w", [Term, Description, Params_formattednl, Return, Warning]).

format_metta_Param(['@param', P], Pf) :- format(string(Pf), "Param: ~w", [P]).

find_at_doc(Term, S) :-
  lsp_metta_changes:doc_text_d4(Path, SplitFile),
  find_at_doc_aux(Path, Term, SplitFile, S).

find_at_doc_aux(_Path, Term, [d(_, Doc, _, Metadata)|_], S) :-
  find_at_doc_aux2(Term, Metadata), !,
  %format(string(S), "@doc found: ~w", [Doc]).
  format(string(S), "~w", [Doc]).


find_at_doc_aux(Path, Term, [_|T], S) :-
  find_at_doc_aux(Path, Term, T, S).

find_at_doc_aux2(Term, [doc(Term)|_]) :- !.
find_at_doc_aux2(Term, [_|T]) :-
  find_at_doc_aux2(Term, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Douglas' initial impl of Hover
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:hover_hook(Path, Loc, Term, Arity, S):-
  make,   % Trigger the make command to compile the knowledge base. (For real-time editing)
  xref_metta_source(Path),   % Possibly cross-reference the file.
  term_info_string(Path, Loc, Term, Arity, S). % Generate a term definition string.

:- multifile(lsp_hooks:handle_msg_hook/3).
:- dynamic(lsp_hooks:handle_msg_hook/3).

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



%! term_info_string(+Path, +Loc, +Term, +Arity, -S) is semidet.
%
%   Provides help documentation for predicates in the form of hooks. If the predicate
%   is a symbol and is documented, it retrieves the help text.
%
%   @arg Path    The file or path associated with the predicate.
%   @arg Loc     Location associated with the predicate.
%   @arg Term    The term (predicate) for which help is sought.
%   @arg Arity   The arity of the predicate.
%   @arg S     The help content to return.

term_info_string(Path, Loc, Term, Arity, S):-  symbol(Term),  % Fail if the term is not a valid symbol.
  \+ is_documented(Term),   % Check if the term is undocumented.
  symbol_resolve(Term,  Resolved),   % Try to resolve the symbol to a documented one.
  is_documented(Resolved),   % Ensure the resolved symbol is documented.
  term_info_string(Path, Loc, resolved(Resolved), Arity, S).  % Recurse to retrieve help for the resolved symbol.
term_info_string(Path, Loc, Term, Arity, Str):-
  term_info_string_resolved(Path, Loc, Term, Arity, Str).

show_term_info(Term):-
   forall(lsp_hooks:term_info(_Path, _Loc, Term, _Arity), true).

term_info_string_resolved(_Path,_Loc, Term, _Arity, _Str):- var(Term),!.
term_info_string_resolved( Path, Loc, resolved(Term), Arity, Str):- !,
  term_info_string_resolved(Path, Loc, Term, Arity, Str).
term_info_string_resolved(Path, Loc, Term, Arity, Str):-
  wots(S0, lsp_hooks:term_info(Path, Loc, Term, Arity)),  % Generate a string output for the term's arity help.
  string(S0),  % Ensure that the output is a valid string.
  trim_white_lines(S0, S),
  S \= "",  % Ensure that the string is not empty.
  atom_length(S, Len),
  Len > 1, % Ensure the string has a minimum length.
  format(string(Str), "~w", [S]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
in_markdown(G):- setup_call_cleanup(format('~n```~n', []), G, format('~n```lisp~n')).
banner_for(Type, Target):- in_markdown(format('---~n ## ~w: ~w', [Type, Target])).
lsp_separator():- in_markdown(format('---',[])).

show_checked(Name, Value, Caption) :- fail,
  format("[~w](file:command:myExtension.toggleValue?{\"name\":\"~w\", \"value\":\"~w\"}) ~w ", [Value, Name, Value, Caption]).
show_checked(Name, Value, Caption) :- format("[~w](file://toggleValue_~w.metta) ~w ", [Value, Name, Caption]).

:- multifile(lsp_hooks:term_info/4).
:- discontiguous(lsp_hooks:term_info/4).

%!  lsp_hooks:term_info(+Path, +Loc, +Target, +Arity) is det.
%
%   Helper predicate to display information about each instance of a term's arity.
%
%   @arg Target The term (predicate) for which help is displayed.
%   @arg Arity The arity to check for.
lsp_hooks:term_info(_Path,_Loc, Target, _) :- fail, % (for debugging) commenting out fail will let the hover show the cross-ref index
  each_type_at_sorted(Target, Sort),
  forall(member(RefType, [definition, declaration, typeDefinition, implementation, references]),
  (banner_for(RefType, Target),
  %ignore((metta_defined_at(Type, HintPath, Target, Term, Path, Loc),
  forall(member(each_type_at(Target, Term, AtPath, AtLoc, Type), Sort),
    ignore((
      once(type_expand(RefType, Type)),
      write_src_xref(Term, Type, AtPath, AtLoc), nl))))),
   banner_for('rest-of', Target).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Douglas' initial impl of Hover Help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:term_info(_Path,_Loc, Target, _) :-
  in_markdown((
  show_checked("show_docs", "(-)", "Show Docs "),
  show_checked("show_refs", "(+)", "Show Refs "),
  show_checked("show_pdbg", "(-)", "Debug Pos "),
  show_checked("show_menu", "(+)", "Show Menu: "),
  format("**~q**", [Target]))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vitaly's initial impl of Help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:term_info(_Path,_Loc, Target, _) :- use_vitalys_help,
  lsp_separator(),
  xref_call(eval(['help!', Target], _)), lsp_separator().  % Evaluate the help command for the term.


lsp_hooks:term_info(_Path,_Loc, Term, Arity):-
  lsp_separator(),
   (((some_arities(Term,Arity, Try, _TryArity),
     get_type(Try,  Type), (Type \=='%Undefined%', Type \==[], % Get the type of the term or default to 'unknownType'.
     true)))*->true; (Try=Term,Type='%Undefined%')),
   in_markdown( (numbervars(Try+Type,0,_,[singletons(true),attvars(skip)]),format("*Type* ~@ **~@**~n~n",  [write_src_xref(Try), write_src_xref(Type)]))),  % Format the output as a help string.
   lsp_separator().

%
some_arities(Term,N,Try,return(N)):- integer(N),!,length(Args,N),Try=[Term|Args].
some_arities(Term,N,Try,RN):- symbol(Term), between(0,5,N), some_arities(Term,N,Try,RN).
some_arities(Term,_,Term,term). % Bare Term

lsp_hooks:term_info(_Path,_Loc, Target, Arity):- number(Arity), Arity > 1,
  findall(A, is_documented_arity(Target, A), ArityDoc),  % Retrieve documented arities for the term.
  ArityDoc \== [],  % Ensure the documentation is not empty.
  \+ memberchk(Arity, ArityDoc),  % Verify if the term's arity DOES NOT matches the documented arity.
  format('Arity expected: ~w vs ~w~n', [ArityDoc, Arity]), lsp_separator() .  % Output a message if there's an arity mismatch.


lsp_hooks:term_info(_Path,_Loc, Target, _) :-
  lsp_separator(),
  each_type_at_sorted(Target, Term, AtPath, AtLoc, Type),
  write_src_xref(Term, Type, AtPath, AtLoc).  % Write the source cross-reference for the atom.

get_code_at_range_type(term).
get_code_at_range_type(expression).
get_code_at_range_type(toplevel_form).
%get_code_at_range_type(exact).
%get_code_at_range_type(symbol).

debug_positions :- debugging(lsp(position)).
/*
<details>
<summary>Q1: What is the best X in the World? </summary>
A1: whY
</details>
*/
lsp_hooks:term_info(Path, Loc, Term, Arity):- debug_positions,
  lsp_separator(),
  setup_call_cleanup(
     lsp_separator(),
     debug_positions(Path, Loc, Term, Arity),
     in_markdown(format('~n</details>~n'))).

debug_positions(_Path, Loc, Term, Arity) :-
   in_markdown((format("*Debug Positions*:\t\t<details><summary>(this and below is normally hidden)</summary>~n~n\t\t**~q**~n~n",  [[Loc, Term, Arity]]))).   % Format the output as a help string.

debug_positions(_Path, _Loc, _Term, _Arity) :-
   user:last_range(Method,Range),
   numbervars(Range,0,_,[singletons(true),attvars(skip)]),
   in_markdown((format("*~w*: **~q**~n~n",  [Method,Range]))).   % Format the output as a help string.

debug_positions(Path, Loc, _Term, _Arity) :-
   get_code_at_range_type(Type),
   (get_code_at_range(Type, Path, Loc, Code) *-> true; Code='failed'),
   in_markdown((format("**~@**: ~@~n~n",  [write_src_xref(Type),  write_src_xref(Code)]))).



%xref_call(G):- catch(G, E, debug(lsp(high), "xref_call ~w", [G])).
%xref_call(G):- catch(with_no_debug(G), E, debug(lsp(high), "xref_call ~w", [G->E])).
xref_call(G):- with_no_debug(G).
%xref_call(G):- call(G).

each_type_at_sorted(Target, Term, Path, Loc, Type):-
  each_type_at_sorted(Target, Sort),
  member(each_type_at(Target, Term, Path, Loc, Type), Sort).

each_type_at_sorted(Target, Sort):-
    findall(each_type_at(Target, Term, Path, Loc, Type),
        each_type_at(Target, Term, Path, Loc, Type),
        List),
     group_by_last_arg(List, Sort).

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


each_type_at(Target, Term, AtPath, AtLoc, Type):-
  no_repeats_var(TermV),
  metta_atom_xref(Term, AtPath, AtLoc), TermV = Term,  % Cross-reference the term with known atoms.
  about_term(Term, Target),  % Determine if the atom is related to the term.
  \+ skip_xref_atom(Term),  % Skip atoms that are not relevant for cross-referencing.
  type_symbol_clause(Type, Target, Term).
  %format('~@', [write_src_xref(Term, Type, AtPath, AtLoc)]).

%!  about_term(+Atom, +Term) is semidet.
%
%   Determines if the Atom is about the given Term.
%
%   @arg Atom The atom to check.
%   @arg Term The term to match with the atom.
about_term([Op, Atom | _], Term):- fail, Op==':', is_documented(Term), !, Atom\==Term, % Dont reshow types
  sub_var(Term, Atom),  !. % Check if the term is a subterm of the atom.
about_term([_, [Atom|_]|_], Term):- sub_var(Term, Atom), !.
about_term([_, Atom|_], Term):- ==(Term, Atom), !.
about_term([Atom|_], Term):- \+ promiscuous_symbol(Term), sub_var(Term, Atom), !.
about_term(exec(Atom), Term):-!, sub_var(Term, Atom).


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


write_src_xref(Term, Type, Path, Loc):-
   catch_skip((write_src_xref(Term),
   ignore(write_file_link(Type, Path, Loc)))).

write_src_xref(Term, Path, Loc):-
   catch_skip((write_src_xref(Term),
   ignore(write_file_link(Path, Loc)))).

catch_skip(G):- ignore(catch(G, _, true)).

% Check for deeply nested lists
very_nested_src([_, _ | Src]):- is_list(Src),
  member(M, Src), is_list(M),
  member(E, M), is_list(E),
  member(I, E), is_list(I), !.

maybe_link_xref(What):-
  ignore(once((
   metta_file_buffer(0, _Ord, _Kind, Atom, _, Path, Pos),
   %symbolic(Path), \+ symbol_contains(Path, 'stdlib_mettalog'),
   once((alpha_unify(What, Atom); \+ (What \= Atom))),
   %next_clause(Ref, metta_file_buffer(0, _Ord, _Kind, _, _, Path, Pos)),
   write_file_link(Path, Pos)))).

% next_clause(Ref, NextTerm)
%   - Ref is the reference of the current clause
%   - NextTerm is the next clause's reference, if it exists
next_clause(Ref, NextTerm) :-
   nth_clause(Pred, Nth, Ref),
   NextIndex is Nth + 1,
   nth_clause(Pred, NextIndex, NextRef), !,
   clause(NextTerm, _, NextRef).  % Get the clause at this reference

%   ~n```~n*~w*~n```lisp~n
write_file_link(Type, Path, Position):-
  must_succeed1(position_line(Position, Line0)), succ(Line0, Line1),
  in_markdown(format('[~w:~w](file://~w#L~w) _(~w)_', [Path, Line1, Path, Line1, Type])).
write_file_link(Path, Position):-
  must_succeed1(position_line(Position, Line0)), succ(Line0, Line1),
  in_markdown(format('[~w:~w](file://~w#L~w)', [Path, Line1, Path, Line1])).


position_line(Position, Line2):-
   into_line_char(Position, line_char(Line1, _)), succl(Line1, Line2).


