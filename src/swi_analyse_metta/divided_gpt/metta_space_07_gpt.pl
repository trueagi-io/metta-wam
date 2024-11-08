
metta_assertdb_iter_bind(KB,Query,Vars):-
  ignore(term_variables(Query,Vars)),
  print(metta_assertdb(['match',KB,Query,Vars])),nl,
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,Query],

  (MP*->true;call_metta_assertdb(KB,Query,Vars)),
  metta_assertdb('RES',metta_assertdb_iter_bind(KB,Query,Vars)).
%metta_assertdb_iter_bind(KB,Atom,Template):- metta_assertdb_stats, findall(Template,metta_assertdb_iter(KB,Atom),VarList).

metta_assertdb_iter_bind(KB,Atoms,Vars):-
  metta_assertdb_stats,
  term_variables(Atoms,AVars),
  metta_assertdb_iter(KB,Atoms), ignore(AVars = Vars).
*/





/**
 * align_varnames/2
 * 
 * Aligns variable names to a list of variables.
 * 
 * @param VarNames List of variable names to align
 * @param Vars List of aligned variables
 */
align_varnames(VarNames, Vars) :-
    % Convert the list of variable names to a set to eliminate duplicates
    list_to_set(VarNames, NameSet),

    % Merge the named variables
    merge_named_vars(NameSet, VarNames, Vars).

/**
 * merge_named_vars/3
 * 
 * Merges a set of named variables with their corresponding values.
 * 
 * @param NameSet Set of names
 * @param VarNames List of variable names
 * @param Vars List of variable values
 */
merge_named_vars([], _VarNames, _Vars) :- !.  % Base case, do nothing when the name set is empty
merge_named_vars([N | NameSet], VarNames, Vars) :-
    % Merge the named variable N with its value
    merge_named(N, _V, VarNames, Vars),

    % Recursively process the remaining names in the set
    merge_named_vars(NameSet, VarNames, Vars).

/* previously: 
 * The following line was part of the original logic but was commented out due to optimization reasons. 
 * Keeping it for reference:
 * 
 * merge_named_vars(_,_,_).
 */

/**
 * merge_named/4
 * 
 * Merges a named variable with its value in a list of variables.
 * 
 * @param N Name of the variable to merge
 * @param V Value of the variable
 * @param VarNames List of variable names
 * @param Vars List of variable values
 */
merge_named(_, _, [], []) :- !.  % Base case, no more names or values to process
merge_named(N, V, [N | VarNames], [V | Vars]) :-
    % Recursively merge the rest of the variables
    merge_named(N, V, VarNames, Vars).

/**
 * call_metta/3
 * 
 * Calls a metta query either directly or through the Python SWI-Prolog interface.
 * 
 * @param KB Knowledge base to use (may be ignored)
 * @param Query Query to be executed
 * @param _Vars Unused list of variables
 */
call_metta(KB, Query, _Vars) :-
    % If the query is already defined in the metta_atom, call it directly
    metta_atom(KB, Query).
call_metta(_KB, Query, _Vars) :-
    % Otherwise, translate the query for the Python SWI-Prolog interface and call it
    metta_to_pyswip([], Query, Call), !,
    % Execute the translated Python call
    user:call(Call).

/**
 * metta_to_pyswip/3
 * 
 * Translates a Prolog query into a format that can be executed by Python SWI-Prolog.
 * 
 * @param PS Prefix stack (for context)
 * @param Query The Prolog query to translate
 * @param Call The resulting translated call
 */
metta_to_pyswip(_PS, Query, Call) :-
    % If the query is a variable, return it directly
    var(Query), !, Call = Query.
metta_to_pyswip(_PS, Query, Call) :-
    % If the query is not compound (e.g., an atom or number), return it directly
    \+ compound(Query), !, Call = Query, !.
metta_to_pyswip(PS, Query, Call) :-
    % If the query is a list, translate the head and tail
    is_list(Query), Query = [Q | Uery], !, cmpd_to_pyswip(PS, Q, Uery, Call).
metta_to_pyswip(PS, Query, Call) :-
    % Otherwise, break the query into its functor and arguments and translate them
    Query =.. [Q | Uery], cmpd_to_pyswip(PS, Q, Uery, Call).

/**
 * cmpd_to_pyswip/4
 * 
 * Translates a compound Prolog term into a format that can be executed in Python.
 * 
 * @param PS Prefix stack (for context)
 * @param Q Functor of the compound term
 * @param Uery Arguments of the compound term
 * @param Call The resulting translated call
 */
cmpd_to_pyswip(PS, Q, Uery, Call) :-
    % If the functor is an atom, translate each argument and reconstruct the term
    atom(Q), maplist(metta_to_pyswip([Q | PS]), Uery, Cery), Call =.. [Q | Cery].
cmpd_to_pyswip(PS, "and", Uery, Call) :-
    % Special case for 'and', translate to a conjunction of arguments
    maplist(metta_to_pyswip(PS), Uery, Args), list_to_conjuncts(Args, Call).

/**
 * show-metta-def/2
 * 
 * Displays the source code for a metta definition.
 * 
 * @param Pred Predicate whose definition is to be shown
 * @param [] Empty list as placeholder for future arguments
 */
'show-metta-def'(Pred, []) :-
    % Retrieve the source code for the given predicate
    'get-metta-src'(Pred, [_ | SrcL]),

    % Write each source line to the output
    maplist(write_src_nl, SrcL).

/**
 * write_src_nl/1
 * 
 * Writes a source line with newlines before and after.
 * 
 * @param Src Source line to write
 */
write_src_nl(Src) :-
    format('~N'), write_src(Src), format('~N').

/* previously:
 * The following commented-out version included the AtomDef wrapper, which was removed 
 * for simplicity and direct handling:
 *
 * 'get-metta-src'(Pred,[Len|SrcL]):- 
 *     findall(['AtomDef',Src], 'get-metta-src1'(Pred, Src), SrcL), 
 *     length(SrcL, Len).
 */

/**
 * get-metta-src/2
 * 
 * Retrieves the source code for a metta predicate.
 * 
 * @param Pred Predicate whose source code is being retrieved
 * @param [Len|SrcL] The length of the source list and the list of source code lines
 */
'get-metta-src'(Pred, [Len | SrcL]) :-
    % Collect all source code lines for the given predicate
    findall(Src, 'get-metta-src1'(Pred, Src), SrcL),

    % Get the length of the source list
    length(SrcL, Len).

/**
 * get-metta-src1/2
 * 
 * Retrieves a single source line for a metta predicate.
 * 
 * @param Pred Predicate being queried
 * @param Src Source line corresponding to the predicate
 */
'get-metta-src1'(Pred, Src) :-
    % Get the current working space for the predicate
    current_self(Space),

    % Find the atom definition in the space
    metta_atom(Space, F, A, List),

    % Return the source line if it matches the predicate
    once((sub_var(Pred, A) -> Src = [F, A, List] ; sub_var(Pred, F) -> Src = [F, A | List])).

/* previously:
 * This code block defined a quine-like structure, which is preserved but commented 
 * because it might not be necessary for all use cases:
 *
 * 'AtomDef'(X, ['AtomDef', X]).
 */

/**
 * sort_on/4
 * 
 * Sorts elements based on a custom comparison predicate.
 * 
 * @param C Custom comparator
 * @param R Result of the comparison (=, <, or >)
 * @param A First element
 * @param B Second element
 */
sort_on(C, R, A, B) :-
    % If A and B are the same, they are equal
    (A == B -> R = (=) 
    % Otherwise, compare them using the custom comparator
    ; must_det_ll((call(C, A, AA), call(C, B, BB), !, compare(R, AA + A, BB + B)))), !.

/**
 * tokens/2
 * 
 * Tokenizes an input string into a sorted list of tokens.
 * 
 * @param X Input string
 * @param VL The last (longest) token in the sorted list
 */
tokens(X, VL) :-
    % Convert the string to its unaccented form
    unaccent_atom(X, A), !,

    % Find all tokens using the available tokenizers
    findall(E, (is_tokenizer(T), call(T, A, E)), L),

    % Sort the tokens by length
    predsort(sort_on(length_fw_len), L, S),

    % Get the last token in the sorted list
    last(S, VL).

/**
 * length_fw_len/2
 * 
 * Custom sorting predicate based on the length of the token and the remaining list.
 * 
 * @param [W|List] Token and its list
 * @param L+WL Combined length of the list and token
 */
length_fw_len([W | List], L + WL) :-
    % Get the length of the list
    length(List, L),

    % Get the length of the token
    atom_length(W, WL).

/**
 * print_token_args/0
 * 
 * Prints arguments and their tokenized forms.
 */
print_token_args :-
    % Rebuilds the environment to ensure tokens are fresh
    make,

    % For each argument X, get its tokens and write the result
    fb_arg(X), tokens(X, A0),
    exclude(is_dash, A0, A), tterm(A, AT),
    writeq(X), write('    '), writeq(AT), write('  '), write_src(A), nl, fail.

/**
 * is_dash/1
 * 
 * Checks if a token is a dash character.
 * 
 * @param Char Character to check
 */
is_dash('_').
is_dash('-').

/**
 * tterm/2
 * 
 * Converts a list of terms into a structured term.
 * 
 * @param [A] Single term case
 * @param A The result when there is only one term
 */
tterm([A], A) :- !.

/**
 * tterm/2 (complex case)
 * 
 * Converts a list of terms into a structured term with functor and arguments.
 * 
 * @param [A,':',B|M] Functor and arguments to be structured
 * @param BA The resulting structured term
 */
tterm([A, ':', B | M], BA) :-
    % If the first element is an atom, construct the term
    atom(A), !, BA =.. [A, B | M].

/**
 * tterm/2 (fallback case)
 * 
 * Handles cases where a term has more than two elements.
 * 
 * @param [A, B | M] Functor and arguments
 * @param BA The resulting structured term
 */
tterm([A, B | M], BA) :-
    % If the second element is an atom, construct the term
    atom(B), !, BA =.. [B, A | M].

/**
 * tterm/2 (general case)
 * 
 * Converts a list of terms into a structured term.
 * 
 * @param [A | B] List of terms to convert
 * @param BA The resulting structured term
 */
tterm([A | B], BA) :-
    % If the first element is an atom, construct the term
    atom(A), !, BA =.. [B | A].