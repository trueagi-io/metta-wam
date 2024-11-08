

  maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; maplist(mort,SourceCode)).



/* PLDoc header for `vars_to_dictation/3` predicate */
/**
 * vars_to_dictation(+Pairs, +TIn, -TOut).
 *
 * Converts a list of variable-value pairs into a dictionary structure.
 *
 * @param Pairs List of variable-value pairs (Name=Value format).
 * @param TIn Input dictionary.
 * @param TOut Output dictionary with added variables.
 *
 * @example
 * ?- vars_to_dictation([a=1, b=2], _{}, Dict).
 * Dict = _{a: 1, b: 2}.
 */

% Process the first element, ensuring it's a Name=Value pair
vars_to_dictation([Name=Value|Gotten], TIn, TOut):- !,
  my_assertion(atom(Name)),  % Ensure that Name is an atom
  vars_to_dictation(Gotten, TIn, TMid),  % Recur on the rest of the list
  to_prop_name(Name, UName),  % Convert the property name
  tio_tersify(Value, ValueT),!,  % Simplify the value
  put_dict(UName, TMid, ValueT, TOut).  % Add the variable to the dictionary

% Process cases where the NameValue is not in Name=Value format
vars_to_dictation([NameValue|Gotten], TIn, TOut):- !,
  vars_to_dictation(Gotten, TIn, TMid),  % Recur on the rest
  to_prop_name(NameValue, UName),  % Convert the name
  tio_tersify(NameValue, ValueT),!,  % Simplify the value
  put_dict(UName, TMid, ValueT, TOut).  % Add to the dictionary

% Handle compound NameValue cases (e.g., Name(Value)) by converting to Name=Value pair
vars_to_dictation([NameValue|Gotten], TIn, TOut):- compound(NameValue), compound_name_arguments(NameValue, Name, Value),!,
  vars_to_dictation([Name=Value|Gotten], TIn, TOut).  % Recur as a Name=Value pair

% Base case: when the list is empty, return the accumulated dictionary
vars_to_dictation([], T, T).

/* PLDoc header for `tio_tersify/2` predicate */
/**
 * tio_tersify(+Value, -ValueT).
 *
 * Simplifies a value if it matches specific conditions like being a grid.
 *
 * @param Value Input value.
 * @param ValueT Simplified output value.
 */

% Simplify the value if it is a grid
tio_tersify(Value, ValueT):- is_grid(Value),!, ValueT = _.  % If a grid, set ValueT to anonymous

% Otherwise, leave the value unchanged
tio_tersify(Value, Value).

/* Directive to export `copy_qq_//1` predicate */
:- export(copy_qq_//1).  % Make `copy_qq_//1` available for external modules

/* PLDoc header for `copy_qq_//1` DCG predicate */
/**
 * copy_qq_(+Codes)//
 *
 * A DCG (Definite Clause Grammar) rule to copy a list of codes.
 *
 * @param Codes List of character codes.
 */

% Base case: empty list
copy_qq_([]) --> [].

% Recursive case: copy the first code and then recur on the rest
copy_qq_([C|Cs]) --> [C], copy_qq_(Cs).

/* Directive to export `copy_qq//1` predicate */
:- export(copy_qq//1).  % Export the `copy_qq//1` DCG

/* PLDoc header for `muarc:copy_qq/1` predicate */
/**
 * muarc:copy_qq(+Atom, -Codes)//
 *
 * Converts an atom to a list of character codes using `copy_qq_//1`.
 *
 * @param Atom The input atom.
 * @param Codes The output list of character codes.
 */

muarc:copy_qq(A) --> copy_qq_(Cs), {atom_codes(A, Cs)}.  % Convert atom to list of codes

/* PLDoc header for `to_prop_name/2` predicate */
/**
 * to_prop_name(+Name, -UName).
 *
 * Converts a Name into a unified property name format.
 *
 * @param Name Input name.
 * @param UName Unified output name.
 */

% Case when Name is in Name=Value format, ignore the Value and process the Name
to_prop_name(Name=_, UName):- nonvar(Name),!, to_prop_name(Name, UName).

% If Name is a compound, extract the functor and process it
to_prop_name(Name, UName):- compound(Name), compound_name_arity(Name, F, _),!, to_prop_name(F, UName).

% Default case: convert the name to a series of case breaks, then make it atomic
to_prop_name(Name, UName):- to_case_breaks(Name, Breaks), xtis_to_atomic(Breaks, UName).

/* Additional helper predicates related to name conversion */

/* PLDoc header for `xtis_to_atomic/2` predicate */
/**
 * xtis_to_atomic(+Breaks, -Atomic).
 *
 * Converts case breaks into an atomic name.
 *
 * @param Breaks List of case breaks.
 * @param Atomic Output atomic name.
 */
xtis_to_atomic([xti(Str, upper), xti(StrL, lower)|Breaks], StrO):- 
    string_upper(Str, Str),  % Ensure the first part is upper case
    symbol_chars(Str, CharsList), append(Left, [U], CharsList),
    name(S1, Left), symbolic_list_concat([S1, '_', U, StrL], '', StrUL),!,
    xtis_to_atomic([xti(StrUL, lower)|Breaks], StrO).

% Base case for xtis_to_atomic
xtis_to_atomic([], '').
xtis_to_atomic([xti(Str, _)], Lower):- downcase_atom(Str, Lower).  % Convert to lower case

% Recursive case to concatenate parts
xtis_to_atomic([XTI|Breaks], Atomic):- 
    xtis_to_atomic([XTI], S1), xtis_to_atomic(Breaks, S2),!,
    symbolic_list_concat([S1, S2], '_', Atomic).

/* PLDoc header for `share_vars/2` predicate */
/**
 * share_vars(+Vs, +Pair).
 *
 * Ensures variables in `Vs` are shared with the given `Name=Value` pair.
 *
 * @param Vs List of variable-value pairs.
 * @param Pair A `Name=Value` pair to check for sharing.
 */

% If Name matches a variable in Vs, ensure Value is shared
share_vars(Vs, Name=Value):- 
    member(VName=VValue, Vs), VName == Name,!, 
    (Value = VValue -> true ; trace_or_throw(cant(share_vars(Vs, Name=Value)))). 

% Skip variables starting with `_` (hidden variables)
share_vars(_, Name=_):- string_concat('_', _, Name),!.

% Handle cases where the variable is missing in `Vs`
share_vars(V, Name=Value):- fbug(missing(share_vars(V, Name=Value))),!.


parse_expansions(_,Vs,Vs,Src,Src):- \+ compound(Src),!.
parse_expansions(_,Vs0,Vs,dont_include(Var),nop(dont_include(Var))):-
  dont_include_var(Vs0,Vs,Var),!.
parse_expansions(F, Vs0,Vs,[Src0|Sourcecode0],[Src|Sourcecode]):- !,
  parse_expansions(F, Vs0, Vs1, Src0, Src),
  parse_expansions(F, Vs1, Vs, Sourcecode0, Sourcecode).
parse_expansions(FF, Vs0, Vs, Cmpd0, Cmpd):-
  compound_name_arguments(Cmpd0,F,Args0),
  parse_expansions([F|FF], Vs0, Vs, Args0,Args),
  compound_name_arguments(Cmpd,F,Args).

dont_include_var(Vs0,Vs,Var):- select(_=VV,Vs0,Vs),VV==Var,!.
dont_include_var(Vs,Vs,_).

append_sets(Sets,Set):- append(Sets,List),list_to_set(List,Set).
append_sets(Set1,Set2,Set):- append(Set1,Set2,List),list_to_set(List,Set).
flatten_sets(Sets,Set):- flatten(Sets,List),list_to_set(List,Set).