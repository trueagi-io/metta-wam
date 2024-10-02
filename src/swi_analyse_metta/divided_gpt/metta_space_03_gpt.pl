    dout(space,['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom).
% Remove Atom


'remove-atom'(Environment, AtomDeclaration, Result) :- 
    eval_args(['remove-atom', Environment, AtomDeclaration], Result).

% Add or replace an atom in the space
/** 'replace-atom'(+SpaceNameOrInstance, +Atom, +New) is det.
    Replaces an atom in the given space with a new one.

    @param SpaceNameOrInstance The identifier of the space or instance.
    @param Atom The existing atom to be replaced.
    @param New The new atom to replace the old one.

    @example
    ?- 'replace-atom'('space1', oldAtom, newAtom).
    true.
*/
'replace-atom'(SpaceNameOrInstance, Atom, New) :-
    dout(space,['replace-atom',SpaceNameOrInstance, Atom, New]),
    space_type_method(Type,replace_atom,Method), 
    call(Type,SpaceNameOrInstance),!,  % Check if the type matches
    dout(space,['type-method',Type,Method]), 
    call(Method,SpaceNameOrInstance,Atom, New).

% Replace Atom within an environment
/** 'atom-replace'(+Environment, +OldAtom, +NewAtom, -Result) is det.
    Replaces the OldAtom with NewAtom in the given environment.

    @param Environment The environment where the atom resides.
    @param OldAtom The atom to be replaced.
    @param NewAtom The new atom to insert.
    @param Result The result of the replacement operation.
*/
'atom-replace'(Environment, OldAtom, NewAtom, Result) :- 
    eval_args(['atom-replace', Environment, OldAtom, NewAtom], Result).

% Count the number of atoms in the space
/** 'atom-count'(+SpaceNameOrInstance, -Count) is det.
    Counts the number of atoms in a given space.

    @param SpaceNameOrInstance The identifier of the space or instance.
    @param Count The number of atoms in the space.

    @example
    ?- 'atom-count'('space1', Count).
    Count = 42.
*/
'atom-count'(SpaceNameOrInstance, Count) :-
    dout(space,['atom-count',SpaceNameOrInstance]),  % Log the request
    space_type_method(Type,atom_count,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance,Count),  % Execute the method
    dout(space,['type-method-result',Type,Method,Count]).  % Log the result

% Alternate method for counting atoms in an environment
/** 'atom-count'(+Environment, -Count) is det.
    Evaluates the number of atoms in an environment.

    @param Environment The environment to check.
    @param Count The number of atoms found.
*/
'atom-count'(Environment, Count):- 
    eval_args(['atom-count', Environment], Count).

% Fetch all atoms from the space
/** 'get-atoms'(+SpaceNameOrInstance, -AtomsL) is det.
    Retrieves all atoms from a space and stores them in AtomsL.

    @param SpaceNameOrInstance The identifier of the space.
    @param AtomsL The list of atoms in the space.

    @example
    ?- 'get-atoms'('space1', Atoms).
    Atoms = [atom1, atom2, atom3].
*/
'get-atoms'(SpaceNameOrInstance, AtomsL) :-
    dout(space,['get-atoms',SpaceNameOrInstance]),  % Log the request
    space_type_method(Type,get_atoms,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, AtomsL),  % Fetch the atoms
    true.  % Prevent backtracking

% Alternate method to fetch atoms in an environment
/** 'get-atoms'(+Environment, -Atoms) is det.
    Evaluates and fetches atoms from an environment.

    @param Environment The environment to check.
    @param Atoms The list of atoms in the environment.
*/
'get-atoms'(Environment, Atoms):- 
    eval_args(['get-atoms', Environment], Atoms).

% Iterate through all atoms in a space
/** 'atoms_iter'(+SpaceNameOrInstance, -Iter) is det.
    Creates an iterator for the atoms in a space.

    @param SpaceNameOrInstance The identifier of the space.
    @param Iter The iterator for atoms.

    @example
    ?- 'atoms_iter'('space1', Iter).
    Iter = iterator_handle.
*/
'atoms_iter'(SpaceNameOrInstance, Iter) :-
    dout(space,['atoms_iter',SpaceNameOrInstance]),  % Log the request
    space_type_method(Type,atoms_iter,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, Iter),  % Create the iterator
    dout(space,['type-method-result',Type,Method,Iter]).  % Log the result

% Match atoms in the space to a template
/** 'atoms_match'(+SpaceNameOrInstance, -Atoms, +Template, +Else) is det.
    Matches all atoms in a space to a template, with a fallback option.

    @param SpaceNameOrInstance The identifier of the space.
    @param Atoms The matched atoms.
    @param Template The template to match against.
    @param Else The fallback in case no match is found.
*/
'atoms_match'(SpaceNameOrInstance, Atoms, Template, Else) :-
    space_type_method(Type,atoms_match,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, Atoms, Template, Else),  % Perform the match
    dout(space,['type-method-result',Type,Method,Atoms, Template, Else]).  % Log the result

% Query atoms in a space
/** 'space_query'(+SpaceNameOrInstance, +QueryAtom, -Result) is det.
    Queries atoms in a space to find matching results.

    @param SpaceNameOrInstance The identifier of the space.
    @param QueryAtom The atom to query.
    @param Result The result of the query.
*/
'space_query'(SpaceNameOrInstance, QueryAtom, Result) :-
    space_type_method(Type,query,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, QueryAtom, Result),  % Perform the query
    dout(space,['type-method-result',Type,Method,Result]).  % Log the result

% Substitute a pattern with a template in a space
/** subst_pattern_template(+SpaceNameOrInstance, +Pattern, +Template) is det.
    Substitutes a pattern with a template in the given space.

    @param SpaceNameOrInstance The identifier of the space.
    @param Pattern The pattern to search for.
    @param Template The template to replace the pattern with.

    @example
    ?- subst_pattern_template('space1', patternX, templateY).
    true.
*/
subst_pattern_template(SpaceNameOrInstance, Pattern, Template) :-
    dout(space,[subst_pattern_template,SpaceNameOrInstance, Pattern, Template]),  % Log the operation
    'atoms_match'(SpaceNameOrInstance, Pattern, Template, []).  % Perform the substitution

/*
Previously: space_query_vars(+SpaceNameOrInstance, +Query, -Vars)
The following code was skipped as it calls a space-specific function, likely outside the current context. The predicate fetch_or_create_space may rely on specific implementations.

space_query_vars(SpaceNameOrInstance, Query, Vars) :- is_as_nb_space(SpaceNameOrInstance),!,
    fetch_or_create_space(SpaceNameOrInstance, Space),
    call_metta(Space,Query,Vars).
*/

% Register space assertions
/** was_asserted_space(+Space) is det.
    Registers that a space was asserted.

    @param Space The space that was asserted.
*/
:- dynamic(was_asserted_space/1).

was_asserted_space('&self').  % Self space assertion
was_asserted_space('&stdlib').  % Standard library space assertion