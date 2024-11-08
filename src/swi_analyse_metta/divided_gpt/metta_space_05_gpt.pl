arg(1, Space, Atoms),
% Get the length of the Atoms list.
length(Atoms, Count).

/* 
    PLDoc header for removing an atom from a space.
    @param SpaceNameOrInstance The name or instance of the space
    @param Atom The atom to be removed from the space
    @example 
      ?- remove_nb_atom(my_space, a).
      true.
*/
% Remove an atom from a space
remove_nb_atom(SpaceNameOrInstance, Atom) :-
    % Fetch or create a space based on SpaceNameOrInstance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the atoms list from the space
    arg(1, Space, Atoms),
    % Select and remove the Atom from the list
    select(Atom, Atoms, UpdatedAtoms),
    % Update the space with the modified list of atoms
    nb_setarg(1, Space, UpdatedAtoms).

/* 
    PLDoc header for fetching all atoms from a space.
    @param SpaceNameOrInstance The name or instance of the space
    @param Atoms The list of atoms found in the space
    @example 
      ?- get_nb_atoms(my_space, Atoms).
      Atoms = [a, b, c].
*/
% Fetch all atoms from a space
get_nb_atoms(SpaceNameOrInstance, Atoms) :-
    % Fetch or create a space based on SpaceNameOrInstance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the atoms from the space
    arg(1, Space, Atoms).

/* 
    PLDoc header for replacing an atom in a space.
    @param SpaceNameOrInstance The name or instance of the space
    @param OldAtom The atom to be replaced
    @param NewAtom The new atom to insert
    @example 
      ?- replace_nb_atom(my_space, a, x).
      true.
*/
% Replace an atom in the space
replace_nb_atom(SpaceNameOrInstance, OldAtom, NewAtom) :-
    % Fetch or create a space based on SpaceNameOrInstance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the atoms list from the space
    arg(1, Space, Atoms),
    % Check if the OldAtom matches any in the space
    ( (select(Found, Atoms, TempAtoms),OldAtom=@=Found)
    % If found, replace OldAtom with NewAtom
    ->  NewAtoms = [NewAtom | TempAtoms],
        % Update the space with the new list of atoms
        nb_setarg(1, Space, NewAtoms)
    ;   false  % If OldAtom is not found, fail the operation
    ).

/* 
    PLDoc header for validating if a term is a space.
    @param Space The term to check
    @example 
      ?- is_valid_nb_space(my_space).
      true.
*/
% Confirm if a term represents a space by checking its structure
is_valid_nb_space(Space):- compound(Space),functor(Space,'Space',_).

/* 
    PLDoc header for finding the original name of a space.
    @param Space The space instance
    @param Name The name of the space
    @example 
      ?- space_original_name(SpaceInstance, Name).
      Name = my_space.
*/
% Find the original name of a given space
space_original_name(Space, Name) :-
    % Check if the name is a registered space name
    is_registered_space_name(Name),
    % Fetch the space instance for the given name
    nb_current(Name, Space).

/* 
    PLDoc header for initializing a new space.
    @param Name The name of the space to initialize
    @example 
      ?- init_space(my_new_space).
      true.
*/
% Register and initialize a new space
init_space(Name) :-
    % Create a new space instance with an empty list of atoms
    Space = 'Space'([]),
    % Register the name as a valid space name
    asserta(is_registered_space_name(Name)),
    % Set the space instance in a non-backtrackable store
    nb_setval(Name, Space).

/* 
    PLDoc header for fetching or creating a space.
    @param NameOrInstance The name or instance of the space
    @param Space The space instance fetched or created
    @example 
      ?- fetch_or_create_space(my_space).
      true.
*/

fetch_or_create_space(Name):- fetch_or_create_space(Name,_).
% Fetch an existing space or create a new one
fetch_or_create_space(NameOrInstance, Space) :-
    (   atom(NameOrInstance)
    ->  (is_registered_space_name(NameOrInstance)
        ->  nb_current(NameOrInstance, Space)
        ;   init_space(NameOrInstance),
            nb_current(NameOrInstance, Space))
    ;   is_valid_nb_space(NameOrInstance)
    ->  Space = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_space(Space).





% Match Pattern in Space and produce Template
'match'(Space, Pattern, Template) :-
    % Fetch the atoms from the space
    'get-atoms'(Space, Atoms),
    % Match the pattern within the atoms and generate the template
    'match-pattern'(Atoms, Pattern, Template).

/* previously: had a more complex pattern matcher but simplified */

% Simple pattern match
'match-pattern'([], _, []).  % Base case: empty list, no match.
'match-pattern'([H |_T], H, H) :- !.  % Match head of the list with the pattern.
'match-pattern'([_H| T], Pattern, Template) :- 
    % Recursively match the pattern in the rest of the list
    'match-pattern'(T, Pattern, Template).

% previously: commented out as this may relate to Python interface (not needed in current use case)
/*
% is_python_space(X):- python_object(X).
*/

% previously: alternative space handling method using Python, not needed in this context

% Ensure space using Python method with a catch for failure.
ensure_space(X,Y):- catch(ensure_space_py(X,Y),_,fail),!.
% Default case: always fail if the above doesn't work
ensure_space(_N,_V):- fail.

/* 
    File directive for handling debugging and output related clauses.
*/

/* PLDoc headers and skipped blocks explained above for predicates. */





% ===============================
% Clause Database interface
% ===============================
%dout(space,Call):- skip(Call).
if_metta_debug(Goal):- getenv('VSPACE_VERBOSE','2'),!,ignore(call(Goal)).
if_metta_debug(_):-!.
if_metta_debug(Goal):- !,ignore(call(Goal)).
dout(_,_):-!.
dout(W,Term):- notrace(if_metta_debug((format('~N; ~w ~@~n',[W,write_src(Term)])))).

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_asserted_space,new_space,init_space).
space_type_method(is_asserted_space,clear_space,clear_nb_atoms).
space_type_method(is_asserted_space,add_atom,metta_assertdb_add).
space_type_method(is_asserted_space,remove_atom,metta_assertdb_rem).
space_type_method(is_asserted_space,replace_atom,metta_assertdb_replace).

