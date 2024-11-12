/** <p2m/3> 
    Translates Prolog terms into their equivalent MeTTa syntax.
    @param _OC Context (Operational Context).
    @param Head:-Body The Prolog term to be converted.
    @param O The output term in MeTTa syntax.
    @example
    ?- p2m([], (is(A, B)), O).
    O = eval(B, A).
*/
% Case when the body is 'true', translate to 'True' in MeTTa
p2m(_OC, (Head:-Body), O):- Body == true, !, O = (=(Head, 'True')).

% Case when the body is 'fail', translate to 'empty' in MeTTa
p2m(_OC, (Head:-Body), O):- Body == fail, !, O = (=(Head, [empty])).

% For other cases, decompose the body into conjuncts and apply p2m recursively
p2m(OC, (Head:-Body), O):-
    % Recursively apply p2m to the head of the clause
    p2m(Head, H),
    % Convert conjunctions to a list of body terms
    conjuncts_to_list(Body, List),
    % Apply p2m to each conjunct
    maplist(p2m([progn|OC]), List, SP), 
    !,
    % The output is a MeTTa representation of the clause
    O = ['=', H | SP].

/** <p2m/3> 
    Handle the case where only the body is provided (a directive).
    @param OC Context.
    @param :-Body Directive in Prolog.
    @param O The output in MeTTa.
*/
% Convert Prolog directive into sequential MeTTa code
p2m(OC, (:-Body), O):- !,
    % Convert the body into a list of conjuncts
    conjuncts_to_list(Body, List),
    % Sequentially process the body in the current context
    into_sequential([progn|OC], List, SP), 
    !,
    % Output the MeTTa exec structure
    O = exec(SP).

/** <p2m/3>
    Handle the case of a query in Prolog (?-) and convert to MeTTa.
    @param OC Context.
    @param ?-Body Query in Prolog.
    @param O Output in MeTTa.
*/
% Convert Prolog queries into sequential MeTTa code
p2m(OC, ( ?- Body), O):- !,
    conjuncts_to_list(Body, List),
    into_sequential([progn|OC], List, SP), 
    !,
    % Output the MeTTa exec structure with a '?-' prefix
    O = exec('?-'(SP)).

/* previously: Handled Prolog clauses by converting the body into a list and applying p2m */
%p2m(_OC,(Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(OC,List,SP),!,O=(=(Head,SP)).

/** <p2m/3>
    Conversion for if-then-else constructs in Prolog.
    @param OC Context.
    @param (A->B;C) If-then-else construct in Prolog.
    @param O Output in MeTTa.
*/
% Convert Prolog if-then-else to MeTTa's det_if_then_else construct
p2m(OC, (A->B;C), O):- !, p2m(OC, det_if_then_else(A, B, C), O).

% Handle Prolog disjunction (A;B) by converting to MeTTa's 'or'
p2m(OC, (A;B), O):- !, p2m(OC, or(A, B), O).

% Convert Prolog soft-cut (A*->B;C) into MeTTa's 'if'
p2m(OC, (A*->B;C), O):- !, p2m(OC, if(A, B, C), O).

% Convert Prolog if-then (A->B) into MeTTa's det_if_then
p2m(OC, (A->B), O):- !, p2m(OC, det_if_then(A, B), O).

% Convert soft cut without else (A*->B) into MeTTa's 'if'
p2m(OC, (A*->B), O):- !, p2m(OC, if(A, B), O).

/** <p2m/3>
    Convert MeTTa specific definitions and constructs like metta_defn, metta_type, etc.
    @param _OC Context.
    @param metta_defn/4 Define a MeTTa atom or definition.
    @param O Output in MeTTa.
*/
p2m(_OC, metta_defn(Eq, Self, H, B), 'add-atom'(Self, [Eq, H, B])).

% Handle MeTTa type retrieval
p2m(_OC, metta_type, 'get-type').

% Handle atom retrieval in MeTTa
p2m(_OC, metta_atom, 'get-atoms').

/* previously: was handling clause in MeTTa */
%p2m(_OC,get_metta_atom,'get-atoms').

% Convert a Prolog clause into an atom retrieval operation in MeTTa
p2m(_OC, clause(H, B), ==([=, H, B], 'get-atoms'('&self'))).

% Convert Prolog assert, assertz, and asserta to MeTTa's add-atom
p2m(_OC, assert(X), 'add-atom'('&self', X)).
p2m(_OC, assertz(X), 'add-atom'('&self', X)).
p2m(_OC, asserta(X), 'add-atom'('&self', X)).

/** <p2m/3>
    Handle Prolog retract operations.
    @param _OC Context.
    @param retract(X) Remove an atom in MeTTa.
    @param O Output in MeTTa.
*/
% Convert Prolog retract into MeTTa's remove-atom
p2m(_OC, retract(X), 'remove-atom'('&self', X)).

% Convert retractall in Prolog to MeTTa's remove-all-atoms
p2m(_OC, retractall(X), 'remove-all-atoms'('&self', X)).

/* previously: A general catch-all case was used to decompose compound terms */
%p2m(_OC,I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=..OO.

/** <p2m/3>
    Catch-all clause to handle general compound terms in Prolog.
    @param OC Context.
    @param I Input term in Prolog.
    @param O Output term in MeTTa.
*/
% Decompose compound terms and apply p2m recursively
p2m(OC, I, O):-
    % Ensure it's a compound term
    compound(I),
    % Break the compound term into its functor and arguments
    I =.. [F | II], 
    % Recursively apply p2m on each argument
    maplist(p2m([F | OC]), II, OO), 
    % Convert functor F into its MeTTa equivalent
    into_hyphens(F, FF),
    % Construct the output with the converted functor and arguments
    O = [FF | OO].

/** <prolog_to_metta/2>
    Converts a Prolog term to a MeTTa term.
    @param V Prolog term.
    @param D Translated MeTTa term.
    @example
    ?- prolog_to_metta((A->B;C), O).
    O = det_if_then_else(A, B, C).
*/
prolog_to_metta(V, D) :-
    % Call p2m to perform the conversion and ensure it's deterministic
    p2m([progn], V, D), !.

/** <into_sequential/3>
    Converts a list of conjunctions into sequential MeTTa commands.
    @param OC Context.
    @param Body Body of the Prolog clause or goal.
    @param SP Sequential MeTTa commands.
*/
% Process Prolog conjunctions and convert them into a MeTTa sequential form
into_sequential(OC, Body, SP) :-
    % Ensure Body is not already a list
    \+ is_list(Body),
    % Break conjunctions into a list of terms
    conjuncts_to_list(Body, List),
    