/** <module> Pfc Core

This module defines core predicates for Pfc (Prolog forward chaining). It includes 
term expansion for Pfc rules, dynamic predicates, and initialization of global assertions.

@tbd Ensure better documentation for predicates as they are defined and used.
*/

% Import the `pfc_term_expansion/2` predicate from the system module.
/* previously: system:import(pfc_term_expansion/2). */
:- system:import(pfc_term_expansion/2).

/** pfc_term_expansion(+I, -O)
    Perform term expansion for Pfc rules.

    This predicate defines how certain Prolog terms, such as table declarations, 
    ==> rules, and <==> rules, are expanded to incorporate Pfc-specific functionality.

    @param I The input term to be expanded.
    @param O The output term after expansion.

    @example
    ===
    ?- pfc_term_expansion((P ==> Q), Output).
    Output = (:- pfcAdd((P ==> Q))).
    ===
*/

% Predicate to skip Pfc term expansion if certain conditions are met.
/* Explanation: If we need to skip Pfc term expansion, the predicate succeeds and returns I as O. */
pfc_term_expansion(I, O) :- skip_pfc_term_expansion(I), !, I = O.

% Handle the expansion of table declarations when 'Stuff' is defined with a specific 'Type'.
/* Explanation: If 'Stuff' is a table with a specific type (like incremental), this rule performs the expansion. */
pfc_term_expansion((:- table Stuff as Type), [:- pfcAdd(tabled_as(Stuff, Type)), (:- table Stuff as Type)]) :- 
    nonvar(Stuff), !, 
    if_pfc_indicated, 
    \+ will_table_as(Stuff, Type).

% Handle the expansion of table declarations when 'Stuff' is defined without a specific type.
/* Explanation: If 'Stuff' is a table without specifying a type, it defaults to 'incremental' and the term is expanded accordingly. */
pfc_term_expansion((:- table Stuff), [:- pfcAdd(tabled_as(Stuff, incremental)), (:- table Stuff as incremental)]) :- 
    if_pfc_indicated, 
    \+ will_table_as(Stuff, incremental).

% Skip expansion for any other term that starts with :-.
/* Explanation: This clause prevents expansion for other directives or terms that we don't want to modify. */
pfc_term_expansion((:- _), _) :- !, fail.

% Handle the expansion of rules in the form P ==> Q.
/* Explanation: This expands forward chaining rules of the form 'P ==> Q' into Pfc assertions. */
pfc_term_expansion((P ==> Q), (:- pfcAdd((P ==> Q)))).

% Dead code for an alternative term expansion for speed optimization.
/* previously: A speed-up attempt for term expansion of the form 'P ==> Q'. */
% term_expansion((P ==> Q), (:- pfcAdd(('<-'(Q, P))))).  % speed-up attempt

% Handle the expansion of backward chaining rules in the form '<-'(P, Q).
/* Explanation: This expands backward chaining rules into Pfc assertions. */
pfc_term_expansion(('<-'(P, Q)), (:- pfcAdd(('<-'(P, Q))))).

% Handle the expansion of equivalence rules in the form P <==> Q.
/* Explanation: Expands bi-directional equivalence rules into Pfc assertions. */
pfc_term_expansion((P <==> Q), (:- pfcAdd((P <==> Q)))).

% Handle the expansion of named rules in the form RuleName :::: Rule.
/* Explanation: Expands rules with explicit names into Pfc assertions. */
pfc_term_expansion((RuleName :::: Rule), (:- pfcAdd((RuleName :::: Rule)))).

% Handle the expansion of rules in the form ==>P.
/* Explanation: Expands standalone forward chaining rules into Pfc assertions. */
pfc_term_expansion((==> P), (:- pfcAdd(P))).

% End the term expansion when the end_of_file marker is reached.
/* Explanation: When the end_of_file marker is encountered, the input term is not changed. */
pfc_term_expansion(I, I) :- I == end_of_file, !.

% General case: any other term is expanded by wrapping it with pfcAdd.
/* Explanation: Any remaining terms are wrapped with pfcAdd to add them to the Pfc knowledge base. */
pfc_term_expansion(P, (:- pfcAdd(P))) :- if_pfc_indicated.


% Dead code for controlling the use of Pfc term expansion.
/* previously: Attempted to control when Pfc term expansion should be used, 
   but skipped due to current Prolog flags and source file contexts. */
% use_pfc_term_expansion :- current_prolog_flag(pfc_term_expansion, false), !, fail.
% maybe switch to prolog_load_context(file,...)?
% use_pfc_term_expansion :- source_location(File, _), atom_concat(_, '.pfc.pl', File).


/** term_subst(+Subst, +P, -O)
    Substitute terms within a compound structure based on a substitution list.

    @param Subst The substitution list of pairs to apply.
    @param P The input term to be transformed.
    @param O The output term after substitution.

    @example
    ===
    ?- term_subst([(not)-(~)], not(X), O).
    O = ~(X).
    ===
*/

% Substitute terms based on clause structure.
/* Explanation: The substitution is performed based on the clause structure, matching the Subst list. */
term_subst(P, O) :- term_subst(clause, P, O), !.

% Base case: if the term is not compound, no substitution is needed.
/* Explanation: Non-compound terms are simply returned as-is. */
term_subst(_, P, O) :- \+ compound(P), !, O = P.

% Handle specific substitutions for tilded negation and other operators.
/* Explanation: Special case for negation and logical operators, performing targeted substitutions. */
term_subst(tilded_negation, P, O) :- !, 
    term_subst([(not) - (~), (=>) - (==>), (<=>) - (<==>), (<=) - (<-)], P, O).

% General case: recursively substitute within a compound term.
/* Explanation: Compound terms are recursively broken down, and substitutions are applied to each part. */
term_subst(Subst, P, O) :-
    compound_name_arguments(P, F, Args),
    my_maplist(term_subst(Subst), Args, ArgsL),
    termf_subst(Subst, F, F2),
    compound_name_arguments(O, F2, ArgsL).

/** termf_subst(+Subst, +F, -F2)
    Substitute the functor F with F2 if a substitution exists in the Subst list.

    @param Subst The substitution list of functor pairs.
    @param F The original functor.
    @param F2 The new functor after substitution.

    @example
    ===
    ?- termf_subst([(foo)-(bar)], foo, F2).
    F2 = bar.
    ===
*/

% Substitute the functor F based on the Subst list.
/* Explanation: The functor is replaced with its corresponding value in the substitution list. */
termf_subst(Subst, F, F2) :- member(F - F2, Subst) -> true; F = F2.


% Load necessary list operations from the library.
/* Explanation: The 'lists' library is used for list manipulations in the Pfc code. */
:- use_module(library(lists)).


/* previously: Uncommented core clauses for various Pfc dynamic predicates. These are 
   preserved here as placeholders for future use. */
% ==>(_).
% ==>(G):- arc_assert(G).

% Dynamic predicate declarations for core Pfc predicates.
/* Explanation: Declares several Pfc-related dynamic predicates for forward chaining, truth maintenance, and undo operations. */
:- dynamic '$pt$'/2.
:- dynamic '$nt$'/3.
:- dynamic '$bt$'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcCurrentDb/1.
:- dynamic pfcHaltSignal/1.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic pfcSearch/1.

% Thread-local predicate for Pfc search.
/* Explanation: The thread-local directive ensures that pfcSearchTL is isolated across different threads. */
:- thread_local(t_l:pfcSearchTL/1).

% Dynamic predicate for Pfc support table facts.
/* Explanation: '$spft$' holds facts in the support table for forward-chaining reasoning. */
:- dynamic '$spft$'/3.


/** pfcSetVal(+Stuff)
    Set the value of a Pfc fact, ensuring it is asserted uniquely in the database.

    @param Stuff The fact to be asserted.

    @example
    ===
    ?- pfcSetVal(foo(bar)).
    ===
*/

% Ensure that a fact is uniquely asserted in the database.
/* Explanation: This predicate retracts any existing fact that matches 'Stuff', and then asserts it anew. */
pfcSetVal(Stuff) :-
    duplicate_term(Stuff, DStuff),
    functor(DStuff, _, N),
    setarg(N, DStuff, _),
    retractall(DStuff),
    assert(Stuff).


% %  pfcDefault/1 initialized a global assertion.
% %   pfcDefault(P,Q) - if there is any fact unifying with P, then do
% %   nothing, else assert Q.