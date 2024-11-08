/**
 * sexpr_s2p_pre_list/4
 * This predicate recursively processes a list, transforming its elements by applying sexpr_s2p.
 * It handles cases where the first element is a list, recursively transforming it if necessary.
 *
 * @param Fn   The function name to apply
 * @param Nth  An index or counter for processing
 * @param [S0|STERM0] The input list of s-expressions
 * @param [S|STERM]   The output list of processed s-expressions
 *
 * @example
 * ?- sexpr_s2p_pre_list(fn, nth, [[a,b],c], Output).
 * Output = [[ProcessedA, ProcessedB], ProcessedC].
 */
sexpr_s2p_pre_list(Fn,Nth,[S0|STERM0],[S|STERM]):-
  % Check if S0 is a list. If so, process it recursively using sexpr_s2p, otherwise apply sexpr_s2p_pre_list.
  (is_list(S0) -> sexpr_s2p(Fn,Nth,S0,S); sexpr_s2p_pre_list(Fn,Nth,S0,S)),
  % Recursively process the rest of the list (STERM0) into STERM.
  sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM), !.
  
% Base case: If there are no more elements to process, the output is the same as the input.
sexpr_s2p_pre_list(_Fn,_,STERM,STERM).

/* previously: There was an attempt to handle a condition for non-list elements,
   but it's now refactored into the is_list condition above. */

/**
 * p2m/1
 * A helper predicate to translate all clauses of a given predicate from Prolog to MeTTa syntax.
 * This version operates on a single predicate indicator (name/arity).
 *
 * @param I   The predicate indicator to translate (name/arity).
 *
 * @example
 * ?- p2m(my_pred).
 * my_pred(a, b) call! my_pred(a, b).
 */
p2m(I):- 
  % Iterate through all clauses of the predicate I/A without repeating.
  forall(no_repeats(current_predicate(I/A)),
    (functor(P,I,A),
     % For each clause, translate the body and print it in MeTTa format.
     forall(clause(P,Body),
       (numbervars(P+Body,0,_,[]),
        write_src(=(P,'call!'(Body))))))).

/**
 * p2m/2
 * Main translation predicate that converts a Prolog term into a MeTTa term.
 * Handles a variety of cases including lists, atomic terms, and Prolog control structures.
 *
 * @param I  Input term in Prolog
 * @param O  Output term in MeTTa
 */
p2m(I,O):- 
  % Call the helper predicate with the initial context of [progn].
  p2m([progn],I,O).

/**
 * p2m/3
 * Translates a Prolog term into a MeTTa term, accounting for different control structures and terms.
 * @param OC   The context in which the translation occurs (e.g., progn, arg)
 * @param NC   The Prolog term being translated
 * @param O    The resulting MeTTa term
 */

% Skip if NC is a variable.
p2m(_OC,NC, NC) :- var(NC), !.  

% Skip if NC is a free-term variable (custom definition).
p2m(_OC,NC, NC) :- is_ftVar(NC), !.

% Translate lists recursively.
p2m(OC,[H|T],'::'(L)):- 
  is_list([H|T]),
  % Map each element of the list recursively.
  maplist(p2m(OC),[H|T],L).

% Convert Prolog lists into 'Cons' notation in MeTTa.
p2m(OC,[H|T], 'Cons'(OH,OT)):- 
  p2m(OC,H, OH), 
  p2m(OC,T, OT).

% Handle atomic terms.
p2m(_OC,A, A):- string(A), !.
p2m(_OC,[], 'Nil').  % Empty list becomes 'Nil' in MeTTa.
p2m(_OC,'[|]','Cons').  % Translate Prolog's '[|]' notation to MeTTa's 'Cons'.
p2m(_OC,!, '!').  % Direct translation for Prolog's cut (!).
p2m(_OC,false, 'False').  % False becomes 'False'.
p2m(_OC,true, 'True').  % True becomes 'True'.

% Handle cases where the input is an atom.
p2m([progn|_],Atom,[O]):- 
  atom(Atom),!, 
  p2m([arg],Atom,O),!.

% Translate conjunctions and disjunctions.
p2m(_OC,( ';' ),'xor').  % Disjunction becomes 'xor'.
p2m(_OC,( ',' ),'and2').  % Conjunction becomes 'and2'.

/* previously: ',' was mapped to 'and', but it was adjusted to 'and2' to fit a new structure. 
   ',' and ';' were skipped earlier due to lacking context-specific translations. */

/* Dead code explanation: The following lines were skipped as they represent an alternate way 
   of translating Prolog control structures that were handled by different MeTTa primitives.
   They remain here for reference. 
   %p2m(_OC,( ',' ),and).
   %p2m(_OC,( '\\+' ),unless).
   %p2m(_OC,( ':-' ),entailed_by).
 */

% Additional atomic translations
p2m(_OC,'atom','is-symbol').  % Prolog's atom check becomes 'is-symbol'.
p2m(_OC,'atomic','symbolic').  % Atomic check becomes 'symbolic'.

% Handle scoped operators.
p2m(_OC,M:I, 'scoped'(N,O)):-  
  p2m(OC,M,N),
  p2m(I,O).

% Negation as failure is translated into a MeTTa 'naf' (negation as failure).
p2m(_OC,(\+ A), O):- !, p2m(_OC,naf(A), O).

/**
 * conjuncts_to_list/2
 * Converts a conjunction of terms (G, E) into a list of terms.
 *
 * @param (G, E)   A conjunction of goals
 * @param List     Output list of goals
 *
 * @example
 * ?- conjuncts_to_list((a, b), List).
 * List = [a, b].
 */
p2m(OC,(G,E),O):-  
  % Convert conjunction into a list of goals.
  conjuncts_to_list((G,E),List),!,
  % Sequentially translate the list of goals.
  into_sequential(OC,List,O),!.