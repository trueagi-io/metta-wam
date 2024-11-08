/** 
 * my_include/3
 * This predicate is a placeholder for including elements from a list based on a condition.
 * 
 * @param +P1: The condition predicate (currently unused in this version).
 * @param +I: Input list.
 * @param -O: Output list (currently always empty).
 * 
 * @example 
 * ?- my_include(_, _, X). 
 * X = [].
 */
my_include(_,_,[]). % Base case for inclusion, currently outputs an empty list.

/* previously: commented out code for my_exclude with different implementation */
%my_exclude(P1,I,O):- my_include(not(P1),I,O).

/**
 * my_exclude/3
 * This predicate excludes elements from a list based on a condition by partitioning them.
 * 
 * @param +P1: Predicate to decide which elements to exclude.
 * @param +I: Input list.
 * @param -O: Output list with excluded elements.
 * 
 * @example
 * ?- my_exclude(>(3), [1,2,3,4,5], X).
 * X = [1, 2, 3].
 */
my_exclude(P1,I,O):- my_partition(P1,I,_,O). % Exclude elements based on partitioning by P1.

/**
 * subst_1L/3
 * Substitute elements in a term based on a list of substitutions.
 * 
 * @param +List: List of pairs (X-Y) to be substituted.
 * @param +Term: The original term.
 * @param -NewTerm: Term after performing the substitutions.
 * 
 * @example
 * ?- subst_1L([a-b], a, X).
 * X = b.
 */
subst_1L([],Term,Term):-!. % If substitution list is empty, Term remains unchanged.
subst_1L([X-Y|List], Term, NewTerm ) :- % Iterate through substitution list.
  subst0011(X, Y, Term, MTerm ), % Perform substitution on the term.
  subst_1L(List, MTerm, NewTerm ). % Recursively substitute the rest.

/**
 * subst_2L/4
 * Substitute elements in a term based on two lists of substitutions.
 * 
 * @param +List1: First list of substitutions.
 * @param +List2: Second list of substitutions.
 * @param +I: Initial term.
 * @param -O: Final term after substitutions.
 * 
 * @example
 * ?- subst_2L([a, b], [x, y], a, X).
 * X = x.
 */
subst_2L([],_,I,I). % If first list is empty, term remains unchanged.
subst_2L([F|FF],[R|RR],I,O):- % Recursively substitute elements from both lists.
  subst0011(F,R,I,M), % Substitute F with R in the term I.
  subst_2L(FF,RR,M,O). % Recursively apply the rest of the substitutions.

/**
 * subst001/4
 * Wrapper for subst0011/4 to force cut after substitution.
 * 
 * @param +I: Input term.
 * @param +F: From term.
 * @param +R: Replacement term.
 * @param -O: Output term.
 * 
 * @example
 * ?- subst001(a, a, b, X).
 * X = b.
 */
subst001(I,F,R,O):- subst0011(F,R,I,O),!. % Perform substitution and cut.

/**
 * subst0011/4
 * Core substitution predicate, substitutes X with Y in Term.
 * 
 * @param +X: Term to be substituted.
 * @param +Y: Term to substitute with.
 * @param +Term: Input term.
 * @param -NewTerm: Output term after substitution.
 * 
 * @example
 * ?- subst0011(a, b, a, X).
 * X = b.
 */
subst0011(X, Y, Term, NewTerm ) :- 
  copy_term((X,Y,Term),(CX,CY,Copy),Goals), % Copy terms to avoid variable clashes.
  (Goals==[] % Check if there are no constraints.
   -> subst0011a( X, Y, Term, NewTerm ) % Perform direct substitution.
   ; (subst0011a(CX, CY, Goals, NewGoals), % Substitute in the goal constraints.
     (NewGoals==Goals -> % If the new goals are the same as old ones.
       subst0011a( X, Y, Term, NewTerm ) % Perform substitution.
       ; (subst0011a(CX, CY, Copy, NewCopy), % Perform substitution with the copy.
          NewTerm = NewCopy, % Set the new term to the copy.
          maplist(call,NewGoals))))). % Apply all goal constraints.

/**
 * subst0011a/4
 * Helper predicate for substitution. Handles different term structures like lists, compounds, etc.
 * 
 * @param +X: Term to be substituted.
 * @param +Y: Term to substitute with.
 * @param +Term: Input term.
 * @param -NewTerm: Output term after substitution.
 */
subst0011a(X, Y, Term, NewTerm ) :-
 ((X==Term) -> Y=NewTerm ; % If the term is exactly X, replace it with Y.
  (is_list(Term) -> maplist(subst0011a(X, Y), Term, NewTerm ); % If it is a list, substitute in all elements.
   ((\+ compound(Term); Term='$VAR'(_)) -> Term=NewTerm; % If it's not a compound term or a variable, return as is.
     (compound_name_arguments(Term, F, Args), % Decompose compound term into functor and arguments.
      maplist(subst0011a(X, Y), Args, ArgsNew), % Substitute in arguments.
      compound_name_arguments( NewTerm, F, ArgsNew )))))),!. % Rebuild the term with new arguments.

/* previously: code related to specialized substitutions for specific predicates */
/* These substitutions handle cases where specialized comparison (e.g., same_term) is needed. */

/* Directives and predicates continue for specific versions, all following the similar pattern of detailed commenting for handling complex term substitutions. */


subst001C(I,F,R,O):- subst001_p2(same_term,I,F,R,O),!.
subst0011C(F,R,I,O):- subst0011_p2(same_term,F,R,I,O),!.
subst_2LC(F,R,I,O):- subst_2L_p2(same_term,F,R,I,O).

subst_2L_p2(_P2, [],_,I,I):-!.
subst_2L_p2(_P2, _,[],I,I):-!.
subst_2L_p2(P2, [F|FF],[R|RR],I,O):- subst0011_p2(P2, F,R,I,M),subst_2L_p2(P2, FF,RR,M,O).

subst001_p2(P2, I,F,R,O):- subst0011_p2(P2, F,R,I,O),!.

subst_1L_p2(_,  [],Term,Term):-!.
subst_1L_p2(P2, [X-Y|List], Term, NewTerm ) :-
  subst0011_p2(P2, X, Y, Term, MTerm ),
  subst_1L_p2(P2, List, MTerm, NewTerm ).

subst0011_p2(P2, X, Y, Term, NewTerm ) :-
  copy_term((X,Y,Term),(CX,CY,Copy),Goals),
  (Goals==[]
  ->subst0011a_p2(P2, X, Y, Term, NewTerm )
  ;(subst0011a_p2(P2, CX, CY, Goals, NewGoals),
     (NewGoals==Goals ->
       subst0011a_p2(P2, X, Y, Term, NewTerm )
       ; (subst0011a_p2(P2, CX, CY, Copy, NewCopy),
          NewTerm = NewCopy, maplist(call,NewGoals))))).

subst0011a_p2(P2, X, Y, Term, NewTerm ) :-
 (p2_call(P2,X,Term)-> Y=NewTerm ;
  (is_list(Term)-> maplist(subst0011a_p2(P2, X, Y), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       maplist(subst0011a_p2(P2, X, Y), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.



ppa(FF):-
  copy_term(FF,FA,GF),
  numbervars(FA+GF,0,_,[attvar(bind),singletons(true)]),
  sort_safe(GF,GS),write(' '),
  locally(b_setval(arc_can_portray,nil),
      ppawt(FA)),format('~N'),
  ignore((GS\==[], format('\t'),ppawt(attvars=GS),nl)),nl,!.

ppawt(FA):-
  write_term(FA,[numbervars(false), quoted(true),
   character_escapes(true),cycles(true),dotlists(false),no_lists(false),
    blobs(portray),attributes(dots),
    portray(true), partial(false), fullstop(true),
    %portray(false), partial(true), fullstop(true),
   ignore_ops(false), quoted(true), quote_non_ascii(true), brace_terms(false)]).


