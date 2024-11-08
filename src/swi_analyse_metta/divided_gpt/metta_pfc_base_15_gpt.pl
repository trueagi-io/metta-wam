% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf1(+P/Cond, -NF) is det.
% %
% % This predicate normalizes a literal `P` with a condition `Cond`.
% % It handles different types of literals such as negated literals,
% % conjunctions, disjunctions, and atomic literals.
% %
% % @example 
% % ?- pfc_nf1((a,b), NF).
% % NF = [a, b].
% %
% % The first clause checks if the literal `P` is negated and converts it 
% % into a normalized form.
% %
pfc_nf1(P/Cond, [( \+P )/Cond]) :- 
    pfcNegatedLiteral(P), % Check if P is a negated literal.
    !. % Cut to prevent backtracking if this clause succeeds.

% % The second clause handles cases where `P` is a non-negated literal 
% % and keeps it in its original form.
pfc_nf1(P/Cond, [P/Cond]) :-  
    pfcLiteral(P),  % Check if P is a literal (non-negated).
    !.

% % The next clause handles the negation of a term by calling pfc_unnegate/2
% % to get the non-negated version and recursively normalizing that.
pfc_nf1(NegTerm, NF) :- 
    pfc_unnegate(NegTerm, Term), % Unnegate NegTerm to obtain Term.
    !, 
    pfc_nf1_negation(Term, NF). % Normalize the negation of the term.

% % Clause for handling disjunction (P ; Q). It recursively normalizes both
% % parts and returns the normalized form of either part.
pfc_nf1((P;Q), NF) :- 
    !, 
    (pfc_nf1(P, NF) ; pfc_nf1(Q, NF)).

% % Clause for handling conjunction (P, Q). Both parts of the conjunction 
% % are normalized separately and then combined using append/3.
pfc_nf1((P, Q), NF) :- 
    !, 
    pfc_nf1(P, NF1), % Normalize first part.
    pfc_nf1(Q, NF2), % Normalize second part.
    append(NF1, NF2, NF). % Combine both normalized parts.

% % This clause handles random atomic literals that are not negated or combined
% % with other terms. It simply returns the literal in a list.
pfc_nf1(P, [P]) :- 
    pfcLiteral(P), % Check if P is a literal.
    !.

% % If none of the above clauses succeed, the predicate issues a warning 
% % indicating that the term cannot be normalized, but still accepts it.
pfc_nf1(Term, [Term]) :- 
    pfcWarn("pfc_nf doesn''t know how to normalize ~p (accepting though)", [Term]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf1_negation(+P, -NF) is det.
% %
% % This predicate normalizes the negation of a term `P`. It handles negation
% % for conjunctions, disjunctions, and atomic terms.
% %
pfc_nf1_negation((P/Cond), [( \+(P) )/Cond]) :- 
    !. % Handle negation of a conditional term.

% % This clause handles the negation of a disjunction (P ; Q). 
% % It recursively negates both parts and appends the results.
pfc_nf1_negation((P;Q), NF) :- 
    !, 
    pfc_nf1_negation(P, NFp), % Negate first part.
    pfc_nf1_negation(Q, NFq), % Negate second part.
    append(NFp, NFq, NF).

% % This clause handles negation of a conjunction (P, Q). 
% % It was noted to be incorrect, and the second part is skipped to prevent issues.
% % The alternative is to normalize both parts and combine the results.
pfc_nf1_negation((P, Q), NF) :- 
    /* previously: this code is not correct! twf. */ 
    !, 
    pfc_nf1_negation(P, NF) % Attempt to negate the first part.
    ; 
    (pfc_nf1(P, Pnf), % Normalize the first part.
     pfc_nf1_negation(Q, Qnf), % Negate the second part.
     append(Pnf, Qnf, NF)). % Combine results.

% % Default clause for negating a single term P.
pfc_nf1_negation(P, [\+P]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf_negations(+List2, -List) is det.
% %
% % This predicate normalizes all negations in a list. It converts negations
% % from the form ~{...} to {\+...}.
% % It is unclear if this is still needed.
% % 
% % @example
% % ?- pfc_nf_negations([a, ~(b)], NF).
% % NF = [a, {\+ b}].
% %
pfc_nf_negations(X, X) :- 
    !. % If X is already normalized, return it as is.

% % Base case for recursion. An empty list is normalized to an empty list.
pfc_nf_negations([], []).

% % Recursive case for lists. The head of the list is normalized, and the
% % normalization continues for the tail.
pfc_nf_negations([H1|T1], [H2|T2]) :- 
    pfc_nf_negation(H1, H2), % Normalize the head.
    pfc_nf_negations(T1, T2). % Recursively normalize the tail.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf_negation(+Form, -NegForm) is det.
% %
% % This predicate handles various forms of negations. It checks for
% % specific patterns of negation (e.g., tilded negations) and converts them 
% % into the standard form {\+ X}.
% %
pfc_nf_negation(Form, {\+ X}) :- 
    nonvar(Form), 
    Form = (~({X})), % Handle tilded negation.
    !.

pfc_nf_negation(Form, {\+ X}) :- 
    tilded_negation, % Check for a tilded negation.
    nonvar(Form), 
    Form = (-({X})), % Handle negation using -.
    !.

pfc_nf_negation(Form, {\+ X}) :- 
    tilded_negation, 
    nonvar(Form), 
    Form = ( \+ ({X})), % Handle negation using \+.
    !.

% % Default case: if no specific negation form is found, return the form unchanged.
pfc_nf_negation(X, X).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % constrain_meta(+Lhs, ?Guard) is semidet.
% %
% % This predicate generates a constraint guard for a given LHS (Left-Hand Side)
% % of a rule. It handles positive facts, negations, and various forms of chaining.
% % The flag `constrain_meta` can be disabled to turn off this feature.
% %
% % To disable:
% % ?- set_prolog_flag(constrain_meta,false).
% %
constrain_meta(_, _) :- 
    current_prolog_flag(constrain_meta, false), % Check if the flag is set to false.
    !, 
    fail.

% % Fact case: If LHS is a fact, constrain it as a positive fact.
constrain_meta(P, mpred_positive_fact(P)) :- 
    is_ftVar(P), % Check if P is a first-order term.
    !.

% % Negation chaining: Handle negations in the LHS by recursively constraining the term.
constrain_meta(~P, CP) :- 
    !, 
    constrain_meta(P, CP).

constrain_meta(\+P, CP) :- 
    !, 
    constrain_meta(P, CP).

% % Forward chaining: Constrain the RHS (Right-Hand Side) of forward chaining rules.
constrain_meta((_ ==> Q), nonvar(Q)) :- 
    !, 
    is_ftVar(Q).

% % Equivalence chaining: Ensure that either side of the equivalence is non-variable.
constrain_meta((P <==> Q), (nonvar(Q); nonvar(P))) :- 
    (is_ftVar(Q); is_ftVar(P)), 
    !.

% % Backward chaining: Constrain the RHS of backward chaining rules.
constrain_meta((Q <- _), mpred_literal(Q)) :- 
    is_ftVar(Q), 
    !.

constrain_meta((Q <- _), CQ) :- 
    !, 
    constrain_meta(Q, CQ).

% % Conditional chaining: Handle LHS of conditional rules.
constrain_meta((Q :- _), mpred_literal(Q)) :- 
    is_ftVar(Q), 
    !.

constrain_meta((Q :- _), CQ) :- 
    !, 
    constrain_meta(Q, CQ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % is_simple_lhs(+Lhs) is semidet.
% %
% % This predicate checks if the given LHS is simple enough to be used in 
% % specific contexts, such as certain rule constraints. Complex terms are
% % filtered out by failing.
% %


 is_simple_lhs(ActN):- is_ftVar(ActN),!,fail.
 is_simple_lhs( \+ _ ):-!,fail.
 is_simple_lhs( ~ _ ):-!,fail.
 is_simple_lhs( _  / _ ):-!,fail.
 is_simple_lhs((Lhs1,Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).