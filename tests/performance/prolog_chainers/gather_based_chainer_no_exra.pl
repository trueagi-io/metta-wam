% Set Prolog flags to reduce verbosity during autoload and loading phases.
:- set_prolog_flag(verbose_autoload,false),
   set_prolog_flag(verbose_load,false),
   ensure_loaded(library(logicmoo_common)).

% Load Constraint Logic Programming over Finite Domains library.
:- ensure_loaded(library(clpfd)).

%%% RULES

% These lines are commented out, they might be for debugging or specific configurations.
% :- use_module(library(sldnfdraw)).
% :- sldnf.

% Define a tabled predicate codes_for/2 to avoid recomputation of results.
:- table(codes_for/2).
codes_for(G, P) :-
    transcribed_to(G, T),
    translates_to(T, P).

% Define a tabled predicate in_tad_with/2. Tables speed up recursive predicates by memoization.
:- table(in_tad_with/2).
in_tad_with(S, G1) :-
    different(G1,G2),
    in_tad_region(G1, T),
    in_tad_region(G2, T),
    closest_gene(S, G2).

% Table the predicate relevant_gene/2 for efficient querying.
:- table(relevant_gene/2).
relevant_gene(G, S) :-
    in_tad_with(S, G),
    eqtl(S, G).

% Table relevant_gene_coexpression/2 to memoize results for performance.
:- table(relevant_gene_coexpression/2).
relevant_gene_coexpression(G1, S) :-
    relevant_gene(G2, S),
    different(G1, G2),
    coexpressed_with(G1, G2).

% Table member_nat/3 for optimization.
:- table(member_nat/3).
member_nat(G, O, 0) :-
    codes_for(G, P),
    go_gene_product(O, P).
member_nat(G, O, s(D0)) :-
    rel_type(ontology_relationship(X, O), subclass),
    member(G, X, D0).

% Another version of member/3 that uses clpfd for the depth.
:- table(member/3).
member(G, O, 0) :-
    codes_for(G, P),
    go_gene_product(O, P).
member(G, O, D) :-
    D #= D0 + 1,
    rel_type(ontology_relationship(X, O), subclass),
    member(G, X, D0).

% Table relevant_go/2 to cache its results.
:- table(relevant_go/2).
relevant_go(O, S) :-
    (relevant_gene(G, S)
    ; relevant_gene_coexpression(G, S)),
     member(G, O, _D).

% Utility predicate to find all rule heads in the file for introspection.
% Assert a given goal G if it's not already true; otherwise, just indicate it's already present.
assert_singly(G):- notrace(catch(G,_,fail)),!,writeln(present(G)).
assert_singly(G):- assert(G).

% Declare dynamic predicate for tracking rule heads for introspection.
:- dynamic(rule_head/1).
% For each rule head found, assert it as a 'rule_head' fact for later use or inspection.
:- forall(find_rule_head(Head),assert_singly(rule_head(Head))).
% List out all asserted 'rule_head' facts for review.
:- listing(rule_head/1).
/* should return
rule_head(member(_,_,_)).
rule_head(relevant_go(_, _)).
rule_head(relevant_gene_coexpression(_, _)).
rule_head(relevant_gene(_, _)).
rule_head(in_tad_with(_, _)).
rule_head(codes_for(_, _)).
*/

%%% FACT FILES

% Configure file search paths for different biological data categories, enabling easy reference to data files.
user:file_search_path(gencode, './metta_out_v4_pl/gencode').
user:file_search_path(uniprot, './metta_out_v4_pl/uniprot').
user:file_search_path(ontology, './metta_out_v4_pl/ontology').
user:file_search_path(gaf, './metta_out_v4_pl/gaf').
user:file_search_path(cellxgene, './metta_out_v4_pl/cellxgene').
user:file_search_path(eqtl, './metta_out_v4_pl/gtex/eqtl').
user:file_search_path(tadmap, './metta_out_v4_pl/tadmap').
user:file_search_path(refseq, './metta_out_v4_pl/refseq').

% Declare which files contain facts, aiding in modular loading of data.
fact_files(eqtl(edges)).
fact_files(gencode(nodes)).  fact_files(gencode(edges)).
fact_files(uniprot(nodes)).  fact_files(uniprot(edges)).
fact_files(ontology(nodes)). fact_files(ontology(edges)).
fact_files(gaf(edges)).
fact_files(cellxgene(edges)).
fact_files(tadmap(edges)).
fact_files(refseq(edges)).

% Declare a dynamic predicate to keep track of which fact files have been loaded.
:- dynamic(is_fact_file_name/1).

% Load all fact files specified by 'fact_files/1' predicates, ensuring they are found and readable.
load_fact_files:- forall(fact_files(Spec),load_fact_file(Spec)).
load_fact_file(Spec):-
  absolute_file_name(Spec,File,
     [access(read), file_errors(fail), file_type(prolog)]),
  load_fact_file_name(File).

% If a fact file has not been previously loaded, record its name and load the file.
load_fact_file_name(File):-
  is_fact_file_name(File),!. % Skip if file is already marked as loaded.
load_fact_file_name(File):-
  assert(is_fact_file_name(File)), % Mark the file as loaded.
  time(ensure_loaded(File)). % Load the file, measuring the time taken.

% Trigger the loading process for all specified fact files.
:- load_fact_files.

% Disable garbage collection for potentially improved performance during these operations.
:- set_prolog_flag(gc,false).



different(G1,G2):- dif:dif(G1,G2).

different_f(G1,G2):-
  freeze(G1, G1\==G2),
  freeze(G2, G2\==G1).


%%% FACT HEADS
% Declare dynamic predicate for tracking fact heads for introspection.
:- dynamic(fact_head/1).
% Define a predicate to find the head of any fact defined in loaded fact files.
find_fact_head(Head):-
    % Check if 'File' is among the names of fact files that have been loaded.
    is_fact_file_name(File),
    % Find a predicate 'Head' that is defined in 'File'.
    source_file(Head,File).
% For each fact head found, assert it as a 'fact_head' fact for later use or inspection.
:- forall(find_fact_head(Head),assert_singly(fact_head(Head))).
% List out all asserted 'fact_head' facts for review.
:- listing(fact_head/1).
/*
    fact_head(gene(_)).
    fact_head(closest_gene(_, _)).
    fact_head(in_tad_region(_, _)).
    fact_head(transcribed_to(_, _)).
    fact_head(translates_to(_, _)).
    fact_head(rel_type(_, _)).
    fact_head(eqtl(_, _)).
    fact_head(go_gene_product(_, _)).
*/


% Example queries/tests that demonstrate how to use the defined predicates and facts.
tests :- solve(relevant_go(ontology_term(go_0045598), sequence_variant(rs1421085))).
tests :- solve(relevant_go(_O, sequence_variant(rs1421085))).
tests :- solve(_Any).

% List out the test predicates for inspection.
:- listing(tests).


% Disable the Prolog garbage collector. (uncomnment so that whn you press control C you can see all stack frames before theyu get GC'd
%:- set_prolog_flag(gc,false).
% Disable the Prolog garbage collector. (uncomnment so that whn you press control C you can see all stack frames before theyu get GC'd
%:- set_prolog_flag(gc,false).

% Attempt to find a solution for 'Goal' and print its proof without repeats.
solve(Goal):-
  % Use 'no_repeats' to ensure each solution for each 'Goal/Proof' pair is unique before proceeding.
  no_repeats(solve(Goal, Proof)),
  % Call 'ppp' to pretty print the found 'Proof'.
  ppp(Proof).

% Define 'ppp' to format and print proofs in a readable manner.
ppp(Proof):-
 % Use 'format' to structure the output, introducing the proof with a newline and indent.
 format('~N~nProof=~n\t'),
 % Employ double negation to ensure 'Proof' variables are universally quantified, making the output cleaner.
 % 'numbervars' attributes unique numbers to variables, enhancing readability.
 \+ \+ (numbervars(Proof,0,_,[attvars(skip), singletons(true)]),pppt(Proof)).
% Utilize 'pppt' to actually print the structured proof tree.
pppt(Proof):-
  % 'print_tree' is called to visually represent 'Proof' in a tree structure, followed by newlines for separation.
  print_tree(Proof),nl,nl.

% Overloaded 'solve' for handling goals and associated proofs.
% Handle unbound variable goals by checking against known facts.
solve(Goal, Proof):- var(Goal),fact_head(Goal),solve(Goal, Proof).
% Handle unbound variable goals by checking against known rules.
solve(Goal, Proof):- var(Goal),!,rule_head(Goal),solve(Goal, Proof).
% Directly solve for 'true', which always succeeds.
solve(true, true) :- !.
% Directly solve for 'fail', which always fails.
solve(fail, _) :- !, fail.

% Handle logical negation through failure-driven loop.
solve(not(Goal), tnot(Proof)) :- !, tnot(solve(Goal, Proof)).

% Optimize the order of AND ('Goal1, Goal2') goals based on a heuristic.
solve((Goal1, Goal2), (Proof2,Proof1)) :-
  % Prefer solving the second goal first if it's determined to be "better".
  second_is_better_than_first(Goal1,Goal2),!,
  % Recursively solve each goal in the optimized order.
  solve(Goal2,Proof1),solve(Goal1,Proof2).

% Optimize the order of OR ('Goal1; Goal2') goals using the same heuristic.
solve((Goal1; Goal2), or_swap(Proof)) :-
  % Swap goals if the second is "better".
  second_is_better_than_first(Goal1,Goal2),!,
  % Attempt to solve the goals in the new order.
  solve((Goal2; Goal1), Proof).

% Recursively solve conjunctions ('Goal1, Goal2') in the provided order.
solve((Goal1, Goal2), (Proof1 , Proof2)) :- !,
    % Solve each part of the conjunction separately.
    solve(Goal1, Proof1),
    solve(Goal2, Proof2).

% Attempt to solve each part of a disjunction ('Goal1; Goal2').
solve((Goal1 ; _), or_l(Proof1)) :-    solve(Goal1, Proof1).
solve((_ ; Goal2), or_r(Proof2)) :- !, solve(Goal2, Proof2).

% Attempt to solve 'Goal' by finding a corresponding rule.
solve(Goal, Proof) :- rule_head(Goal),!,
  % Look for a clause where 'Goal' matches the head.
  clause(Goal, Body, Ref),
  clause(HeadC, BodyC, Ref),
  % Solve the body of the clause as the next step in the proof.
  solve_body(HeadC, BodyC, Goal, Body, Proof).

% Check if 'Goal' matches a known fact in the database.
solve(Goal, fact(Goal)) :- fact_head(Goal),!, no_repeats_g(Goal).
% Check if 'Goal' is a built-in Prolog predicate and solve directly.
solve(Goal, built_in(Goal)) :-
    % Avoid repeats for built-in predicates.
    no_repeats_g(Goal).

% Helper to avoid repeating solutions for ground goals.
no_repeats_g(G):- ground(G),!,once(G).
% For non-ground goals, use 'no_repeats' to prevent duplicate solutions.
no_repeats_g(G):- no_repeats(G).

% Differentiate solving for direct fact matches versus implications.
solve_body(HeadC,    _,  Goal, true, rfact(Goal)):- HeadC  == Goal,!.
solve_body(HeadC,    _,  Goal, true, (rule(HeadC),implied(Goal))):- HeadC \== Goal,!.
% For other cases, solve the body to continue building the proof.
solve_body(HeadC, BodyC, Goal, Body, (Goal --> ((HeadC:-BodyC), Proof))):-
  % Recurse into the body of the rule for proof.
  solve(Body, Proof).



% =============================
% UTILITY: second_is_better_than_first/2
% =============================

% Heuristic for prioritizing goal order based on variable binding and complexity.
second_is_better_than_first(Hypo1, Hypo2) :-
    % Count the unbound variables in each hypothesis.
    term_variables(Hypo1,TV1),length(TV1,C1),
    term_variables(Hypo2,TV2),length(TV2,C2),
    % Prefer hypotheses with fewer unbound variables.
    C1\=C2, !, C1>C2.

% Use the size of predicates to decide order, preferring smaller predicates.
second_is_better_than_first(Hypo1, Hypo2) :-
    predicate_size(Hypo1, Size1),
    predicate_size(Hypo2, Size2), !,
    Size2 < Size1.

% Calculate the "size" or complexity of a hypothesis.
predicate_size(Hypo, Size):-
  predicate_size([], Hypo, Size).

% Check if a function is already considered in the size calculation.
f_memberchk(F,List):- \+ \+ (member(E,List),E=@=F).

% Calculate the size based on structure and grounding.
predicate_size(_Completed, Hypo, Size) :- \+ callable(Hypo), !, Size=1.
predicate_size(Completed, Hypo, Size):- f_memberchk(Hypo, Completed), !, Size=0.
predicate_size(_Completed, Hypo, Size):- ground(Hypo),!, Size=1.
predicate_size(Completed, (Hypo1, Hypo2), Size):- !,
  % Sum the sizes of individual components in a conjunction or disjunction.
  predicate_size(Completed, Hypo1, S1),
  predicate_size(Completed, Hypo2, S2),
  size_sm(S1, S2, SizeM), Size is SizeM + 1,!.
predicate_size(Completed, (Hypo1;Hypo2), Size):-
  % For disjunctions, add the sizes of the options.
  predicate_size(Completed, Hypo1, S1),
  predicate_size(Completed, Hypo2, S2),
  Size is S1+S2,!.
predicate_size(Completed, Hypo, Size):-
   % Account for the number of clauses and rules in determining size.
   predicate_property(Hypo, number_of_clauses(F)),
   predicate_property(Hypo, number_of_rules(R)),
   predicate_size(Completed, Hypo, F, R, Size),!.
predicate_size(_Completed, _Hypo, 1).

% Helper functions for size comparison.
size_sm(A, B, A) :- B =:= 0, !.
size_sm(A, B, B) :- A =:= 0, !.
size_sm(A, B, A) :- A =< B.
size_sm(A, B, B) :- B < A.

% Handle the case where a predicate is defined entirely by facts.
predicate_size(_Completed, _, F, 0, F):-!.
/*
  - When the number of rules (R) is 0, implying the predicate is defined only by facts,
    the size is directly the number of facts (F).
*/

% Handle the case where a predicate is defined entirely by rules.
predicate_size(Completed, Hypo, R, R, Size):- !,
  % Calculate the size based on the total extent of these rules.
  rule_extent(Completed, Hypo, Size).
/*
  - If a predicate is defined only by rules (when F is equal to R),
    determine its size by calculating the extent of these rules.
*/

% Handle predicates defined by a mix of rules and facts.
predicate_size(Completed, Hypo, F, R, Size):-
   % Calculate the rule extent to understand the complexity added by rules.
   rule_extent(Completed, Hypo, RSize),
   % Adjust the size by subtracting rule count from fact count and adding calculated rule size.
   Size is F - R + RSize.
/*
  - For predicates defined by both facts and rules, the size calculation adjusts
    for the rule complexity (RSize) on top of the base fact count (F), corrected by the rule count (R).
*/

% Calculate the "extent" of rules for a hypothesis, considering no prior completions.
rule_extent(Hypo, Size):-
  rule_extent([], Hypo, Size).
/*
  - This entry point allows for calculating the rule extent of a hypothesis from scratch,
    initializing the 'Completed' list as empty.
*/

% Avoid recalculating the size for a hypothesis already considered.
rule_extent(Completed, Hypo, Size):- f_memberchk(Hypo, Completed), !, Size=0.
/*
  - If 'Hypo' is already in the 'Completed' list, its size is considered as 0 to avoid
    double-counting, indicating it's already been accounted for in the complexity calculation.
*/

% Calculate the rule extent for a hypothesis not yet completed.
rule_extent(Completed, Hypo, Size):-
  % Find the size of each clause's body that isn't trivially true, indicating rule complexity.
  findall(S1ze, (clause(Hypo, Body), Body\==true, predicate_size([Hypo|Completed], Body, S1ze)), SizeL),
  % Sum these sizes to get the total complexity contributed by the hypothesis' rules.
  sumlist(SizeL, Size), !.
/*
  - For each rule defining 'Hypo', calculate the complexity of its body, excluding
    trivial truths. The total rule complexity (Size) is the sum of these individual complexities.
*/

% Default case for rule extent calculation when no specific conditions are met.
rule_extent(_,_Hypo, 0).
/*
  - If none of the specific conditions for calculating rule extent are met,
    default the rule extent to 0. This might occur if the hypothesis doesn't match
    any rules or if all rules are trivially true.
*/

