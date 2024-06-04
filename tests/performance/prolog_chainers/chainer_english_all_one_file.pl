% Set Prolog flags to reduce verbosity during autoload and loading phases.
:- set_prolog_flag(verbose_autoload,false),
   set_prolog_flag(verbose_load,false),
   ensure_loaded(library(logicmoo_common)).
:- use_module(library(logicmoo_utils)).
:- abolish('$exported_op'/3).

:- op(500, xfy, =>).
:- use_module(library(clpfd)).

:- multifile fact_head/1.
:- multifile closest_gene/2.
:- multifile coexpressed_with/2.
:- multifile in_tad_region/2.
:- multifile transcribed_to/2.
:- multifile translates_to/2.
:- multifile rel_type/2.
:- multifile eqtl/2.
:- multifile go_gene_product/2.

:- dynamic fact_head/1.
:- dynamic closest_gene/2.
:- dynamic coexpressed_with/2.
:- dynamic in_tad_region/2.
:- dynamic transcribed_to/2.
:- dynamic translates_to/2.
:- dynamic rel_type/2.
:- dynamic eqtl/2.
:- dynamic go_gene_product/2.


solve(Solve):-
  once(mi(Solve,Proof)),
  once(ppp(Proof)).

ppp(Proof):-
 % Use 'format' to structure the output, introducing the proof with a newline and indent.
    draw_tty_line,
 format('~N~nProof=~n '),
 % Employ double negation to ensure 'Proof' variables are universally quantified, making the output cleaner.
 % 'numbervars' attributes unique numbers to variables, enhancing readability.
 \+ \+ (%numbervars(Proof,0,_,[attvars(bind), singletons(true)]),
   pppt(Proof)).


pppt(Proof):-
  % 'print_tree' being called to visually represent 'Proof' in a tree structure, followed by newlines for separation.
  %print_tree(Proof),nl,nl,
  show_english(Proof).

show_english_f:- forall(fact_head(Head),
  forall(two_versions(Head),show_english(Head))).

show_english_r:- forall(rule_head(Head),
  forall(two_versions(Head),show_english(Head))).

show_english_rc:-
 forall(rule_head(Head),
  forall(clause(Head,Body),
   forall(two_versions(Body),
    show_english(Body=>Head)))).

two_versions(_). % show what happens when all vars
two_versions(Head):- once(Head).

demo_e:-
    show_english_r,
    show_english_f,
    show_english_rc,
    forall(tests,true),
    listing(used).

% Example queries/tests that demonstrate how to use the defined predicates and facts.
tests :- solve(relevant_go(ontology_term(go_0045598), sequence_variant(rs1421085))).
%tests :- solve(relevant_go(_O, sequence_variant(rs1421085))).
%tests :- solve(_Any).

% List out the test predicates for inspection.
:- listing(tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% record patterns
atomic_or_ftVar(E):- var(E),!,fail.
atomic_or_ftVar(E):- \+compound(E),!.
atomic_or_ftVar(E):- is_ftVar(E),!.

record_used(X1):- var(X1),!.
record_used((X1,X2)):- !,   record_used(X1), record_used(X2).
record_used((X1=>X2)):- !,   record_used(X1),   record_used(X2).
record_used((X1;X2)):- !,   record_used(X1),   record_used(X2).
record_used(X1):- sub_term(E,X1),atomic_or_ftVar(E),
   subst(X1,E,_,X2),record_used(X2).
record_used(X2):-
  forall((sub_term(E,X2),compound(E)), assert_used(E)).

:- dynamic(used/1).
%assert_used(X2):- functor(X2,F,A),functor(X1,F,A),X1\=@=X2,!,assert_used(X1).
assert_used(X1):- \+ \+ (used(X2),X1=@=X2),!.
assert_used(X2):- retractall(assert(used(X2)), assert(used(X2)),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SHOW ENGLISH
draw_tty_line :- format('~N=============================================~n').
draw_tty_line_s :- format('~N     ==============================~n').
% Define the entry point for the translation
show_english(X):-%writeq(X),draw_tty_line_s,
  once(translate_to_english(X,Y)),writeln(Y),!,
  draw_tty_line.

translate_to_english(X0, English) :-
  copy_term(X0,X1),
  record_used(X1),
  phrase_to_string(tr(X1), English).

%pretty_numbervars(X1,X),
phrase_to_string(DCG,English):-
  phrase(DCG, EnglishList),
  atomics_to_string(EnglishList, " ", English),!.

quote(DCG) --> 
  {phrase_to_string(DCG,Eng),
    atomics_to_string(['"',Eng,'"'],String)},[String].
  
w_nl(DCG) --> DCG, ['\n'].
 

% Typed Noun Phase

typed_np(Name, G)          --> {toPropercase(Name,FF),Name\==FF},!,typed_np(FF, G).
typed_np(Name, G)          --> {var(G), gensym("#", G)}, [Name, G].
typed_np(Name, G)          --> {atom(G)}, [Name, G].
typed_np(Name, G)          --> [Name, with, symbol], np(G).

% Noun Phase
np(Var)                --> {var(Var)},!, typed_np('Something', Var).
np(gene(G))            --> typed_np('Gene', G).
np(sequence_variant(G))--> typed_np('SNP', G).
np(ontology_term(C))       --> np(C).
% thunk
np(X)                  --> tr(X).

safe_p_univ(P,[F|Args]):- compound(P),!,compound_name_arguments(P,F,Args).
safe_p_univ(P,[P]).

compound_remove_if(C,P1,R):- compound(C),
   safe_p_univ(C,[F|Args]), Args = [_,_|_],
   exclude(P1,Args,NewArgs),  Args\==NewArgs,!,
 ((NewArgs==[] -> R=[];
  (NewArgs=[R] -> true;
   compound_name_arguments(R,F,NewArgs)))).

hide_calls(V):- var(V),!,fail.
hide_calls(hidden(_)).
hide_calls(py_call(_)).
hide_calls(py_call(_,_)).

% Translation rules for logical operators and structures
tr(Var)                --> {var(Var)},!, typed_np('Something', Var).
tr(Const)              --> {atom(Const)}, [Const].
tr(String)              --> {\+ callable(String)},!,[String].
tr(hidden(_))         --> [].

% unwraps
tr(built_in(C))       --> tr(C).
tr(ontology_term(C))       --> tr(C).

tr(C)                 --> {compound_remove_if(C,hide_calls,R)}, tr(R).  % Logical AND
tr((A, B))            --> w_nl(tr(A)), w_nl(([and], tr(B))).  % Logical AND
tr((A; B))            --> tr(A), [or], tr(B).   % Logical OR
tr(not(A))            --> [not], tr(A).         % Logical NOT
tr(\+(A))             --> [not, provable], tr(A). % Negation as failure (NAF)

tr(:-(A, B))          --> tr(B=>A).
tr((B => A))          --> ["The statement"], w_nl(quote(tr(A))), [is, proved, by], w_nl((tr(B),['.'])).

% Translation rules for different predicates and structures
tr(member(A, B, C))   --> np(A), [being, a, member, of], np(B), [at, level], np(C).
tr(member_nat(A, B, C)) --> tr(member(A, B, C)).
tr(relevant_go(A, B)) --> np(A), [being, relevant, to], np(B), [through, gene, ontology].
tr(relevant_gene(A, B)) --> np(A), [being, relevant, to], np(B).
tr(relevant_gene_coexpression(A, B))
                      --> np(A), [being, coexpressed, with, a, gene, relevant, to], np(B).
tr(in_tad_with(A, B)) --> np(A), [being, in, a, 'TAD', with], np(B).
tr(codes_for(A, B))   --> np(A), [codes, for], np(B).
tr(gene(A))           --> np(A), [is, a, gene].
tr(fact(A))           --> [the, fact], tr(A).
tr(closest_gene(A, B))--> np(A), [being, the, closest, gene, to], np(B).
tr(in_tad_region(A, B))
                      --> np(A), [being, in, 'TAD region'], np(B).
tr(transcribed_to(A, B))
                      --> np(A), [being, transcribed, to], np(B).
tr(translates_to(A, B))      --> np(A), [translates, to], np(B).

tr(rel_type(ontology_relationship(ontology_term(A), ontology_term(B)), P))
                                -->  tr(triple(A,P,B)).   
tr(rel_type(ontology_relationship(A,B),P)) -->  tr(triple(A,P,B)).
tr(rel_type(A, B))    --> [the], np(A), [is], np(B).

tr(triple(A,P,B))  -->  np(A),[is,a],tr(P),[of],np(B).
  
tr(eqtl(A, B))        --> [there, being, an, eQTL, between], np(A), [and], np(B).
tr(go_gene_product(A, B))
                      --> np(A), [being, a, 'GO gene product', for], np(B).

tr(P)                 --> {safe_p_univ(P,[F,A,B])}, tr(pso(F,A,B)).

tr(pso(F,A,B))        -->
                          {atomic_list_concat(WordList,' ',F)},
                             np(A),WordList,np(B).

tr(P)                 --> {safe_p_univ(P,[F,A])}, typed_np(F, A).

tr(P)                 --> {sformat(S,'~q',[P])}, [a,logical,atom,S].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






:- dynamic(natnum/1).

natnum(0).
natnum(s(X)) :-
    natnum(X).

list_length(Ls, L) :-
    list_length_(Ls, 0, L).

list_length_([], L, L).
list_length_([_|Ls], L0, L) :-
    '#='(L1 ,(L0 + 1)),
    list_length_(Ls, L1, L).

mi(true, true).
% basecase 1

mi(Goal, hidden(Goal)) :-
    hide_calls(Goal),!,
    no_repeats_g(Goal). %

mi(Goal, Goal) :- % Check if the goal being a built-in predicate.
    (predicate_property(Goal, built_in) ; %or
  \+ predicate_property(Goal,number_of_clauses(_))), !, % or if it was FFI
    no_repeats_g(Goal). % Directly call the built-in predicate.

mi(G, P => G) :-
    G \= true,
    G \=  (_,_),
    clause(G, Body), mi(Body,  P).

% recurse
mi((A, B),  (PA, PB)) :-
    mi(A, PA), mi(B, PB).

mi((A; B), Proof) :-
    mi(A, Proof); mi(B, Proof).



% Configure file search paths for different biological data categories, enabling easy reference to data files.
user:file_search_path(gencode, './metta_out_v4_pl/gencode').
user:file_search_path(uniprot, './metta_out_v4_pl/uniprot').
user:file_search_path(ontology, './metta_out_v4_pl/ontology').
user:file_search_path(gaf, './metta_out_v4_pl/gaf').
user:file_search_path(cellxgene, './metta_out_v4_pl/cellxgene').
user:file_search_path(eqtl, './metta_out_v4_pl/gtex/eqtl').
user:file_search_path(tadmap, './metta_out_v4_pl/tadmap').
user:file_search_path(refseq, './metta_out_v4_pl/refseq').

%%% FACT FILES

% Declare which files contain facts, aiding in modular loading of data.
fact_files(eqtl(edges)).
fact_files(gencode(nodes)).  fact_files(gencode(edges)).
fact_files(uniprot(nodes)).  fact_files(uniprot(edges)).
fact_files(ontology(nodes)). fact_files(ontology(edges)).
fact_files(gaf(edges)).
fact_files(cellxgene(edges)).
fact_files(tadmap(edges)).
fact_files(refseq(edges)).

% Load Constraint Logic Programming over Finite Domains library.
:- ensure_loaded(library(clpfd)).
% :- use_module(library(sldnfdraw)).
% :- sldnf.


%%% RULES START
rules_start.  % Rules (for use in proofs) come after this

% Define a tabled predicate codes_for/2 to avoid recomputation of results.
:- multifile(codes_for/2).
codes_for(G, P) :-
    transcribed_to(G, T),
    translates_to(T, P).

% Define a tabled predicate in_tad_with/2. Tables speed up recursive predicates by memoization.
:- multifile(in_tad_with/2).
in_tad_with(S, G1) :-
    different(G1,G2),
    in_tad_region(G1, T),
    in_tad_region(G2, T),
    closest_gene(S, G2).

% Table the predicate relevant_gene/2 for efficient querying.
:- multifile(relevant_gene/2).
relevant_gene(G, S) :-
    in_tad_with(S, G),
    eqtl(S, G).

% Table relevant_gene_coexpression/2 to memoize results for performance.
:- multifile(relevant_gene_coexpression/2).
relevant_gene_coexpression(G1, S) :-
    relevant_gene(G2, S),
    different(G1, G2),
    coexpressed_with(G1, G2).

% Table member_nat/3 for optimization.
:- multifile(member_nat/3).
member_nat(G, O, 0) :-
    codes_for(G, P),
    go_gene_product(O, P).
member_nat(G, O, s(D0)) :-
    rel_type(ontology_relationship(X, O), subclass),
    member(G, X, D0).

% Another version of member/3 that uses clpfd for the depth.
:- multifile(member/3).
member(G, O, 0) :-
    codes_for(G, P),
    go_gene_product(O, P).
member(G, O, D) :-
  #=(D ,( D0 + 1)),
    rel_type(ontology_relationship(X, O), subclass),
    member(G, X, D0).

% Table relevant_go/2 to cache its results.
:- multifile(relevant_go/2).
relevant_go(O, S) :-
    (relevant_gene(G, S)
    ; relevant_gene_coexpression(G, S)),
     member(G, O, _D).  % can swap this out for speed checks

%%% RULES END
rules_end.


% Ensures two terms, G1 and G2, are different using the standard dif predicate.
different(G1,G2):-
    dif:dif(G1,G2).
    % The dif:dif(G1, G2) ensures G1 and G2 cannot unify now or in the future,
    % supporting logical purity by allowing Prolog to delay evaluation until
    % enough information being available.

% Ensures two terms, G1 and G2, are different by deferring the inequality check
% until either G1 or G2 being instantiated.
different_f(G1,G2):-
    % freeze/2 delays execution of its second argument (a goal) until its first
    % argument (G1 or G2 here) being instantiated.
    freeze(G1, G1\==G2),
    freeze(G2, G2\==G1).
    % G1\==G2 being the goal that checks if G1 and G2 are not strictly equal.
    % If either G1 or G2 becomes instantiated, Prolog then checks if this goal can succeed.
    % This approach defers the check for G1 and G2 being different until
    % one of them gets a concrete value, at which point the inequality check being performed.



/*
?- solve(relevant_go(ontology_term(go_0045598), sequence_variant(rs1421085))).

Proof=
        proven(relevant_go(ontology_term(go_0045598),sequence_variant(rs1421085))) -->
          ( rule(relevant_go(A,B)) :-
              relevant_gene(C,B);relevant_gene_coexpression(C,B) ,
              member(C,A,_)  ,
            or_r( proven(relevant_gene_coexpression(gene(ensg00000175602),sequence_variant(rs1421085))) -->
                    ( rule(relevant_gene_coexpression(D,E)) :-
                        relevant_gene(F,E) ,
                        different(D,F) ,
                        coexpressed_with(D,F)  ,
                      proven(relevant_gene(gene(ensg00000177508),sequence_variant(rs1421085))) -->
                        ( rule(relevant_gene(G,H)):-in_tad_with(H,G),eqtl(H,G)  ,
                          proven(in_tad_with(sequence_variant(rs1421085),gene(ensg00000177508))) -->
                            ( rule(in_tad_with(I,J)) :-
                                different(J,K) ,
                                in_tad_region(J,L) ,
                                in_tad_region(K,L) ,
                                closest_gene(I,K)  ,
                              built_in(different(gene(ensg00000177508),gene(ensg00000140718))) ,
                              fact(in_tad_region(gene(ensg00000177508),tad(chr16_53550000_55450000_grch38))) ,
                              fact(in_tad_region(gene(ensg00000140718),tad(chr16_53550000_55450000_grch38))) ,
                              fact(closest_gene(sequence_variant(rs1421085),gene(ensg00000140718)))) ,
                          fact(eqtl(sequence_variant(rs1421085),gene(ensg00000177508)))) ,
                      built_in(different(gene(ensg00000175602),gene(ensg00000177508))) ,
                      fact(coexpressed_with(gene(ensg00000175602),gene(ensg00000177508))))) ,
            proven(member(gene(ensg00000175602),ontology_term(go_0045598),1)) -->
              ( rule(member(M,N,O)) :-
                    ((
                     (integer(O) ->
                        P=O,clpfd:clpfd_equal(P,Q+1));
                     (clpfd: clpfd_equal(O,Q+1))  )) ,
                  rel_type(ontology_relationship(R,N),subclass) ,
                  member(M,R,Q)  ,
                fact(rel_type(ontology_relationship(ontology_term(go_0045599),ontology_term(go_0045598)),subclass)) ,
                proven(member(gene(ensg00000175602),ontology_term(go_0045599),0)) -->
                  ( rule(member(S,T,0)):-codes_for(S,U),go_gene_product(T,U)  ,
                    proven(codes_for(gene(ensg00000175602),protein(q15834))) -->
                      ( rule(codes_for(V,W)) :-
                          transcribed_to(V,X) ,
                          translates_to(X,W)  ,
                        fact(transcribed_to(gene(ensg00000175602),transcript(enst00000312579))) ,
                        fact(translates_to(transcript(enst00000312579),protein(q15834)))) ,
                    fact(go_gene_product(ontology_term(go_0045599),protein(q15834)))) ,
                or_swap(or_l(built_in(clpfd:clpfd_equal(1,0+1))))))


*/


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
  is_fact_file_name(File),!. % Skip if file being already marked as loaded.
load_fact_file_name(File):-
  assert(is_fact_file_name(File)), % Mark the file as loaded.
  time(ensure_loaded(File)). % Load the file, measuring the time taken.

% Trigger the loading process for all specified fact files.
:- time(load_fact_files).


% Assert a given Clause if it's not already true; otherwise, just indicate it's already present.
assert_singly(Clause):- notrace(catch(Clause,_,fail)),!,writeln(present(Clause)).
assert_singly(Clause):- assert(Clause).

%%% FIND AND CACHE RULES HEADS
% Rebuilt if the file being releaded anyways
:- abolish(rule_head/1).
% Declare dynamic predicate for tracking rule heads from introspection.
:- dynamic(rule_head/1).
% Utility predicate to find all rule heads in the file using introspection.
find_rule_head(Head):-
    % Get the line number where 'rules_start' and 'rules_end' are defined.
    predicate_property(rules_start, line_count(LineNumberStart)),
    predicate_property(rules_end, line_count(LineNumberEnd)),
    % Get the file (this file name) in which 'rules_start' being defined.
    source_file(rules_start, ThisFile),
 % We iterate from here
    % Ensure 'Head' being defined in the same file as 'find_rule_head'.
    source_file(Head, ThisFile),
    % Get the line number where 'Head' being defined.
    predicate_property(Head, line_count(LineNumber)),
    % Make sure that line being between rule_state and rule_end
    LineNumberStart < LineNumber,  LineNumber < LineNumberEnd,
    % Ensure that 'Head' has at least one rule associated with it.
    \+ predicate_property(Head, number_of_rules(0)).
% For each rule head found, assert it as a 'rule_head' fact for later use or inspection.
:- forall(find_rule_head(Head),assert_singly(rule_head(Head))).
% List out all asserted 'rule_head' facts for review.
:- listing(rule_head/1).
/* should return
rule_head(member(_,_,_)).
rule_head(relevant_go(A,B)).
rule_head(relevant_gene_coexpression(A,B)).
rule_head(relevant_gene(A,B)).
rule_head(in_tad_with(A,B)).
rule_head(codes_for(A,B)).
*/



%%% FACT HEADS
% Rebuilt if the file is reloaded anyways
:- abolish(fact_head/1).
% Declare dynamic predicate for tracking fact heads for introspection.
:- dynamic(fact_head/1).
% Define a predicate to find the head of any fact defined in loaded fact files.
find_fact_head(Head):-
    % Check if 'File' is among the names of fact files that have been loaded.
    is_fact_file_name(File),
    % Find a predicate 'Head' that being defined in 'File'.
    source_file(Head,File).
% For each fact head found, assert it as a 'fact_head' fact for later use or inspection.
:- forall(find_fact_head(Head),assert_singly(fact_head(Head))).
% List out all asserted 'fact_head' facts for review.
:- listing(fact_head/1).
/* should return
fact_head(gene(_)).
fact_head(closest_gene(A,B)).
fact_head(in_tad_region(A,B)).
fact_head(transcribed_to(A,B)).
fact_head(translates_to(A,B)).
fact_head(rel_type(A,B)).
fact_head(eqtl(A,B)).
fact_head(go_gene_product(A,B)).
*/


% 'mt' for handling goals and associated proofs.
% Handle unbound Goal by trying all known rules.
mt(Goal, Proof):- var(Goal), rule_head(Goal),mt(Goal, Proof).
% Handle unbound Goal by returning all known facts.
mt(Goal, Proof):- var(Goal), fact_head(Goal),mt(Goal, Proof).
% Directly solve for 'true', which always succeeds.
mt(true, true) :- !.
% Directly solve for 'fail', which always fails.
mt(fail, _) :- !, fail.
% Handle logical negation through Trie-Not
mt(not(Goal), tnot(Proof)) :- !, tnot(mt(Goal, Proof)).
% Handle negation by failure
mt( \+ (Goal), naf(Proof)) :- !, \+ mt(Goal, Proof).
% Optimize the order of AND ('Goal1, Goal2') goals based on a heuristic.
% Goal3 exists to make sure Goal2 being a Literal (nonconnective Goal)

% Recursively solve conjunctions ('Goal1, Goal2') in the provided order.
mt((Goal1, Goal2), (Proof1 , Proof2)) :- !,
    % Solve each part of the conjunction separately.
    mt(Goal1, Proof1),
    mt(Goal2, Proof2).

% Attempt to solve each part of a disjunction ('Goal1; Goal2').
mt((Goal1 ; _), or_l(Proof1)) :-    mt(Goal1, Proof1).
mt((_ ; Goal2), or_r(Proof2)) :- !, mt(Goal2, Proof2).

mt(Goal, hidden) :-
    hide_calls(Goal),!,
    call(Goal). %

% Attempt to solve 'Goal' by finding a corresponding rule.
mt(Goal, Proof) :- rule_head(Goal),!,
  % Look for a clause where 'Goal' matches the head.
  clause(Goal, Body, Ref),
  clause(HeadC, BodyC, Ref),
  % Solve the body of the clause as the next step in the proof.
  mt_body(HeadC, BodyC, Goal, Body, Proof).

% Check if 'Goal' matches a known fact in the database.
mt(Goal, fact(Goal)) :- fact_head(Goal),!, no_repeats_g(Goal).
% Check if 'Goal' being a built-in Prolog predicate and solve directly.
mt(Goal, built_in(Goal)) :-
    % Avoid repeats for built-in predicates.
    no_repeats_g(Goal).

% Helper to avoid repeating solutions for ground goals.
no_repeats_g(G):- ground(G),!,once(G).
% For non-ground goals, use 'no_repeats' to prevent duplicate solutions.
no_repeats_g(G):- no_repeats(G).

% Differentiate solving for direct fact matches versus implications.
mt_body(HeadC,    _,  Goal, true, rfact(Goal)):- HeadC  == Goal,!.
mt_body(HeadC,    _,  Goal, true, (rule(HeadC),implied(Goal))):- HeadC \== Goal,!.
% For other cases, solve the body to continue building the proof.
mt_body(HeadC, BodyC, Goal, Body, (proven(Goal), (rule(HeadC):-BodyC), Proof)):-
  % Recurse into the body of the rule for proof.
  mt(Body, Proof).


:- demo_e.
