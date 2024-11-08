% =========================================
%  PLDoc Header for show_cvts/1
% =========================================

%% show_cvts(+Clause) is det.
% 
% Display a clause in a specific form where the body is processed.
% 
% @param Clause A clause of the form Head :- Body.
%
% @example
% ?- show_cvts(foo :- bar).
% This will process and display foo :- bar in a specific form.
%
show_cvts(Head:-Body). 

% =========================================
% PLDoc Header for print_metta_clause0/2
% =========================================

%% print_metta_clause0(+Head, +Body) is det.
%
% Prints a metta clause by transforming the Body into a list if necessary.
% It will handle cases where the Body is either true or false and print them as
% metta statements accordingly.
%
% @param Head The head of the clause.
% @param Body The body of the clause, which can be true, false, or a conjunction of predicates.
%
% @example
% ?- print_metta_clause0(foo, true).
% This will print the metta clause "foo = True".
%
print_metta_clause0(Head,Body):- 
    % If Body is true, print Head as equal to 'True'
    Body == true,!, 
    pp_metta([=,Head,'True']).

print_metta_clause0(Head,Body):- 
    % If Body is false, print Head as equal to 'False'
    Body == false,!, 
    pp_metta([=,Head,'False']).

print_metta_clause0(Head,Body):- 
    % Transform Body into a list of conjuncts, then into a sequential form
    conjuncts_to_list(Body,List), 
    into_sequential([':-'],List,SP), 
    pp_metta([=,Head,SP]).

% =========================================
%  STERM -> PTERM Conversion
% =========================================

% PLDoc Header for iz_exact_symbol/2
%% iz_exact_symbol(+N, +P) is semidet.
%
% Checks if N is an exact symbol and if it matches with P. 
% Handles specific cases for special symbols like :-, ?-, and ??.
%
% @param N The input to check.
% @param P The output symbol.
%
iz_exact_symbol(N,_):- 
    % If N is not an atom, fail.
    \+ atom(N),!,fail.

iz_exact_symbol(N,P):- 
    % If P is non-variable, recursively check for exact symbol match.
    nonvar(P),!,iz_exact_symbol(N,PP),zalwayz(P=PP).

% Match exact symbols for specific operators like :- and ?-
iz_exact_symbol(':-',':-').
iz_exact_symbol('?-','?-').
iz_exact_symbol('??',_).

% This file directive was commented out as it is not currently needed.
% It loads specific logic rewriting utilities from another module.
%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

% PLDoc Header for maybe_varz/3
%% maybe_varz(+S, +Name, -Var) is semidet.
%
% Converts a variable into a special '$VAR' term if the input symbol is '?' and the Name is an atom.
%
% @param S The input symbol to check.
% @param Name The name of the variable.
% @param Var The resulting '$VAR'(Name) term.
%
maybe_varz(S,Name,'$VAR'(Name)):- 
    % If S is '?', and Name is an atom, then it's a special variable.
    S=='?',atom(Name),!.

% PLDoc Header for sexpr_s2p/2
%% sexpr_s2p(+HB, -P) is det.
%
% Converts a S-expression (STERM) to PTERM. 
% The implementation handles various forms of S-expressions, including lists and compound terms.
%
% @param HB The S-expression to convert.
% @param P The resulting PTERM.
%
sexpr_s2p(HB,P):- 
    % This clause was skipped because it's marked as 'fail', indicating it should not be executed.
    fail, 
    compound(HB), 
    HB=~ (H=B), 
    compile_for_assert(H,B,Cl),
    clause_to_code(Cl,P),!.

sexpr_s2p(S,P):- 
    % Default case that calls sexpr_s2p with a predefined function and term.
    sexpr_s2p(progn,1,S,P).

% PLDoc Header for clause_to_code/2
%% clause_to_code(+P, -Code) is det.
%
% Transforms a clause into its code representation.
%
% @param P The input clause.
% @param Code The resulting code after transformation.
%
clause_to_code(P,P):- 
    % If P is a free variable, it remains unchanged.
    is_ftVar(P),!.

clause_to_code((H:-B),P):- 
    % If the Body is true, combine the Head and Body into the resulting code.
    B==true, !, 
    combine_code(B,H,P).

clause_to_code(P,P). 

% PLDoc Header for sexpr_s2p/4
%% sexpr_s2p(+Fn, +Nth, +S, -P) is det.
%
% A more complex variant of sexpr_s2p that converts based on a function (Fn) and argument index (Nth).
% Handles various types of S-expressions.
%
% @param Fn The function to apply.
% @param Nth The argument index.
% @param S The S-expression to convert.
% @param P The resulting PTERM.
%
sexpr_s2p(Fn,_Nth,VAR,VAR):- 
    % If the variable is a free variable, return it unchanged.
    is_ftVar(VAR),!.

sexpr_s2p(Fn,_Nth,S,P):- 
    % If S is an exact symbol, return P as the matching symbol.
    iz_exact_symbol(S,P),!.

sexpr_s2p(Fn,_Nth,'#'(S),P):- 
    % Handle the case where S is prefixed with '#'.
    iz_exact_symbol(S,P),!.

sexpr_s2p(Fn,Nth,S,P):- 
    % Convert a list to PTERM using function Fn and argument index Nth.
    S==[], iz_fun_argz(Fn,Nth),!,P=S.

% This clause was commented out, but the original intent was to handle expected types for specific functions.
% sexpr_s2p(Fn,Nth,S,P):- expects_type(Fn,Nth,Type),will_become_type(Type,S,P),!.

sexpr_s2p(_Fn,_Nth,[F|SList],P):- 
    % Convert a list starting with F and SList into a predicate call if F is a system predicate.
    is_list(SList), length(SList,Len),is_syspred(F,Len,Pred), 
    sexpr_s2p_arglist(F,1,SList,PList), !, 
    P=..[Pred|PList].

% Disable the singleton variable warning for the rest of the code.
:- style_check(-singleton).

sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- 
    % Convert a list recursively.
    iz_fun_argz(Fn,Nth),!,
    sexpr_s2p(S,P), 
    sexpr_s2p(Fn,Nth,SList,PList).

sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- 
    % If S is not an atom or SList is not a list, convert recursively.
    ( \+ atom(S) ; \+ is_list(SList)), !,
    sexpr_s2p(list(Fn),Nth,S,P), 
    sexpr_s2p(list(Fn),Nth,SList,PList).

sexpr_s2p(_Fn,_Nth,[S,STERM0],PTERM):- 
    % Handle quoted terms, converting them into compound terms.
    iz_quoter(S),
    sexpr_s2p_pre_list(S,0,STERM0,STERM), !,
    PTERM=..[S,STERM],!.

sexpr_s2p(_Fn,_Nth,[S|SList],P):- 
    % Handle the case where S is an atom and SList is empty, resulting in a compound term with arity 0.
    atom(S), SList == [], compound_name_arity(P,S,0).

% This commented-out clause handled special cases with variables, but it was skipped to simplify the code.
% sexpr_s2p(Fn,Nth,List,PTERM):- append(Left,[S,Name|TERM],List),maybe_varz(S,Name,Var),!,append(Left,[Var|TERM],NewList), sexpr_s2p(Fn,Nth,NewList,PTERM).

% This block handled 'dot_holds' cases, which are not currently needed in the main flow.
% sexpr_s2p(Fn,Nth,[S|TERM],dot_holds(PTERM)):- \+ (is_list(TERM)),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.

% The following commented-out block dealt with quantifiers but was not necessary for this implementation.
% sexpr_s2p(Fn,Nth,[S,Vars|TERM],PTERM):- nonvar(S),
%    call_if_defined(common_logic_snark:iz_quantifier(S)),
%    zalwayz((sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),
%    PTERM =~ [S,Vars|PLIST])),!.

% This clause handled logical conjunctions in S-expressions but was commented out for performance reasons.
% sexpr_s2p(progn,_,[S|TERM],PTERM):- S==AND,!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).