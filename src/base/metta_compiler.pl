% ==============================
% Prolog to MeTTa translator
% This Prolog code block is mainly aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================

% Setting the file encoding to ISO-Latin-1
:- encoding(iso_latin_1).
% Flushing the current output
:- flush_output.
% Setting the Rust backtrace to Full
:- setenv('RUST_BACKTRACE',full).
% Loading various library files
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
% =======================================
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(swi_flybase).
% =======================================
:- set_option_value(encoding,iso_latin_1).

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).


% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% ===============================


% Conversion is possible between a function and a predicate of arity when the result is at the nth arg
:- dynamic decl_functional_predicate_arg/3.

% Converion is possible between a  function and predicate is tricky
functional_predicate_arg_tricky(is, 2, 1). % E.g. eval(is(+(1,2)),Result) converts to is(Result,+(1,2)).

% Defining standard mappings for some common functions/predicates
decl_functional_predicate_arg(append, 3, 3).
decl_functional_predicate_arg(+, 3, 3).
decl_functional_predicate_arg(pi, 1, 1).
decl_functional_predicate_arg(edge, 2, 2).
decl_functional_predicate_arg('==', 3, 3).
decl_functional_predicate_arg(assertTrue, 2, 2).
decl_functional_predicate_arg(assertFalse, 2, 2).



% Mapping any current predicate F/A to a function, if it's not tricky
functional_predicate_arg(F, A, L):- decl_functional_predicate_arg(F, A, L).
functional_predicate_arg(F, A, L):- (atom(F)->true;trace), predicate_arity(F,A),
  \+ functional_predicate_arg_tricky(F,A,_), L=A,
  \+ decl_functional_predicate_arg(F, A, _).
functional_predicate_arg(F, A, L):- functional_predicate_arg_tricky(F, A, L).

predicate_arity(F,A):- metta_atom('&self',[:,F,[->|Args]]), length(Args,A).
predicate_arity(F,A):- current_predicate(F/A).
% Certain constructs should not be converted to functions.
not_function(P):- atom(P),!,not_function(P,0).
not_function(P):- callable(P),!,functor(P,F,A),not_function(F,A).
not_function(!,0).
not_function(print,1).
not_function((':-'),2).
not_function((','),2).
not_function((';'),2).
not_function(('='),2).
not_function(('or'),2).
not_function('a',0).
not_function('b',0).
not_function(A,0):- atom(A),!.
not_function('True',0).
not_function(F,A):- predicate_arity(F,A),AA is A+1, \+ decl_functional_predicate_arg(F,AA,_).

==(A, B, Res):- as_tf(close_enough(A, B),Res).

% Function without arguments can be converted directly.
is_arity_0(AsFunction):- compound(AsFunction), compound_name_arity(AsFunction,_,0).

% Determines whether a given term is a function and retrieves the position
% in the predicate where the function Result is stored/retrieved
is_function(AsFunction, _):- is_ftVar(AsFunction),!,fail.
is_function(AsFunction, _):- AsFunction=='$VAR',!, trace, fail.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction), \+ not_function(AsFunction,0), !,Nth=1.
is_function(AsFunction, Nth) :-
    callable(AsFunction),
    functor(AsFunction, Functor, A),
    \+ not_function(Functor, A),
    AA is A + 1,
    functional_predicate_arg_maybe(Functor, AA, Nth).

functional_predicate_arg_maybe(F, AA, Nth):- functional_predicate_arg(F, AA, Nth),!.
functional_predicate_arg_maybe(F, AA, _):-
   A is AA - 1,functional_predicate_arg(F,A,_),!,fail.
functional_predicate_arg_maybe(F, Nth, Nth):-
   asserta(decl_functional_predicate_arg(F, Nth, Nth)),!.

compile_for_exec(Res,Term,Goal):-
   functs_to_preds0(Res,Term,Goal).

% --------------------------------
%    FUNCTS_TO_PREDS EXPLANATION
% --------------------------------

% functs_to_preds is a predicate that recursively converts a function to its equivalent predicate.
% It takes three arguments - RetResult, which will hold the result of the function evaluation,
% Convert, which is the function that needs to be converted, and Converted, which will hold the equivalent predicate.
% Example:
% ?- functs_to_preds(RetResult,is(pi+pi),Converted),write_src(Converted).
% Converted = (pi(_A), +(_A, _A, _B), _C is _B, eval(_C, _)).
%functs_to_preds(I,O):- functs_to_preds0(_,I,M),functs_to_preds1(M,O).
functs_to_preds(I,O):- functs_to_preds0(_,I,O).

join_into_body(Head,Body,Head,Body):- \+ compound(Head),!.
join_into_body((PreHead,True),Converted,Head,Body):- True==true,!,
  join_into_body(PreHead,Converted,Head,Body).
join_into_body((True,PreHead),Converted,Head,Body):- True==true,!,
  join_into_body(PreHead,Converted,Head,Body).
join_into_body(PreHead,(True,Converted),Head,Body):- True==true,!,
  join_into_body(PreHead,Converted,Head,Body).
join_into_body(PreHead,(Converted,True),Head,Body):- True==true,!,
  join_into_body(PreHead,Converted,Head,Body).
join_into_body((Pre,AsPredO),Converted,Head,Body):-
  join_into_body(Pre,(AsPredO,Converted),Head,Body).
join_into_body(Head,Body,Head,Body).
/*
% Recursive Case: If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
functs_to_preds1((AsPredI:-Convert), (AsPredO:-Converted)) :- !,
  must_det_ll((functs_to_preds1(AsPredI, AsPredO),
  functs_to_preds1(Convert, Converted))).
functs_to_preds1(Convert, Converted) :- compound(Convert),
    sub_term_from_last(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
    %writeln(sub_term_from_last(AsFunction)),
    callable(AsFunction), % Check if AsFunction is callable
    compound(AsFunction),
    (functor(AsFunction,F,A),AA is A+1,predicate_arity(F,AA)),
    is_function(AsFunction, Nth),  % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
    funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    functs_to_preds1((AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms
functs_to_preds1(Convert, Converted) :- compound(Convert),
    sub_term_from_last(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
    callable(AsFunction),  % Check if AsFunction is callable
    compound(AsFunction),
    %trace,
    is_function(AsFunction, Nth),  % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
    funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    functs_to_preds1((AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms
functs_to_preds1(Convert, Convert).
*/

% Base case: If Convert is a variable, the corresponding predicate is just eval(Convert, RetResult)
functs_to_preds0(RetResult,Convert, Converted) :-
     is_ftVar(Convert),  % Check if Convert is a variable
      must_det_ll((
    % Converted = eval(Convert, RetResult).  % Set Converted to eval(Convert, RetResult)
     RetResult=Convert,!, Converted = true)).

% Case: If Convert is an "is" function, we convert it to the equivalent "is" predicate.
functs_to_preds0(RetResult,is(Convert),(Converted,is(RetResult,Result))):- !,
   must_det_ll((functs_to_preds0(Result,Convert, Converted))).

% Case: If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
functs_to_preds0(RetResult,or(AsPredI,Convert), or(AsPredO, Converted)) :- !,
  must_det_ll((functs_to_preds0(RetResult,AsPredI, AsPredO),
  functs_to_preds0(RetResult,Convert, Converted))).

functs_to_preds0(RetResult,(AsPredI:-Convert), (Head:-Body)) :- !,
  must_det_ll((functs_to_preds0(RetResult,AsPredI, AsPredO),
               functs_to_preds0(RetResult,Convert, Converted),
               join_into_body(AsPredO,Converted,Head,Body))).

functs_to_preds0(RetResult,(AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_ll((functs_to_preds0(RetResult,AsPredI, AsPredO),
  functs_to_preds0(RetResult,Convert, Converted))).

% Case: If Convert is a "," (and) function, we convert it to the equivalent "," (and) predicate.
functs_to_preds0(RetResult,(AsPredI, Convert), (AsPredO, Converted)) :- !,
  must_det_ll((functs_to_preds0(_RetResult, AsPredI, AsPredO),
  functs_to_preds0(RetResult,Convert, Converted))).

% Case: If Convert is a list, we convert it to its predicate form recursively and then proceed with the conversion.
functs_to_preds0(RetResult,Convert, Converted) :- is_list(Convert),
   once(( s2p(Convert, IS), \+ is_list(IS))), !,  % Check if Convert is a list and not in predicate form
   must_det_ll((functs_to_preds0(RetResult, IS, Converted))).  % Proceed with the conversion of the predicate form of the list.

% Case: If Convert is a number or an atom, it is considered as already converted.
functs_to_preds0(RetResult, Convert, true) :-
    once(number(Convert); atom(Convert); data_term(Convert)),  % Check if Convert is a number or an atom
    ignore(RetResult = Convert),!.  % Set RetResult to Convert as it is already in predicate form

% Case: If Convert is of the form (AsFunction=AsBodyFn), we perform conversion recursively to obtain the equivalent predicate.
functs_to_preds0(RetResult,Convert, Converted) :-
     Convert = (AsFunction = AsBodyFn),!,
     must_det_ll((
     Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
     %funct_with_result_is_nth_of_pred(AsFunction, Result, _Nth, Head),
     functs_to_preds0(HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
        funct_with_result_is_nth_of_pred(AsFunction, Result, _Nth, Head)),
     verbose_unify(Convert),
     functs_to_preds0(Result,AsBodyFn,NextBody),
    % RetResult = Converted,
     RetResult = _,
     join_into_body(Head,NextBody,HeadC,NextBodyC),
     wdmsg([convert=Convert,join_into_body(Head,NextBody,HeadC,NextBodyC)]),
     ignore(Result = '$VAR'('HeadRes')))).

% Recursive Case: If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
functs_to_preds0(RetResult,Convert, Converted) :-
    sub_term_from_last(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
    callable(AsFunction),  % Check if AsFunction is callable
    is_function(AsFunction, Nth),  % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
    funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    functs_to_preds0(RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

% Case: If AsFunction is a recognized function, convert it to a predicate.
functs_to_preds0(RetResult,AsFunction,AsPred):-
   is_function(AsFunction, Nth),  % Check if AsFunction is a recognized function and get the position Nth where the result is stored/retrieved
   funct_with_result_is_nth_of_pred(AsFunction, RetResult, Nth, AsPred),
   \+ ( compound(AsFunction), arg(_,AsFunction, Arg), is_function(Arg,_)),!.

/* MAYBE USE ?*/
% Case: If Convert is a compound term, we need to recursively convert its arguments.
functs_to_preds0(RetResult, Convert, Converted) :- fail,
    compound(Convert), !,
    Convert =.. [Functor|Args],  % Deconstruct Convert to functor and arguments
    maplist(convert_argument, Args, ConvertedArgs),  % Recursively convert each argument
    Converted =.. [Functor|ConvertedArgs],  % Reconstruct Converted with the converted arguments
    (callable(Converted) -> functs_to_preds0(RetResult, Converted, _); true).  % If Converted is callable, proceed with its conversion

% The catch-all case: If no specific case is matched, consider Convert as already converted.
functs_to_preds0(_RetResult,Convert, Convert).

% Helper predicate to convert an argument of a compound term
convert_argument(Arg, ConvertedArg) :-
    (callable(Arg) -> functs_to_preds(_, Arg, ConvertedArg); ConvertedArg = Arg).


data_term(Convert):- self_eval(Convert),!.


% This predicate is responsible for converting functions to their equivalent predicates.
% It takes a function 'AsFunction' and determines the predicate 'AsPred' which will be
% equivalent to the given function, placing the result of the function at the 'Nth' position
% of the predicate arguments. The 'Result' will be used to store the result of the 'AsFunction'.
%
% It handles cases where 'AsFunction' is a variable and when it's an atom or a compound term.
% For compound terms, it decomposes them to get the functor and arguments and then reconstructs
% the equivalent predicate with the 'Result' at the 'Nth' position.
%
% Example:
% funct_with_result_is_nth_of_pred(+(1, 2), Result, 3, +(1, 2, Result)).

% Handles the case where AsFunction is a variable.
% It creates a compound term 'AsPred' and places the 'Result' at the 'Nth' position
% of the predicate arguments, and the 'AsFunction' represents the functional form with
% arguments excluding the result.
funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred) :-
    is_ftVar(AsFunction),!,
    compound(AsPred),
    compound_name_arguments(AsPred,F,PredArgs),
    nth1(Nth,PredArgs,Result,FuncArgs),
    compound_name_arguments(AsFunction,F,FuncArgs).

% Handles the case where 'AsFunction' is not a variable.
% It decomposes 'AsFunction' to get the functor and arguments (FuncArgs) of the function
% and then it constructs the equivalent predicate 'AsPred' with 'Result' at the 'Nth'
% position of the predicate arguments.
funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred) :-
   (atom(AsFunction)->AsFunction =.. [F | FuncArgs]; compound_name_arguments(AsFunction,F,FuncArgs)),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
    nth1(Nth, PredArgs, Result, FuncArgs), % It places 'Result' at the 'Nth' position
    AsPred =.. [F | PredArgs]. % It forms the predicate 'AsPred' by joining the functor with the modified arguments list.
funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred) :-
    nonvar(AsFunction),
    AsFunction =.. [F | FuncArgs],
    length(FuncArgs, Len),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
   ((number(Nth),Nth > Len + 1) -> throw(error(index_out_of_bounds, _)); true),
   (var(Nth)->(between(1,Len,From1),Nth is Len-From1+1);true),
    nth1(Nth,PredArgs,Result,FuncArgs),
    AsPred =.. [F | PredArgs].

% optionally remove next line
funct_with_result_is_nth_of_pred(AsFunction, _, _, _) :-
    var(AsFunction),
    throw(error(instantiation_error, _)).

% The remove_funct_arg/3 predicate is a utility predicate that removes
% the Nth argument from a predicate term, effectively converting a
% predicate to a function. The first argument is the input predicate term,
% the second is the position of the argument to be removed, and the third
% is the output function term.
remove_funct_arg(AsPred, Nth, AsFunction) :-
    % Decompose AsPred into its functor and arguments.
    AsPred =.. [F | PredArgs],
    % Remove the Nth element from PredArgs, getting the list FuncArgs.
    nth1(Nth,PredArgs,_Result,FuncArgs),
    % Construct AsFunction using the functor and the list FuncArgs.
    compound_name_arguments(AsFunction,F,FuncArgs).

% sub_term_from_last/2 predicate traverses through a given Term
% and finds a sub-term within it. The sub-term is unifiable with ST.
% This is a helper predicate used in conjunction with others to inspect
% and transform terms.

never_subterm(Term):- is_ftVar(Term).
never_subterm([]).
never_subterm('Nil').
%never_subterm(F):- atom(F),not_function(F,0).

sub_term_from_last(_, Term):- never_subterm(Term),!,fail.
sub_term_from_last(ST, Term):-
    % If Term is a list, it reverses the list and searches for a member
    % in the reversed list that is unifiable with ST.
    is_list(Term),!,rev_member(E,Term),sub_term_from_last(ST, E).
sub_term_from_last(ST, Term):-
    % If Term is a compound term, it gets its arguments and then recursively
    % searches in those arguments for a sub-term unifiable with ST.
    compound(Term), compound_name_arguments(Term,_,Args),sub_term_from_last(ST, Args).
sub_term_from_last(ST, ST):-
    % If ST is non-var, not an empty list, and callable, it unifies
    % ST with Term if it is unifiable.
    nonvar(ST), ST\==[], callable(ST).

% rev_member/2 predicate is a helper predicate used to find a member
% of a list. It is primarily used within sub_term_from_last/2 to
% traverse through lists and find sub-terms. It traverses the list
% from the end to the beginning, reversing the order of traversal.
rev_member(E,[_|L]):- rev_member(E,L).
rev_member(E,[E|_]).

% Continuing from preds_to_functs/2
% Converts a given predicate representation to its equivalent function representation
preds_to_functs(Convert, Converted):-
  % Verbose_unify/1 here may be used for debugging or to display detailed unification information
  verbose_unify(Convert),
  % Calls the auxiliary predicate preds_to_functs0/2 to perform the actual conversion
  preds_to_functs0(Convert, Converted).

% Base case: if Convert is a variable, Converted will be the same variable
preds_to_functs0(Convert, Converted) :-
    is_ftVar(Convert), !,
    Converted = Convert.

% Converts the rule (Head :- Body) to its function equivalent
preds_to_functs0((Head:-Body), Converted) :- !,
  % The rule is converted by transforming Head to a function AsFunction and the Body to ConvertedBody
 (
   pred_to_funct(Head, AsFunction, Result),
   %ignore(Result = '$VAR'('HeadRes')),
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   compound(BE),arg(Nth,BE,ArgRes),sub_var(Result,ArgRes),
   remove_funct_arg(BE, Nth, AsBodyFunction),
   append(Left,[eval(AsBodyFunction,Result)|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody),
   preds_to_functs0(NewBody,ConvertedBody),
   % The final Converted term is constructed
   (Converted = (AsFunction=ConvertedBody))).

% Handles the case where Convert is a conjunction, and AsPred is not not_function.
% It converts predicates to functions inside a conjunction
preds_to_functs0((AsPred, Convert), Converted) :-
    \+ not_function(AsPred),
    pred_to_funct(AsPred, AsFunction, Result),
    sub_var(Result, Convert), !,
    % The function equivalent of AsPred replaces Result in Convert
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs0(Converting, Converted).

% Handles the special case where eval/2 is used and returns the function represented by the first argument of eval/2
preds_to_functs0(eval(AsFunction, _Result), AsFunction) :- !.

% Handles the general case where Convert is a conjunction.
% It converts the predicates to functions inside a conjunction
preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

% Handles the case where AsPred is a compound term that can be converted to a function
preds_to_functs0(AsPred, eval(AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.

% Base case: any other term remains unchanged
preds_to_functs0(X, X).

% Converts a given predicate AsPred to its equivalent function term AsFunction
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), % Checks if AsPred is a compound term
    functor(AsPred, F, A), % Retrieves the functor F and arity A of AsPred
    functional_predicate_arg(F, A, Nth),!, % Finds the Nth argument where the result should be
    arg(Nth, AsPred, Result), % Retrieves the result from the Nth argument of AsPred
    remove_funct_arg(AsPred, Nth, AsFunction). % Constructs the function AsFunction by removing the Nth argument from AsPred

% If not found in functional_predicate_arg/3, it tries to construct AsFunction by removing the last argument from AsPred
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    functor(AsPred, _, Nth),
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

% body_member/4 is utility predicate to handle manipulation of body elements in the clause, but the exact implementation details and usage are not provided in the given code.
body_member(Body,BE,NewBE,NewBody):-
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   append(Left,[NewBE|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody).



% p2m/2 is a translation utility to convert Prolog constructs to MeTTa constructs.
% It handles a variety of cases, including different types of compound terms,
% control structures, and predicate definitions.
% The first argument is the input in Prolog syntax,
% and the second argument is the output converted to MeTTa syntax.
p2m(NC, NC) :- var(NC), !.  % If NC is a variable, do not translate.
p2m(NC, NC) :- is_ftVar(NC), !.  % If NC is a free term variable, do not translate.
% Conversion for lists
p2m([], 'Nil'). % empty list
p2m([H|T], 'Cons'(OH, OT)):- p2m(H, OH), p2m(T, OT).
p2m(NC, OO) :-
    % If NC is a list, map each element of the list from Prolog to MeTTa
    is_list(NC),!,
    maplist(p2m, NC, OO).
p2m(!, '!').  % Translate the cut operation directly.
p2m(fail, 'False').  % Translate Prolog’s fail to MeTTa’s False.
p2m(true, 'True').  % Translate Prolog’s true to MeTTa’s True.
p2m(prolog, meTTa).  % Translate the atom prolog to meTTa.

p2m('[|]','Cons').
p2m(( ';' ),or).
p2m(( ',' ),and).
p2m(( '\\+' ),unless).
%p2m(( ':-' ),entailed_by).
p2m('=..','atom_2_list').

% Conversion for any atomic term
p2m(A, A):- atomic(A).

p2m(NC,NC):- \+ compound(NC),!.
p2m(NC,[F]):- compound_name_arity(NC,F,0),!.

% Conversion for the negation as failure
p2m((\+ A), O):- !, p2m(not(A), O).

% Conversion for arithmetic evaluation
%p2m(is(A, B), O):- !, p2m(eval(B, A), O).
%p2m(is(V,Expr),let(V,Expr,'True')).
p2m((Head:-Body),O):- Body == true,!, O = (=(Head,'True')).
p2m((Head:-Body),O):- Body == fail,!, O = (=(Head,'False')).
p2m((Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(List,SP),!,O=(=(Head,SP)).

p2m((G,E),O):- conjuncts_to_list((G,E),List),into_sequential(List,O),!.
% Conversion for if-then-else constructs
p2m((A->B;C),O):- !, p2m(if_then_else(A,B,C),O).
p2m((A;B),O):- !, p2m(or(A,B),O).
p2m((A*->B;C),O):- !, p2m(each_then_otherwise(A,B,C),O).
p2m((A->B),O):- !, p2m(if_then(A,B),O).
p2m((A*->B),O):- !, p2m(each_then(A,B),O).
p2m(metta_defn(Self,Eq,H,B),'add-atom'(Self,[Eq,H,B])).
p2m(metta_type,'add-atom').
p2m(metta_atom,'add-atom').
p2m(retractall(X),'remove-all-atoms'('&self',X)).
p2m(clause(H,B),'get-atoms'('&self',[=,H,B])).
p2m(retract(X),'remove-atom'('&self',X)).
p2m(assert(X),'add-atom'('&self',X)).
% The catch-all case for the other compound terms.
% It will break down compound terms into their functor and arguments and apply p2m recursively
p2m(I, O):-
    compound(I),
    I =.. [F|II], % univ operator to convert between a term and a list consisting of functor name and arguments
    maplist(p2m, II, OO), % applying p2m recursively on each argument of the compound term
    O =.. [F|OO]. % constructing the output term with the converted arguments

% In the context of this conversion predicate, each branch of the p2m predicate
% is handling a different type or structure of term, translating it into its
% equivalent representation in another logic programming language named MeTTa.
% The actual transformations are dependent on the correspondence between Prolog
% constructs and MeTTa constructs, as defined by the specific implementations
% of Prolog and MeTTa being used.
prolog_to_metta(V, D) :-
    % Perform the translation from Prolog to MeTTa
    p2m(V, D),!.


% Define predicates to support the transformation from Prolog to MeTTa syntax
% (Continuing the translation from Prolog to MeTTa syntax as per the given code)
% Handle the case where the body is a conjunction of terms
into_sequential(Body, SP) :-
    % Check if Body is not a list and convert conjunctions in Body to a list of conjuncts.
    \+ is_list(Body), 
    conjuncts_to_list(Body, List), % Converts a list of conjunctions into a sequential representation in MeTTa
    into_sequential(List, SP), !.
into_sequential(Nothing,'True'):- Nothing ==[],!.
 % If there's only one element
into_sequential([SP],O):- prolog_to_metta(SP,O).
% Otherwise, construct sequential representation using 'and'.
into_sequential(List, ['and'|SPList]) :- maplist(prolog_to_metta, List, SPList),!.



% combine_clauses/3 is the main predicate combining clauses with similar heads and bodies.
% HeadBodiesList is a list of clauses (Head:-Body)
% NewHead will be the generalized head representing all clauses in HeadBodiesList
% NewCombinedBodies will be the combined bodies of all clauses in HeadBodiesList.
combine_clauses(HeadBodiesList, NewHead, NewCombinedBodies) :-
    % If HeadBodiesList is empty, then NewCombinedBodies is 'false' and NewHead is an anonymous variable.
    (HeadBodiesList = [] -> NewCombinedBodies = false, NewHead = _ ;
    % Find all Heads in HeadBodiesList and collect them in the list Heads
    findall(Head, member((Head:-_), HeadBodiesList), Heads),
    % Find the least general head among the collected Heads
    least_general_head(Heads, LeastHead),
    functor(LeastHead,F,A),functor(NewHead,F,A),
    % Transform and combine bodies according to the new head found
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)),
    \+ \+ (
     Print=[converting=HeadBodiesList,newHead=NewHead],
     numbervars(Print,0,_,[]),wdmsg(Print),in_cmt(portray_clause(( NewHead :- NewCombinedBodies)))),!.

% Predicate to find the least general unified head (LGU) among the given list of heads.
% Heads is a list of head terms, and LeastGeneralHead is the least general term that unifies all terms in Heads.
least_general_head(Heads, LeastGeneralHead) :-
    lgu(Heads, LeastGeneralHead).

% Base case: the LGU of a single head is the head itself.
lgu([Head], Head) :- !.
% Recursive case: find the LGU of the head and the rest of the list.
lgu([H1|T], LGU) :-
    lgu(T, TempLGU),
    % Find generalization between head H1 and temporary LGU
    generalization(H1, TempLGU, LGU).

% generalization/3 finds the generalization of two heads, Head1 and Head2, which is represented by GeneralizedHead.
% This predicate is conceptual and will require more complex processing depending on the actual structures of the heads.
generalization(Head1, Head2, GeneralizedHead) :-
    % Ensure the functor names and arities are the same between Head1 and Head2.
    functor(Head1, Name, Arity),
    functor(Head2, Name, Arity),
    functor(GeneralizedHead, Name, Arity),
    % Generalize the arguments of the heads.
    generalize_args(Arity, Head1, Head2, GeneralizedHead).

% Base case: no more arguments to generalize.
generalize_args(0, _, _, _) :- !.
% Recursive case: generalize the corresponding arguments of the heads.
generalize_args(N, Head1, Head2, GeneralizedHead) :-
    arg(N, Head1, Arg1),
    arg(N, Head2, Arg2),
    % If the corresponding arguments are equal, use them. Otherwise, create a new variable.
    (Arg1 = Arg2 -> arg(N, GeneralizedHead, Arg1); arg(N, GeneralizedHead, _)),
    % Continue with the next argument.
    N1 is N - 1,
    generalize_args(N1, Head1, Head2, GeneralizedHead).

% transform_and_combine_bodies/3 takes a list of clause heads and bodies, a new head, and produces a combined body representing all the original bodies.
% The new body is created according to the transformations required by the new head.
transform_and_combine_bodies([(Head:-Body)|T], NewHead, CombinedBodies) :-
    % Transform the body according to the new head.
    transform(Head, NewHead, Body, TransformedBody),
    % Combine the transformed body with the rest.
    combine_bodies(T, NewHead, TransformedBody, CombinedBodies).

/* OLD
% Define predicate combine_clauses to merge multiple Prolog clauses with the same head.
% It receives a list of clauses as input and returns a combined clause.
combine_clauses([Clause], Clause) :- !.  % If there's only one clause, return it as is.
combine_clauses(Clauses, (Head :- Body)) :-  % If there are multiple clauses, combine them.
    Clauses = [(Head :- FirstBody)|RestClauses],  % Decompose the list into the first clause and the rest.
    combine_bodies(RestClauses, FirstBody, Body).  % Combine the bodies of all the clauses.

% Helper predicate to combine the bodies of a list of clauses.
% The base case is when there are no more clauses to combine; the combined body is the current body.
combine_bodies([], Body, Body).
combine_bodies([(Head :- CurrentBody)|RestClauses], PrevBody, Body) :-
    % Combine the current body with the previous body using a conjunction (,).
    combine_two_bodies(PrevBody, CurrentBody, CombinedBody),
    % Recursively combine the rest of the bodies.
    combine_bodies(RestClauses, CombinedBody, Body).

% Predicate to combine two bodies.
% Handles the combination of different Prolog constructs like conjunctions, disjunctions, etc.
combine_two_bodies((A, B), (C, D), (A, B, C, D)) :- !.  % Combine conjunctions.
combine_two_bodies((A; B), (C; D), (A; B; C; D)) :- !.  % Combine disjunctions.
combine_two_bodies(A, B, (A, B)).  % Combine simple terms using conjunction.
*/

% Base case: if there are no more bodies, the accumulated Combined is the final CombinedBodies.
combine_bodies([], _, Combined, Combined).
% Recursive case: combine the transformed body with the accumulated bodies.
combine_bodies([(Head:-Body)|T], NewHead, Acc, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    % Create a disjunction between the accumulated bodies and the transformed body.
    NewAcc = (Acc;TransformedBody),
    combine_bodies(T, NewHead, NewAcc, CombinedBodies).

% combine_code/3 combines Guard and Body to produce either Guard, Body, or a conjunction of both, depending on the values of Guard and Body.
combine_code(Guard, Body, Guard) :- Body==true, !.
combine_code(Guard, Body, Body) :- Guard==true, !.
combine_code(Guard, Body, (Guard, Body)).

% create_unifier/3 creates a unification code that unifies OneHead with NewHead.
% If OneHead and NewHead are structurally equal, then they are unified and the unification Guard is 'true'.
% Otherwise, the unification code is 'unify_with_occurs_check(OneHead,NewHead)'.

create_unifier(OneHead,NewHead,Guard):- OneHead=@=NewHead,OneHead=NewHead,!,Guard=true.
create_unifier(OneHead,NewHead,Guard):- compound(OneHead),
  compound_name_arguments(OneHead,_,Args1),
  compound_name_arguments(NewHead,_,Args2),
  create_unifier_goals(Args1,Args2,Guard),!.
create_unifier(OneHead,NewHead,u(OneHead,NewHead)).

create_unifier_goals([V1],[V2],u(V1,V2)):-!.
create_unifier_goals([V1|Args1],[V2|Args2],RightGuard):-!,
  create_unifier_goals(Args1,Args2,Guard),
  combine_code(u(V1,V2),Guard,RightGuard).
create_unifier_goals([],[],true).


% transform/4 combines unification code with Body to produce NewBody according to the transformations required by NewHead.
% It uses create_unifier/3 to generate the unification code between OneHead and NewHead.
transform(OneHead, NewHead, Body, NewBody):- create_unifier(OneHead,NewHead,Guard),
   combine_code(Guard,Body,NewBody).



% ===============================
%    TESTING
% ===============================
% Define 'fb', a rule that calls 'make' and writes information for each clause of 'fb0'.
% 'make' compiles the program.
% The 'forall' loop will write and call all goals of the 'fb0' clauses.


fb:- make,
   writeln(';; ==========================================='),
   forall((clause(fb0,Goal),write(';; '),writeq(?- Goal),nl,call(Goal)),
   writeln(';; ===========================================')).

% The 'fb0' rule showing mettalog sources with specific definitions.
fb0:- show_mettalog_src((two_pi(R):-(pi(A), +(A, A, B), R is B))).
fb0:- show_mettalog_src(factorial_tail_basic).
fb0:- show_mettalog_src(funct).

% 'show_mettalog_src' rule compiles the program and shows mettalog sources for each source file containing 'metta'.
show_mettalog_src:- make,
  forall((source_file(AsPred,File),
          symbol_contains(File,metta)),
         show_mettalog_src(AsPred)).


% Handling different cases for 'show_mettalog_src' with different input parameters.
% These rules use nonvar, current_predicate, and other built-ins to perform various checks and actions
% based on the type and value of the input to 'show_mettalog_src'.
show_mettalog_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_mettalog_src(F,A)).
show_mettalog_src(C):- show_cvts(C),!.

% The 'show_space_src' rules compile the program and show space sources for each space predicate.
show_space_src:- make,
  forall(space_preds(AsPred),show_space_src(AsPred)).


% Similar to the 'show_mettalog_src' rules, these rules handle different cases for 'show_space_src'
% with different input parameters and perform various checks and actions based on the type and value of the input.
show_space_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_space_src(F,A)).
show_space_src(C):- show_cvts(C),!.

% 'show_cvts' rule processes a term, performing different actions based on the structure of the term.
show_cvts(Term):-
  once((is_list(Term), s2p(Term,PF))), \+ is_list(PF),!,show_cvts(PF).

% 'show_cvts' continues processing, performing conversions between predicates and functions,
% and pretty-printing original terms, function forms, and Prolog forms.
show_cvts(Term):- compound(Term),Term=(_=_),!, ppc(orig,Term),Term = FunctForm,
  functs_to_preds(_RetResult,FunctForm,Prolog), ppc(preds,Prolog),
  preds_to_functs(Prolog,NFunctForm), ppc(functs,NFunctForm).
show_cvts(Term):- ppc(orig,Term),
  preds_to_functs(Term,FunctForm), ppc(functs,FunctForm),
  functs_to_preds(_RetResult,FunctForm,Prolog), ppc(preds,Prolog).

% 'show_mettalog_src' for specific predicate, prints metta clauses if they exist in the source file containing 'metta'.
show_mettalog_src(F,A):- functor(Head,F,A),
  ignore((predicate_property(Head,number_of_clauses(_)),
    source_file(Head,File),atom_contains(File,metta),!,
    nl,findall((Head:-Body),
       clause(Head,Body), Clauses),
    print_metta_clauses(Clauses))),nl.

% 'print_metta_clauses' rule is handling the printing of metta clauses.
% It checks the form of the input clauses and calls 'print_metta_clause' accordingly.
print_metta_clauses([]):- !.
print_metta_clauses([Head:-Body]):- !, print_metta_clause(Head,Body).
print_metta_clauses(Clauses):- combine_clauses(Clauses,Head,Body),!,print_metta_clause(Head,Body).
print_metta_clause(Head,Body):-
  print_metta_clause0(Head,Body),
  show_cvts(Head:-Body).

% 'print_metta_clause0' rule prints metta clauses based on the body.
% It transforms the body to a list, if needed, and prints it in a sequential form.
print_metta_clause0(Head,Body):- Body == true,!, pp_metta([=,Head,'True']).
print_metta_clause0(Head,Body):- Body == false,!, pp_metta([=,Head,'False']).
print_metta_clause0(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP), pp_metta([=,Head,SP]).



% ===============================
%       PRINTERS
% ===============================
% 'ppc' and 'ppc1' rules pretty-print original terms and convert them to metta if different,
% printing the converted forms as well.
ppc(Msg,Term):- ppc1(Msg,Term), p2m(Term,MeTTa),!, (MeTTa\==Term -> ppc1(p2m(Msg),MeTTa) ; true).

ppc1(Msg,Term):- \+ \+ ( ppct(Msg,Term) ),!.

ppc1(Msg,Term):- \+ \+ ( ignore(guess_pretty(Term)),

                         writeln('---------------------'),
                         write(p(Msg)),write(':'),nl,
   portray_clause(Term),
   writeln('---------------------'),
   \+ \+ (print_tree(?-show_cvts(Term))),nl,
    writeln('---------------------'),
     write(s(Msg)),write(':'),nl,
     write_src(Term),nl).

ppct(Msg,Term):- is_list(Term),!,
  writeln('---------------------'),
  numbervars(Term,666,_,[attvar(bind)]),
  write((Msg)),write(':'),nl,
  write_src(Term),nl.
ppct(Msg,Term):- Term=(_ :- _),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  portray_clause(Term),nl.
ppct(Msg,Term):- Term=(_=_),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  numbervars(Term,444,_,[attvar(bind)]),
  write_src(Term),nl.
ppct(Msg,Term):- Term=(_ :- _),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  numbervars(Term,222,_,[attvar(bind)]),
  print_tree(Term),nl.

% 'pp_metta' rule is responsible for pretty-printing metta terms.
pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

% The predicate with_indents/2 modifies the src_indents option value during the execution of a goal.
% The first argument is the desired value for src_indents,
% and the second argument is the Goal to be executed with the given src_indents setting.
with_indents(TF, Goal) :-
    % Set the value of the `src_indents` option to TF and then execute the Goal
    with_option(src_indents, TF, Goal).

% The predicate allow_concepts/0 checks whether the use of concepts is allowed.
% It does this by checking the value of the concepts option and ensuring it is not false.
allow_concepts :-
    % Check if the option `concepts` is not set to false
    option_else(concepts, TF, true),
    \+ TF == false.

% The predicate with_concepts/2 enables or disables the use of concepts during the execution of a given goal.
% The first argument is a Boolean indicating whether to enable (true) or disable (false) concepts.
% The second argument is the Goal to be executed with the given concepts setting.
with_concepts(TF, Goal) :-
    % Set the value of the `concepts` option to TF and then execute the Goal
    with_option(concepts, TF, Goal).


% Various 'write_src' and 'write_src0' rules are handling the writing of the source,
% dealing with different types of values, whether they are lists, atoms, numbers, strings, compounds, or symbols.
write_src(V):- notrace(write_src0(V)).
write_src0(V):- allow_concepts,!,with_concepts(false,write_src1(V)),flush_output.
write_src0(V):- is_list(V),!,pp_sexi(V).
write_src0(V):- write_src1(V),!.

% Handling the final write when the value is a variable or a '$VAR' structure.
is_final_write(V):- var(V), !, format('$~p',[V]).
is_final_write('$VAR'(S)):- !, write('$'),write(S).

% Handling more cases for 'write_src1', when the value is a number, a string, a symbol, or a compound.
write_src1(V) :- is_final_write(V),!.
write_src1((Head:-Body)) :- !, print_metta_clause0(Head,Body).
write_src1(''):- !, writeq('').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).

% Continuing with 'write_src1', 'write_mobj', and related rules,
% handling different cases based on the value’s type and structure, and performing the appropriate writing action.
write_src1(V):- symbol(V),needs_quoted_in_metta(V,_),!, symbol_string(V,S),writeq(S).
write_src1(V):- symbol(V),!,write(V).
write_src1(V):- compound(V), \+ is_list(V),!,write_mobj(V).
write_src1(V):- pp_sex(V),!.

write_mobj(V) :- is_final_write(V),!.
write_mobj(V):- ( \+ compound(V) ; is_list(V)),!, write_src0(V).

write_mobj(V):- compound_name_arguments(V,F,Args),write_mobj(F,Args),!.
write_mobj(V):- writeq(V).
write_mobj(exec,[V]):- !, write('!'),with_indents(false,write_src(V)).
write_mobj('$STRING',[S]):- !, writeq(S).
write_mobj(F,Args):- mlog_sym(K),pp_sexi([K,F|Args]).

% Rules for determining when a symbol needs to be quoted in metta.
needs_quoted_in_metta(H,_):- upcase_atom(H,U),downcase_atom(H,U),!,fail.
needs_quoted_in_metta('','"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V," ").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"/").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,'"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V,'"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V,',').
%needs_quoted_in_metta(V,"'"):- symbol_length(V,L),L==1.
%needs_quoted_in_metta(V,"'"):- symbol_contains(V,")").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"|").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"'").


