
% ============================
% %%%% Arithmetic Operations
% ============================
% Addition
'+'(Addend1, Addend2, Sum):- eval_args(['+', Addend1, Addend2], Sum).
% Subtraction
'-'(Minuend, Subtrahend, Difference):- eval_args(['-', Minuend, Subtrahend], Difference).
% Multiplication
'*'(Factor1, Factor2, Product):- eval_args(['*', Factor1, Factor2], Product).
% Division
'/'(Dividend, Divisor, Quotient):- eval_args(['/', Dividend, Divisor], Quotient).
% Modulus
'mod'(Dividend, Divisor, Remainder):- eval_args(['mod', Dividend, Divisor], Remainder).
% Exponentiation
'exp'(Base, Exponent, Result):- eval_args(['exp', Base, Exponent], Result).
% Square Root
'sqrt'(Number, Root):- eval_args(['sqrt', Number], Root).

% ============================
% %%%% List Operations
% ============================
% Retrieve Head of the List
'car-atom'(List, Head):- eval_args(['car-atom', List], Head).
% Retrieve Tail of the List
'cdr-atom'(List, Tail):- eval_args(['cdr-atom', List], Tail).
% Construct a List
'Cons'(Element, List, 'Cons'(Element, List)):- !.
% Collapse List
'collapse'(List, CollapsedList):- eval_args(['collapse', List], CollapsedList).
% Count Elements in List
'CountElement'(List, Count):- eval_args(['CountElement', List], Count).
% Find Length of List
%'length'(List, Length):- eval_args(['length', List], Length).

% ============================
% %%%% Nondet Opteration
% ============================
% Superpose a List
'superpose'(List, SuperposedList):- eval_args(['superpose', List], SuperposedList).

% ============================
% %%%% Testing
% ============================

% `assertEqual` Predicate
% This predicate is used for asserting that the Expected value is equal to the Actual value.
% Expected: The value that is expected.
% Actual: The value that is being checked against the Expected value.
% Result: The result of the evaluation of the equality.
% Example: `assertEqual(5, 5, Result).` would succeed, setting Result to true (or some success indicator).
'assertEqual'(Expected, Actual, Result):- eval_args(['assertEqual', Expected, Actual], Result).

% `assertEqualToResult` Predicate
% This predicate asserts that the Expected value is equal to the Result of evaluating Actual.
% Expected: The value that is expected.
% Actual: The expression whose evaluation is being checked against the Expected value.
% Result: The result of the evaluation of the equality.
% Example: If Actual evaluates to the Expected value, this would succeed, setting Result to true (or some success indicator).
'assertEqualToResult'(Expected, Actual, Result):- eval_args(['assertEqualToResult', Expected, Actual], Result).

% `assertFalse` Predicate
% This predicate is used to assert that the evaluation of EvalThis is false.
% EvalThis: The expression that is being evaluated and checked for falsehood.
% Result: The result of the evaluation.
% Example: `assertFalse((1 > 2), Result).` would succeed, setting Result to true (or some success indicator), as 1 > 2 is false.
'assertFalse'(EvalThis, Result):- eval_args(['assertFalse', EvalThis], Result).

% `assertNotEqual` Predicate
% This predicate asserts that the Expected value is not equal to the Actual value.
% Expected: The value that is expected not to match the Actual value.
% Actual: The value that is being checked against the Expected value.
% Result: The result of the evaluation of the inequality.
% Example: `assertNotEqual(5, 6, Result).` would succeed, setting Result to true (or some success indicator).
'assertNotEqual'(Expected, Actual, Result):- eval_args(['assertNotEqual', Expected, Actual], Result).

% `assertTrue` Predicate
% This predicate is used to assert that the evaluation of EvalThis is true.
% EvalThis: The expression that is being evaluated and checked for truth.
% Result: The result of the evaluation.
% Example: `assertTrue((2 > 1), Result).` would succeed, setting Result to true (or some success indicator), as 2 > 1 is true.
'assertTrue'(EvalThis, Result):- eval_args(['assertTrue', EvalThis], Result).

% `rtrace` Predicate
% This predicate is likely used for debugging; possibly for tracing the evaluation of Condition.
% Condition: The condition/expression being traced.
% EvalResult: The result of the evaluation of Condition.
% Example: `rtrace((2 + 2), EvalResult).` would trace the evaluation of 2 + 2 and store its result in EvalResult.
'rtrace'(Condition, EvalResult):- eval_args(['rtrace', Condition], EvalResult).

% `time` Predicate
% This predicate is used to measure the time taken to evaluate EvalThis.
% EvalThis: The expression whose evaluation time is being measured.
% EvalResult: The result of the evaluation of EvalThis.
% Example: `time((factorial(5)), EvalResult).` would measure the time taken to evaluate factorial(5) and store its result in EvalResult.
'time'(EvalThis, EvalResult):- eval_args(['time', EvalThis], EvalResult).

% ============================
% %%%% Debugging, Printing and Utility Operations
% ============================
% REPL Evaluation
'repl!'(EvalResult):- eval_args(['repl!'], EvalResult).
% Condition Evaluation
'!'(Condition, EvalResult):- eval_args(['!', Condition], EvalResult).
% Import File into Environment
'import!'(Environment, FileName, Namespace):- eval_args(['import!', Environment, FileName], Namespace).
% Evaluate Expression with Pragma
'pragma!'(Environment, Expression, EvalValue):- eval_args(['pragma!', Environment, Expression], EvalValue).
% Print Message to Console
'print'(Message, EvalResult):- eval_args(['print', Message], EvalResult).
% No Operation, Returns EvalResult unchanged
'nop'(Expression, EvalResult):- eval_args(['nop', Expression], EvalResult).

% ============================
% %%%% Variable Bindings
% ============================
% Bind Variables
'bind!'(Environment, Variable, Value):- eval_args(['bind!', Environment, Variable], Value).
% Let binding for single variable
'let'(Variable, Expression, Body, Result):- eval_args(['let', Variable, Expression, Body], Result).
% Sequential let binding
'let*'(Bindings, Body, Result):- eval_args(['let*', Bindings, Body], Result).

% ============================
% %%%% Pattern Matching
% ============================
% Pattern Matching with an else branch
'match'(Environment, Pattern, Template, ElseBranch, Result):- eval_args(['match', Environment, Pattern, Template, ElseBranch], Result).
% Pattern Matching without an else branch
'match'(Environment, Pattern, Template, Result):- eval_args(['match', Environment, Pattern, Template], Result).

% ============================
% %%%% Atom Manipulations
% ============================

% ============================
% %%%% Reflection
% ============================
% Get Type of Value
'get-type'(Value, Type):- eval_args(['get-type', Value], Type).






self_eval(X):- var(X),!.
self_eval(X):- is_valid_nb_state(X),!.
self_eval(X):- string(X),!.
self_eval(X):- number(X),!.
self_eval([]).
self_eval(X):- is_list(X),!,fail.
%self_eval(X):- compound(X),!.
%self_eval(X):- is_ref(X),!,fail.
self_eval(X):- atom(X),!, \+ nb_current(X,_),!.
self_eval('True'). self_eval('False').


:- nb_setval(self_space, '&self').
evals_to(XX,Y):- Y==XX,!.   evals_to(XX,Y):- Y=='True',!, XX\=='False'.

eval_args(A,AA):-
  nb_current(self_space,Space),
  eval_args(11,Space,A,AA).

%eval_args(Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval_args(X)),fail.

eval_args(_Dpth,_Slf,X,Y):- nonvar(Y),X=Y,!.

eval_args(Depth,Self,X,Y):- nonvar(Y),!,eval_args(Depth,Self,X,XX),evals_to(XX,Y).

eval_args(_Dpth,_Slf,[X|T],Y):- T==[], number(X),!,Y=[X].

eval_args(Depth,Self,X,Y):-
  mnotrace((no_repeats_var(YY),
  D1 is Depth-1)),
  eval_args0(D1,Self,X,Y),
   mnotrace(( \+ (Y\=YY))).


debugging_metta(G):-debugging(metta(eval))->ignore(G);true.


:- nodebug(metta(eval)).


w_indent(Depth,Goal):-
  \+ \+ mnotrace(ignore(((
    format('~N'),
    setup_call_cleanup(forall(between(Depth,101,_),write('  ')),Goal, format('~N')))))).
indentq(Depth,Term):-
  \+ \+ mnotrace(ignore(((
    format('~N'),
    setup_call_cleanup(forall(between(Depth,101,_),write('  ')),format('~q',[Term]),
    format('~N')))))).


with_debug(Flag,Goal):- debugging(Flag),!, call(Goal).
with_debug(Flag,Goal):- flag(eval_num,_,0),
  setup_call_cleanup(debug(Flag),call(Goal), nodebug(Flag)).

if_trace(Flag,Goal):- catch(ignore((debugging(Flag),Goal)),_,true).


trace_on_fail:- option_else('trace-on-fail',TF,'True'), TF=='True'.
trace_on_overflow:- option_else('trace-on-overflow',TF,'True'), TF=='True'.
trace_on_pass:- option_else('trace-on-overflow',TF,'True'), TF=='True'.

eval_args0(Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ trace_on_overflow-> true; flag(eval_num,_,0),debug(metta(eval))).
eval_args0(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args0(Depth,Self,X,Y):-
  Depth2 is Depth-1,
  eval_args11(Depth,Self,X,M),
  (M\=@=X ->eval_args0(Depth2,Self,M,Y);Y=X).



eval_args11(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

eval_args11(Depth,Self,X,Y):- \+ debugging(metta(eval)),!, eval_args1(Depth,Self,X,Y).
eval_args11(Depth,Self,X,Y):- flag(eval_num,EX,EX+1),
  option_else(traclen,Max,100),
  (EX>Max->(nodebug(metta(eval)),write('Switched off tracing. For a longer trace !(pragma! tracelen 101))'));true),
  mnotrace((no_repeats_var(YY), D1 is Depth-1)),
  DR is 99-D1,
  if_trace(metta(eval),indentq(Depth,'-->'(EX,Self,X,depth(DR)))),
  Ret=retval(fail),
  call_cleanup((
    eval_args1(D1,Self,X,Y),
    mnotrace(( \+ (Y\=YY), nb_setarg(1,Ret,Y)))),
    mnotrace(ignore(((Y\=@=X,if_trace(metta(eval),indentq(Depth,'<--'(EX,Ret)))))))),
  (Ret\=@=retval(fail)->true;(rtrace(eval_args0(D1,Self,X,Y)),fail)).


:- discontiguous eval_args1/4.
:- discontiguous eval_args2/4.

eval_args1(_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.

eval_args1(Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval_args(Depth,Self,VI,VM),
  ( VM\==VI -> eval_args(Depth,Self,[V|VM],VVO) ;
    (eval_args(Depth,Self,V,VV), (V\==VV -> eval_args(Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_args1(_Dpth,_Slf,X,Y):- \+ is_list(X),!,Y=X.
eval_args1(_Dpth,_Slf,List,Y):- maplist(self_eval,List),List=[H|_], \+ atom(H), !,Y=List.

eval_args1(Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval_args(Depth,Self),VI,VO).


eval_args1(_Dpth,_Slf,['repl!'],'True'):- !, repl.
eval_args1(Depth,Self,['!',Cond],Res):- !, call(eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['rtrace',Cond],Res):- !, rtrace(eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['time',Cond],Res):- !, time(eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['print',Cond],Res):- !, eval_args(Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
eval_args1(Depth,Self,['assertTrue', X],TF):- !, eval_args(Depth,Self,['assertEqual',X,'True'],TF).
eval_args1(Depth,Self,['assertFalse',X],TF):- !, eval_args(Depth,Self,['assertEqual',X,'False'],TF).

eval_args1(Depth,Self,['assertEqual',X,Y],TF):- !,
   loonit_assert_source_tf(
        ['assertEqual',X,Y],
        (setof_eval(Depth,Self,X,XX), setof_eval(Depth,Self,Y,YY)),
         equal_enough(XX,YY), TF).

eval_args1(Depth,Self,['assertNotEqual',X,Y],TF):- !,
   loonit_assert_source_tf(
        ['assertEqual',X,Y],
        (setof_eval(Depth,Self,X,XX), setof_eval(Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF).

eval_args1(Depth,Self,['assertEqualToResult',X,Y],TF):- !,
   loonit_assert_source_tf(
        ['assertEqualToResult',X,Y],
        (setof_eval(Depth,Self,X,XX), sort(Y,YY)),
         equal_enough(XX,YY), TF).


loonit_assert_source_tf(Src,Goal,Check,TF):-
   copy_term(Goal,OrigGoal),
   loonit_asserts(Src, Goal, Check),
   as_tf(Check,TF),!,
  ignore((
          once((TF='True', trace_on_pass);(TF='False', trace_on_fail)),
     with_debug(metta(eval),OrigGoal))).


equal_enough(R,V):- R=@=V, !.
equal_enough(R,V):- number(R),number(V),!, RV is abs(R-V), RV < 0.03 .
equal_enough(R,V):- (\+ compound(R) ; \+ compound(V)),!, R==V.
equal_enough([R|RT],[V|VT]):- !, equal_enough(R,V),equal_enough(RT,VT).
equal_enough(R,V):-
  compound_name_arguments(R,F,RA),
  compound_name_arguments(V,F,VA), !,
  maplist(equal_enough,RA,VA).




eval_args1(Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Depth,Space,Goal).
eval_args1(Depth,Self,['match',Other,Goal,Template,Else],Template):- into_space(Self,Other,Space),!,  (metta_atom_iter(Depth,Space,Goal)*->true;Else=Template).

% Macro: case
eval_args1(Depth,Self,X,Res):-
   X= [CaseSym,A,CL],CaseSym == 'case', !,
   into_case_list(CL,CASES),
   findall(Key-Value,
     (nth0(Nth,CASES,Case0),
       (is_case(Key,Case0,Value),
        debug_only((format('~N'),writeqln(c(Nth,Key)=Value))))),KVs),!,
   ((eval_args(Depth,Self,A,AA),debug_only((writeqln(switch=AA))),
    (select_case(Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    eval_args(Depth,Self,Value,Res).

  select_case(Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Depth,Self,Cases,CasES),
       (best_key(AA,CasES,Value) -> true ;
        (member(Void -Value,CasES),Void=='%void%')))).

  best_key(AA,Cases,Value):-
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),AA=@=Match)->true;
        (member(Match-Value,Cases),AA = Match))).

		%into_case_list([[C|ASES0]],CASES):-  is_list(C),!, into_case_list([C|ASES0],CASES),!.
	into_case_list(CASES,CASES):- is_list(CASES),!.
		is_case(AA,[AA,Value],Value):-!.
		is_case(AA,[AA|Value],Value).

   maybe_special_keys(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval_args(Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(Depth,Self,[_|KVI],KVO):-
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(_Depth,_Self,[],[]).


%[superpose,[1,2,3]]
eval_args1(Depth,Self,['superpose',List],Res):- !, member(E,List),eval_args(Depth,Self,E,Res).
get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).
eval_args1(Depth,Self, Term, Res):-
  mnotrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval_args(Depth,Self, Term, Res).

%[collapse,[1,2,3]]
eval_args1(Depth,Self,['collapse',List],Res):-!, setof_eval(Depth,Self,List,Res).
eval_args1(Depth,Self, Term, Res):-
   mnotrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, setof_eval(Depth,Self,List,Var),
   eval_args(Depth,Self, Term, Res).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).


eval_args1(Depth,Self,['if',Cond,Then],Res):- !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res) ; Res = []).

eval_args1(Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res);eval_args(Depth,Self,Else,Res)).

eval_args1(_Dpth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.

eval_args1(Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((eval_args(Depth,Self,A5,AE), AE=A)),
  eval_args(Depth,Self,AA,OO).
%eval_args1(Depth,Self,['let',A,A5,AA],AAO):- !,eval_args(Depth,Self,A5,A),eval_args(Depth,Self,AA,AAO).
eval_args1(Depth,Self,['let*',[],Body],RetVal):- !, eval_args(Depth,Self,Body,RetVal).
eval_args1(Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    eval_args1(Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).

eval_args1(Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args(Depth,Self),List,Res),flatten(Res,Flat).
eval_args1(Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Depth,Space,PredDecl).
eval_args1(_Dpth,_Slf,['car-atom',Atom],CAR):- !, Atom=[CAR|_],!.
eval_args1(_Dpth,_Slf,['cdr-atom',Atom],CDR):- !, Atom=[_|CDR],!.

eval_args1(Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval_args(Depth,Self,A,AA), eval_args(Depth,Self,B,BB).

eval_args1(Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   eval_args(Depth,Self,A,AA), eval_args(Depth,Self,B,BB).


eval_args1(Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !, eval_args(Depth,Self,StateExpr,StateMonad),
  eval_args(Depth,Self,UpdatedValue,Value),  'change-state!'(Depth,Self,StateMonad, Value, Ret).
eval_args1(Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  eval_args(Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad).
eval_args1(Depth,Self,['get-state',StateExpr],Value):- !,
  eval_args(Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value).



% eval_args1(Depth,Self,['get-state',Expr],Value):- !, eval_args(Depth,Self,Expr,State), arg(1,State,Value).



check_type:- option_else(typecheck,TF,'False'), TF=='True'.

:- dynamic is_registered_state/1.
:- flush_output.
:- setenv('RUST_BACKTRACE',full).

% Function to check if an value is registered as a state name
:- dynamic(is_registered_state/1).
is_nb_state(G):- is_valid_nb_state(G) -> true ;
                 is_registered_state(G),nb_current(G,S),is_valid_nb_state(S).


:- multifile(state_type_method/3).
:- dynamic(state_type_method/3).
space_type_method(is_nb_state,new_space,init_state).
space_type_method(is_nb_state,clear_space,clear_nb_values).
space_type_method(is_nb_state,add_atom,add_nb_value).
space_type_method(is_nb_state,remove_atom,'change-state!').
space_type_method(is_nb_state,replace_atom,replace_nb_value).
space_type_method(is_nb_state,atom_count,value_nb_count).
space_type_method(is_nb_state,get_atoms,'get-state').
space_type_method(is_nb_state,atom_iter,value_nb_iter).

state_type_method(is_nb_state,new_state,init_state).
state_type_method(is_nb_state,clear_state,clear_nb_values).
state_type_method(is_nb_state,add_value,add_nb_value).
state_type_method(is_nb_state,remove_value,'change-state!').
state_type_method(is_nb_state,replace_value,replace_nb_value).
state_type_method(is_nb_state,value_count,value_nb_count).
state_type_method(is_nb_state,'get-state','get-state').
state_type_method(is_nb_state,value_iter,value_nb_iter).
%state_type_method(is_nb_state,query,state_nb_query).

% Clear all values from a state
clear_nb_values(StateNameOrInstance) :-
    fetch_or_create_state(StateNameOrInstance, State),
    nb_setarg(1, State, []).



% Function to confirm if a term represents a state
is_valid_nb_state(State):- compound(State),functor(State,'State',_).

% Find the original name of a given state
state_original_name(State, Name) :-
    is_registered_state(Name),
    nb_current(Name, State).

% Register and initialize a new state
init_state(Name) :-
    State = 'State'(_,_),
    asserta(is_registered_state(Name)),
    nb_setval(Name, State).

% Change a value in a state
'change-state!'(Depth,Self,StateNameOrInstance, UpdatedValue, Out) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(2, State, Type),
    ( (check_type,\+ get_type(Depth,Self,UpdatedValue,Type))
     -> (Out = ['Error', UpdatedValue, 'BadType'])
     ; (nb_setarg(1, State, UpdatedValue), Out = State) ).

% Fetch all values from a state
'get-state'(StateNameOrInstance, Values) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(1, State, Values).

'new-state'(Depth,Self,Init,'State'(Init, Type)):- check_type->get_type(Depth,Self,Init,Type);true.

fetch_or_create_state(Name):- fetch_or_create_state(Name,_).
% Fetch an existing state or create a new one

fetch_or_create_state(State, State) :- is_valid_nb_state(State),!.
fetch_or_create_state(NameOrInstance, State) :-
    (   atom(NameOrInstance)
    ->  (is_registered_state(NameOrInstance)
        ->  nb_current(NameOrInstance, State)
        ;   init_state(NameOrInstance),
            nb_current(NameOrInstance, State))
    ;   is_valid_nb_state(NameOrInstance)
    ->  State = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_state(State).


eval_args1(Depth,Self,['get-type',Val],Type):- !, get_type(Depth,Self,Val,Type),ground(Type),Type\==[], Type\==Val,!.


mnotrace(G):- once(G).

is_decl_type(ST):- metta_type(_,_,Type),(ST=Type;(sub_term(ST,Type),atom(ST))), \+ nontype(ST).
is_type(Type):- nontype(Type),!,fail.
is_type(Type):- is_decl_type(Type).
is_type(Type):- atom(Type).

nontype(Type):- var(Type),!.
nontype('->').
nontype(N):- number(N).

get_type(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_type(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_type(Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,get_type(Depth,Self,Val,Type).
get_type(_Dpth,Self,[Fn|_],Type):- symbol(Fn),metta_type(Self,Fn,List),last_element(List,Type), nonvar(Type),is_type(Type).
get_type(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,List,LType),last_element(LType,Type), nonvar(Type),is_type(Type).

get_type(Depth,_Slf,Type,Type):- Depth<1,!.
get_type(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_type(Depth,Self,List,Types):- List\==[], is_list(List),Depth2 is Depth-1,maplist(get_type(Depth2,Self),List,Types).
get_type(_Dpth,Self,Fn,Type):- symbol(Fn),metta_type(Self,Fn,Type),!.
%get_type(Depth,Self,Fn,Type):- nonvar(Fn),metta_type(Self,Fn,Type2),Depth2 is Depth-1,get_type(Depth2,Self,Type2,Type).
%get_type(Depth,Self,Fn,Type):- Depth>0,nonvar(Fn),metta_type(Self,Type,Fn),!. %,!,last_element(List,Type).

get_type(Depth,Self,Expr,Type):-Depth2 is Depth-1, eval_args(Depth2,Self,Expr,Val),Expr\=@=Val,get_type(Depth2,Self,Val,Type).


get_type(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_type(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
get_type(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
%get_type(Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type(Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type(Depth2,Self,Ele,Type)),!.
%get_type(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
get_type(_Dpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
get_type(_Dpth,_Slf,_,'%Undefined%'):- fail.
eval_args1(Depth,Self,['length',L],Res):- !, eval_args(Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_args1(Depth,Self,['CountElement',L],Res):- !, eval_args(Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).


is_feo_f('Cons').

is_seo_f('{}').
is_seo_f('[]').
is_seo_f('StateMonad').
is_seo_f('State').
is_seo_f('Event').
is_seo_f(N):- number(N),!.

eval_args1(Depth,Self,['+',N1,N2],N):- number(N1),
   eval_args(Depth,Self,N2,N2Res), catch(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).
eval_args1(Depth,Self,['-',N1,N2],N):- number(N1),
   eval_args(Depth,Self,N2,N2Res), catch(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).

set_last_error(_).




eval_args1(Depth,Self,X,Y):-
  (eval_args2(Depth,Self,X,Y)*->true;
    (eval_args2_failed(Depth,Self,X,Y)*->true;X=Y)).


eval_args2_failed(_Dpth,_Slf,T,TT):- T==[],!,TT=[].
eval_args2_failed(_Dpth,_Slf,T,TT):- var(T),!,TT=T.
eval_args2_failed(_Dpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace([F|LESS]\==Res),!.
%eval_args2_failed(Depth,Self,[V|Nil],[O]):- Nil==[], once(eval_args(Depth,Self,V,O)),V\=@=O,!.
eval_args2_failed(Depth,Self,[H|T],[HH|TT]):- !,
  eval_args(Depth,Self,H,HH),
  eval_args2_failed(Depth,Self,T,TT).

eval_args2_failed(Depth,Self,T,TT):- eval_args(Depth,Self,T,TT).

   %eval_args(Depth,Self,X,Y):- eval_args1(Depth,Self,X,Y)*->true;Y=[].

%eval_args1(Depth,_,_,_):- Depth<1,!,fail.
%eval_args1(Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).
%eval_args1(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

% Kills zero arity functions eval_args1(Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval_args(Depth,Self,X,Y).


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
eval_args2(_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
*/
% Macro Functions
%eval_args1(Depth,_,_,_):- Depth<1,!,fail.
eval_args2(Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).
eval_args2(Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   mnotrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval_args(Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   eval_args(Depth,Self,[F|Temp],Res).



/*
eval_args2(Depth,Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval_args(Depth,Self,A,AA),AA\==A,
   eval_args(Depth,Self,[F,AA|Args],Res).


eval_args2(Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval_args(Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval_args(Depth,Self,[F,A1|NewArgs],Res)))).
*/

/* %%

% !(assertEqualToResult ((inc) 2) (3))
eval_args2(Depth,Self,[F|Args],Res):- is_list(F),
  metta_atom_iter(Depth,Self,['=',F,R]), eval_args(Depth,Self,[R|Args],Res).

eval_args2(Depth,Self,[F|Args],Res):- is_list(F), Args\==[],
  append(F,Args,FArgs),!,eval_args(Depth,Self,FArgs,Res).
*/
eval_args2(_Dpth,Self,['import!',Other,File],Space):- into_space(Self,Other,Space),!, include_metta(Space,File).
eval_args2(Depth,Self,['bind!',Other,Expr],Value):-
   into_name(Self,Other,Name),!,eval_args(Depth,Self,Expr,Value),nb_setval(Name,Value).
eval_args2(Depth,Self,['pragma!',Other,Expr],Value):-
   into_name(Self,Other,Name),!,eval_args(Depth,Self,Expr,Value),set_option_value(Name,Value).





eval_args2(Depth,Self,['nop',Expr],[]):- !,  eval_args(Depth,Self,Expr,_).

is_True(T):- T\=='False',T\==[].

is_and(S):- \+ atom(S),!,fail.
is_and('#COMMA'). is_and(','). is_and('and'). is_and('And').

eval_args2(_Dpth,_Slf,[And],'True'):- is_and(And),!.
eval_args2(Depth,Self,['and',X,Y],TF):- !, as_tf((eval_args(Depth,Self,X,'True'),eval_args(Depth,Self,Y,'True')),TF).
eval_args2(Depth,Self,[And,X|Y],TF):- is_and(And),!,eval_args(Depth,Self,X,TF1),
  is_True(TF1),eval_args2(Depth,Self,[And|Y],TF).
%eval_args2(Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args2(Depth,Self,['or',X,Y],TF):- !, as_tf((eval_args(Depth,Self,X,'True');eval_args(Depth,Self,Y,'True')),TF).



eval_args2(_Dpth,Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
eval_args2(_Dpth,Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
eval_args2(_Dpth,Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2.
eval_args2(_Dpth,Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).


% user defined function
eval_args2(Depth,Self,[H|PredDecl],Res):- mnotrace(is_user_defined_head(Self,H)),!,
   eval_args30(Depth,Self,[H|PredDecl],Res).

% function inherited by system
eval_args2(Depth,Self,PredDecl,Res):- eval_args40(Depth,Self,PredDecl,Res).


last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.




catch_warn(G):- notrace(catch(G,E,(wdmsg(catch_warn(G)-->E),fail))).
catch_nowarn(G):- notrace(catch(G,error(_,_),fail)).

as_tf(G,TF):- catch_nowarn((call(G)*->TF='True';TF='False')).
eval_selfless(['==',X,Y],TF):- as_tf(X=:=Y,TF),!.
eval_selfless(['==',X,Y],TF):- as_tf(X=@=Y,TF),!.
eval_selfless(['=',X,Y],TF):-!,as_tf(X=Y,TF).
eval_selfless(['>',X,Y],TF):-!,as_tf(X>Y,TF).
eval_selfless(['<',X,Y],TF):-!,as_tf(X<Y,TF).
eval_selfless(['=>',X,Y],TF):-!,as_tf(X>=Y,TF).
eval_selfless(['<=',X,Y],TF):-!,as_tf(X=<Y,TF).

eval_selfless(['%',X,Y],TF):-!,eval_selfless(['mod',X,Y],TF).

eval_selfless(LIS,Y):-  notrace((
   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   catch((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.

% less Macro-ey Functions




/*
; Bind &kb22 to a new empty Space
!(bind! &kb22 (new-space))

; Some knowledge
(= (frog $x)
   (and (croaks $x)
        (eat_flies $x)))
(= (croaks Fritz) True)
(= (eat_flies Fritz) True)
(= (croaks Sam) True)
(= (eat_flies Sam) True)
(= (green $x)
   (frog $x))

; Define conditional
(: ift (-> Bool Atom Atom))
(= (ift True $then) $then)

; For anything that is green, assert it is Green in &kb22
!(ift (green $x)
      (add-atom &kb22 (Green $x)))

; Retrieve the inferred Green things: Fritz and Sam.
!(assertEqualToResult
  (match &kb22 (Green $x) $x)
  (Fritz Sam))
*/
:- discontiguous eval_args3/4.
%eval_args2(Depth,Self,PredDecl,Res):- eval_args3(Depth,Self,PredDecl,Res).

%eval_args2(_Dpth,_Slf,L1,Res):- is_list(L1),maplist(self_eval,L1),!,Res=L1.
%eval_args2(_Depth,_Self,X,X).


is_user_defined_head(Other,H):- mnotrace(is_user_defined_head0(Other,H)).
is_user_defined_head0(Other,[H|_]):- !, nonvar(H),!, is_user_defined_head_f(Other,H).
is_user_defined_head0(Other,H):- callable(H),!,functor(H,F,_), is_user_defined_head_f(Other,F).
is_user_defined_head0(Other,H):- is_user_defined_head_f(Other,H).

is_user_defined_head_f(Other,H):- metta_type(Other,H,_).
is_user_defined_head_f(Other,H):- metta_atom(Other,[H|_]).
is_user_defined_head_f(Other,H):- metta_defn(Other,[H|_],_).
%is_user_defined_head_f(_,H):- is_metta_builtin(H).


is_special_op(F):- \+ atom(F), \+ var(F), !, fail.
is_special_op('case').
is_special_op(':').
is_special_op('=').
is_special_op('->').
is_special_op('let').
is_special_op('let*').
is_special_op('if').
is_special_op('rtrace').
is_special_op('or').
is_special_op('and').
is_special_op('not').
is_special_op('match').
is_special_op('call').
is_special_op('let').
is_special_op('let*').
is_special_op('nop').
is_special_op('assertEqual').
is_special_op('assertEqualToResult').

is_metta_builtin(Special):- is_special_op(Special).
is_metta_builtin('==').
is_metta_builtin(F):- once(atom(F);var(F)), current_op(_,yfx,F).
is_metta_builtin('println!').
is_metta_builtin('transfer!').
is_metta_builtin('collapse').
is_metta_builtin('superpose').
is_metta_builtin('+').
is_metta_builtin('-').
is_metta_builtin('*').
is_metta_builtin('/').
is_metta_builtin('%').
is_metta_builtin('==').
is_metta_builtin('<').
is_metta_builtin('>').
is_metta_builtin('all').
is_metta_builtin('import!').
is_metta_builtin('pragma!').



eval_args30(Depth,Self,H,B):-  (eval_args34(Depth,Self,H,B)*->true;eval_args37(Depth,Self,H,B)).

eval_args34(_Dpth,Self,H,B):-  (metta_defn(Self,H,B);(metta_atom(Self,H),B='True')).

% Has argument that is headed by the same function
eval_args37(Depth,Self,[H1|Args],Res):-
   mnotrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   eval_args(Depth,Self,[H2|H2Args],ArgRes),
   mnotrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   eval_args30(Depth,Self,[H1|NewArgs],Res).

eval_args37(Depth,Self,[[H|Start]|T1],Y):-
   mnotrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Self,[H|Start],Left),
   eval_args(Depth,Self,[Left|T1],Y).

% Has subterm to eval
eval_args37(Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   quietly(sub_sterm1(SSub,PredDecl)),
   mnotrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   eval_args(Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   eval_args30(Depth,Self,[F|Temp],Res).

%eval_args37(Depth,Self,X,Y):- (eval_args38(Depth,Self,X,Y)*->true;metta_atom_iter(Depth,Self,[=,X,Y])).

eval_args37(Depth,Self,PredDecl,Res):- fail,
 ((term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval_args(Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))))),
 PredDecl\=@=Res.

eval_args38(_Dpth,Self,[H|_],_):- mnotrace( \+ is_user_defined_head_f(Self,H) ), !,fail.
eval_args38(_Dpth,Self,[H|T1],Y):- metta_defn(Self,[H|T1],Y).
eval_args38(_Dpth,Self,[H|T1],'True'):- metta_atom(Self,[H|T1]).
eval_args38(_Dpth,Self,CALL,Y):- fail,append(Left,[Y],CALL),metta_defn(Self,Left,Y).


%eval_args3(Depth,Self,['ift',CR,Then],RO):- fail, !, %fail, % trace,
%   metta_defn(Self,['ift',R,Then],Become),eval_args(Depth,Self,CR,R),eval_args(Depth,Self,Then,_True),eval_args(Depth,Self,Become,RO).

metta_atom_iter(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!,
  (metta_defn(Other,H,B)*->true;(metta_atom(Other,H),B='True')).

metta_atom_iter(Depth,_,_):- Depth<3,!,fail.
metta_atom_iter(_Dpth,_Slf,[]):-!.
metta_atom_iter(_Dpth,Other,H):- metta_atom(Other,H).
metta_atom_iter(Depth,Other,H):- D2 is Depth -1, metta_defn(Other,H,B),metta_atom_iter(D2,Other,B).
metta_atom_iter(_Dpth,_Slf,[And]):- is_and(And),!.
metta_atom_iter(Depth,Self,[And,X|Y]):- is_and(And),!,D2 is Depth -1, metta_atom_iter(D2,Self,X),metta_atom_iter(D2,Self,[And|Y]).
/*
metta_atom_iter2(_,Self,[=,X,Y]):- metta_defn(Self,X,Y).
metta_atom_iter2(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!, metta_defn(Other,H,B).
metta_atom_iter2(_Dpth,Self,X,Y):- metta_defn(Self,X,Y). %, Y\=='True'.
metta_atom_iter2(_Dpth,Self,X,Y):- metta_atom(Self,[=,X,Y]). %, Y\=='True'.

*/
metta_atom_iter_ref(Other,['=',H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).

sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).

%not_compound(Term):- \+ is_list(Term),!.
%eval_args2(Depth,Self,Term,Res):- maplist(not_compound,Term),!,eval_args345(Depth,Self,Term,Res).


% function inherited by system
eval_args40(Depth,Self,[F|X],FY):- is_function(F), \+ is_special_op(F), is_list(X),
  maplist(eval_args(Depth,Self),X,Y),!,eval_args5(Depth,Self,[F|Y],FY).
eval_args40(Depth,Self,FX,FY):- eval_args5(Depth,Self,FX,FY).

eval_args5(_Dpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace(([F|LESS]\==Res)),!.
eval_args5(Depth,Self,[AE|More],TF):- length(More,Len),
  (is_syspred(AE,Len,Pred),catch_warn(as_tf(apply(Pred,More),TF)))*->true;eval_args6(Depth,Self,[AE|More],TF).
eval_args6(_Dpth,_Slf,[AE|More],TF):- length([AE|More],Len), is_syspred(AE,Len,Pred),append(More,[TF],Args),!,catch_warn(apply(Pred,Args)).

%eval_args40(Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args(Depth,Self,[F2|X2],Y2),eval_args(Depth,Self,X1,Y1).


cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
setof_eval(Depth,Self,X,S):- !,findall(E,eval_args(Depth,Self,X,E),L),sort(L,S).
%setof_eval(Depth,Self,X,S):- setof(E,eval_args(Depth,Self,X,E),S)*->true;S=[].

