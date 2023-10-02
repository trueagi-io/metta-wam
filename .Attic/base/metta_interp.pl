:- encoding(iso_latin_1).
:- set_stream(user_input,tty(true)).
:- use_module(library(readline)).
:- use_module(library(editline)).
:- use_module(library(filesex)).
:- use_module(library(shell)).
%:- use_module(library(tabling)).
:- use_module(library(system)).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_data).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_eval).
/*
Now PASSING NARS.TESTS1.01)
Now PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.08)
Now PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.14)
Now PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.15)
Now PASSING TEST-SCRIPTS.C1-GROUNDED-BASIC.15)
Now PASSING TEST-SCRIPTS.E2-STATES.08)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.02)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.07)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.09)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.11)
PASSING TEST-SCRIPTS.C1-GROUNDED-BASIC.14)
PASSING TEST-SCRIPTS.E2-STATES.07)
-----------------------------------------
FAILING TEST-SCRIPTS.D5-AUTO-TYPES.01)
Now FAILING TEST-SCRIPTS.00-LANG-CASE.03)
Now FAILING TEST-SCRIPTS.B5-TYPES-PRELIM.19)
Now FAILING TEST-SCRIPTS.C1-GROUNDED-BASIC.20)

*/


early_opts('repl',false).
early_opts('compile',false).
early_opts('table',false).
early_opts('time',true).
early_opts('exec',true).
early_opts('html',false).
early_opts('python',false).
early_opts('prolog',false).

early_opts('trace-on-fail',true).
early_opts('trace-on-pass',true).
early_opts('trace-on-overflow',true).
early_opts('trace-on-error',true).
early_opts('trace-length',100).
early_opts('stack-max',100).


trace_on_fail:-     option_value('trace-on-fail',true).
trace_on_overflow:- option_value('trace-on-overflow',true).
trace_on_pass:-     option_value('trace-on-pass',true).


% ============================
% %%%% Arithmetic Operations
% ============================
% Addition
'+'(Addend1, Addend2, Sum):- plus(Addend1, Addend2, Sum).
% Subtraction
'-'(Sum, Addend1, Addend2):- plus(Addend1, Addend2, Sum).

:- use_module(library(clpq)).
% Multiplication
'*'(Factor1, Factor2, Product):- {Product = Factor1*Factor2}.
% Division
'/'(Dividend, Divisor, Quotient):- {Dividend = Quotient * Divisor}.
% Modulus
'mod'(Dividend, Divisor, Remainder):- {Remainder = Dividend mod Divisor}.
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
%'assertEqual'(Expected, Actual, Result):- use_metta_compiler,!,as_tf((Expected=Actual),Result).
'assertEqual'(Expected, Actual, Result):- ignore(Expected=Actual), eval_args(['assertEqual', Expected, Actual], Result).

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
'match'(_Environment, Pattern, Template, Result):- callable(Pattern),!, call(Pattern),Result=Template.
'match'(_Environment, Pattern, Template, Result):- !, is_True(Pattern),Result=Template.
'match'(Environment, Pattern, Template, Result):- eval_args(['match', Environment, Pattern, Template], Result).

% ============================
% %%%% Reflection
% ============================
% Get Type of Value
'get-type'(Value, Type):- eval_args(['get-type', Value], Type).


'new-space'(Space):- gensym(new_space_,Name), fetch_or_create_space(Name, Space).

% Function to check if an atom is registered as a space name
:- dynamic is_registered_space_name/1.
is_nb_space(G):- is_valid_nb_space(G) -> true ;
                 is_registered_space_name(G),nb_current(G,S),is_valid_nb_space(S).

:- dynamic(is_python_space/1).
% ===============================
% MeTTa Python incoming interface
% ===============================

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_as_nb_space,new_space,init_space).
space_type_method(is_as_nb_space,clear_space,clear_nb_atoms).
space_type_method(is_as_nb_space,add_atom,add_nb_atom).
space_type_method(is_as_nb_space,remove_atom,remove_nb_atom).
space_type_method(is_as_nb_space,replace_atom,replace_nb_atom).
space_type_method(is_as_nb_space,atom_count,atom_nb_count).
space_type_method(is_as_nb_space,get_atoms,get_nb_atoms).
space_type_method(is_as_nb_space,atom_iter,atom_nb_iter).
%space_type_method(is_as_nb_space,query,space_nb_query).

% Clear all atoms from a space
clear_nb_atoms(SpaceNameOrInstance) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    nb_setarg(1, Space, []).

% Add an atom to the space
add_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    NewAtoms = [Atom | Atoms],
    nb_setarg(1, Space, NewAtoms).

% Count atoms in a space
atom_nb_count(SpaceNameOrInstance, Count) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    length(Atoms, Count).

% Remove an atom from a space
remove_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    select(Atom, Atoms, UpdatedAtoms),
    nb_setarg(1, Space, UpdatedAtoms).

% Fetch all atoms from a space
get_nb_atoms(SpaceNameOrInstance, Atoms) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms).

% Replace an atom in the space
replace_nb_atom(SpaceNameOrInstance, OldAtom, NewAtom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    ( (select(Found, Atoms, TempAtoms),OldAtom=@=Found)
    ->  NewAtoms = [NewAtom | TempAtoms],
        nb_setarg(1, Space, NewAtoms)
    ;   false
    ).




% Function to confirm if a term represents a space
is_valid_nb_space(Space):- compound(Space),functor(Space,'Space',_).

% Find the original name of a given space
space_original_name(Space, Name) :-
    is_registered_space_name(Name),
    nb_current(Name, Space).

% Register and initialize a new space
init_space(Name) :-
    Space = 'Space'([]),
    asserta(is_registered_space_name(Name)),
    nb_setval(Name, Space).

fetch_or_create_space(Name):- fetch_or_create_space(Name,_).
% Fetch an existing space or create a new one
fetch_or_create_space(NameOrInstance, Space) :-
    (   atom(NameOrInstance)
    ->  (is_registered_space_name(NameOrInstance)
        ->  nb_current(NameOrInstance, Space)
        ;   init_space(NameOrInstance),
            nb_current(NameOrInstance, Space))
    ;   is_valid_nb_space(NameOrInstance)
    ->  Space = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_space(Space).


% Match Pattern in Space and produce Template
'match'(Space, Pattern, Template) :-
    'get-atoms'(Space, Atoms),
    'match-pattern'(Atoms, Pattern, Template).

% Simple pattern match
'match-pattern'([], _, []).
'match-pattern'([H |_T], H, H) :- !.
'match-pattern'([_H| T], Pattern, Template) :- 'match-pattern'(T, Pattern, Template).


metta_cmd_args(Rest):- current_prolog_flag(argv,P),append(_,['--'|Rest],P),!.
metta_cmd_args(Rest):- current_prolog_flag(os_argv,P),append(_,['--'|Rest],P),!.
metta_cmd_args(Rest):- current_prolog_flag(argv,Rest).
run_file_arg:- metta_cmd_args(Rest), !,  do_cmdline_load_metta('&self',Rest).
loon:- run_file_arg, !, loonit_report, halt(7).
%loon:- time(loon_metta('./examples/compat/test_scripts/*.metta')),fail.
loon:- repl, halt(7).

metta_make_hook:-  loonit_reset, option_value(not_a_reload,true),!.
metta_make_hook:-
  metta_cmd_args(Rest), into_reload_options(Rest,Reload), cmdline_load_metta('&self',Reload).

:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).
prolog:make_hook(after, _Some):- metta_make_hook.

into_reload_options(Reload,Reload).

is_cmd_option(Opt,M, TF):- atom(M),
   atom_concat('-',Opt,Flag),
   atom_contains(M,Flag),!,
   get_flag_value(M,FV),
   TF=FV.

get_flag_value(M,V):- atomic_list_concat([_,V],'=',M),!.
get_flag_value(M,false):- atom_contains(M,'-no'),!.
get_flag_value(_,true).


:- ignore(((
   \+ prolog_load_context(reloading,true),
   forall(early_opts(Opt,Default),set_option_value(Opt,Default))))).

process_early_opts:- \+ option_value('python',false), skip(ensure_loaded(metta_python)).
process_early_opts.


process_late_opts:- option_value('html',true), shell('./total_loonits.sh').
process_late_opts:- \+ option_value('repl',false), repl.
process_late_opts:- halt(7).

do_cmdline_load_metta(Self,List):-
  select(M,List,Rest),
  atom_concat('-',_,M),
  early_opts(Opt,_),
  is_cmd_option(Opt,M, TF),!,
  write(' '), write_src(M), nl, !, set_option_value(Opt,TF),
  do_cmdline_load_metta(Self,Rest).

do_cmdline_load_metta(Self,Rest):-
  set_prolog_flag(late_metta_opts,Rest),
  forall(process_early_opts,true),
  cmdline_load_metta(Self,Rest),
  forall(process_late_opts,true).


cmdline_load_metta(Self,[Filemask|Rest]):- atom(Filemask), \+ atom_concat('-',_,Filemask),
  must_det_ll((Src=load_metta(Self,Filemask),nl,write_src(Src),nl,catch_red(Src),!,flush_output,
  cmdline_load_metta(Self,Rest))).
cmdline_load_metta(Self,[M|Rest]):-
  write(' '), write_src(M), nl, !,
  cmdline_load_metta(Self,Rest).
cmdline_load_metta(_,Nil):- Nil==[],!.

:- set_prolog_flag(occurs_check,true).

start_html_of(_Filename):-
 must_det_ll((
  S = _,
  retractall(metta_defn(S,_,_)),
  nop(retractall(metta_type(S,_,_))),
  retractall(metta_atom(S,_)),
  loonit_reset,
  tee_file(TEE_FILE),
  sformat(S,'cat /dev/null > "~w"',[TEE_FILE]),

  writeln(doing(S)),
  shell(S))).

save_html_of(_):- \+ has_loonit_results, \+ option_value('html',true).
save_html_of(Filename):-
 must_det_ll((
  file_name_extension(Base,_,Filename),
  file_name_extension(Base,html,HtmlFilename),
  loonit_reset,
  tee_file(TEE_FILE),
  writeln('<br/><a href="https://github.com/logicmoo/vspace-metta/blob/main/MeTTaLog.md">Return to Summaries</a><br/>'),
  sformat(S,'ansi2html -u < "~w" > "~w" ',[TEE_FILE,HtmlFilename]),
  writeln(doing(S)),
  shell(S))).

tee_file(TEE_FILE):- getenv('TEE_FILE',TEE_FILE),!.
tee_file(TEE_FILE):- metta_dir(Dir),directory_file_path(Dir,'TEE.ansi',TEE_FILE),!.
metta_dir(Dir):- getenv('METTA_DIR',Dir),!.

load_metta(Filename):-
 clear_spaces, load_metta('&self',Filename).

load_metta(Self,Filename):-
 atom(Filename),exists_file(Filename),!,
 track_load_into_file(Filename,
   setup_call_cleanup(open(Filename,read,In),
    ((directory_file_path(Directory, _BaseName, Filename),
      with_cwd(Directory,
         load_metta_stream(Self,In)))),close(In))).

load_metta(_Self,Filename):- Filename=='--repl',!,repl.
load_metta(Self,Filename):-
  (\+ atom(Filename); \+ exists_file(Filename)),!,
  with_wild_path(load_metta(Self),Filename),!,loonit_report.

include_metta(Self,Filename):-
 atom(Filename),exists_file(Filename),!,
   setup_call_cleanup(open(Filename,read,In),
    ((directory_file_path(Directory, _BaseName, Filename),
      with_cwd(Directory,
         load_metta_stream(Self,In)))),close(In)).
include_metta(Self,Filename):-
  (\+ atom(Filename); \+ exists_file(Filename)),!,
  with_wild_path(include_metta(Self),Filename),!.

%writeqln(Q):- write(' '),writeq(Q),nl.
writeqln(Q):- format('~N'),write(' '),writeq(Q),nl.

clear_spaces:- clear_space(_).
clear_space(S):-
   retractall(metta_defn(S,_,_)),
   nop(retractall(metta_type(S,_,_))),
   retractall(metta_atom(S,_)).

metta_type(S,H,B):- metta_atom(S,[':',H,B]).

load_metta_stream(Fn,String):- string(String),!,open_string(String,Stream),load_metta_stream(Fn,Stream).
load_metta_stream(_Fn,In):- (at_end_of_stream(In)/*;reached_file_max*/),!.
load_metta_stream(Self,In):-
 repeat,
  once(read_metta(In,Read)), %write_src(read_metta=Read),nl,
  once(do_metta(Self,load,Read)),
  flush_output,
  at_end_of_stream(In),!.




debug_only(G):- ignore(mnotrace(catch_warn(G))).
debug_only(_What,G):- ignore((fail,mnotrace(catch_warn(G)))).


'True':- true.
'False':- fail.


'metta_learner::vspace-main':- repl.

into_underscores(D,U):- atom(D),!,atomic_list_concat(L,'-',D),atomic_list_concat(L,'_',U).
into_underscores(D,U):- descend_and_transform(into_underscores,D,U),!.

into_hyphens(D,U):- atom(D),!,atomic_list_concat(L,'_',D),atomic_list_concat(L,'-',U).
into_hyphens(D,U):- descend_and_transform(into_hyphens,D,U),!.

descend_and_transform(P2, Input, Transformed) :-
    (   var(Input)
    ->  Transformed = Input  % Keep variables as they are
    ;   compound(Input)
    -> (compound_name_arguments(Input, Functor, Args),
        maplist(descend_and_transform(P2), Args, TransformedArgs),
        compound_name_arguments(Transformed, Functor, TransformedArgs))
    ;   (atom(Input),call(P2,Input,Transformed))
    ->  true % Transform atoms using xform_atom/2
    ;   Transformed = Input  % Keep other non-compound terms as they are
    ).

is_syspred(H,Len,Pred):- notrace(is_syspred0(H,Len,Pred)).
is_syspred0(H,_Ln,_Prd):- \+ atom(H),!,fail.
is_syspred0(H,_Ln,_Prd):- upcase_atom(H,U),downcase_atom(H,U),!,fail.
is_syspred0(H,Len,Pred):- current_predicate(H/Len),!,Pred=H.
is_syspred0(H,Len,Pred):- atom_concat(Mid,'!',H), H\==Mid, is_syspred0(Mid,Len,Pred),!.
is_syspred0(H,Len,Pred):- into_underscores(H,Mid), H\==Mid, is_syspred0(Mid,Len,Pred),!.

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).

is_function(F):- atom(F).

is_False(X):- X\=='True', (is_False1(X)-> true ; (eval_args(X,Y),is_False1(Y))).
is_False1(Y):- (Y==0;Y==[];Y=='False').

is_conz(Self):- compound(Self), Self=[_|_].

%dont_x(eval_args(Depth,Self,metta_if(A<B,L1,L2),R)).
dont_x(eval_args(_<_,_)).

into_fp(D,D):- \+ \+ dont_x(D),!.
into_fp(ListX,CallAB):-
  sub_term(STerm,ListX),needs_expanded(STerm,Term),
  %copy_term(Term,CTerm),
  =(Term,CTerm),
  substM(ListX,CTerm,Var,CallB), fn_append1(Term,Var,CallA),
  into_fp((CallA,CallB),CallAB).
into_fp(A,A).

needs_expand(Expand):- compound(Expand),functor(Expand,F,N),N>=1,atom_concat(metta_,_,F).
needs_expanded(eval_args(Term,_),Expand):- !,sub_term(Expand,Term),compound(Expand),Expand\=@=Term,
   compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).
needs_expanded([A|B],Expand):- sub_term(Expand,[A|B]), compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).

fn_append1(eval_args(Term,X),X,eval_args(Term,X)):-!.
fn_append1(Term,X,eval_args(Term,X)).


% Check if parentheses are balanced in a list of characters
balanced_parentheses(Chars) :- balanced_parentheses(Chars, 0).
balanced_parentheses([], 0).
balanced_parentheses(['('|T], N) :- N1 is N + 1, balanced_parentheses(T, N1).
balanced_parentheses([')'|T], N) :- N > 0, N1 is N - 1, balanced_parentheses(T, N1).
balanced_parentheses([H|T], N) :- H \= '(', H \= ')', balanced_parentheses(T, N).
% Recursive function to read lines until parentheses are balanced.
repl_read(NewAccumulated, Read):-
    atom_concat(Atom, '.', NewAccumulated),
    catch((read_term_from_atom(Atom, Term, []), Read=call(Term)), E,
       (write('Syntax error: '), writeq(E), nl, repl_read(Read))),!.
repl_read(NewAccumulated, Read):-
    normalize_space(string(Renew),NewAccumulated), Renew \== NewAccumulated, !,
    repl_read(Renew, Read).
repl_read(NewAccumulated,exec(Read)):- string_concat("!",Renew,NewAccumulated), !,
    repl_read(Renew, Read).
repl_read(NewAccumulated, Read):- string_chars(NewAccumulated, Chars),
    balanced_parentheses(Chars), length(Chars, Len), Len > 0, parse_sexpr_metta(NewAccumulated, Read), !.
repl_read(Accumulated, Read) :- read_line_to_string(current_input, Line), repl_read(Accumulated, Line, Read).
repl_read(Accumulated, "", Read):- !, repl_read(Accumulated, Read).
repl_read(_Accumulated, Line, Read):- Line == end_of_file, !, Read = Line.
repl_read(Accumulated, Line, Read) :- atomics_to_string([Accumulated," ",Line], NewAccumulated), !,
    repl_read(NewAccumulated, Read).
repl_read(Read) :- mnotrace(repl_read("", Read)).



repl:- option_value('repl',prolog),!,prolog.
repl:-
   mnotrace((current_input(In),ignore(catch(load_history,_,true)))),
   repeat,
   mnotrace((with_option(not_a_reload,true,make),
     ((nb_current(self_space,Self),Self\==[])->true;Self='&self'),
     format('~N~n'), format(atom(P),'metta@~w: ',[Self]),
     write(P))),
   setup_call_cleanup(mnotrace(prompt(Was,'')),
      (mnotrace(read_metta(In,Read))),
       mnotrace(prompt(_,Was))),
   once(do_repl(Self,Read)),
   mnotrace(Read==end_of_file),!.

do_repl(_Self,end_of_file):- !, writeln('\n\n% To restart, use: ?- repl.').
do_repl(_Slf,call(Term)):- add_history1(Term), !, repl_call(Term).

do_repl(Self,!):- !, mnotrace(repl_read(Exec)),do_repl(Self,exec(Exec)).

do_repl(Self,Read):- mnotrace((string(Read),add_history_string(Read))),!,mnotrace(repl_read(Read,Term)),!, do_metta(Self,load,Term).

do_repl(Self,exec(Exec)):- !, mnotrace(save_exec_history(Exec)), do_metta_exec(Self,Exec).
do_repl(Self,Read):-
  mnotrace(((with_output_to(string(H),write_src(Read)),add_history_string(H)))), do_metta(Self,load,Read).

add_history_string(Str):- ignore(catch_i(add_history01(Str))),!.

save_exec_history(exec(Exec)):- !, mnotrace((save_exec_history(Exec))).
save_exec_history(Exec):- mnotrace((with_output_to(string(H),(write('!'),write_src(Exec))),add_history_string(H))).

read_metta1(_,O2):- clause(t_l:s_reader_info(O2),_,Ref),erase(Ref).
read_metta1(In,Read):- current_input(In0),In==In0,!, repl_read(Read).
read_metta1(In,Read):- peek_char(In,Char), read_metta1(In,Char,Read).

read_metta1(In,Char,Read):- char_type(Char,white),get_char(In,Char),put(Char),!,read_metta1(In,Read).
read_metta1(In,';',Read):- read_line_to_string(In,Str),write_comment(Str),!,read_metta1(In,Read).
read_metta1(In,_,Read1):- parse_sexpr_metta(In,Read),!,must_det_ll(Read=Read1).



read_metta(In,Read):-
 read_metta1(In,Read1),
  (Read1=='!'
     -> (read_metta1(In,Read2), Read=exec(Read2), save_exec_history(Read))
     ; Read = Read1),!.

write_comment(Cmt):- format('~N;~w~n',[Cmt]).
do_metta_cmt(_,'$COMMENT'(Cmt,_,_)):- write_comment(Cmt),!.
do_metta_cmt(_,'$STRING'(Cmt)):- write_comment(Cmt),!.
do_metta_cmt(Self,[Cmt]):- !, do_metta_cmt(Self, Cmt),!.



parse_sexpr_metta(I,O):- parse_sexpr_untyped(I,U),trly(untyped_to_metta,U,O).

mlog_sym('@').

%untyped_to_metta(I,exec(O)):- compound(I),I=exec(M),!,untyped_to_metta(M,O).
untyped_to_metta(I,O):-
 must_det_ll((
  trly(mfix_vars1,I,M),
  trly(cons_to_c,M,OM),
  trly(cons_to_l,OM,O))).


trly(P2,A,B):- once(call(P2,A,M)),A\=@=M,!,trly(P2,M,B).
trly(_,A,A).

mfix_vars1(I,O):- var(I),!,I=O.
mfix_vars1(I,O):- I=='T',!,O='True'.
mfix_vars1(I,O):- I=='F',!,O='False'.
mfix_vars1(I,O):- I=='Nil',!,O=[].
mfix_vars1(I,O):- I=='true',!,O='True'.
mfix_vars1(I,O):- I=='false',!,O='False'.
mfix_vars1('$STRING'(I),O):- !, text_to_string(I,O).
mfix_vars1(I,O):-  I = ['[', X, ']'], nonvar(X), !, O = ['[]',X].
mfix_vars1(I,O):-  I = ['{', X, '}'], nonvar(X), !, O = ['{}',X].
mfix_vars1('$OBJ'(claz_bracket_vector,List),Res):- is_list(List),!, append(['['|List],[']'],Res),!.
mfix_vars1(I,O):- I==[Quote, S], Quote==quote,S==s,!, O=is.
mfix_vars1([K,H|T],Cmpd):- atom(K),mlog_sym(K),is_list(T),mfix_vars1([H|T],[HH|TT]),atom(HH),is_list(TT),!,
  compound_name_arguments(Cmpd,HH,TT).
%mfix_vars1([H|T],[HH|TT]):- !, mfix_vars1(H,HH),mfix_vars1(T,TT).
mfix_vars1(List,ListO):- is_list(List),!,maplist(mfix_vars1,List,ListO).
mfix_vars1(I,O):- compound(I),!,compound_name_arguments(I,F,II),maplist(mfix_vars1,II,OO),!,compound_name_arguments(O,F,OO).
mfix_vars1(I,O):- \+ atom(I),!,I=O.
mfix_vars1(I,'$VAR'(O)):- atom_concat('$',N,I),dvar_name(N,O),!.
mfix_vars1(I,I).

no_cons_reduce.

dvar_name(N,O):- atom(N),atom_number(N,Num),atom_concat('Num',Num,M),!,svar_fixvarname(M,O).
dvar_name(N,O):- number(N),atom_concat('Num',N,M),!,svar_fixvarname(M,O).
dvar_name('','__'):-!. % "$"
dvar_name('_','_'):-!. % "$_"
dvar_name(N,O):- svar_fixvarname(N,O),!.

cons_to_l(I,I):- no_cons_reduce,!.
cons_to_l(I,O):- var(I),!,O=I.
cons_to_l(I,O):- I=='Nil',!,O=[].
cons_to_l(I,O):- I=='nil',!,O=[].
cons_to_l(C,O):- \+ compound(C),!,O=C.
cons_to_l([Cons,H,T|List],[HH|TT]):- List==[], atom(Cons),is_cons_f(Cons), t_is_ttable(T), cons_to_l(H,HH),!,cons_to_l(T,TT).
cons_to_l(List,ListO):- is_list(List),!,maplist(cons_to_l,List,ListO).
cons_to_l(I,I).

cons_to_c(I,I):- no_cons_reduce,!.
cons_to_c(I,O):- var(I),!,O=I.
cons_to_c(I,O):- I=='Nil',!,O=[].
cons_to_c(I,O):- I=='nil',!,O=[].
cons_to_c(C,O):- \+ compound(C),!,O=C.
cons_to_c([Cons,H,T|List],[HH|TT]):- List==[], atom(Cons),is_cons_f(Cons), t_is_ttable(T), cons_to_c(H,HH),!,cons_to_c(T,TT).
cons_to_c(I,O):- \+ is_list(I), compound_name_arguments(I,F,II),maplist(cons_to_c,II,OO),!,compound_name_arguments(O,F,OO).
cons_to_c(I,I).



t_is_ttable(T):- var(T),!.
t_is_ttable(T):- T=='Nil',!.
t_is_ttable(T):- is_ftVar(T),!.
t_is_ttable([F|Args]):- F=='Cons',!,is_list(Args).
t_is_ttable([_|Args]):- !, \+ is_list(Args).
t_is_ttable(_).

is_cons_f(Cons):- is_cf_nil(Cons,_).
is_cf_nil('Cons','Nil').
%is_cf_nil('::','nil').


subst_vars(TermWDV, NewTerm):-
   subst_vars(TermWDV, NewTerm, NamedVarsList),
   b_setval(variable_names,NamedVarsList).

subst_vars(TermWDV, NewTerm, NamedVarsList) :-
    subst_vars(TermWDV, NewTerm, [], NamedVarsList).

subst_vars(Term, Term, NamedVarsList, NamedVarsList) :- var(Term), !.
subst_vars([], [], NamedVarsList, NamedVarsList):- !.
subst_vars([TermWDV|RestWDV], [Term|Rest], Acc, NamedVarsList) :- !,
    subst_vars(TermWDV, Term, Acc, IntermediateNamedVarsList),
    subst_vars(RestWDV, Rest, IntermediateNamedVarsList, NamedVarsList).
subst_vars('$VAR'('_'), _, NamedVarsList, NamedVarsList) :- !.
subst_vars('$VAR'(VName), Var, Acc, NamedVarsList) :- nonvar(VName), svar_fixvarname(VName,Name), !,
    (memberchk(Name=Var, Acc) -> NamedVarsList = Acc ; ( !, Var = _, NamedVarsList = [Name=Var|Acc])).
subst_vars(Term, Var, Acc, NamedVarsList) :- atom(Term),atom_concat('$',DName,Term),
   dvar_name(DName,Name),!,subst_vars('$VAR'(Name), Var, Acc, NamedVarsList).

subst_vars(TermWDV, NewTerm, Acc, NamedVarsList) :-
    compound(TermWDV), !,
    compound_name_arguments(TermWDV, Functor, ArgsWDV),
    subst_vars(ArgsWDV, Args, Acc, NamedVarsList),
    compound_name_arguments(NewTerm, Functor, Args).
subst_vars(Term, Term, NamedVarsList, NamedVarsList).



:- nb_setval(variable_names,[]).


assert_preds(_Self,_Load,_Preds):- \+ preview_compiler,!.
assert_preds(_Self,Load,Preds):-
  expand_to_hb(Preds,H,_B),functor(H,F,A),
  color_g_mesg('#005288',(
   ignore((
    \+ predicate_property(H,defined),
    if_t(use_metta_compiler,catch_i(dynamic(F,A))),
    format('  :- ~q.~n',[dynamic(F,A)]),
    if_t(option_value('tabling',true), format('  :- ~q.~n',[table(F/A)])))),
   if_t((preview_compiler),
     format('~N~n  ~@',[portray_clause(Preds)])),
   if_t(use_metta_compiler,if_t(\+ predicate_property(H,static),add_assertion(Preds))))),
   nop(metta_anew1(Load,Preds)).


%load_hook(_Load,_Hooked):- !.
load_hook(Load,Hooked):- ignore(( \+ ((forall(load_hook0(Load,Hooked),true))))),!.


load_hook0(_,_):- \+ preview_compiler,!.
load_hook0(Load,metta_defn(Self,H,B)):-
       functs_to_preds([=,H,B],Preds),
       assert_preds(Self,Load,Preds).
load_hook0(Load,metta_atom(Self,H)):- B = 'True',
       H\=[':'|_], functs_to_preds([=,H,B],Preds),
       assert_preds(Self,Load,Preds).


use_metta_compiler:- option_value('compile','full'), !.
preview_compiler:- \+ option_value('compile',false), !.
%preview_compiler:- use_metta_compiler,!.

metta_anew1(load,OBO):- subst_vars(OBO,Cl),assert_if_new(Cl). %to_metta(Cl).
metta_anew1(unload,OBO):- subst_vars(OBO,Cl),ignore((clause(Cl,_,Ref),clause(Cl2,_,Ref),Cl=@=Cl2,erase(Ref),pp_m(Cl))).

metta_anew(Load,Src,OBO):- format('~N'), color_g_mesg('#0f0f0f',(write('  ; Action: '),writeq(Load=OBO))),
   color_g_mesg('#ffa500', write_src(Src)),metta_anew1(Load,OBO),format('~n').

assert_to_metta(_):- reached_file_max,!.
assert_to_metta(OBO):-
 functor(OBO,Fn,A),
 ignore(( A>=2,A<700,
 must_det_ll((
  heartbeat,
  OBO=..[Fn|Cols],
  make_assertion4(Fn,Cols,Data,OldData),
  functor(Data,FF,AA),
  decl_fb_pred(FF,AA),
  ((fail,call(Data))->true;(
   must_det_ll((assert(Data),incr_file_count(_),
     ignore((((should_show_data(X),
       ignore((OldData\==Data,write('; oldData '),write_src(OldData),format('  ; ~w ~n',[X]))),
       write_src(Data),format('  ; ~w ~n',[X]))))),
     ignore((
       fail, option_value(output_stream,OutputStream),
       is_stream(OutputStream),
       should_show_data(X1),X1<1000,must_det_ll((display(OutputStream,Data),writeln(OutputStream,'.'))))))))))))),!.

assert_MeTTa(_):- reached_file_max,!.
assert_MeTTa(OBO):- assert_to_metta(OBO),!.
assert_MeTTa(Data):- !, heartbeat, functor(Data,F,A), A>=2,
   decl_fb_pred(F,A),
   incr_file_count(_),
   ignore((((should_show_data(X),
       write(newData(X)),write(=),write_src(Data))))),
   assert(Data),!.



:- dynamic((metta_type/3,metta_defn/3,metta_atom/2)).

into_space(Self,'&self',Self):-!.
into_space(_,Other,Other).
into_name(_,Other,Other).

%eval_f_args(Depth,Self,F,ARGS,[F|EARGS]):- maplist(eval_args(Depth,Self),ARGS,EARGS).


combine_result(TF,R2,R2):- TF == [], !.
combine_result(TF,_,TF):-!.


do_metta1_e(_Self,_,exec(Exec)):- !,write_exec(Exec),!.
do_metta1_e(_Self,_,[=,A,B]):- !, with_concepts(false,
  (write('(= '), with_indents(false,write_src(A)), (is_list(B) -> nl ; true),write(' '),with_indents(true,write_src(B)),write(')'))),nl.
do_metta1_e(_Self,_LoadExec,Term):- write_src(Term),nl.

write_exec(Exec):-
  wots(S,write_src(exec(Exec))),
  nb_setval(exec_src,Exec),
  ignore((format('~N'),mnotrace((color_g_mesg('#004400',(writeln(S))))))).

%do_metta(Self,LoadExec,Term):-
%  once(untyped_to_metta(Term,NewTerm)),Term\=@=NewTerm,!,
%  do_metta(Self,LoadExec,NewTerm),!.
do_metta(Self,LoadExec,Term):- must_det_ll(do_metta1(Self,LoadExec,Term))*->true;
                                pp_m(unknown_do_metta(Self,LoadExec,Term)).

do_metta1(Self,_,Cmt):- nonvar(Cmt),do_metta_cmt(Self,Cmt),!.

do_metta1(_Slf,load,exec(Exec)):- option_value('exec',skip),!,write_exec(Exec),!.
do_metta1(_Slf,load,call(:- Term)):- !, repl_call(Term).
do_metta1(_Slf,load,call(Term)):- !, repl_call(Term).

do_metta1(Self,_,exec(Exec)):- !,do_metta_exec(Self,Exec),!.
do_metta1(_Slf,exec,Exec):- option_value('exec',skip),!,write_exec(Exec),!.
do_metta1(Self,exec,Exec):- !,do_metta_exec(Self,Exec),!.

do_metta1(Self,Load,Src):- do_metta1(Self,Load,Src,Src),!.

do_metta1(Self,Load,[':',Fn,Type], Src):- \+ is_list(Type),!,
 must_det_ll((
  color_g_mesg('#ffa500',metta_anew(Load,Src,metta_atom(Self,[':',Fn,Type]))))),!.

do_metta1(Self,Load,[':',Fn,TypeDecL], Src):-
 must_det_ll((
  decl_length(TypeDecL,Len),LenM1 is Len - 1, last_element(TypeDecL,LE),
  color_g_mesg('#ffa500',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecL]))),
  metta_anew1(Load,metta_arity(Self,Fn,LenM1)),
  arg_types(TypeDecL,[],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),!,
  metta_anew1(Load,metta_last(Self,Fn,LE)))).


do_metta1(Self,Load,[':',Fn,TypeDecL,RetType], Src):-
 must_det_ll((
  decl_length(TypeDecL,Len),
  append(TypeDecL,[RetType],TypeDecLRet),
  color_g_mesg('#ffa500',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecLRet]))),
  metta_anew1(Load,metta_arity(Self,Fn,Len)),
  arg_types(TypeDecL,[RetType],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),
  metta_anew1(Load,metta_return(Self,Fn,RetType)))),!.

/*do_metta1(Self,Load,PredDecl, Src):-fail,
   metta_anew(Load,Src,metta_atom(Self,PredDecl)),
   ignore((PredDecl=['=',Head,Body], metta_anew(Load,Src,metta_defn(Self,Head,Body)))),
   ignore((Body == 'True',!,do_metta1(Self,Load,Head))),
   nop((fn_append(Head,X,Head), fn_append(PredDecl,X,Body), metta_anew((Head:- Body)))),!.*/

do_metta1(Self,Load,['=',PredDecl,False], Src):- (False == [];False == 'Nil';False == 'F'),!,
  do_metta1(Self,Load,['=',PredDecl,'False'], Src).

do_metta1(Self,Load,['=',Head,PredDecl], Src):-!,
 must_det_ll((
    discover_head(Self,Load,Head),
    color_g_mesg('#ffa500',metta_anew(Load,Src,metta_defn(Self,Head,PredDecl))),
    discover_body(Self,Load,PredDecl),
    nop((fn_append(Head,X,Head),fn_append(PredDecl,X,Body), metta_anew((Head:- Body)))))),!.

do_metta1(Self,Load,PredDecl, Src):-
   ignore(discover_head(Self,Load,PredDecl)),
   color_g_mesg('#ffa500',metta_anew(Load,Src,metta_atom(Self,PredDecl))).

do_metta_exec(Self,Var):- var(Var), !, pp_m(eval(Var)), freeze(Var,wdmsg(laterVar(Self,Var))).

do_metta_exec(_Self,TermV):- use_metta_compiler, !,
 (( /*must_det_ll*/((
  write_exec(TermV),
 % ignore(Res = '$VAR'('ExecRes')),
  RealRes = Res,
  compile_for_exec(Res,TermV,ExecGoal),!,
  subst_vars(Res+ExecGoal,Res+Term,NamedVarsList),
  copy_term(NamedVarsList,Was),
  term_variables(Term,Vars),
  %nl,writeq(Term),nl,
  ((\+ \+
  ((numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(bind)]),
  %nb_current(variable_names,NamedVarsList),
  nl,print(subst_vars(Term,NamedVarsList,Vars)),nl)))),
  nop(maplist(verbose_unify,Vars)))))),
  %NamedVarsList=[_=RealRealRes|_],
  var(RealRes),
  X = RealRes,
  forall(may_rtrace(Term),
     ignore(mnotrace(((color_g_mesg(yellow,
     ((write(' '),

        write_src(X),nl,
        (NamedVarsList\=@=Was-> (color_g_mesg(green,writeq(NamedVarsList)),nl); true),
        ignore(( \+ is_list(X),compound(X),format(' % '),writeq(X),nl)))))))))).

do_metta_exec(Self,TermV):-!,
 mnotrace(( must_det_ll((
  if_t(preview_compiler,write_compiled_exec(TermV,_Goal)),
  \+ \+ write_exec(TermV),
  subst_vars(TermV,Term,NamedVarsList),
  copy_term(NamedVarsList,Was),
  term_variables(Term,Vars),
  %nl,writeq(Term),nl,
  skip((\+ \+
  ((numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(bind)]),
  %nb_current(variable_names,NamedVarsList),
  nl,print(subst_vars(TermV,Term,NamedVarsList,Vars)),nl)))),
  option_else('stack-max',StackMax,100),
  nop(maplist(verbose_unify,Vars)))))),
  forall(may_rtrace(eval_args(StackMax,Self,Term,X)),
     ignore(mnotrace(((color_g_mesg(yellow,
     ((write(' '),
        write_src(X),nl,
        (NamedVarsList\=@=Was-> (color_g_mesg(green,writeq(NamedVarsList)),nl); true),
        ignore(( \+ is_list(X),compound(X),format(' % '),writeq(X),nl)))))))))).

write_compiled_exec(Exec,Goal):-
%  ignore(Res = '$VAR'('ExecRes')),
  compile_for_exec(Res,Exec,Goal),
  mnotrace((color_g_mesg('#114411',portray_clause(exec(Res):-Goal)))).

verbose_unify(Term):- verbose_unify(trace,Term).
verbose_unify(What,Term):- term_variables(Term,Vars),maplist(verbose_unify0(What),Vars),!.
verbose_unify0(What,Var):- put_attr(Var,verbose_unify,What).
verbose_unify:attr_unify_hook(Attr, Value) :-
    format('~N~q~n',[verbose_unify:attr_unify_hook(Attr, Value)]),
    vu(Attr,Value).
vu(_Attr,Value):- is_ftVar(Value),!.
vu(fail,_Value):- !, fail.
vu(true,_Value):- !.
vu(trace,_Value):- trace.
:- nodebug(metta(eval)).
:- nodebug(metta(exec)).

may_rtrace(Goal):- debugging(metta(eval)),use_metta_compiler,!,rtrace(call(Goal)).
may_rtrace(Goal):-
(option_value('exec',rtrace);debugging(metta(exec))),!,  rtrace(Goal).
may_rtrace(Goal):- use_metta_compiler,!, (time_eval(Goal)*->true;(trace_on_fail,rtrace(call(Goal)))).
may_rtrace(Goal):- time_eval(Goal).

% Measures the execution time of a Prolog goal and displays the duration in seconds,
% milliseconds, or microseconds, depending on the execution time.
%
% Args:
%   - Goal: The Prolog goal to be executed and timed.
%
% The predicate uses the `statistics/2` predicate to measure the CPU time before
% and after executing the provided goal. It calculates the elapsed time in seconds
% and converts it to milliseconds and microseconds. The output is formatted to
% provide clear timing information:
%
% - If the execution takes more than 2 seconds, it displays the time in seconds.
% - If the execution takes between 1 millisecond and 2 seconds, it displays the time
%   in milliseconds.
% - If the execution takes less than 1 millisecond, it displays the time in microseconds.
%
% Example usage:
%   ?- time_eval(my_goal(X)).
%
%   ?- time_eval(sleep(0.95)).
%
% Output examples:
%   ; Evaluation took 2.34 seconds.
%   ; Evaluation took 123.45 ms.
%   ; Evaluation took 0.012 ms. (12.33 microseconds)
%
time_eval(Goal) :-
    statistics(cputime, Start),
    call(Goal),
    statistics(cputime, End),
    Seconds is End - Start,
    Milliseconds is Seconds * 1_000,
    (Seconds > 2
        -> format('; Evaluation took ~2f seconds.~n', [Seconds])
        ; (Milliseconds >= 1
            -> format('; Evaluation took ~2f ms.~n', [Milliseconds])
            ;( Micro is Milliseconds * 1_000,
              format('; Evaluation took ~3f ms. (~2f microseconds) ~n', [Milliseconds, Micro])))).



repl_call(Term):- catch_red(Term).

catch_red(Term):- catch(Term,E,pp_m(red,in(Term,E))).

s2p(I,O):- sexpr_s2p(I,O),!.


discover_head(Self,Load,[Fn|PredDecl]):-
 nop(( arg_types(PredDecl,[],EachArg),
  metta_anew1(Load,metta_head(Self,Fn,EachArg)))).

discover_body(Self,Load,Body):-
  nop(( [Fn|PredDecl] = Body, arg_types(PredDecl,[],EachArg),
  metta_anew1(Load,metta_body(Self,Fn,EachArg)))).

decl_length(TypeDecL,Len):- is_list(TypeDecL),!,length(TypeDecL,Len).
decl_length(_TypeDecL,1).

arg_types([['->'|L]],R,LR):-!, arg_types(L,R,LR).
arg_types(['->'|L],R,LR):-!, arg_types(L,R,LR).
arg_types(L,R,LR):- append(L,R,LR).

:- ensure_loaded('../../examples/factorial').
:- ensure_loaded('../../examples/fibonacci').

%print_preds_to_functs:-preds_to_functs_src(factorial_tail_basic)


:- ignore(((
   \+ prolog_load_context(reloading,true),
   metta_final,
   load_history,
   loon))).

