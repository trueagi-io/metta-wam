/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Lisprolog -- Interpreter for a simple Lisp. Written in Prolog.
    Written Nov. 26th, 2006 by Markus Triska (triska@gmx.at).
    Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:-module(metta_interp,[codelist_to_forms_i/2]).
:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
:- set_prolog_flag(double_quotes, codes).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:-dynamic(user:mpred_prop/2).
:-multifile(user:mpred_prop/2).
%:- use_module(library(logicmoo_utils)).

:- assert((user:'$exported_op'(_,_,_):- fail)).
:- abolish((system:'$exported_op'/3)).
:- assert((system:'$exported_op'(_,_,_):- fail)).

parsing(String, Expr) :- string(String),!,string_codes(String,Codes),phrase(expressions(Expr), Codes).
parsing(String, Expr) :- phrase(expressions(Expr), String).

expressions([E|Es]) -->
    ws, expression(E), ws,
    !, % single solution: longest input match
    expressions(Es).
expressions([]) --> [].

% ws --> ";",until_eol,
ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% A number N is represented as n(N), a symbol S as s(S).

expression(s(A))         --> symbol(Cs), { atom_codes(A, Cs) }.
expression(n(N))         --> number(Cs), { number_codes(N, Cs) }.
expression(List)         --> [L],{is_bracket_lr(L,R)},expressions(List), [R].
expression([s(quote),Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

symbol([A|As]) -->
    [A],
    { is_ok_symbolchar(A) },
    symbolr(As).

symbolr([A|As]) -->
    [A],
    { is_ok_symbolchar(A) ; code_type(A, alnum) },
    symbolr(As).
symbolr([]) --> [].

is_bracket_lr(L,R):- member(LR,["()","{}","[]","\"\""]), nth0(0,LR,L),nth0(1,LR,R).
is_ok_symbolchar(A):- \+ code_type(A, space), \+ code_type(A, white), \+ is_bracket_lr(A,_), \+ is_bracket_lr(_,A).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

codelist_to_forms_i(AsciiCodesList,FormsOut):-
    parsing(AsciiCodesList, Forms0),
    compile_all(Forms0, FormsOut),!.

run(Program, Values) :-
    parsing(Program, Forms0),
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).

unfunc(s(S), S).
unfunc(t, t).
unfunc(n(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).

fold([], _, V, n(V)).
fold([n(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).

compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

compile(F0, F) :-
    (   F0 = n(_)   -> F = F0
    ;   F0 = s(t)   -> F = t
    ;   F0 = s(nil) -> F = []
    ;   F0 = s(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = [s(quote),Arg] -> F = [quote,Arg]
    ;   F0 = [s(setq),s(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = [s(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = [s(defun),s(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = [s(Op)|Args0] -> compile_all(Args0, Args), F = [user(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval(n(N), n(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval(s(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, [n(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], s(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval(user(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval('bind!', [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate goal_truth(0,*,//,//).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.

bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).

run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).

%if_script_file_time(X):-if_startup_script(time(X)).
if_script_file_time(_):-!.
%if_script_file_time(X):- nop(time(X)).

% Append:
    :- if_script_file_time(run("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].


% Fibonacci, naive version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].


% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].


% Fibonacci, iterative version:
    :- if_script_file_time(run("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 30,794 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 12831368 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].



% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 350)")).

    %@ % 44,595 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 14526532 Lips)
    %@ V = [fib, fib1, 6254449428820551641549772190170184190608177514674331726439961915653414425].


% Higher-order programming and eval:
    :- if_script_file_time(run("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].

%:- ensure_loaded(metta_reader).

:- set_prolog_flag(double_quotes, string).

:- ensure_loaded(swi_support).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(swi_flybase).
:- set_prolog_flag(verbose_autoload, false).
:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(verbose_load, silent).
:- ensure_loaded(library(logicmoo_utils)).

load_metta(File):- load_metta('&self',File).
load_metta(Self,Filename):-
 atom(Filename),exists_file(Filename),!,
 track_load_into_file(Filename,
   setup_call_cleanup(open(Filename,read,In),
    ((directory_file_path(Directory, _BaseName, Filename),
      with_cwd(Directory,once(load_metta_stream(Self,In))))), close(In))).
load_metta(Self,Filename):- with_wild_path(load_metta(Self),Filename),!.

load_metta_stream(Fn,String):- string(String),!,open_string(String,Stream),load_metta_stream(Fn,Stream).
load_metta_stream(_Fn,In):- (at_end_of_stream(In);reached_file_max),!.
load_metta_stream(Self,In):- repeat,
  once(read_metta(In,Read)),
  once((Read==end_of_file->true;do_metta(Self,load,Read))),
  (at_end_of_stream(In);reached_file_max),!.

read_metta(In,Read):- read_metta1(In,Read1),
  (Read1=='!'
     -> (read_metta1(In,Read2), Read=exec(Read2))
     ; Read = Read1).

repl:-
   repeat, make,
   ((nb_current(self_space,Self),Self\==[])->true;Self='&self'),
   format('~N'), format(atom(P),'metta@~w: ',[Self]),
   setup_call_cleanup(prompt(Was,P),
      once((current_input_to_forms(Read,_),do_repl(Self,Read))),
       prompt(_,Was)).
do_repl(_Self,end_of_file):- writeln('\n\n% To restart, use: ?- repl.').
do_repl(Self,!):- !, current_input_to_forms(Exec,_), !,
  (with_output_to(string(H),(write('!'),write_src(Exec))),add_history1(H)),
  do_metta_exec(Self,Exec),!,fail.
do_repl(Self,Read):-
  (with_output_to(string(H),write_src(Read)),add_history1(H)),
  do_metta(Self,load,Read),!,fail.

read_metta1(In,Read):- current_input(In0),In==In0,!,current_input_to_forms(Read,_Vars).
read_metta1(_,O2):- clause(t_l:s_reader_info(O2),_,Ref),erase(Ref).
read_metta1(In,Read):- peek_char(In,Char), read_metta1(In,Char,Read).
read_metta1(In,Char,Read):- char_type(Char,white),get_char(In,Char),put(Char),!,read_metta1(In,Read).
read_metta1(In,';',Read):- read_line_to_string(In,Str),write_comment(Str),!,read_metta1(In,Read).
read_metta1(In,_,Read1):- once(parse_sexpr_untyped(In,Read1)),!.


write_comment(Cmt):- format('~N%;~w~n',[Cmt]).
do_metta_cmt(_,'$COMMENT'(Cmt,_,_)):- write_comment(Cmt),!.
do_metta_cmt(_,'$STRING'(Cmt)):- write_comment(Cmt),!.
do_metta_cmt(Self,[Cmt]):- !, do_metta_cmt(Self, Cmt),!.


mfix_vars1(I,O):- var(I),!,I=O.
mfix_vars1([H|T],[HH|TT]):- !, mfix_vars1(H,HH),mfix_vars1(T,TT).
mfix_vars1(I,O):- \+ atom(I),!,I=O.
mfix_vars1('$_','$VAR'('_')).
mfix_vars1('$','$VAR'('_1')).
mfix_vars1(I,'$VAR'(O)):- atom_concat('$',N,I),atom_number(N,Num),atom_concat('Num',Num,M),!,svar_fixvarname(M,O).
mfix_vars1(I,'$VAR'(O)):- atom_concat('$',M,I),!,svar_fixvarname(M,O).
mfix_vars1(I,I).

cons_to_l3(Cons,[Cons0,H,T],[H|TT]):- !, Cons0==Cons,!, cons_to_l3(Cons,T,TT).
cons_to_l3(Cons,Nil0,T):- is_cf_nil(Cons,Nil),Nil0==Nil,!,T=[].
cons_to_l3(_Cons,A,A).

cons_to_l(I,O):- I=='Nil',!,O=[].
cons_to_l(C,O):- \+ compound(C),!,O=C.
%cons_to_l(N,NO):- cons_to_l3('Cons',N,NO),!.
cons_to_l([Cons,H|T],[HH|TT]):- Cons=='Cons',!, cons_to_l(H,HH),cons_to_l(T,TT).
cons_to_l([Cons|List],List):- Cons=='::',!.
cons_to_l([H|T],[HH|TT]):- !, cons_to_l(H,HH),cons_to_l(T,TT).
cons_to_l(I,I).

is_cons_f(Cons):- is_cf_nil(Cons,_).
is_cf_nil('Cons','Nil').

maybe_fix_vars(I,exec(O)):- compound(I),I=exec(M),!,maybe_fix_vars(M,O).
maybe_fix_vars(I,O):-
 must_det_ll((
  mfix_vars1(I,M),
  subst_vars(M,N),
  cons_to_l(N,O))).

subst_vars(M,N):- sub_term(V,M),compound(V),V='$VAR'(_),!,
  substM(M,V,_NewVar,MM),!,subst_vars(MM,N).
subst_vars(M,M).

metta_anew(load,Cl):- assert_if_new(Cl),ppm(Cl).
metta_anew(unload,Cl):- ignore((clause(Cl,_,Ref),clause(Cl2,_,Ref),Cl=@=Cl2,erase(Ref),ppm(Cl))).

ppm(Cl):- format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print(Cl),writeln('.'))))).

:- dynamic((metta_type/3,metta_defn/3,metta_atom/2)).

into_space(Self,'&self',Self):-!.
into_space(_,Other,Other).



substM(T, F, R, R):- T==F,!.
substM(T, _, _, R):- \+ compound(T),!,R=T.
substM([H1|T1], F, R, [H2|T2]) :- !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- C1 =.. [Fn|A1], substM_l(A1,F,R,A2),!, C2 =.. [Fn|A2].
substM_l([], _, _, []).  substM_l([H1|T1], F, R, [H2|T2]) :- substM(H1, F, R, H2), substM_l(T1, F, R, T2).


eval_f_args(Self,F,ARGS,[F|EARGS]):- maplist(eval_args(Self),ARGS,EARGS).

s2p(I,O):- sexpr_sterm_to_pterm(I,O),!.

self_eval(X):- var(X),!.
self_eval(X):- number(X),!.
self_eval([]). self_eval('True'). self_eval('False').

combine_result(TF,_,TF):-!.
/*
; Bind &kb to a new empty Space
!(bind! &kb (new-space))

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

; For anything that is green, assert it is Green in &kb
!(ift (green $x)
      (add-atom &kb (Green $x)))

; Retrieve the inferred Green things: Fritz and Sam.
!(assertEqualToResult
  (match &kb (Green $x) $x)
  (Fritz Sam))
*/

eval_args(Self,X,Y):- nonvar(Y),Y=='True',!,eval_args(Self,X,XX),XX\=='False'.
eval_args(_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args(Self,[V|VL],[V|VL]):- var(V),!.
eval_args(Self,[X|Nil],[Y]):- Nil ==[],!,eval_args(Self,X,Y).
eval_args(Self,[assertEqualToResult,X,Y],TF):- !, findall(E,eval_args(Self,X,E),L),!,trace,as_f_t(L\==Y,TF).
eval_args(Self,X,Y):- is_list(X),!,eval_args1(Self,X,M),(M\==X->eval_args(Self,M,Y);Y=X).

%eval_args1(Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args1(Self,PredDecl,Res):- term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Var=Res);
   (eval_args2(Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Var=Res))).
%eval_args1(Self,PredDecl,Res):- eval_args2(Self,PredDecl,Res).

do_metta(Self,LoadExec,Term):- once(maybe_fix_vars(Term,NewTerm)),Term\=@=NewTerm,!,
  do_metta(Self,LoadExec,NewTerm),!.
do_metta(Self,LoadExec,Term):- do_metta1(Self,LoadExec,Term),!.
do_metta(Self,LoadExec,Term):- ppm(unknown_do_metta(Self,LoadExec,Term)).

do_metta1(Self,_,Cmt):- nonvar(Cmt),do_metta_cmt(Self,Cmt),!.
do_metta1(Self,_,exec(Exec)):- !,do_metta_exec(Self,Exec),!.
do_metta1(Self,exec,Exec):- !,do_metta_exec(Self,Exec),!.

do_metta1(Self,Load,[':',Fn,TypeDecl]):- metta_anew(Load,metta_type(Self,Fn,TypeDecl)),!.
do_metta1(Self,Load,['=',PredDecl,True]):- True == 'True',!, metta_anew(Load,metta_atom(Self,PredDecl)).
do_metta1(Self,Load,['=',HeadFn,PredDecl]):- metta_anew(Load,metta_defn(Self,HeadFn,PredDecl)), nop((fn_append(HeadFn,X,Head), fn_append(PredDecl,X,Body), metta_anew((Head:- Body)))),!.
do_metta1(Self,Load,PredDecl):- metta_anew(Load,metta_atom(Self,PredDecl)).

do_metta_exec(Self,['import!',Other,File]):- into_space(Self,Other,Space),!, load_metta(Space,File).
do_metta_exec(Self,Var):- var(Var), !, ppm(eval(Var)), freeze(Var,wdmsg(laterVar(Self,Var))).
do_metta_exec(Self,Term):-!, ppm(:- metta_eval(Term)),forall(eval_args(Self,Term,X),(format('%'),writeln(X))),!.

eval_args2(Self,[ift,CR,Then],RO):- trace,
   metta_defn(Self,[ift,R,Then],Become),eval_args(Self,CR,R),eval_args(Self,Then,_True),eval_args(Self,Become,RO).
eval_args2(Self,X,Y):- metta_defn(Self,X,W),ppm(metta_defn(Self,X,W)), eval_args(Self,W,Y).
eval_args2(Self,['superpose'|List],Res):- !, maplist(eval_args(Self),List,Res).
eval_args2(Self,['colapse'|List],Flat):- !, maplist(eval_args(Self),List,Res),flatten(Res,Flat).
eval_args2(Self,['let',A,A5,AA],AAO):- !,eval_args(Self,A5,A),eval_args(Self,AA,AAO).
eval_args2(Self,['let*',[Let0|LetRest],Body],RetVal):-
    findall('let'(Var,Val), member([Var,Val],[Let0|LetRest]),LetStars),
    eval_args(Self,[progn,[progn|LetStars],Body],RetVal).

eval_args2(Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args(Self,[F2|X2],Y2),eval_args(Self,X1,Y1).
eval_args2(Self,[F|X],[F|Y]):- is_function(F),is_list(X),maplist(eval_args(Self),X,Y),X\=@=Y.
eval_args2(Self,LIS,Y):-  notrace((catch((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.
eval_args2(Self,[or,X,Y],TF):- !, (eval_args(Self,X,TF);eval_args(Self,Y,TF)).
eval_args2(Self,[and,X|Y],TF):- eval_args(Self,X,TF1),eval_args2(Self,[and|Y],TF2),combine_result(TF1,TF2,TF).
eval_args2(Self,[if,TF,Then,Else],Res):- !, ( \+ eval_args(Self,TF,'False') -> eval_args(Self,Then,Res);eval_args(Self,Then,Res) ).
eval_args2(Self,['add-atom',Other,PredDecl],PredDecl):- !, do_metta(Other,load,PredDecl).
eval_args2(Self,['remove-atom',Other,PredDecl],PredDecl):- !, do_metta(Other,unload,PredDecl).
eval_args2(Self,['atom-count',Other],Count):- !, findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Other,_),L2),length(L2,C2),Count is C1+C2.
eval_args2(Self,['atom-replace',Other,Rem,Add],PredDecl):- !, copy_term(Rem,RCopy),metta_atom_iter_ref(Other,RCopy,Ref),
  RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add).
eval_args2(Self,['get-atoms',Other],PredDecl):- !, metta_atom_iter(Other,PredDecl).
eval_args2(Self,['match',Other,Goal,Template],Template):- !, metta_atom_iter(Other,Goal).
eval_args2(Self,['case',A,[Case1|CaseN]],Res):-
          eval_args(Self,A,AR),
          member(Case,[Case1|CaseN]),
          Case=[A|More],
          eval_args2(Self,More,AR).

eval_args2(Self,[==,X,Y],TF):-!,as_f_t(X\=Y,TF).
eval_args2(Self,['>',X,Y],TF):-!,as_f_t(X@=<Y,TF).
eval_args2(Self,['<',X,Y],TF):-!,as_f_t(X@>=Y,TF).
eval_args2(Self,['=>',X,Y],TF):-!,as_f_t(X@<Y,TF).
eval_args2(Self,['<=',X,Y],TF):-!,as_f_t(X@>Y,TF).
eval_args2(Self,[assertEqual,X,Y],TF):-!,as_f_t(X\=Y,TF).
as_f_t(G,'False'):- call(G),!. as_f_t(_, 'True').

%metta_atom_iter(Other,H):- metta_atom(Other,H).
metta_atom_iter(Other,H):- eval_args(Other,H,_).


metta_atom_iter(Other,[=,H,B]):-metta_defn(Other,H,B).
metta_atom_iter(Other,H):-metta_atom(Other,H).
metta_atom_iter_ref(Other,[=,H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).

is_function(F):- atom(F).
is_false(X):- eval_args(Self,X,Y), (Y=0;Y='False'),!.
is_conz(S):- compound(S), S=[_|_].

%dont_x(eval_args(Self,metta_if(A<B,L1,L2),R)).
dont_x(eval_args(Self,A<B,R)).

into_fp(D,D):- \+ \+ dont_x(D),!.
into_fp(ListX,CallAB):-
  sub_term(STerm,ListX),needs_expanded(STerm,Term),
  %copy_term(Term,CTerm),
  =(Term,CTerm),
  substM(ListX,CTerm,Var,CallB), fn_append1(Term,Var,CallA),
  into_fp((CallA,CallB),CallAB).
into_fp(A,A).

needs_expand(Expand):- compound(Expand),functor(Expand,F,N),N>=1,atom_concat(metta_,_,F).
needs_expanded(eval_args(Self,Term,_),Expand):- !,sub_term(Expand,Term),compound(Expand),Expand\=@=Term,
   compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).
needs_expanded([A|B],Expand):- sub_term(Expand,[A|B]), compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).

fn_append1(eval_args(Self,Term,X),X,eval_args(Self,Term,X)):-!.
fn_append1(Term,X,eval_args(Self,Term,X)).
do:- cls, make, current_prolog_flag(argv,P),append(_,['--args'|Rest],P),maplist(load_metta('&self'),Rest).


quick_test:-
  set_prolog_flag(encoding,iso_latin_1),
   forall(quick_test(Test),
                  forall(open_string(Test,Stream),
                    load_metta_stream('&self',Stream))).

end_of_file. % comment this out once to get these files in your readline history
mf('./1-VSpaceTest.metta').
mf('./2-VSpaceTest.metta').
mf('./3-Learn-Rules.metta').
mf('./4-VSpaceTest.metta').
mf('./5-Learn-Flybase.metta').
mf('./6-Learn-Flybase-Full.metta').
mf('./8-VSpaceTest.metta').
mf('./autoexec.metta').
mf('./data/OBO-Metta/export/Alliance_of_Genome_Resources.metta').
mf('./data/OBO-Metta/export/biosapiens.metta').
mf('./data/OBO-Metta/export/chebi_fb_2023_04.metta').
mf('./data/OBO-Metta/export/DBVAR.metta').
mf('./data/OBO-Metta/export/doid.metta').
mf('./data/OBO-Metta/export/flybase_controlled_vocabulary.metta').
mf('./data/OBO-Metta/export/flybase_stock_vocabulary.metta').
mf('./data/OBO-Metta/export/fly_anatomy.metta').
mf('./data/OBO-Metta/export/fly_development.metta').
mf('./data/OBO-Metta/export/gene_group_FB2023_04.metta').
mf('./data/OBO-Metta/export/go-basic.metta').
mf('./data/OBO-Metta/export/image.metta').
mf('./data/OBO-Metta/export/psi-mi.metta').
mf('./data/OBO-Metta/export/slice.chebi.metta').
mf('./data/OBO-Metta/export/so-simple.metta').
mf('./data/OBO-Metta/export/so.metta').
mf('./data/OBO-Metta/export/SOFA.metta').
mf('./examples/compat/common/BelieveMe.metta').
mf('./examples/compat/common/EqualityType.metta').
mf('./examples/compat/common/EqualityTypeTest.metta').
mf('./examples/compat/common/formula/DeductionFormula.metta').
mf('./examples/compat/common/formula/DeductionFormulaTest.metta').
mf('./examples/compat/common/formula/ImplicationDirectIntroductionFormula.metta').
mf('./examples/compat/common/formula/ModusPonensFormula.metta').
mf('./examples/compat/common/In.metta').
mf('./examples/compat/common/InTest.metta').
mf('./examples/compat/common/List.metta').
mf('./examples/compat/common/ListTest.metta').
mf('./examples/compat/common/Maybe.metta').
mf('./examples/compat/common/MaybeTest.metta').
mf('./examples/compat/common/Num.metta').
mf('./examples/compat/common/NumTest.metta').
mf('./examples/compat/common/OrderedSet.metta').
mf('./examples/compat/common/OrderedSetTest.metta').
mf('./examples/compat/common/Record.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValue.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValueTest.metta').
mf('./examples/compat/common/truthvalue/MeasEq.metta').
mf('./examples/compat/common/truthvalue/TemporalTruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValueTest.metta').
mf('./examples/compat/dependent-types/DeductionDTL.metta').
mf('./examples/compat/dependent-types/DeductionDTLTest.metta').
mf('./examples/compat/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTL.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ModusPonensDTL.metta').
mf('./examples/compat/dependent-types/ModusPonensDTLTest.metta').
mf('./examples/compat/entail/DeductionEntail.metta').
mf('./examples/compat/entail/DeductionEntailTest.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntail.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntailTest.metta').
mf('./examples/compat/equal/DeductionEqual.metta').
mf('./examples/compat/equal/DeductionEqualTest.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqual.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqualTest.metta').
mf('./examples/compat/match/DeductionImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/match/DeductionMatch.metta').
mf('./examples/compat/match/DeductionMatchTest.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatch.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/prob-dep-types/inf_order_probs.metta').
mf('./examples/compat/prob-dep-types/prob_dep_types.metta').
mf('./examples/compat/recursion-schemes/src/base.metta').
mf('./examples/compat/recursion-schemes/src/examples/benchmark.metta').
mf('./examples/compat/recursion-schemes/src/examples/expression.metta').
mf('./examples/compat/recursion-schemes/src/schemes.metta').
mf('./examples/compat/synthesis/experiments/non-determinism.metta').
mf('./examples/compat/synthesis/experiments/self-contained-synthesize.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-superpose.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-type-checking.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify.metta').
mf('./examples/compat/synthesis/experiments/unify-via-case.metta').
mf('./examples/compat/synthesis/experiments/unify-via-let.metta').
mf('./examples/compat/synthesis/Synthesize.metta').
mf('./examples/compat/synthesis/SynthesizeTest.metta').
mf('./examples/compat/synthesis/Unify.metta').
mf('./examples/compat/synthesis/UnifyTest.metta').
mf('./examples/compat/test_scripts/a1_symbols.metta').
mf('./examples/compat/test_scripts/a2_opencoggy.metta').
mf('./examples/compat/test_scripts/a3_twoside.metta').
mf('./examples/compat/test_scripts/b0_chaining_prelim.metta').
mf('./examples/compat/test_scripts/b1_equal_chain.metta').
mf('./examples/compat/test_scripts/b2_backchain.metta').
mf('./examples/compat/test_scripts/b3_direct.metta').
mf('./examples/compat/test_scripts/b4_nondeterm.metta').
mf('./examples/compat/test_scripts/b5_types_prelim.metta').
mf('./examples/compat/test_scripts/c1_grounded_basic.metta').
mf('./examples/compat/test_scripts/c2_spaces.metta').
mf('./examples/compat/test_scripts/c2_spaces_kb.metta').
mf('./examples/compat/test_scripts/c3_pln_stv.metta').
mf('./examples/compat/test_scripts/d1_gadt.metta').
mf('./examples/compat/test_scripts/d2_higherfunc.metta').
mf('./examples/compat/test_scripts/d3_deptypes.metta').
mf('./examples/compat/test_scripts/d4_type_prop.metta').
mf('./examples/compat/test_scripts/d5_auto_types.metta').
mf('./examples/compat/test_scripts/e1_kb_write.metta').
mf('./examples/compat/test_scripts/e2_states.metta').
mf('./examples/compat/test_scripts/e3_match_states.metta').
mf('./examples/compat/test_scripts/f1_imports.metta').
mf('./examples/compat/test_scripts/f1_moduleA.metta').
mf('./examples/compat/test_scripts/f1_moduleB.metta').
mf('./examples/compat/test_scripts/f1_moduleC.metta').
mf('./examples/compat/test_scripts/_e2_states_dia.metta').
mf('./examples/fibo.metta').
mf('./examples/fwgc.metta').
mf('./examples/httpclient.metta').
mf('./examples/NARS.metta').
mf('./examples/NARS_listing.metta').
mf('./examples/RUN_minnars.metta').
mf('./examples/RUN_tests0.metta').
mf('./examples/RUN_tests1.metta').
mf('./examples/RUN_tests2.metta').
mf('./examples/RUN_tests3.metta').
mf('./examples/send-more.metta').
mf('./examples/talk80.metta').
mf('./examples/VRUN_tests0.metta').
mf('./examples/VRUN_tests1.metta').
mf('./examples/VRUN_tests2.metta').
mf('./examples/VRUN_tests3.metta').
mf('./metta_vspace/nm_test.metta').
mf('./metta_vspace/r.metta').
mf('./metta_vspace/test_nspace.metta').
:- forall(mf(H),add_history1(load_metta(H))).
:- load_metta
