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

ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% A number N is represented as n(N), a symbol S as s(S).

expression(s(A))         --> symbol(Cs), { atom_codes(A, Cs) }.
expression(n(N))         --> number(Cs), { number_codes(N, Cs) }.
expression(List)         --> "(", expressions(List), ")".
expression([s(quote),Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

symbol([A|As]) -->
    [A],
    { memberchk(A, "+/-*><=") ; code_type(A, alpha) },
    symbolr(As).

symbolr([A|As]) -->
    [A],
    { memberchk(A, "+/-*><=") ; code_type(A, alnum) },
    symbolr(As).
symbolr([]) --> [].

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

load_metta_stream(_Fn,In):- (at_end_of_stream(In);reached_file_max),!.
load_metta_stream(Self,In):- repeat,
  once(read_metta(In,Read)),
  once((Read==end_of_file->true;do_metta_once(Self,load,Read))),
  (at_end_of_stream(In);reached_file_max),!.

read_metta(In,Read):- read_metta1(In,Read1),
  (Read1=='!'
     -> (read_metta1(In,Read2), Read=exec(Read2))
     ; Read = Read1).

read_metta1(_,O2):- clause(t_l:s_reader_info(O2),_,Ref),erase(Ref).
read_metta1(In,Read):- peek_char(In,Char), read_metta1(In,Char,Read).
read_metta1(In,Char,Read):- char_type(Char,white),get_char(In,Char),put(Char),!,read_metta1(In,Read).
read_metta1(In,';',Read):- read_line_to_string(In,Str),write_comment(Str),!,read_metta1(In,Read).
read_metta1(In,_,Read1):- once(parse_sexpr_untyped(In,Read1)),!.


write_comment(Cmt):- format('~N~w~n',[Cmt]).
do_metta_cmt(_,_,'$COMMENT'(Cmt,_,_)):- write_comment(Cmt),!.
do_metta_cmt(_,_,'$STRING'(Cmt)):- write_comment(Cmt),!.
do_metta_cmt(Self,Exec,[Cmt]):- !, do_metta_cmt(Self,Exec, Cmt),!.

dont_prexfix(F):- \+ atom(F),!.
dont_prexfix(F):- upcase_atom(F,U),downcase_atom(F,D),U==D,!.
dont_prexfix(call).
dont_prexfix(exec).

%dont_prexfix(if).
dont_prexfix(Bang):- atom_concat(_,'!',Bang).
convert_functor(V,_,V):- \+ atom(V).
convert_functor('Cons',2,'[|]').
convert_functor('cons',2,'[|]').
convert_functor('::',2,'[|]').
convert_functor('Nil',0,[]).
convert_functor('NIL',0,[]).
convert_functor('nil',0,[]).
convert_functor(F,_,F):- dont_prexfix(F),!.
convert_functor(A,_,A):- atom_chars(A,[C|_]),upcase_atom(C,C),!.
%convert_functor(F,N,FN):- N>0, atom_concat(metta_,_,F),!,FN=F.
%convert_functor(F,N,FN):- N>0, atom_concat(metta_,F,FN).
convert_functor(F,_,F).

mfix_vars1(I,O):- compound(I), !,
  compound_name_arguments(I,F,A),
  functor(I,F,N),
  (convert_functor(F,N,C)->true;C=F),
  maplist(mfix_vars1,A,B),
  compound_name_arguments(O,C,B).
mfix_vars1(I,O):- \+ atom(I),!,I=O.
mfix_vars1('$_','$VAR'('_')).
mfix_vars1('$','$VAR'('_1')).
mfix_vars1(I,'$VAR'(O)):- atom_concat('$',M,I),!,svar_fixvarname(M,O).
mfix_vars1(I,O):- convert_functor(I,0,O),!.
mfix_vars1(I,I).

maybe_fix_vars(I,exec(O)):- compound(I),I=exec(M),!,maybe_fix_vars(M,O).
maybe_fix_vars(I,O):-
 must_det_ll((
  once(mfix_vars1(I,M)),
  subst_vars(M,N),
  sexpr_sterm_to_pterm(N,NO),
  once(mfix_vars1(NO,O)))).

'metta_=='(X,Y,XY):- (X==Y->XY=true;XY=false).
'metta_if'(If,Then,Else,Result):- metta_call(If,TF),
  (\+ metta_not(TF)->metta_call(Then,Result);metta_call(Else,Result)).

subst_vars(M,N):- sub_term(V,M),compound(V),V='$VAR'(_),!,
  substM(M,V,_NewVar,MM),!,subst_vars(MM,N).
subst_vars(M,M).

metta_anew(Cl):- assert_if_new(Cl),ppm(Cl).

ppm(Cl):- format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print(Cl))), nl )).

do_metta_once(Self,LoadExec,Term):- once(maybe_fix_vars(Term,NewTerm)),Term\=@=NewTerm,!,
  do_metta_once(Self,LoadExec,NewTerm),!.
do_metta_once(Self,Exec,Term):- do_metta(Self,Exec,Term),!.
do_metta_once(Self,LoadExec,Term):- ppm(unknown_do_metta(Self,LoadExec,Term)).
%do_metta_once(Self,Exec,Term):- fbug((unknown_do_metta(Self,Exec,Term))),!.


into_space(Self,'&self',Self):-!.
into_space(_,Other,Other).

do_metta(Self,Exec,Term):- do_metta_cmt(Self,Exec,Term),!.
do_metta(Self,load,':'(Fn,TypeDecl)):- metta_anew(metta_type(Self,Fn,TypeDecl)),!.
do_metta(Self,load,'='(Fn,PredDecl)):- metta_anew(metta_defn(Self,Fn,PredDecl)), nop((fn_append(Fn,X,Head), fn_append(PredDecl,X,Body), metta_anew((Head:- Body)))),!.
do_metta(Self,_,exec(Exec)):- !,do_metta(Self,exec,Exec),!.
do_metta(Self,_,'import!'(Other,File)):- into_space(Self,Other,Space),!, load_metta(Space,File).
do_metta(Self,load,PredDecl):- metta_anew(metta_decl(Self,PredDecl)).
do_metta(Self,exec,Term):-!,eval_args(Term,X),ppm(X).

substM(T, F, R, R):- T==F,!.
substM(T, _, _, R):- \+ compound(T),!,R=T.
substM([H1|T1], F, R, [H2|T2]) :- !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- C1 =.. [Fn|A1], substM_l(A1,F,R,A2),!, C2 =.. [Fn|A2].
substM_l([], _, _, []).  substM_l([H1|T1], F, R, [H2|T2]) :- substM(H1, F, R, H2), substM_l(T1, F, R, T2).


eval_f_args(F,ARGS,XX):- maplist(eval_args,ARGS,EARGS), compound_name_arguments(XX,F,EARGS).



eval_args(X,Y):- number(X),Y=X,!.
eval_args(X,Y):- var(X),Y=X,!.
eval_args(X,Y):- eval_args1(X,M),(M\==X->eval_args(M,Y);Y=X),!.
eval_args(X,X).

eval_args1([X1|X2],[Y1|Y2]):- compound(X2),eval_args(X1,Y1),eval_args(X2,Y2),!.
eval_args1(IS,Y):- catch(Y is IS,_,fail),!.
eval_args1(if(TF,Then,Else),Res):- !, ( \+ eval_args(TF,'False') -> eval_args(Then,Res);eval_args(Then,Res) ).
eval_args1(==(X,Y),TF):- !, ( X \= Y ->TF='False';TF='True').
eval_args1((X>Y),TF):- !, ( X @=< Y ->TF='False';TF='True').
eval_args1((X<Y),TF):- !, ( X @>= Y ->TF='False';TF='True').
eval_args1(=>(X,Y),TF):- !, ( X @< Y ->TF='False';TF='True').
eval_args1(<=(X,Y),TF):- !, ( X @> Y ->TF='False';TF='True').
eval_args1(assertEqual(X,Y),TF):- !, ( X \= Y ->TF='False';TF='True').
eval_args1(or(X,Y),TF):- !, ( (is_false(X), is_false(Y)) ->TF='False';TF='True').
eval_args1(X,Y):- is_list(X),!,maplist(eval_args,X,Y).
eval_args1(X,Y):- compound(X),compound_name_arguments(X,F,ARGS), once((eval_f_args(F,ARGS,XX), XX\==X,Y=XX)).
eval_args1(X,Y):- metta_defn('&self',X,W),pp(X->W),eval_args(W,Y).

is_false(X):- eval_args(X,Y), (Y=0;Y='False'),!.

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).

is_conz(S):- compound(S), S=[_|_].

%dont_x(eval_args(metta_if(A<B,L1,L2),R)).
dont_x(eval_args(A<B,R)).

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
do:- cls, make, current_prolog_flag(argv,P),append(_,['--args'|Rest],P),maplist(load_metta('&self'),Rest).

%metta_defn(Self,Fn,X,List):- fbug(metta_defn(Self,Fn,X,List)).
