% ==============================
% MeTTa to Prolog transpilation (which uses the Host SWI-Prolog compiler)
% Aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================
:- dynamic(metta_compiled_predicate/3).
:- multifile(metta_compiled_predicate/3).

:- multifile(each_recompile/0).


%:- include(metta_compiler_werk).


:- dynamic(metta_compiled_predicate/3).

%unnumbervars_clause(Cl, ClU):-
%  copy_term_nat(Cl, AC), unnumbervars(AC, UA), copy_term_nat(UA, ClU).
% ===============================
%  Compile in memory buffer
% ===============================
is_clause_asserted(AC):- unnumbervars_clause(AC, UAC),
  expand_to_hb(UAC, H, B), clause(H, B, Ref), clause(HH, BB, Ref),
  strip_m(HH, HHH), HHH=@=H,
  strip_m(BB, BBB), BBB=@=B, !.

strip_m(M:BB, BB):- nonvar(BB), nonvar(M), !.
strip_m(BB, BB).

get_clause_pred(UAC, F, A):- expand_to_hb(UAC, H, _), strip_m(H, HH), functor(HH, F, A).


:- dynamic(needs_tabled/2).


add_assertion(Space, List):- is_list(List), !, maplist(add_assertion(Space), List).
add_assertion(Space, List):- add_assertion_u(Space, List), !.

add_assertion_u(AC):- current_self(Space), add_assertion_u(Space, AC).

add_assertion_u(Space, List):- is_list(List), !, maplist(add_assertion_u(Space), List).
add_assertion_u(Space, AC):- unnumbervars_clause(AC, UAC), add_assertion1(Space, UAC).

add_assertion1(_, AC):- /*'&self':*/is_clause_asserted(AC), !.
add_assertion1(_, AC):- compiler_assertz(AC), !.
%add_assertion1(_, AC):- get_clause_pred(AC, F, A), \+ needs_tabled(F, A), !, pfcAdd(/*'&self':*/AC), !.

add_assertion1(Space, ACC) :-
 must_det_ll((
     copy_term(ACC, AC, _),
     expand_to_hb(AC, H, _),
     as_functor_args(H, F, A), as_functor_args(HH, F, A),
    % assert(AC),
    % Get the current clauses of my_predicate/1
    findall(HH:-B, clause(/*'&self':*/HH, B), Prev),
    copy_term(Prev, CPrev, _),
    % Create a temporary file and add the new assertion along with existing clauses
    append(CPrev, [AC], NewList),
    cl_list_to_set(NewList, Set),
    length(Set, N),
    if_t(N=2,
        (Set=[X, Y],
          numbervars(X),
          numbervars(Y),
        nl, display(X),
        nl, display(Y),
        nl)),
    %wdmsg(list_to_set(F/A, N)),
    abolish(/*'&self':*/F/A),
    create_and_consult_temp_file(Space, F/A, Set))).


cl_list_to_set([A|List], Set):-
  member(B, List), same_clause(A, B), !,
  cl_list_to_set(List, Set).
cl_list_to_set([New|List], [New|Set]):-!,
  cl_list_to_set(List, Set).
cl_list_to_set([A, B], [A]):- same_clause(A, B), !.
cl_list_to_set(List, Set):- list_to_set(List, Set).

same_clause(A, B):- A==B, !.
same_clause(A, B):- A=@=B, !.
same_clause(A, B):- unnumbervars_clause(A, AA), unnumbervars_clause(B, BB), same_clause1(AA, BB).
same_clause1(A, B):- A=@=B.
same_clause1(A, B):- expand_to_hb(A, AH, AB), expand_to_hb(B, BH, BB), AB=@=BB, AH=@=BH, !.

%clause('is-closed'(X), OO1, Ref), clause('is-closed'(X), OO2, Ref2), Ref2\==Ref, OO1=@=OO2.

% Predicate to create a temporary file and write the tabled predicate
create_and_consult_temp_file(Space, F/A, PredClauses) :-
  must_det_ll((
    % Generate a unique temporary memory buffer
    tmp_file_stream(text, TempFileName, TempFileStream),
    % Write the tabled predicate to the temporary file
    format(TempFileStream, ':- multifile((~q)/~w).~n', [F, A]),
    format(TempFileStream, ':- dynamic((~q)/~w).~n', [F, A]),
    %if_t( \+ option_value('tabling', false),
    if_t(option_value('tabling', 'True'), format(TempFileStream, ':- ~q.~n', [table(F/A)])),
    maplist(write_clause(TempFileStream), PredClauses),
    % Close the temporary file
    close(TempFileStream),
    % Consult the temporary file
    % abolish(F/A),
    /*'&self':*/
    consult(TempFileName),

    listing(F/A),
    % Delete the temporary file after consulting
    %delete_file(TempFileName),
    asserta(metta_compiled_predicate(Space, F, A)),
    current_predicate(F/A),
    listing(metta_compiled_predicate/3),
    true)).

%metta_compiled_predicate(_, F, A):- metta_compiled_predicate(F, A).

% Helper predicate to write a clause to the file
write_clause(Stream, Clause) :-
    subst_vars(Clause, Can),
    write_canonical(Stream, Can),
    write(Stream, '.'),
    nl(Stream).

%same(X, Y):- X =~ Y.







% Setting the file encoding to ISO-Latin-1
:- encoding(iso_latin_1).
% Flushing the current output
:- flush_output.
% Setting the Rust backtrace to Full
:- setenv('RUST_BACKTRACE', full).
% Loading various library files
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).

:- ensure_loaded(metta_compiler_mizer).

:- ensure_loaded(metta_parser).
% When the the `metta_interp` library is loaded, it makes sure the rest of the files are intially loaded in
% the correct order independent of which file is loaded first the needed predicates and ops are defined.
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
% =======================================
% TODO move non flybase specific code between here and the compiler
%:- ensure_loaded(flybase_main).
% =======================================
:- ensure_loaded(metta_compiler_lib).

:- dynamic(metta_compiled_predicate/3).
:- multifile(metta_compiled_predicate/3).


builtin_metta_function(F/A):- integer(A), atom(F), !, compound_name_arity(P, F, A), builtin_metta_function(P).
%builtin_metta_function(Hc):- predicate_property(Hc, static), !.
builtin_metta_function(Hc):- source_file(Hc, File), (source_file(this_is_in_compiler_lib, File);source_file(scan_exists_in_interp, File)), !.

setup_library_call(Source, FnName, LenArgs, MettaTypeArgs, MettaTypeResult, InternalTypeArgs, InternalTypeResult) :-
    (transpiler_predicate_store(_, FnName, LenArgs, _, _, _, _) -> true ;
      compiler_assertz(transpiler_predicate_store(Source, FnName, LenArgs, MettaTypeArgs, MettaTypeResult, InternalTypeArgs, InternalTypeResult))),
    setup_mi_me(FnName, LenArgs, InternalTypeArgs, InternalTypeResult)
    .

setup_mi_me(_, _, _, _).

'~..'(A, B):- a_f_args(A, B), must_det_lls(cmpd4lst(A, B)), !.
:- op(700, xfx, '~..').

a_f_args(_, B):- is_list(B), !.
a_f_args(A, _):- compound(A), !.
a_f_args(A, _):- atom(A), !.
a_f_args(_A, _B):- bt, !, ds, break, mctrace(compiler1).

%:- style_check(-singleton).

:- op(700, xfx, '@..').

A @.. B:- a_f_args(A, B), must_det_llu(A=..B).

is_proper_arg(O):- compound(O), iz_conz(O), \+ is_list(O), !, bt, mctrace(compiler2).
is_proper_arg(_).
% This hook is called when an attributed var is unified
proper_list_attr:attr_unify_hook(_, Value) :- \+ compound(Value), !.
proper_list_attr:attr_unify_hook(_, Value) :- is_list(Value), !.
proper_list_attr:attr_unify_hook(_, Value) :- iz_conz(Value), !, mctrace(compiler3).
proper_list_attr:attr_unify_hook(_, _Value).
% Attach the attribute if not already present and not already a proper list
ensure_proper_list_var(Var) :- var(Var), !, put_attr(Var, proper_list_attr, is_proper_arg).
ensure_proper_list_var(Var) :- is_proper_arg(Var), !.


eval_at(_Fn, Where):- nb_current('eval_in_only', NonNil), NonNil\==[], !, Where=NonNil.
eval_at( Fn, Where):- use_evaluator(fa(Fn, _), Only, only), !, Only=Where.
eval_at(_Fn, Where):- option_value(compile, false), !, Where=interp.
eval_at( Fn, Where):- use_evaluator(fa(Fn, _), Where, enabled), !.
eval_at( Fn, Where):- nb_current(disable_compiler, WasDC), member(Fn, WasDC), Where==compiler, !, fail.
eval_at( Fn, Where):- nb_current(disable_interp, WasDC), member(Fn, WasDC), Where==interp, !, fail.
eval_at(_Fn, Where):- option_value(compile, full), !, Where=compiler.
eval_at(_Fn, _Any):- !.

must_use_interp(Fn, only_interp(Fn), true):- use_evaluator(fa(Fn, _), interp, only).
must_use_interp(_ , eval_in_only(compiler), never):- nb_current('eval_in_only', compiler).
must_use_interp(_ , eval_in_only(interp), true):- nb_current('eval_in_only', interp).
must_use_interp(Fn, disable_compiler(Fn), true):- nb_current(disable_compiler, WasDC), member(Fn, WasDC).
must_use_interp(Fn, compiler_disabled(Fn), true):- use_evaluator(fa(Fn, _), compiler, disabled).
must_use_interp(Fn, unknown(Fn), unknown).

must_use_compiler(_ , eval_in_only(compiler)):- nb_current('eval_in_only', compiler).
must_use_compiler(_ , eval_in_only(interp)):- nb_current('eval_in_only', interp), fail.
must_use_compiler(Fn, only_compiler(Fn)):- use_evaluator(fa(Fn, _), compiler, only).
must_use_compiler(Fn, disable_interp(Fn)):- nb_current(disable_interp, WasDC), member(Fn, WasDC).
must_use_compiler(Fn, interp_disabled(Fn)):- use_evaluator(fa(Fn, _), interp, disabled).

ci(_, _, _, G):- call(G).
ci(_, _, _, _, _, _, G):- !, call(G).
% Compiler is Disabled for Fn
ci(PreInterp, Fn, Len, Eval, RetVal, _PreComp, _Compiled):- fail,
    once(must_use_interp(Fn, Why, TF)),
    TF \== unknown, TF \== never,
    debug_info(must_use_interp, why(Why, Fn=TF)),
    TF == true, !,

    % \+ nb_current(disable_interp, WasDI), member(Fn, WasDI),
    call(PreInterp),
    maplist(lazy_eval_to_src, Eval, Src),
    if_t(Eval\=@=Src,
       debug_info(lazy_eval_to_src, ci(Fn, Len, Eval, RetVal))),
    %eval_fn_disable(Fn, disable_compiler, interp, ((call(PreComp), call(Compiled)))),
    debug_info(Why, eval_args(Src, RetVal)), !,
    eval_args(Src, RetVal).

ci(_PreInterp, Fn, Len, _Eval, _RetVal, PreComp, Compiled):-
    %(nb_current(disable_interp, WasDI), member(Fn, WasDI);
    %\+ nb_current(disable_compiler, WasDC), member(Fn, WasDC)), !,
    %\+ \+ (maplist(lazy_eval_to_src, Eval, Src),
    %       if_t(Eval\=@=Src, debug_info(lazy_eval_to_src, ci(Fn, Len, Eval, RetVal)))),
    if_t(false, debug_info(call_in_only_compiler, ci(Fn, Len, Compiled))), !,
    % eval_fn_disable(Fn, disable_compiler, eval_args(EvalM, Ret))
    %show_eval_into_src(PreInterp, Eval, _EvalM),
    (call(PreComp), call(Compiled)),
    %eval_fn_disable(Fn, disable_compiler, (call(PreComp), call(Compiled))),
    true.

eval_fn_disable(Fn, DisableCompiler, Call):-
   (nb_current(DisableCompiler, Was)->true;Was=[]),
   (New = [Fn|Was]),
   Setup = nb_setval(DisableCompiler, New),
   Restore = nb_setval(DisableCompiler, Was),
   redo_call_cleanup(Setup, Call, Restore).


lazy_eval_to_src(A, O):- nonvar(O), mctrace(compilerAALE), A=O.
%lazy_eval_to_src(A, O):- var(A), !, O=A, ensure_proper_list_var(A).
lazy_eval_to_src(A, O):- \+ compound(A), !, O=A.
%lazy_eval_to_src(A, P):- is_list(A), maplist(lazy_eval_to_src, A, P), !.
lazy_eval_to_src(A, P):- [H|T] = A, lazy_eval_to_src(H, HH), lazy_eval_to_src(T, TT), !, P= [HH|TT].
lazy_eval_to_src(A, P):- as_p1_expr(A, P), !.

delistify(L, D):- is_list(L), L=[D], !.
delistify(L, L).

unnumbervars_wco(X, XXX):- compound(X),
   sub_term_safely(E, X), compound(E), E = '$VAR'(_), !,
   subst001(X, E, _, XX), unnumbervars_wco(XX, XXX).
unnumbervars_wco(X, X).

% max_var_integer_in_term(+Term, -Max)
max_var_integer_in_term(Term, Start, Max) :-
        Box = box(Start), % Correct initialization
        forall( ( sub_term_safely(CmpdVar, Term), compound(CmpdVar), CmpdVar = '$VAR'(Int), integer(Int), ( box(Int) @> Box )),
            nb_setarg(1, Box, Int)),
        arg(1, Box, Max), !.

number_vars_wo_conficts(X, XX):-
   copy_term(X, XX),
   woct(max_var_integer_in_term(XX, 0, N)),
   succ(N, N2),
   numbervars(XX, N2, _, [attvar(skip)]).



% ~..
dmp_break:- st, ds, break.
cmpd4lst(A, _):- nonvar(A), dmp_break, fail.
cmpd4lst(_A, [Cmpd, _F|_Args]):- \+ compound(Cmpd), dmp_break, fail.
cmpd4lst(_A, [_Cmpd, _F|Args]):- \+ is_list(Args), dmp_break, fail.
%cmpd4lst(A, [_Cmpd, F|Args]):- atom(F), is_cmp_builtin(F), A=..[F|Args], !.
%cmpd4lst(call_fn_native(F, XXX, Args), [Cmpd, F|Args]):- compound(Cmpd), f(XXX)=Cmpd, !.
cmpd4lst(A, [_Cmpd, F|Args]):- atom(F), !, must_det_llu(A=..[F|Args]), !.
cmpd4lst(call_fn_native_error(F, xxx(?), Args), [F|Args]):- !.

is_cmp_builtin(is_True).
is_cmp_builtin(as_p1_expr).
is_cmp_builtin(as_p1_exec).
is_cmp_builtin(ispeEnN).
is_cmp_builtin(call_fn_native).


%try_optimize_prolog(_, A, A).




correct_assertz(Info, InfoC):- \+ compound(Info), !, InfoC=Info.
correct_assertz(M:Info, MM:InfoC):- !, correct_assertz(M, MM), correct_assertz(Info, InfoC).
correct_assertz((Info:- (T, B)), (Info:- (T, B))):- compound(Info), atom(T), !.
correct_assertz((Info:-B), (InfoC:-B)):- !, correct_assertz(Info, InfoC).
correct_assertz(call_fn_native(X, _Info, Y), InfoC):-
 !, must_det_llu(InfoC=..[X|Y]).
correct_assertz(Info, Info).

same_info_term(Info1,Info2):- copy_term_nat(Info1+Info2,CInfo1+CInfo2),CInfo1=@=CInfo2.

is_prolog_rule(Info):- strip_module(Info, _, Neck), compound(Neck), compound_name_arity(Neck, F, _), F == ':-'.
is_compiler_data(Info):- strip_module(Info, _, Neck), compound(Neck), compound_name_arity(Neck, F, _), compiler_data(F/_), !.

compiler_assertz(Info):- is_list(Info), !, maplist(compiler_assertz, Info), !.


% compiler_assertz(Info):-  if_t(same_info_term(Info, metta_atom_asserted( '&corelib', [ =, [ unquote, [quote,_atom]], _atom])), rtrace),fail.

compiler_assertz(Info):- (once(correct_assertz(Info, InfoC))), Info\=@=InfoC, !,
  must_det_lls(( debug_info(compiler_assertz, correct_assertz(ca)),
   compiler_assertz(InfoC))).

compiler_assertz(Info):- (once(maybe_fix_prolog_term(ca, Info, InfoC))), Info\=@=InfoC, !,
   must_det_lls((debug_info(compiler_assertz, maybe_fix_prolog_term(ca)),
   compiler_assertz(InfoC))).

compiler_assertz(Info):-
     once(try_optimize_prolog(ca, Info, Info2)),
     Info\=@=Info2, !,
    must_det_lls(( info_identity(Info, Id),
     debug_info(compiler_assertz, optimized_code(Id, ca)),
     send_to_pl_file(optimized_code(Id, ca)),
     send_to_pl_file(in_cmt(Info)),
     compiler_assertz(Info2))).


compiler_assertz(Info):-
   (is_prolog_rule(Info)->   debug_info(assertz_code, t(Info));
   (is_compiler_data(Info)-> debug_info(assertz_compiler_data, t(Info));
                             debug_info(compiler_assertz, Info))), fail.


%compiler_assertz(Info):- skip_redef(Info), !, debug_info(skipping_redef, Info).
compiler_assertz(Info):- must_det_lls((info_assertz(Info))).

info_assertz(Info):-
  must_det_lls((unnumbervars_clause(Info, Assert),
  %transpiler_debug(2, output_prolog(Info)),
  info_assertz(Info, Assert))), !.

info_assertz(Info, Assert):- is_clause_asserted(Assert), !, send_to_pl_file(==>(Info)),
  debug_info(compiler_assertz, skip(Info)).
info_assertz( Info, Assert):- send_to_pl_file(==>(Info)), pfcAdd(Assert), make_recompile_dirty, !.
info_assertz_old(Info, Assert):-
  must_det_lls((get_side_effects(pfcAdd(Assert), SifeEffects),
  once((SifeEffects==[] -> send_to_pl_file(==>(Info)) ; send_se_to_pl_file(SifeEffects))),
  maplist(do_side_effects, SifeEffects))), !.

send_se_to_pl_file(List):- is_list(List), !, maplist(send_se_to_pl_file, List).
send_se_to_pl_file(Goal):- send_to_pl_file(:- Goal).


compiler_assertz_verbose(G):- seen_check(compiler_assertz_verbose(G)), !.
%compiler_assertz_verbose(G):- with_se_verbose(compiler_assertz(G)).
compiler_assertz_verbose(G):- compiler_assertz(G).

with_se_verbose(Goal):-
  locally(nb_setval('$se_verbose', true), Goal).


% Extract side effects of a goal execution into a normalized list format
get_side_effects(Goal, SideEffects) :-
    snapshot((Goal, transaction_updates(X), cvt_tru(X, SideEffects))).

% Demonstration predicate for collecting side effects
catst :-
    abolish(foo, 1),
    assert(foo(2)), assert(foo(3)),
    get_side_effects((asserta(foo(1)), retract(foo(2))), SE),
    writeln(SE).

% Normalize transaction update references into clauses
cvt_tru(List, Updates) :- \+ compound(List), !, Updates = List.
cvt_tru(List, Updates) :- is_list(List), !, maplist(cvt_tru, List, Updates).
cvt_tru(asserta(Ref), asserta((H :- B))) :- catch(clause(H, B, Ref), _, fail).
cvt_tru(assertz(Ref), assertz((H :- B))) :- catch(clause(H, B, Ref), _, fail).
cvt_tru(erased(Ref), erase_ref((H :- B), Ref)) :- catch(clause(H, B, Ref), _, fail).
cvt_tru(U, U).

% Pretty-print normalized side effects
print_side_effects(P) :- is_list(P), maplist(print_side_effects, P), !.
print_side_effects(P) :- cvt_tru(P, Q), !, pp_se(Q).

% Pretty-printer helper
pp_se(SE) :- \+ compound(SE), !, ppt(SE).
pp_se(SE) :- dont_print_se(SE), !.
pp_se(assertz(SE)) :- !, pp_se(SE), !.
pp_se(SE:-True):- True==true, !, pp_se(SE).
pp_se(metta_atom_asserted('&top', Cmt)) :- extract_cmmt(Cmt, Str), !, pp_se(in_cmt((Str))).
pp_se(metta_atom_asserted('&top', Expr)) :- nonvar(Expr), !, show_doing_src(Expr), !.
pp_se(SE) :- maybe_write_info(SE).

% Execute side effects
do_side_effects(List) :- is_list(List), !, maplist(do_side_effects, List).
do_side_effects(Goal) :- must_det_lls(call(Goal)).



% Erase clause reference
erase_ref(_, Ref) :- erase(Ref), !.
erase_ref(G, _) :- ignore(retract(G)).

% Filter out side effects not to print
dont_print_se_eq(metta_atom_asserted('&top', end_of_file)).
dont_print_se(Cmpd):- dont_print_se_eq(Cmpd1), Cmpd=@=Cmpd1, !.


dont_print_se(Cmpd):- \+ compound(Cmpd), !, fail.
dont_print_se(Cmpd):- compound_name_arguments(Cmpd, Name, Arity), dont_print_se_fa(Name, Arity), !.
dont_print_se(asserta(SE)):- !, dont_print_se(SE).
dont_print_se(assertz(SE)):- !, dont_print_se(SE).
dont_print_se(erase_ref(SE, _)):- !, dont_print_se(SE).
dont_print_se((_:SE)):- !, dont_print_se(SE).
dont_print_se((SE:-True)):- True==true, !, dont_print_se(SE).
dont_print_se_fa(last_source_file, _).
dont_print_se_fa('$spft$', _).
dont_print_se_fa(compiled_clauses, _).


write_translation(Expr):-
   wots(WriteThis, convert_to_pfctrans(Expr)),
   send_to_pl_file(WriteThis).


% Convert Meta expressions to PFC assertions with side effect printing
convert_to_pfctrans(Expr):-
  current_self(Self),
  get_side_effects(pfcAdd(metta_atom_asserted(Self, Expr)), SideEffects),
  print_side_effects(SideEffects).

% Convert Meta expressions to Prolog form and print
convert_to_pl(Expr):-
 allow_type_violations((show_doing_src(Expr),
  convert_to_prolog(Expr, Out), !,
  print_side_effects(Out))).

extract_cmmt(Cmt, _Str):- \+ compound(Cmt), !, fail.
extract_cmmt('$COMMENT'(Str, _, _), Str):- nonvar(Str).


show_doing_src(Cmt):- extract_cmmt(Cmt, _Str), !.
show_doing_src(Expr) :- pp_se(in_cmt(call(write_src_wi(Expr)))).

is_metta_decl_f(Eq):- var(Eq), !, fail.
is_metta_decl_f('=').
is_metta_decl_f('ML=').

% Compile Meta expression to Prolog clause
convert_to_prolog(Cmt, Out):- extract_cmmt(Cmt, Str), !, Out = in_cmt(Str).
convert_to_prolog(Expr, ReOut):-
   convert_to_prolog_c(Expr, Out), !, renumvars(Out, ReOut).
convert_to_prolog(Expr, metta_atom_asserted(Self, Expr)):- current_self(Self).

convert_to_prolog_c([Eq, H, B], Out):- is_metta_decl_f(Eq),
    compile_for_assert(H, B, Out).
convert_to_prolog_c(exec(Eval), (:- do_metta_runtime(Result, Code))):-
    compile_for_exec(Result, Eval, Code).
convert_to_prolog_c(Expr, Out):-
    compile_fact_for_assert(Expr, Out), !.


% Utility to remove variable numbering
unnumbervars_clause(Cl, ClU):-
  woc((copy_term_nat(Cl, AC), unnumbervars(AC, UA), copy_term_nat(UA, ClU))), !.

%add_buffer_file_src(KB, Filename):-
  %find_directory()
  %metta_file(Self, Filename, Directory),
  %pfcAdd(loaded_file_src(KB, Filename)).

% File handling and logging
send_to_metta_pl_file(MeTTaFile, Info):-
  metta_to_plfile(MeTTaFile, PlFile),
  send_to_txt_file(PlFile, Info).

metta_to_plfile(MeTTaFile, PlFile):-
  atom(MeTTaFile),
  atom_concat(MeTTaFile, '.pl', PlFile), % append .pl to the .metta name
  ensure_compiled_created(MeTTaFile, PlFile).

send_to_pl_file(Info):-
  ignore((current_pl_file(PlFile),
    send_to_txt_file(PlFile, Info))).

current_pl_file(PlFile):-
    option_value(loading_file, MeTTaFile), MeTTaFile \==[],
    metta_to_plfile(MeTTaFile, PlFile), !.
current_pl_file(unknown).

% Avoid redundant log messages
seen_check(G):- functor(G, F, A), seen_check(G, F, A), !.
seen_check(G, F, A):- functor(C, F, A), nb_current(F, C), G=@=C, !.
seen_check(G, F, _):- nb_setval(F, G), fail.

inform_send_one_file(PlFile):- seen_check(inform_send_one_file(PlFile)), !.
inform_send_one_file(PlFile):- in_cmt(user_err(ppt(inform_send_one_file(PlFile)))), !.
inform_send_pl_file(PlFile, Info):-
  inform_send_one_file(PlFile),
  \+ \+ ignore((user_err(cppt(Info)))).

cppt(PlFile):-
  notrace(cppt0(PlFile)).

dppt(_PlFile):- !.
dppt(PlFile):-
  notrace(ppt(PlFile)).

cppt0(PlFile):- seen_check(cppt(PlFile)), !.
cppt0(P):- maybe_write_info(in_color(P)), !.
/*
cppt(P):- goal_color(P, C), guess_varnames(P, G),
  solid_varnames(G, SG),
  numbervars(SG, 0, _, [attvar(skip)]), ansicall(C, maybe_write_info(SG)), !.
   %numbervars(SG, 0, _, [attvar(skip)]), with_output_to(string(S), maybe_write_info(SG)), !, ansicall(C, write(S)).
*/


% Variable name solidification
solid_varnames(G, SG):- var(G), get_attr(G, vn, VN), SG='$VAR'(VN), !.
solid_varnames(G, SG):- get_var_name(G, VN), SG='$VAR'(VN), !.
solid_varnames(G, SG):- is_ftVar(G), !, SG=G.
solid_varnames(G, SG):- copy_term_nat(G, SG), term_variables(G, GVars), term_variables(SG, SGVars), maplist(solid_varnames, GVars, SGVars).

renumvars(P, XX):-
  must_det_lls((guess_varnames(P, G), solid_varnames(G, SG),
  number_vars_wo_conficts(SG, XX))), !.
renumvars1(P, YY):-
  unnumbervars_wco(P, YY).


% Write to Prolog source file
send_to_txt_file(PlFile, Info):- (Info \= ( :- _)), clause_asserted(is_in_file(PlFile, Info)), !.
send_to_txt_file(PlFile, Info):-
    assertz(is_in_file(PlFile, Info)),
    if_t((nb_current('$se_verbose', true);is_douglas),
              inform_send_pl_file(PlFile, Info)),
    setup_call_cleanup(open(PlFile, append, Stream, [encoding(utf8)]),
      with_output_to(Stream, maybe_write_info(Info)), close(Stream)), !.

% Helper for writing various forms of Info
maybe_write_info(Info):- var(Info), !.
maybe_write_info(call(Info)):- !, ignore(Info), !.
maybe_write_info(in_color(P)):-
  must_det_lls(( notrace(goal_color(P, C)), renumvars(P, SG), ansicall(C, maybe_write_info(SG)))), !.
maybe_write_info(in_color(Info)):- !, in_color(maybe_write_info(Info)), !.
maybe_write_info(in_cmt(Info)):- !, setup_call_cleanup(format('~N/*~n', []), maybe_write_info(Info), format('~N*/~n', [])).
maybe_write_info(Info):- string(Info), !, writeln(Info).
maybe_write_info(Info):- \+ compound(Info), !, ppt(Info).
maybe_write_info(Info):- \+ \+ (no_conflict_numbervars(Info), maybe_write_info0(Info)).
maybe_write_info0((:-B)):-  compound(B), gensym(top_call_, Sym), !, maybe_write_info1((Sym:-B)), add_assertion_u(Sym:-B), maybe_write_info2((:- Sym)).
maybe_write_info0(Info):- maybe_write_info1(Info).

maybe_write_info1(Info):- fail, once(try_harder_optimize_prolog(wa, Info, Info2)),
     Info\=@=Info2, !,
     %setup_call_cleanup(format('~N/* try_harder_optimize_prolog ~n', []), maybe_write_info2(Info), format(' */~n', [])),
     maybe_write_info1(Info2).
maybe_write_info1(Info):- maybe_write_info2(Info), !.

is_immediately_called(Cmpd):- compound(Cmpd), compound_name_arity(Cmpd, F, A), is_immediately_called_fa(F, A).
is_immediately_called_fa(eval_H, _). is_immediately_called_fa(mi, _).  is_immediately_called_fa(normalIO, _).

maybe_write_info2( Info ):- \+ compound(Info), !, writeq(Info), writeln('.').
maybe_write_info2((:-B)):-  is_immediately_called(B), !, nl, no_conflict_numbervars(B), portray_clause(:- B), nl, nl.
maybe_write_info2((:-B)):-  into_plnamed((top_call:- time(B)), Info2), !, nl, nl, no_conflict_numbervars(Info2), add_assertion_u(Info2), portray_clause(Info2), nl, nl.
maybe_write_info2((H:-B)):- into_plnamed((H:-B), Info2), !, nl, nl, no_conflict_numbervars(Info2), ppt(Info2), nl, nl.
maybe_write_info2('==>'(H:-B)):- into_plnamed((H:-B), Info2), !, nl, nl, no_conflict_numbervars(Info2), ppt('==>'(Info2)), nl, nl.
maybe_write_info2( Info ):- into_plnamed(Info, Info2), !, writeq(Info2), writeln('.').

% Ensure output variable numbering does not conflict
no_conflict_numbervars(Term) :-
    findall(N, (sub_term_safely(E, Term), compound(E), '$VAR'(N) = E, integer(N)), NL), !,
    max_list([-1|NL], Max),
    Start is Max + 1, !,
    numbervars(Term, Start, _, [attvar(skip), singletons(true)]).

% Ensure compiled .pl file exists
ensure_compiled_created(MeTTaFile, PlFile) :-
    \+ exists_file(PlFile), !, write_new_plfile(MeTTaFile, PlFile).
ensure_compiled_created(MeTTaFile, PlFile) :-
    nop((
        time_file(PlFile, PlTime),
        time_file(MeTTaFile, MeTTaTime),
        if_t(PlTime < MeTTaTime, write_new_plfile(MeTTaFile, PlFile)))).

:- thread_local(wrote_pl_file/2).

% Create a new .pl file from a MeTTa file
write_new_plfile(MeTTaFile, PlFile) :-
    asserta(wrote_pl_file(MeTTaFile, PlFile)),
    setup_call_cleanup(open(PlFile, write, Stream, [encoding(utf8)]),
                       with_output_to(Stream, setup_pl_file(MeTTaFile)),
                       close(Stream)), !.

% Write standard headers to a new .pl file
setup_pl_file(MeTTaFile) :-
    get_time(Now),
    format_time(atom(Timestamp), '%FT%T%:z', Now),
    format('%% Generated from ~w at ~w~n', [MeTTaFile, Timestamp]),
    writeln(":- style_check(-discontiguous)."),
    writeln(":- style_check(-singleton)."),
    %writeln("%:- set_prolog_flag(mettalog_rt, true)."),
    %writeln("%:- set_prolog_flag(mettalog_rt_args, ['--repl=false'])."), writeln("%:- set_prolog_flag(mettalog_rt_args, ['--repl'])."),
    writeln(":- include(library(metta_lang/metta_transpiled_header))."),
    writeln(":- set_prolog_flag(pfc_term_expansion, true)."),

    nl.

% Hook called after MeTTa file is loaded
:- dynamic(user:on_finish_load_metta/1).
:- multifile(user:on_finish_load_metta/1).

on_finish_load_metta(MeTTaFile) :-
    atom_concat(MeTTaFile, '.pl', PlFile),
    ignore(retract(wrote_pl_file(MeTTaFile, PlFile))),
    get_time(Now),
    format_time(atom(Timestamp), '%FT%T%:z', Now),
    sformat(S, '%% Finished generating ~w at ~w~n', [MeTTaFile, Timestamp]),
    send_to_txt_file(PlFile, S), !,
    send_to_txt_file(PlFile, ":- normal_IO."),
    send_to_txt_file(PlFile, ":- initialization(transpiled_main, program)."),
    !. % setup_library_calls.



info_identity(_Info, ID):- nb_current('$info_id', ID), !.
info_identity(_Info, info_id).


skip_redef(Info):- \+ callable(Info), !, fail.
skip_redef(Info:-_):- !, skip_redef_head(user, Info).
skip_redef(_:Info):- \+ callable(Info), !, fail.
skip_redef(M:(Info:-_)):- !, skip_redef_head(M, Info).

skip_redef_head(_, Info):- \+ callable(Info), !, fail.
skip_redef_head(_, M:Info):- !, skip_redef_head(M, Info).
skip_redef_head(M, Info):- predicate_property(M:Info, static), !.
skip_redef_head(_, Info):- predicate_property(Info, static), !.
skip_redef_head(_, Info):- compound(Info), compound_name_arity(Info, F, A), compiler_data(F/A), !, fail.
skip_redef_head(M, Info):- source_file(this_is_in_compiler_lib, F), once(source_file(M:Info, F);source_file(Info, F)).
%skip_redef(Info):- source_file(Info, _). % diallow otehr places

skip_redef_fa(Fn, Arity) :- integer(Arity), !, skip_redef_fa(Fn, [Arity]).
skip_redef_fa(Fn, LenArgs) :-
   create_prefixed_name('mc_', LenArgs, Fn, FnWPrefix),
   sum_list(LenArgs, LenArgsTotal),
   LenArgsTotalPlus1 is LenArgsTotal+1,
   functor_chkd(Info, FnWPrefix, LenArgsTotalPlus1),
   skip_redef_head(user, Info), !.

into_fa(Fn/[Arity], Fn, Arity):- must_be(number, Arity).
into_fa(Fn/Arity, Fn, Arity):- must_be(number, Arity).
into_fa(FnArity, _Fn, _Arity):- throw(type_error(f/a, FnArity)).

%must_det_lls(G):- catch(G, E, (wdmsg(E), fail)), !.
%must_det_lls(G):- rtrace(G), !.
%user:numbervars(Term):- varnumbers:numbervars(Term).
%must_det_llu(compound_name_arguments(C, F, Args)):- var(C), mctrace(compiler), !, must_det_lls(compound_name_arguments(C, F, Args)).
must_det_llu(A=..[B|L]):- \+ atom(B), !, A=[B|L], !.
%must_det_llu(A=..B):- var(A), mctrace(compiler), !, must_det_lls(A=..B).
must_det_llu(A=..B):- !, must_det_lls(A=..B).
must_det_llu(G):- must_det_lls(G).

must_det_lls(G):- tracing, !, call(G). % already tracing
must_det_lls((A, B)):- !, must_det_lls(A), must_det_lls(B).
%must_det_lls((A, B)):- !, (A, B).
%must_det_lls(G):- !, call(G). % already tracing
%must_det_lls((G, B)):- catch(G, E, (wdmsg(E), fail)), !, must_det_lls(B).
%must_det_lls((A, B)):- !, must_det_lls(A), must_det_lls(B).
%must_det_lls(G):- tracing, !, (real_notrace(G)*->true;fail).
must_det_lls(G):- catch((G->true;must_det_llf(G)), E, (wdmsg(E), mctrace(compiler4), rtrace(G), fail)), !.
%must_det_lls(G):- must_det_ll(G).
must_det_llf(G):- ignore((notrace, nortrace)), mctrace(compiler5), rtrace(G), !.

mctrace(A):- notrace, wdmsg(mctrace(A)), trace.

extract_constraints(V, VS):- var(V), get_attr(V, cns, _Self=Set), !, extract_constraints(_Name, Set, VS), !.
extract_constraints(V, VS):- var(V), VS=[], !.
extract_constraints(V, VS):- var(V), !, ignore(get_types_of(V, Types)), extract_constraints(V, Types, VS), !.
extract_constraints(Converted, VSS):- term_variables(Converted, Vars),
      % assign_vns(0, Vars, _),
       maplist(extract_constraints, Vars, VSS).
extract_constraints(V, [], V=[]):-!.
extract_constraints(V, Types, V=Types).


label_vns(S, G, E):- term_variables(G, Vars), assign_vns(S, Vars, E), !.
assign_vns(S, [], S):-!.
assign_vns(N, [V|Vars], O):- get_attr(V, vn, _), !, assign_vns(N, Vars, O).
assign_vns(N, [V|Vars], O):- format(atom(VN), '~w', ['$VAR'(N)]),
  put_attr(V, vn, VN), N2 is N+1, assign_vns(N2, Vars, O).

label_arg_types(_, _, []):-!.
label_arg_types(F, N, [A|Args]):-
  label_arg_n_type(F, N, A), N2 is N+1,
  label_arg_types(F, N2, Args).

% label_arg_n_type(F, 0, A):- !, label_type_assignment(A, F).
label_arg_n_type(F, N, A):- is_arity_0(F, Fn), !, label_arg_n_type(Fn, N, A).
label_arg_n_type(F, N, A):- compound(F), !, functor_chkd(F, Fn, Add), Is is Add+N, !, label_arg_n_type(Fn, Is, A).
label_arg_n_type(F, N, A):- add_type_to(A, arg(F, N)), !.

add_type_to_var(_, Var):- nonvar(Var), !.
add_type_to_var(Type, Var):-
    add_type_to(Var, Type).

add_type_to(V, T):- is_list(T), !, maplist(add_type_to(V), T).
add_type_to(V, T):- T == val(V), !.
add_type_to(V, T):- var(T), !, ignore((get_types_of(T, TO), !, maplist(add_type_to(V), TO))).
%add_type_to(V, T):- ground(T), arg_type_hints(T, H), H\==[], nonvar(H), !, add_1type_to(V, H).
add_type_to(V, T):- add_1type_to(V, T), !.

add_1type_to(V, T):- is_list(T), !, maplist(add_1type_to(V), T).
add_1type_to(V, T):-
 quietly(must_det_lls((
   get_types_of(V, TV),
   append([T], TV, TTV),
   set_types_of(V, TTV)))).

label_type_assignment(V, O):-
 must_det_lls((
   get_types_of(V, TV), get_types_of(O, TO),
   if_t(\+ (member(val(X), TV), fullvar(X)),
           (add_type_to(V, val(O)), add_type_to(V, TO))),
   %add_type_to(O, val(V)),
           add_type_to(O, TV),
   !)).

is_functor_val(val(_)).

%(: if (-> False $_ $else $else))
%(: if (-> False $T $T $T))

arg_type_hint1(arg(is_True, 1), 'Bool').
arg_type_hint1(arg(==, 0), 'Bool').
arg_type_hint1(arg(match, 0), ['Empty', '%Undefined%']).
arg_type_hint1(arg(empty, 0), 'Empty').
arg_type_hint1(val('Empty'), 'Empty').
arg_type_hint1(val('True'), 'Bool').
arg_type_hint1(val('False'), 'Bool').
arg_type_hint1(arg('println!', 0), 'UnitAtom').
arg_type_hints(T, R):- arg_type_hint1(T, R), !.
arg_type_hints(val(Val), [val(Val)|Types]):- !, nonvar(Val), get_val_types(Val, Types), Types\==[], !.
arg_type_hints(arg(F, Arg), Types):- !,
   findall(Type, get_farg_type(F, Arg, Type), List), merge_types(List, Types), Types\==[].

get_type_src(I, T):- buffer_src_isa(I, T).
get_type_src(I, T):- compiler_self(Self), metta_type_info(Self, I, T).

get_farg_type(F, Arg, Type):- quietly(get_farg_type0(F, Arg, Type)).
get_farg_type0(F, Arg, Type):- get_type_src(F, Res), (Res=[Ar|List], Ar=='->'), (Arg==0->last(List, TypeM);nth1(Arg, List, TypeM)), (nonvar(TypeM)->TypeM=Type;Type='%Var%').
get_farg_type0(F, Arg, Type):- compiler_self(KB), metta_params_and_return_type(KB, F, _, ParamTypes, RetType),
   (Arg==0->(RetType=TypeM);nth1(Arg, ParamTypes, TypeM)), (nonvar(TypeM)->TypeM=Type;Type='%Var%').
get_farg_type0(F, A, TT):- A==0, !,
   get_operator_typedef_cmp(_Self, F, _, ParamTypes, TT), if_t(var(TT), maplist_nth(ignore(put_arg_n_except(0, F)), 1, ParamTypes)).
get_farg_type0(F, A, TT):-
   get_operator_typedef_cmp(_Self, F, _, ParamTypes, RetType), nth1(A, ParamTypes, TT), if_t(var(TT), maplist_nth(ignore(put_arg_n_except(A, F)), 0, [RetType|ParamTypes])).
get_farg_type0(F, A, arg(F, A)).

get_val_type(Val, Type):- quietly((get_type_src(Val, TypeM), type_ok_for_val(TypeM), (nonvar(TypeM)->TypeM=Type;Type='%Var%'))).
get_val_types(Val, Types):- findall(Type, get_val_type(Val, Type), List), merge_types(List, Types).
merge_types(List, Types):- list_to_set(List, Types), !.

get_just_types_of(V, Types):- get_types_of(V, VTypes), exclude(is_functor_val, VTypes, Types).

get_types_of(V, Types):- attvar(V), get_attr(V, cns, _Self=Types), !.
get_types_of(V, Types):- var(V), !, Types=[].
get_types_of(V, Types):- compound(V), V=list(_), !, Types=['Expression'].
get_types_of(V, Types):- compound(V), V=arg(_, _), !, Types=[V].
get_types_of(V, Types):- compound(V), V=val(_), !, Types=[V].
%get_types_of(V, Types):- compound(V), arg(1, V, E), attvar(E), get_attr(E, cns, _Self=Types), !.
get_types_of(V, Types):- findall(Type, get_type_for_args(V, Type), Types).

get_type_for_args(V, Type):- nonvar(V), get_type_src(V, Type), type_ok_for_val(Type), Type\=='%Undefined%', Type\=='list'.

type_ok_for_val(Type):- if_t(nonvar(Type), \+ f_type(Type)), Type\==[].

f_behavioural_type(T):- subtype(Enum, 'FunctionTypeEnum'), explicit_isa(T, Enum).

f_type(T):- f_behavioural_type(T).
f_type(T):- all_atom_type(T).
f_type(T):- all_eval_arg_type(T).
f_type(T):- arrow_type(T, _, _).

set_types_of(V, _Types):- nonvar(V), !.
set_types_of(V, Types):- list_to_set(Types, Set), put_attr_someplace(V, cns, _Self=Set), nop(wdmsg(V=Types)).


put_attr_someplace(V, A, AV):-  var(V), !, put_attr(V, A, AV).
put_attr_someplace(V, A, AV):- compound(V), arg(_, V, E), var(E), !, put_attr(E, A, AV).



precompute_typeinfo(HResult, HeadIs, AsBodyFn, Ast, Result) :-
 must_det_lls((
    HeadIs = [FnName|Args],
    LazyArgsList=[], FinalLazyOnlyRet = lazy,
    f2p(HeadIs, _CType, _Caller, _Nth, LazyArgsList, HResult, FinalLazyOnlyRet, AsBodyFn, NextBody),
    HeadAST=[assign, HResult, [call(FnName)|Args]],
    Ast = [=, HeadIs, NextBody],
    ast_to_prolog_aux(no_caller, [], HeadAST, _HeadC),
    ast_to_prolog(no_caller, [], NextBody, _NextBodyC),
    extract_constraints(Ast, Result))).

:- use_module(library(gensym)).          % for gensym/2
:- use_module(library(pairs)).           % for group_pair_by_key/2
:- use_module(library(logicmoo_utils)).  % for ppt/1 (pretty-print)

/** <module> combine_transform_and_collect_subterm

    Demonstration of a two-pass approach:
      1) Transform an S-expression so that *nested* calls `[Fn|Args]`
         become `'$VAR'('temp_N')` with an assignment `'temp_N' = eval([Fn|...])`.
         The top-level call is preserved.
      2) Collect underscore variables in the *final expression* by
         enumerating all subterms with sub_term/2. Whenever we see a call
         (either `[Fn|Args]` or a compound `Fn(...)`), we look for underscore
         variables in the arguments and note them as `arg(Fn, Pos)`.

    We then show how to run this on a "big" expression with match-body, if,
    let*, etc., using logicmoo_utils to print results.
*/


/* ---------------------------------------------------------------------
   (1) TRANSFORMATION PASS
   --------------------------------------------------------------------- */

/** transform_expr(+OldExpr, -Assignments, -NewTopExpr) is det

    Leaves the **top-level** `[Fn|Args]` intact,
    but for each *nested* call `[SubFn|SubArgs]`, create a fresh
    variable `'$VAR'('temp_N')` and an assignment `'temp_N' = eval([...])`.
*/
transform_expr(OldExpr, Assignments, NewTopExpr) :-
    % unify real Prolog variables as '$VAR'(N), though underscores remain atoms
    numbervars(OldExpr, 0, _, [attvar(skip)]),
    transform_top(OldExpr, NewTopExpr, Assignments, 0, _).

transform_top(Var, Var, [], C, C) :- fullvar(Var), !.
transform_top(Var, Var, [], C, C) :- as_is_data_term(Var), !.
transform_top(Var, Var, [], C, C) :- Var==[], !.
transform_top(Var, Var, [], C, C) :- \+ is_list(Var).
transform_top([Fn|Args], [Fn|NewVarL], Assignments, C0, C2) :- atom(Fn), !, transform_list_subcalls(Args, NewVarL, Assignments, C0, C2).
transform_top(List, ListOut, Assignments, C0, C2) :- is_list(List), !, transform_list_subcalls(List, ListOut, Assignments, C0, C2).
transform_top(Anything, Anything, [], C, C).

transform_list_subcalls([], [], [], C, C).
transform_list_subcalls([X|Xs], [Xn|Xsn], Assignments, C0, C2) :-
    transform_subcall(X, Xn, A1, C0, C1),
    transform_list_subcalls(Xs, Xsn, A2, C1, C2),
    append(A1, A2, Assignments).

/** transform_subcall(+Expr, -ExprOut, -Assignments, +C0, -C1) is det

    For *nested* calls `[Fn|Args]`, produce `'$VAR'(temp_N)` plus assignment.
    Otherwise, just keep recursing.
*/
transform_subcall(Var, Var, [], C, C) :-
    \+ is_list(Var), !.
transform_subcall([], [], [], C, C) :- !.
transform_subcall([Fn|Args], TmpVar, [Assignment|Arest], C0, C2) :- atom(Fn), !,
    transform_list_subcalls(Args, NewVarL, Aargs, C0, C1),
    gensym('_temp_', TempName),
    TmpVar = '$VAR'(TempName),
    Assignment = (TmpVar - eval([Fn|NewVarL])),
    append(Aargs, [], Arest),
    C2 is C1.

transform_subcall(List, ListOut, A, C0, C2) :-
    is_list(List),
    transform_list_subcalls(List, ListOut, A, C0, C2).


/** var_call_refs(+Expression, -VarMappings) is det

    After transformation, we gather references to "underscore variables."
    We do this by enumerating all subterms with sub_term/2, checking for
    calls that are either:
      - `[Fn|Args]` (a Prolog list with an atom head), or
      - A compound with an atom functor.

    For each Arg that starts with `_`, or is a Prolog var,
    we produce `_var - arg(Fn, Pos)`.

    Finally group and produce `_varName = [arg(Fn, Pos1), arg(Fn, Pos2), ...]`.
*/
var_call_refs(Expression, VarMappings) :-
    numbervars(Expression, 0, _, [attvar(skip)]),

    % collect all subterms
    findall(Sub, sub_term_safely(Sub, Expression), SubTerms),

    % for each subterm that is a "function call", gather references
    gather_all_function_calls(SubTerms, RawPairs),

    % group and unify
    sort(RawPairs, Sorted),
    group_pair_by_key(Sorted, Grouped),
    maplist(to_equals_pair, Grouped, VarMappings).

to_equals_pair(K-List, K-List).

%group_pair_by_key(X, Y):- !, group_pairs_by_key(X, Y).
group_pair_by_key([], []):-!.
group_pair_by_key([M-N|T0], Done) :- select(K-Vs, T0, TR), M=@=K, !,
     flatten([N, Vs], Flat), list_to_set(Flat, Set), group_pair_by_key([M-Set|TR], Done).
group_pair_by_key([M-N|T0], [M-Set|Done]):-
  flatten([N], Flat), list_to_set(Flat, Set),
  group_pair_by_key(T0, Done).

/** gather_all_function_calls(+SubTerms, -AllPairs) is det

    For each subterm in SubTerms, if it's recognized as a function call,
    call process_function_args/4 to gather underscore variables in the arguments.
*/
gather_all_function_calls([], []).
gather_all_function_calls([Term|Rest], AllPairs) :-
    (   is_function_call(Term, Fn, Args)
    ->  process_function_args(Fn, Args, 1, Pairs)
    ;   Pairs = []
    ),
    gather_all_function_calls(Rest, More),
    append(Pairs, More, AllPairs).

/** is_function_call(+Term, -Fn, -Args) is semidet

    1. If Term is a list [Fn|Args] with `atom(Fn)`, treat it as a call.
    2. If Term is a compound with `functor = Fn` (an atom) and arguments `Args`,
       also treat it as a call. For example, `eval([quote, F])` => Fn=eval, Args=[[quote, F]].
*/
is_function_call(List, Fn, Args) :-
    is_list(List),
    List = [Fn|Args],
    atom(Fn), !, \+ non_function(Fn).

is_function_call(Compound, Fn, Var) :-
    compound(Compound),
    Compound = (Var - eval([Fn|_])),
    atom(Fn), !, \+ non_function(Fn).


non_function(Fn):- var(Fn), !, fail.
non_function('&self').

process_function_args(Fn, Var, _, [Var-arg(Fn, 0)]):- is_underscore_var(Var), !.
process_function_args(_, [], _, []).
process_function_args(Fn, [Arg|R], N, [Arg-arg(Fn, N)|Out]) :-
    is_underscore_var(Arg),
    !,
    N2 is N + 1,
    process_function_args(Fn, R, N2, Out).
process_function_args(Fn, [_|R], N, Out) :-
    N2 is N + 1,
    process_function_args(Fn, R, N2, Out).

/** is_underscore_var(+X) is semidet

    True if X is:
      - a real Prolog variable (var/1 or '$VAR'(_)),
      - or an atom starting with `_`.
*/
is_underscore_var(X) :- var(X), !.
is_underscore_var('$VAR'(_)) :- !.
is_underscore_var(X) :-
    atom(X),
    sub_atom(X, 0, 1, _, '_').



/* ---------------------------------------------------------------------
   (3) PUTTING IT ALL TOGETHER
   --------------------------------------------------------------------- */

combine_transform_and_collect(OldExpr, Assignments, NewExpr, VarMappings) :-
    transform_expr(OldExpr, Assignments, NewExpr),
    % Collect references from both the new expression AND the assignments.
    % That way, if you have  _temp_9 = eval([quote, F]) ...
    % the subterm [quote, F] is recognized.
    var_call_refs(OldExpr+Assignments, VarMappings).


/** test_combine_big is det

    Demo with `=`, `match-body`, `if`, nested `let*`, etc.
    Also picks up any compound calls like eval([quote, _goal]).
*/
test_combine_big :-
    OldExpr =
      [ =,
        [ match_body, _info, _body, _kb, _rb, _goal ],
        [ if,
          [==, _body, []],
          eval([quote, _goal]),        % an example compound call
          [ 'let*',
            [ [ [ _cur, _rest ],
                [ decons_atom, _body ]],
              [ _debugging, 'True' ],
              [ _bugcheck, 'False' ],
              [ [],
                [ if,
                  _debugging,
                  [ println,
                    [ quote,
                      [ "IN",
                        ["cur=", _cur],
                        ["goal=", _goal]]]],
                  []]],
              [ _12,
                [ if,
                  _bugcheck,
                  [ 'let*',
                    [ [ RetVal,
                        [ backward_chain, _info, _cur, _kb, _rb ] ],
                      [ _m,
                        [ collapse, [ equalz, [quote, _cur], _RetVal ] ] ],
                      [ [],
                        [ if,
                          [==, _m, []],
                          [ println,
                            [ quote,
                              [ "BAD",
                                ["cur=", _cur ],
                                ["retVal=", RetVal]]]],
                          []]]],
                    []],
                  []]],
              [ [ quote, _cur ],
                [ backward_chain, _info, _cur, _kb, _rb ]],
              [ [],
                [ if,
                  _debugging,
                  [ println,
                    [ quote,
                      [ 'OUT',
                        ["cur=", _cur],
                        ["goal=", _goal]]]],
                  []]]],
            [ match_body, _info, _rest, _kb, _rb, _goal]]]],

    combine_transform_and_collect(OldExpr, Assignments, NewExpr, VarMappings),

    writeln("=== Original Expression ==="),
    ppt(OldExpr),

    writeln("=== Assignments (subcalls replaced) ==="),
    ppt(Assignments),

    writeln("=== New Expression ==="),
    ppt(NewExpr),

    writeln("=== Var Mappings (underscore variables) ==="),
    append(Assignments, VarMappings, SM), sort(SM, S),
    ppt(S).

%:- test_combine_big.



in_type_set(Set, Type):- Set==Type, !.
in_type_set(Set, Type):- compound(Set), arg(_, Set, Arg), in_type_set(Arg, Type).

b_put_set(Set, Type):- functor_chkd(Set, _, Arg), !, b_put_nset(Set, Arg, Type).
b_put_nset(Set, _, Type):- in_type_set(Set, Type), !.
b_put_nset(Set, N, Type):- arg(N, Set, Arg),
   (compound(Arg)->b_put_set(Arg, Type);b_setarg(N, Set, [Type|Arg])).

is_type_set(Set):-compound(Set), Set=ts(_).
is_var_set(Set):- compound(Set), Set=vs(_).
foc_var(Cond, vs([Var-Set|LazyVars]), TypeSet):-!,
    (var(Set)->(Cond=Var, TypeSet=Set, TypeSet=ts([]));
   (Var==Cond -> TypeSet = Set ;
   (nonvar(LazyVars) -> foc_var(Cond, vs(LazyVars), TypeSet);
    (TypeSet=ts([]), LazyVars=[Var-TypeSet|_])))).
foc_var(Cond, Set, TSet):-add_type(Set, [Cond-TSet]), ignore(TSet=ts(List)), ignore(List=[]).

add_type(Cond, Type, LazyVars):-is_var_set(LazyVars), !, must_det_lls((foc_var(Cond, LazyVars, TypeSet), !, add_type(TypeSet, Type))).
add_type(Cond, Type, _LazyVars):- add_type(Cond, Type), !.

add_type(Cond, Type):-attvar(Cond), get_attr(Cond, ti, TypeSet), !, must_det_lls(add_type(TypeSet, Type)).
add_type(Cond, Type):-var(Cond), !, must_det_lls(put_attr(Cond, ti, ts(Type))), !.
add_type(Cond, Type):-is_type_set(Cond), !, must_det_lls(b_put_set(Cond, Type)), !.
add_type(Cond, Type):-is_var_set(Cond), !, must_det_lls(b_put_set(Cond, Type)), !.
add_type(Cond, Type):- dmsg(unable_to_add_type(Cond, Type)).

remove_stub(Space, Fn, Arity):- \+ transpiler_stub_created(Space, Fn, Arity), !.
remove_stub(Space, Fn, Arity):- retract(transpiler_stub_created(Space, Fn, Arity)), !,
  transpile_impl_prefix(Fn, Arity, IFn), abolish(IFn/Arity), !.

% !(listing! cdr-atom)
% transpiler_predicate_store(builtin, 'listing!', [1], [], '', [x(doeval, eager, [])], x(doeval, eager, [])).
mc('listing!', S, RetVal):-
  find_compiled_refs(S, Refs),
  locally(nb_setval(focal_symbol, S), print_refs(Refs)), !,
  length(Refs, RetVal).

%    mi(P):- mc(P).
%    mi(P, A):- mc(P, A).
'compiled_info'(S):- mc('listing!', S, _RetVal).

print_refs(Refs):- is_list(Refs), !, maplist(print_refs, Refs).
print_refs(Refs):- atomic(Refs), clause(M:H, B, Refs), !, print_itree(((M:H):-B)).
print_refs(Refs):- print_itree(Refs).
print_itree(C):- \+ compound(C), !, nl_print_tree(C).
print_itree((H:-B)):- B==true, !, print_itree((H)).
print_itree((M:H)):- M==user, !, print_itree((H)).
print_itree(((M:H):-B)):- M==user, !, print_itree((H:-B)).
print_itree(T):- \+ \+ nl_print_tree(T).

nl_print_tree(PT):-
  seen_check(nl_print_tree(PT)), !.
nl_print_tree(PT):-
  stream_property(Err, file_no(2)),
  mesg_color(PT, Color),
  numbervars(PT, 55, _, [attvar(skip), singletons(true)]),
  maybe_subcolor(PT, CPT),

  with_output_to(Err, (format('~N'), ansicall(Color, ppt(CPT)), format('~N'))).

maybe_subcolor(PT, CPT):- fail, nb_current(focal_symbol, S), mesg_color(PT, Color), wots(Str, ansicall(Color, ppt1(S))),
   subst001(PT, S, Str, CPT), !.
maybe_subcolor(PT, PT).

find_compiled_refs(S, Refs):-
   atom_concat('_', S, Dashed),
   compiled_info_s(S, Refs1),
   findall(Refs, (current_atom(F), atom_concat(_, Dashed, F), compiled_info_f(F, Refs)), Refs2),
   append_sets([Refs1, Refs2], Refs).

append_sets(RefsL, Refs):- flatten(RefsL, Flat), list_to_set(Flat, Refs).
compiled_info_s(S, Refs):-
   findall(Ref, (compiler_data(F/A), compiled_refs(S, F, A, Ref)), RefsL), append_sets(RefsL, Refs1),
   findall(Ref, (current_predicate(S/A), functor_chkd(P, S, A), clause(P, _, Ref)), Refs2), append_sets([Refs1, Refs2], Refs).
compiled_info_f(F, Refs):- compiled_info_s(F, Refs1), compiled_info_p(F, Refs2), append_sets([Refs1, Refs2], Refs).
compiled_info_p(F, Refs):-
   findall(Ref, (current_predicate(F/A), functor_chkd(P, F, A), current_module(M),
    \+ \+ predicate_property(M:P, _), \+ predicate_property(M:P, imported_from(_)),
    clause(M:P, _, Ref)), Refs).

compiled_refs(Symbol, F, A, Info):-
 functor_chkd(P, F, A), clause(P, B, Ref), (\+ compiler_data_no_call(F/A) -> call(B) ; true), symbol_in(2, Symbol, P),
   (B==true->Info=Ref;Info= (P:-B) ).


symbol_in(_, Symbol, P):-Symbol=@=P, !.
symbol_in(N, Symbol, P):- N>0, compound(P), N2 is N-1, symbol_in_sub(N2, Symbol, P).
symbol_in_sub(N, Symbol, P):- is_list(P), P=[S1, S2, _|_], !, symbol_in_sub(N, Symbol, [S1, S2]).
symbol_in_sub(N, Symbol, P):- is_list(P), !, member(S, P), symbol_in(N, Symbol, S).
symbol_in_sub(N, Symbol, P):- arg(_, P, S), symbol_in(N, Symbol, S).


compiler_data_mf(metta_compiled_predicate/3).
compiler_data_mf(is_transpile_call_prefix/3).
compiler_data_mf(is_transpile_impl_prefix/3).
compiler_data_mf(transpiler_stub_created/3).
compiler_data_mf(transpiler_depends_on/4).
compiler_data_mf(transpiler_clause_store/9).
compiler_data_mf(transpiler_predicate_nary_store/9).
compiler_data_mf(transpiler_predicate_store/7).
compiler_data_mf(metta_function_asserted/3).
compiler_data_mf(metta_other_asserted/2).
compiler_data_mf(transpiler_stored_eval/3).
compiler_data_mf(mc/N):- between(1, 10, N).

compiler_data(F/A):- compiler_data_mf(F/A).
compiler_data(metta_atom/2).
compiler_data(metta_type/3).
compiler_data(metta_defn/3).
compiler_data(eval_20/6).
compiler_data_no_call(eval_20/6).
compiler_data_no_call(mc/_).

%compiler_data(metta_atom_asserted/2).

%compiler_data(metta_file_buffer/7).



%transpile_prefix('').
transpile_impl_prefix('mi__1_').
:- dynamic(is_transpile_impl_prefix/3).
transpile_impl_prefix(F, Arity, Fn):- is_transpile_impl_prefix(F, Arity, Fn)*->true;(transpile_impl_prefix(Prefix), FNArity is Arity-1, atomic_list_concat([Prefix, FNArity, '__', F], Fn), asserta(is_transpile_impl_prefix(F, Arity, Fn))).

transpile_call_prefix('mc__1_').
:- dynamic(is_transpile_call_prefix/3).
transpile_call_prefix(F, Arity, Fn):- is_transpile_call_prefix(F, Arity, Fn)*->true;(transpile_call_prefix(Prefix), FNArity is Arity-1, atomic_list_concat([Prefix, FNArity, '__', F], Fn), asserta(is_transpile_call_prefix(F, Arity, Fn))).


prefix_impl_preds(Prefix, F, A):- prefix_impl_preds_pp(Prefix, F, A).
prefix_impl_preds('mc__1_', F, A):- is_transpile_call_prefix(F, A, Fn), current_predicate(Fn/A), \+ prefix_impl_preds_pp(_, F, A).
prefix_impl_preds('mi__1_', F, A):- is_transpile_impl_prefix(F, A, Fn), current_predicate(Fn/A), \+ prefix_impl_preds_pp(_, F, A).

prefix_impl_preds_pp(Prefix, F, A):- predicate_property('mc__1_2_:'(_, _, _), file(File)), predicate_property(Preds, file(File)), functor_chkd(Preds, Fn, A),
    ((transpile_impl_prefix(Prefix);transpile_call_prefix(Prefix)), atomic_list_concat([Prefix, _FNArity, '_', F], Fn)).

maplist_and_conj(_, A, B):- fullvar(A), !, B=A.
maplist_and_conj(_, A, B):- \+ compound(A), !, B=A.
maplist_and_conj(P2, (A, AA), [B|BB]):- !, maplist_and_conj(P2, A, B), maplist_and_conj(P2, AA, BB).
maplist_and_conj(P2, [A|AA], [B|BB]):- !, call(P2, A, B), maplist_and_conj(P2, AA, BB).
maplist_and_conj(P2, A, B):- call(P2, A, B), !.

as_is_data_term(Var):- var(Var), !, fail.
as_is_data_term(Term):- py_is_py(Term), !.
as_is_data_term(Term):- is_valid_nb_state(Term), !.
as_is_data_term(Term):- \+ callable(Term), !.
%as_is_data_term(Convert):- self_eval(Convert), !, (iz_conz(Convert) ;  \+ compound(Convert)).
as_is_data_term(Term):- compound(Term), !, compound_name_arity(Term, F, A), as_is_no_convert_f_a(F, A).
as_is_no_convert_f_a(rng, 2).
as_is_no_convert_f_a('Evaluation', _).



eopfc:- ensure_loaded(mettalog('metta_ontology.pfc.pl')).

%:- dynamic(metta_compiled_predicate/2).
%:- multifile(metta_compiled_predicate/2).
:- dynamic(metta_compiled_predicate/3).
:- multifile(metta_compiled_predicate/3).


w_cl(P1, F):- atom(F), !, w_cl(P1, F/_).
w_cl(P1, F/A):- atom(F), integer(A), !, functor(P, F, A), w_cl(P1, P).
w_cl(P1, F/A):- forall((current_predicate(F/A), A>0), w_cl(P1, F/A)).
w_cl(P1, P):- call(P1, P).



dedupe_p1(P):- current_predicate(_, P),
  forall((copy_term(P, P2),
     clause(P, Bd, Ref),
     clause(P2, Bd2, Ref2), Ref@<Ref2,
     a2(P, Bd)=@=a2(P2, Bd2),
     erase(Ref2), fail), true).

info_ls(F):- pp_about((F-compiled_clauses)), ls_p1(F).
ls_p1(P):- @(listing(P), user).
dedupe_cl(F):- w_cl(dedupe_p1, F).

dedupe_ls(F):- dedupe_cl(F), info_ls(F).

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0, 0)).
for_all(G1, G2):- forall(G1, G2).

:- op(700, xfx, '=~').
:- op(690, xfx, =~ ).

:- dynamic(is_absorbed_return/3).
:- dynamic(is_absorbed_return/4).
:- dynamic(is_non_absorbed_return/3).
:- dynamic(is_non_absorbed_return/4).

is_absorbed_return(_, _, _):-!, fail.
is_absorbed_return(F, A, T):-
   current_self(Self),
   is_absorbed_return(Self, F, A, T).

is_non_absorbed_return(_, _, _):-!.
is_non_absorbed_return(F, A, T):-
   current_self(Self),
   is_non_absorbed_return(Self, F, A, T).

compound_non_cons(B):-  compound(B), \+ B = [_|_].
iz_conz(B):- compound(B), B=[_|_].


%'=~'(A, B):- var(A), iz_conz(B), !, A=B.
%'=~'(A, B):- iz_conz(A), var(B), !, A=B.
%'=~'(A, B):- iz_conz(A), iz_conz(B), !, A=B.

'=~'(A, B):- quietlY('=~_impl'(A, B)).

'=~_impl'(A, B):- iz_conz(B), B=[F|Args], nonvar(F), !, '=~'(A, [FF|Args]), F=FF.
'=~_impl'(A, B):- \+ iz_conz(B), compound(B), compound_name_arguments(B, F, Args), !, '=~'(A, [F|Args]).
'=~_impl'(A, B):- into_list_args(A, AA), copy_term(AA, AAA, G), copy_term_nat(AAA, AAAC), !, into_list_args(B, BB),
  !, AAAC=BB, !, AAAC=@=AAA, AA=AAAC, !, maplist(call, G).

% non-singleton Variable
is_nsVar(NS):- is_ftVar(NS), NS\=@= '$VAR'('_').

skip_me( AA, AAA):- \+ compound(AA), !, AA=AAA.
skip_me([A, _, C|AA], AAA):- A == u_assign, AAA=[C|AA], !.
%skip_me([A, B, C|AA], AAA):- symbol(A), skip_me_f(A), !, skip_me([B, C|AA], AAA).
skip_me([A, B, C|AA], AAA):- symbol(A), skip_me_f(A), !, =([B, C|AA], AAA).
skip_me(AA, AA).

%skip_me_f('@').
%skip_me_f('call-fn').
skip_me_f(A):- var(A), !, fail.
skip_me_f(A):- metta_meta_f(A), !.
skip_me_f(A):- atom(A), atom_concat(L, '!', A), !, skip_me_f(L).

into_list_args(A, AA):- into_list_args0(A, AAA), !, skip_me(AAA, AAAA), AAAA=AA.
into_list_args0(A, A):- is_ftVar(A).
into_list_args0(A, A):- is_nsVar(A).
into_list_args0([], []):-!.
into_list_args0(C, [C]):- \+ compound(C), !.
into_list_args0([H|T], [H|T]):- \+ is_list(T), !.
into_list_args0([H, _, List, A], HT):- H == u_assign, !, append(List, [A], HT), !.
into_list_args0([H|T], [H|T]):-!.
into_list_args0(u_assign(_NN, List, A), [H|T]):- append(List, [A], [H|T]), !.
into_list_args0(holds(A), AA):- !, into_list_args(A, AA), !.
into_list_args0(C, [F|Args]):- must_det_llu(compound_name_arguments(C, F, Args)), !.



%constraintFor(Op, Len, [Ar|ATypes], Rel, ArgType, Nth):- Ar=='->', !, skelectalFor(Op, Len, [Op|ATypes], argNIsa, Rel, ArgType, Nth).
%argumentsFor(Op, Len, Args, Rel, ArgType, Nth):- skelectalFor(Op, Len, Relation, Args, Ret, Pred, Rel, Arg, Nth)

constraintFor(Op, _Len, Args, Ret, Pred, Rel, Arg, Nth):- %relation_args(MaybeOp, Len, Relation, Args, Ret), ignore(MaybeOp=Op),
   nth0(Nth, [Ret|Args], Arg), s_or_p_term(Rel, [Pred, Op, Nth, Arg]).

relation_args(Op, Len, Relation, Args, Ret):- (nonvar(Args);nonvar(Len)), !,
   length(Args, Len),
   append([Op|Args], [Ret], OpArgsWRet),
   s_or_p_term(Relation, OpArgsWRet).

relation_args(Op, Len, Relation, Args, Ret):- (nonvar(Op), nonvar(Relation)), !,
   Relation =.. List, append(_, [Op|ArgsWRet], List),
   append(Args, [Ret], ArgsWRet), length(Args, Len).


s_or_p_term(Relation, OpArgsWRet):- nonvar(Relation), nonvar(OpArgsWRet), !, Relation =.. List, append(_, OpArgsWRet, List).
s_or_p_term(Relation, [Op|ArgsWRet]):- var(Relation), is_list(ArgsWRet), !, (var(Op)-> Relation =.. [v, Op|ArgsWRet] ; Relation =..[Op|ArgsWRet]).
s_or_p_term(Relation, OpArgsWRet):- nonvar(Relation), var(OpArgsWRet), Relation =.. List, append(Left, [Op|ArgsWRet], List), is_ok_op(Left, [Op|ArgsWRet]), !.

is_ok_op([], [Op|_ArgsWRet]):- nonvar(Op), !, \+ metta_meta_f(Op).
is_ok_op([NotOp], [Op|_ArgsWRet]):- metta_meta_f(NotOp), \+ metta_meta_f(Op).

is_metta_meta_f(Op):- nonvar(Op), metta_meta_f(Op).


%metta_meta_f(v):- !, f
metta_meta_f(M):- atom(M), atom_chars(M, Chars), metta_meta_chars(Chars).
%metta_meta_chars([L]).
metta_meta_chars([m, _]).



compound_name_list(AsPred, FP, PredArgs):- var(AsPred), !, AsPred=[FP|PredArgs].
compound_name_list(AsPred, FP, PredArgs):- iz_conz(AsPred), !, AsPred=[FP|PredArgs].
compound_name_list(AsPred, FP, PredArgs):- into_list_args(AsPred, [FP|PredArgs]), !.
compound_name_list(AsPred, FP, PredArgs):- compound_non_cons(AsPred), !, compound_name_arguments(AsPred, FP, PredArgs).


% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% ===============================
count_var(HB, C, N):- findall(V, (sub_term(V, HB), C==V), L), !, length(L, N).



% Conversion is possible between a function and a predicate of arity when the result is at the nth arg
:- dynamic decl_functional_predicate_arg/3.

% Converion is possible between a  function and predicate is tricky
functional_predicate_arg_tricky(is, 2, 1). % E.g. u_assign(NN, is(+(1, 2)), Result) converts to is(Result, +(1, 2)).
% Defining standard mappings for some common functions/predicates
decl_functional_predicate_arg(append, 3, 3).
decl_functional_predicate_arg(+, 3, 3).
decl_functional_predicate_arg(-, 3, 3).
decl_functional_predicate_arg(*, 3, 3).
decl_functional_predicate_arg(pi, 1, 1).
decl_functional_predicate_arg('Empty', 1, 1). %:- mctrace(compiler).
decl_functional_predicate_arg(call, 4, 4).
decl_functional_predicate_arg(u_assign, 2, 2).
decl_functional_predicate_arg(iz, 2, 2).
decl_functional_predicate_arg(edge, 2, 2).
decl_functional_predicate_arg('==', 2, 2).
decl_functional_predicate_arg('=', 2, 2).
decl_functional_predicate_arg('is-same', 2, 2).
decl_functional_predicate_arg(assertTrue, 1, 1).
decl_functional_predicate_arg(assertEqual, 2, 2).
decl_functional_predicate_arg(case, 3, 3).
decl_functional_predicate_arg(collapse, 2, 2).
decl_functional_predicate_arg('PredArity', 2, 2).
decl_functional_predicate_arg('car-atom', 2, 2).
decl_functional_predicate_arg('cdr-atom', 2, 2).

decl_functional_predicate_arg(superpose, 2, 2).

decl_functional_predicate_arg(assertFalse, 2, 2).
decl_functional_predicate_arg(match, 4, 4).
decl_functional_predicate_arg('TupleConcat', 3, 3).
decl_functional_predicate_arg('new-space', 1, 1).

%decl_functional_predicate_arg('If', 4, 4).
%decl_functional_predicate_arg('If', 3, 3).
%decl_functional_predicate_arg('if', 4, 4).
%decl_functional_predicate_arg('if', 3, 3).

decl_functional_predicate_arg('exec0', 1, 1).
decl_functional_predicate_arg('exec1', 1, 1).

decl_functional_predicate_arg(superpose, 2, 2).

decl_functional_predicate_arg(+, 3, 3).

do_predicate_function_canonical(F, FF):- predicate_function_canonical(F, FF), !.
do_predicate_function_canonical(F, F).
predicate_function_canonical(is_Empty, 'Empty').
%predicate_function_canonical(is_Empty, empty).

pi(PI):- PI is pi.



always_function_in_src(F, _):- \+ symbol(F), fail.
always_function_in_src('If', _).
always_function_in_src(F, _):- buffer_src_isa(F, 'SrcFunction'), !.
always_function_in_src(F, A):- A1 is A + 1, predicate_arity(F, A1).

always_predicate_in_src(F, _):- \+ symbol(F), fail.
always_predicate_in_src(F, _):- buffer_src_isa(F, 'SrcPredicate'), !.
always_predicate_in_src(':', 2).
always_predicate_in_src('iz', 2).
always_predicate_in_src('=', _).
always_predicate_in_src(F, A):- predicate_arity(F, A).
%always_function_in_src(F, A):- A1 is A+1, decl_functional_predicate_arg(F, _, A1).

symbol_or_var(V):- var(V), !.
symbol_or_var(F):- symbol(F), !.

eval_false(X):- \+ eval_true(X).
do(X):- ignore(eval(X, _)).

%is_devel.

functional_predicate_arg(F, A, L):- \+ symbol(F), \+ var(F), !,
  is_devel, throw(datatype_functional_predicate_arg(F, A, L)).
functional_predicate_arg(F, A, L):-
  decl_functional_predicate_arg(F, A, L), \+ is_data_functor(F, A),
  \+ is_absorbed_return(F, _, _Bool).

% Mapping any current predicate F/A to a function, if it is not tricky
functional_predicate_arg(F, A, L):-
  defined_arity(F, A),
  \+ is_absorbed_return(F, _, _Bool),
  \+ functional_predicate_arg_tricky(F, A, _), L=A,
  \+ is_data_functor(F, _),
  \+ decl_functional_predicate_arg(F, A, _).
functional_predicate_arg(F, A, L):- functional_predicate_arg_tricky(F, A, L),
  \+ is_absorbed_return(F, _, _Bool).


file_decl_arity(F, A):- freeze(Arity, 'PredArity' == Arity), buffer_src([Arity, F, A]).
:- dynamic(function_arity/3).
:- dynamic(predicate_arity/3).
predicate_arity(F, A):- current_self(KB), predicate_arity(KB, F, A).
predicate_arity(F, A):- file_decl_arity(F, A).
%predicate_arity(F, A):- buffer_src([:, F, [Ar|Args]]), Ar == '->',
%  \+ file_decl_arity(F, _), length(Args, AA), !, A=AA.
function_arity(F, A):- current_self(KB), function_arity(KB, F, A).


defined_arity(F, A):- predicate_arity(F, A).
defined_arity(F, A):- current_predicate(F/A), \+ predicate_arity(F, _).

% defined as (= .. .....)
decl_arity(F, A):- buffer_src([Eq, [FF|Len]|_]),
   is_metta_decl_f(Eq), nonvar(FF), F==FF, is_list(Len), length([FF|Len], A).

import_arity(_, _):- fail, todo(metta_file_buffer(_Atom, _NamedVarsList, _Filename, _LineCount)).

is_data_functor(Op,Len):- compiler_self(KB), returnType(KB,Op,Len,DFRT),nonvar(DFRT),buffer_src_isa(DFRT,'DataFunctorReturnType').
is_data_functor(Op,_L ):- buffer_src_isa(Op,'DataFunctor').
is_data_functor(Op,Len):-
   metta_params_and_return_type(KB,Op,Len,[P|ParamTypes],RetType),
   maplist(is_verbatum_noncode_kind,[P|ParamTypes]), is_ok_noneval(RetType),
   \+ has_function(KB,Op,Len), \+ buffer_src_isa(Op,'MeTTaLog'), \+ buffer_src_isa(Op,'CodeFunction').

is_data_functor(DataFunctor, DenotationalArity):- nonvar(DataFunctor),
       buffer_src(['DataFunctor', DataFunctor, DenotationalArity]).
is_data_functor(F, _):- \+ import_arity(F, _), \+ decl_arity(F, _).

% Certain constructs should not be converted to functions.
not_function(P):- symbol(P), !, not_function(P, 0).
not_function(P):- callable(P), !, as_functor_args(P, F, A), not_function(F, A).
not_function(F, A):- is_arity_0(F, FF), !, not_function(FF, A).
not_function(!, 0).
not_function(print, 1).
not_function((':-'), 2).
not_function((', '), 2).
not_function((';'), 2).
not_function(('='), 2).
not_function(('or'), 2).
not_function('a', 0).
not_function('b', 0).
not_function(F, A):- is_control_structure(F, A).
not_function(A, 0):- symbol(A), !.
not_function('True', 0).
not_function(F, A):- predicate_arity(F, A), AA is A+1, \+ decl_functional_predicate_arg(F, AA, _).

needs_call_fr(P):- is_function(P, _Nth), as_functor_args(P, F, A), AA is A+1, \+ current_predicate(F/AA).

is_control_structure(F, A):- symbol(F), atom_concat('if-', _, F), A>2.

%'=='(A, B, Res):- as_tf(equal_enough(A, B), Res).
%'or'(G1, G2):- G1 *-> true ; G2.
%'or'(G1, G2, Res):- as_tf((G1 ; G2), Res).

% Function without arguments can be converted directly.
is_arity_0(AsFunction, F):- compound(AsFunction), compound_name_arity(AsFunction, F, 0).
% Determines whether a given term is a function and retrieves the position
% in the predicate where the function Result is stored/retrieved
is_function(AsFunction, _):- is_ftVar(AsFunction), !, fail.
is_function(AsFunction, _):- AsFunction=='$VAR', !, mctrace(compilerVAR), fail.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction, F), \+ not_function(F, 0), !, Nth=1.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction, _), !, Nth=1.
is_function([F|Function], Nth) :-
  is_list(Function), length(Function, N),
  functional_predicate_arg_maybe(F, N, Nth).

is_function(AsFunction, Nth) :-
    callable(AsFunction),
    as_functor_args(AsFunction, Functor, A),
    \+ not_function(Functor, A),
    AA is A + 1,
    functional_predicate_arg_maybe(Functor, AA, Nth).

functional_predicate_arg_maybe(F, _, _):- \+ symbol(F), !, fail.
functional_predicate_arg_maybe(F, AA, Nth):- functional_predicate_arg(F, AA, Nth), !.
functional_predicate_arg_maybe(F, AA, _):- A is AA - 1, functional_predicate_arg(F, A, _), !, fail.
functional_predicate_arg_maybe(F, Nth, Nth):- asserta(decl_functional_predicate_arg(F, Nth, Nth)), !.

% --------------------------------
%    FUNCTS_TO_PREDS EXPLANATION
% --------------------------------

% functs_to_preds is a predicate that converts all Term functions to their equivalent predicates.
% It takes three arguments - RetResult, which will hold the result of the function evaluation,
% Convert, which is the function that needs to be converted, and Converted, which will hold the equivalent predicate.
% Example:
%
%     ?- functs_to_preds(RetResult, is(pi+pi), Converted).
%
%     Converted =  (pi(_A),
%                   +(_A, _A, _B),
%                   _C is _B,
%                   u_assign(NN, _C, RetResult)).
%
functs_to_preds(I, OO):-
   notrace(is_html->true; non_compat_io(color_g_mesg('yellow', (write_src(I), nl)))),
   must_det_ll(functs_to_preds0(I, OO)), !,
   wdmsg(functs_to_preds(OO)).

functs_to_preds0(I, OO):- \+ compound(I), !, OO=I.
%functs_to_preds0(I, OO):- data_term(I), !, OO=I.
functs_to_preds0(I, OO):- \+ is_conz(I), once(into_list_args(I, II)), I\=@=II, functs_to_preds0(II, OO), !.

functs_to_preds0([Eq, H, B], OO):- is_metta_decl_f(Eq), !, compile_for_assert(H, B, OO), !.
functs_to_preds0(=(H, B), OO):- !, compile_for_assert(H, B, OO), !.
functs_to_preds0(EqHB, OO):- compile_for_assert(EqHB, (X==X), OO), !.
functs_to_preds0(I, OO):-
  must_det_ll((
   sexpr_s2p(I, M),
   f2p(_, _, _, M, O),
   expand_to_hb(O, H, B),
   optimize_head_and_body(H, B, HH, BB), !,
   OO = ':-'(HH, BB))).



% ?- compile_for_exec(RetResult, is(pi+pi), Converted).

compile_for_exec(Res, I, O):-
 allow_type_violations((
   calc_precompiled_types(I),
   %ignore(Res='$VAR'('RetResult')),
   compile_for_exec0(Res, I, O))), !.

compile_for_exec0(Res, I, u_assign(1, I, Res)):- is_ftVar(I), !.
compile_for_exec0(Res, (:- I), O):- !,
  compile_for_exec0(Res, I, O).
compile_for_exec0(Res, (?- I), O):- !,
  compile_for_exec0(Res, I, O).
compile_for_exec0(Res, I, BB):-
   %ignore(Res='$VAR'('RetResult')),
   compound_name_arguments(EXEC1, exec1, [Res]),
   CType = '%Undefined%', RetType = '%Undefined%', Caller = eval, Nth = 1,
   f2p(EXEC1, CType, Caller, Nth, RetType, Res, I, O),
   optimize_head_and_body(exec1(Res), O, _, BB).

compile_for_exec0(Res, I, BB):- fail,
   compound_name_arguments(EXEC0, exec0, [Res]),
   compile_for_assert(EXEC0, I, _H:-BB).
   %arg(1, H, Res).



/*
compile_metta_defn(_KB, _F, _Len, Args, _BodyFn, _Clause):- \+ is_list(Args), !, fail.
%compile_metta_defn(_KB, _F, _Len, _Args, BodyFn, _Clause):- var(BodyFn), !, fail.
compile_metta_defn(KB, F, Len, Args, [WB|AL], ClauseU):- 'wam-body'==WB, !,
  must_det_ll((
    if_t(var(Len), ignore((function_arity(KB, F, Len)))),
    if_t(var(Arity), ignore((is_non_absorbed_return(KB, F, Len, _), ignore(Arity is Len+1)))),
    if_t(var(Arity), ignore((is_absorbed_return(KB, F, Arity, _), ignore(Len is Arity)))),
    if_t(var(Arity), ignore((predicate_arity(KB, F, Arity)))),
    if_t(var(Arity), length(Args, Arity)),
    if_t(var(Len), ignore(Len is Arity-1)),
    if_t(var(Len), if_t(integer(SLen), Len = SLen)),
    pfcAdd(metta_compiled_predicate(KB, F, Len)),
    Clause=(H:-B), s2c([F|Args], H), maplist(s2c, AL, ALB),
    list_to_conjuncts(ALB, B),
    %nl, print_tree(Clause), nl,
    add_unnumbered_clause(KB, F, Len, Clause, ClauseU))), !.
compile_metta_defn(KB, F, Len, Args, BodyFn, ClauseU):-
   must_det_ll((
        if_t(var(Len), length(Args, Len)),
        pfcAdd(metta_compiled_predicate(KB, F, Len)),
        compile_for_assert([F|Args], BodyFn, Clause),
        add_unnumbered_clause(KB, F, Len, Clause, ClauseU))).

add_unnumbered_clause(KB, F, Len, ClauseN, Clause):-
    must_det_ll((
       unnumbervars_clause(ClauseN, Clause),
       pfcAdd(metta_compiled_predicate(KB, F, Len)),
       add_assertion(KB, Clause))), !.
merge_structures([F|HeadAsFunction0], AsBodyFn0, A, B, (=(NewVar, Cept), C)):- fail,
   append(Left, [Merge|Right], HeadAsFunction0), nonvar(Merge),
   append(Left, [Cept|Right], HeadAsFunctionM),
   %HeadAsFunctionM=REPH,
   HeadAsFunction0=REPH,
   subst(AsBodyFn0+REPH, Merge, NewVar, NextBodyFn+NextHead),
   NextBodyFn+NextHead \=@= AsBodyFn0+HeadAsFunctionM,
   merge_structures([F|NextHead], NextBodyFn, A, B, C),
   Cept=Merge.
merge_structures(A, B, A, B, true).
*/



%compile_for_exec0(Res, I, O):- f2p(exec(), Res, I, O).


% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_fact_for_assert(HeadIs, (Head:-Body)):-
  allow_type_violations((compile_head_for_assert(HeadIs, NewHeadIs, Converted),
    optimize_head_and_body(NewHeadIs, Converted, Head, Body))).

head_as_is(Head):-
   as_functor_args(Head, Functor, A, _), !,
   head_as_is_fa(Functor, A).
head_as_is_fa('If', 3).

rewrite_sym(S, F):- \+ atomic(S), !, F=S.
rewrite_sym(':', F):- var(F), !, 'iz' == F, !.
rewrite_sym(F, F).

as_functor_args(AsPred, F, A):-
  as_functor_args(AsPred, F, A, _ArgsL).

as_functor_args(AsPred, F, A, ArgsL):-var(AsPred), !, % mctrace(compilerAFA),
  (is_list(ArgsL);(integer(A), A>=0)), !,
   length(ArgsL, A),
   (symbol(F)-> must_det_llu(AsPred =..[F|ArgsL]); (AsPred = [F|ArgsL])).

as_functor_args(AsPred, _, _, _Args):- is_ftVar(AsPred), !, fail.
%as_functor_args([F|ArgsL], F, A, ArgsL):- is_list(ArgsL), length(ArgsL, A), !.
as_functor_args(AsPred, F, A, ArgsL):- into_list_args(AsPred, List), !, if_t(integer(A), length(ArgsL, A)), [F|ArgsL]=List, if_t(var(A), length(ArgsL, A)).
%as_functor_args([Eq, R, Stuff], F, A, ArgsL):- (is_metta_decl_f(Eq)),
%   into_list_args(Stuff, List), append(List, [R], AsPred), !,
%   as_functor_args(AsPred, F, A, ArgsL).



compile_head_for_assert(Head, NewHead, BodyOut):- \+ is_list(Head),
  as_functor_args(Head, F, _A, ArgsL), !,
  compile_head_for_assert([F|ArgsL], NewHead, BodyOut).

compile_head_for_assert(Head, Head, true):- head_as_is(Head), !.
% compile_head_for_assert(Head, NewestHead, HeadCode):- skip_mizer, Head= NewestHead, HeadCode = true , !.
compile_head_for_assert(Head, NewestHead, HeadCode):-
 must_det_ll(
  (compile_head_variablization(Head, NewHead, VHeadCode),
   compile_head_args(NewHead, NewestHead, AHeadCode),
   combine_code(VHeadCode, AHeadCode, HeadCode))).



% Construct the new head and the match body
%compile_head_args(Head, NewHead, HeadCode):- skip_mizer, Head= NewHead, HeadCode = true , !.
compile_head_args(Head, NewHead, HeadCode) :-
   must_det_ll(
     (as_functor_args(Head, Functor, A, Args),
      call((compiler_self(Self),
      get_operator_typedef_cmp(Self, Functor, A, ParamTypes, _RetType))),
      maplist_nth(compile_one_head_arg_now(Head), 1, ParamTypes, NewVarL, Args, CodeL),
      as_functor_args(NewHead, Functor, A, NewVarL),
      list_to_conjuncts(CodeL, HeadCode))), !.

compile_one_head_arg_now(Head, Nth, CType, NewArg, Arg, Code):-
  compile_one_head_arg(Head, Nth, CType, NewArg, Arg, Code),
  share_varnames(Arg,NewArg), add_type_to(Arg,CType),add_type_to(NewArg,CType).

share_varnames(Arg,NewArg):- Arg==NewArg,!.
share_varnames(Arg,NewArg):- nonvar(Arg),nonvar(NewArg),!.
share_varnames(Arg,NewArg):- as_variable_name(Arg,Named),cname_var_unnamed(Named,NewArg),!.
share_varnames(Arg,NewArg):- as_variable_name(NewArg,Named),cname_var_unnamed(Named,Arg),!.
share_varnames(_Arg,_NewArg).

compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, true):- is_ftVar(Arg), CType=='Atom', NewArg=Arg, !.
compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, true):- is_ftVar(Arg), CType=='Expression', NewArg=Arg, !.
compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, true):- is_ftVar(Arg), CType=='Variable', NewArg=Arg, !. % maybe hav ethis check the incomming type and throw and error otherwaise
compile_one_head_arg(_Head,  Nth, CType, NewArg, Arg, info(is_non_eval_kind_param(Nth,CType))):- is_ftVar(Arg), is_non_eval_kind(CType), \+ is_code_kind(CType), NewArg=Arg, !.
compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, ensure_evaled(CType, NewArg, Arg)):- is_ftVar(Arg), is_pro_eval_kind(CType), \+ is_code_kind(CType),!.
   %ppt(ensure_evaled(CType, NewArg, Arg)),

compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, ensure_code(CType, NewArg, Arg)):- is_ftVar(Arg), is_non_eval_kind(CType), is_code_kind(CType), !.
%compile_one_head_arg(_Head, Nth, CType, NewArg, Arg, eval_true(NewArg)):- Arg=='True', !.
%compile_one_head_arg(_Head, Nth, CType, NewArg, Arg, eval_false(NewArg)):- Arg=='False', !.
compile_one_head_arg(_Head,  Nth, CType, NewArg, Arg, info(is_non_eval_kind(Nth,CType))):- is_ftVar(Arg), is_non_eval_kind(CType), NewArg=Arg, !.
compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, ensure_evaled(NewArg, Arg)):- is_ftVar(Arg), is_pro_eval_kind(CType),!.
compile_one_head_arg(_Head,  Nth, CType, NewArg, Arg, unknown_var_unify(Nth, CType, NewArg, Arg)):- is_ftVar(Arg), !.

%compile_one_head_arg(_Head, Nth, CType, NewArg, Arg, ensure_unifies(NewArg, Arg)):- \+ is_ftVar(Arg), is_non_eval_kind(CType),!.
compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, true):- \+ is_ftVar(Arg), is_non_eval_kind(CType), NewArg=Arg, !.
compile_one_head_arg(_Head, _Nth, CType, NewArg, Arg, eval_and_unify(NewArg, Arg)):- \+ is_ftVar(Arg), is_pro_eval_kind(CType),!.
compile_one_head_arg(_Head,  Nth, CType, NewArg, Arg, unknown_unify(Nth, CType, NewArg, Arg)):-!.
%compile_one_head_arg(_Head, NewArg, Arg, (NewArg=~Arg)):- data_term(Arg), !.
%compile_one_head_arg(_Head, NewArg, Arg, (NewArg=~Arg)):- !.
%compile_one_head_arg(Head, NewArg, Arg, Code):- f2p_assign(_HeadIs, CType, 10, Head, NewArg, Arg, Code).

compile_head_variablization(Head, NewHead, HeadCode) :- !, Head= NewHead, HeadCode = true , !.
compile_head_variablization(Head, NewHead, HeadCode) :-
   must_det_ll(
     (as_functor_args(Head, Functor, A, Args),
      % Find non-singleton variables in Args
      fix_non_singletons(Args, NewVarL, Conditions),
      list_to_conjunction(Conditions, HeadCode),
      as_functor_args(NewHead, Functor, A, NewVarL))).


is_f('S'):- fail.
is_mf(', '). is_mf(';'). is_mf('call').
is_lf(':').


s2c(Args, true):- Args==[], !.
s2c(Args, call(Args)):- \+ iz_conz(Args), !.
s2c([F|Args], C):- \+ symbol(F), !, C=[F|Args].
s2c([F|Args], C):- is_lf(F), !, C=[F|Args].
s2c([At, F|Args], C):- symbol(F), At== '@', is_list(Args), !, maplist(s2c, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2c([F|Args], C):- is_f(F), is_list(Args), !, maplist(s2ca, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2c([F|Args], C):- is_mf(F), is_list(Args), !, maplist(s2c, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2c([F|Args], C):- is_list(Args), !, maplist(s2ca, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2c(C, call(C)).


s2ca(Args, Args):- \+ iz_conz(Args), !.
s2ca([H|T], [HH|TT]):- \+ symbol(H), !, s2ca(H, HH), s2ca(T, TT).
s2ca([F|Args], C):- is_lf(F), !, C=[F|Args].
s2ca([At, F|Args], C):- symbol(F), At== '@', is_list(Args), !, maplist(s2ca, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2ca([F|Args], C):- is_f(F), is_list(Args), !, maplist(s2ca, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2ca([F|Args], C):- is_mf(F), is_list(Args), !, maplist(s2c, Args, ArgsL), must_det_llu(compound_name_arguments(C, F, ArgsL)).
s2ca([H|T], [HH|TT]):- s2ca(H, HH), s2ca(T, TT).


fix_non_singletons(Args, NewVarL, [Code|Conditions]) :-
   sub_term_loc(Var, Args, Loc1), is_nsVar(Var),
   sub_term_loc_replaced(==(Var), _Var2, Args, Loc2, ReplVar2, NewVarLM),
   Loc1 \=@= Loc2,
   Code = same(ReplVar2, Var),
   fix_non_singletons(NewVarLM, NewVarL, Conditions), !.
fix_non_singletons(Args, Args, []):-!.


sub_term_loc(A, A, self).
sub_term_loc(E, Args, e(N, nth1)+Loc):- is_list(Args), !, nth1(N, Args, ST), sub_term_loc(E, ST, Loc).
sub_term_loc(E, Args, e(N, arg)+Loc):- compound(Args), arg(N, Args, ST), sub_term_loc(E, ST, Loc).

sub_term_loc_replaced(P1, E, Args, LOC, Var, NewVarL):- is_list(Args), !, sub_term_loc_l(nth1, P1, E, Args, LOC, Var, NewVarL).
sub_term_loc_replaced(P1, E, FArgs, LOC, Var, NewVarLLWRet):- compound(FArgs), \+ is_nsVar(FArgs), !,
   compound_name_arguments(FArgs, Name, Args),
   sub_term_loc_l(arg, P1, E, Args, LOC, Var, NewVarL),
   must_det_llu(compound_name_arguments(NewCompound, Name, NewVarL)), NewVarLLWRet=NewCompound.
sub_term_loc_replaced(P1, A, A, self, Var, Var):- call(P1, A), !.


sub_term_loc_l(Nth, P1, E, Args, e(N, Nth)+Loc, Var, NewVarL):-
   reverse(Args, RevArgs),
   append(Left, [ST|Right], RevArgs),
   sub_term_loc_replaced(P1, E, ST, Loc, Var, ReplaceST),
   append(Left, [ReplaceST|Right], RevNewVarL),
   reverse(RevNewVarL, NewVarL),
   length([_|Right], N), !.


% Convert a list of conditions into a conjunction
list_to_conjunction([], true).
list_to_conjunction([Cond], Cond).
list_to_conjunction([H|T], RestConj) :- H==true,
   list_to_conjunction(T, RestConj).
list_to_conjunction([H|T], (H, RestConj)) :-
   list_to_conjunction(T, RestConj), !.

fix_equals_in_head(Convert, Convert):- \+ compound(Convert), !.
fix_equals_in_head(Convert:-Vert, Comp:-Vert):-!, fix_equals_in_head(Convert, Comp).
fix_equals_in_head(R=C, Converted):- append_term(C, R, Convert), !,
   fix_equals_in_head(Convert, Converted).
fix_equals_in_head(Comp, Converted):-
   once(as_compound_head(Comp, Convert)), Comp\==Convert, !,
   fix_equals_in_head(Convert, Converted).
fix_equals_in_head((A:B), iz(A, B)):- !.
fix_equals_in_head(u_assign(_NN, FList, R), HeadO):- FList =~ [F|List],
   append(List, [R], NewVarL), symbol(F), must_det_llu(Head=..[F|NewVarL]), !, fix_equals_in_head(Head, HeadO).
fix_equals_in_head(Convert, MCConvert):- prepend_functor(mc, Convert, MCConvert), !.


prepend_functor(MC, Convert, MCConvert):- is_list(Convert), !, prepend_functor3(MC, Convert, MCConvert).
prepend_functor(MC, Convert, MCConvert):- compound(Convert), !, compound_name_arguments(Convert, F, Args), !, prepend_functor3(MC, [F|Args], MCConvert).
prepend_functor(MC, Convert, MCConvert):- prepend_functor3(MC, [Convert], MCConvert).
prepend_functor3(MC, [F|Args], MCConvert):- F==MC, !, must_det_lls(MCConvert=..[F|Args]).
prepend_functor3(MC, [F|Args], MCConvert):- must_det_lls(MCConvert=..[MC, F|Args]).

as_compound_head([F|Converted], Comp):- symbol(F), !, must_det_llu(compound_name_arguments(Comp, F, Converted)).
as_compound_head(Comp, Comp).

:- op(700, xfx, '=~').



% compile_for_assert_eq(_Eq, H, B, Result):- compile_for_assert(H, B, Result), !.
compile_for_assert_eq('=', HeadInC, AsBodyFnC, Converted):-
    subst_vars(['=', HeadInC, AsBodyFnC], ['=', HeadIn, AsBodyFn], NamedVarsList),
    maplist(cname_var, NamedVarsList), !,
    compile_for_assert(HeadIn, AsBodyFn, Converted).
compile_for_assert_eq(':-', HeadIn, BodyIn, Converted):-
    call(ensure_corelib_types_file),
    Converted=(H:-B), s2p(HeadIn, H), s2p(BodyIn, B), !.


ensure_corelib_types_file.

ensure_corelib_types:- pfcAdd(please_do_corelib_types).

each_recompile:- ensure_corelib_types.

compile_for_assert(HeadInC, AsBodyFn, ConvertedO):-
 call(ensure_corelib_types),
 allow_type_violations((
     calc_precompiled_types(['=', HeadInC, AsBodyFn]),
     compile_for_assert_now(HeadInC, AsBodyFn, Converted),
     continue_opimize(Converted, ConvertedO))).

append_cl_term(List, Arg, Appended):- is_list(List), !, append(List, [Arg], Appended).
append_cl_term(List, Arg, Appended):- append_term(List, Arg, Appended), !.



maybe_remember_type_isa(_F,_N,Type):- nonvar(Type),!. % append_varname
maybe_remember_type_isa( F, N,Type):- add_type_to(Type,arg(F,N)),!,ignore(cname_var_unnamed(['Arg',F,N,'__'],Type)),!.

compile_for_assert_now(HeadInC, AsBodyFn, Converted):-
 must_det_lls((
    as_functor_args(HeadInC, F, Len, OrigArgsL),
    nop(maplist_nth(maybe_remember_type_isa(F),1,OrigArgsL)),
    compile_head_variablization(HeadInC, NewHead, VHeadCode),
    compile_head_args(NewHead, NewestHeadC, AHeadCode),
    combine_code(VHeadCode, AHeadCode, CodeForHeadArgs),
    cname_var('RetVal', Result), add_type_to(Result,arg(F,0)),!,
    as_functor_args(NewestHeadC, F, Len, ArgsL),
    nop(maplist_nth(maybe_remember_type_isa(F),1,ArgsL)),
    append(ArgsL, [Result], ArgsLWRet),
    prepend_functor(mc, [F|ArgsLWRet], HeadC),
    compound(HeadC),
    call((compiler_self(Self),
    get_operator_typedef_cmp(Self, F, Len, ParamTypes, RetType))),
    %f2p_assign_args(CType, RetType, F, HeadC, ParamTypes, ParamL, ArgsL, CodeForHeadArgsL),
    %f2p([], CType, Caller, Nth, Result, [F|ArgsL], NextBody),
     CType = '%Undefined%', Caller = body, Nth = 0,
    %if_t(F=='unquote',rtrace),
    f2p(h_pt_r_s(HeadC, ParamTypes, RetType, src(['=', HeadInC, AsBodyFn])), CType, Caller, Nth, RetType, Result, AsBodyFn, NextBody),
    combine_code(CodeForHeadArgs, NextBody, BodyC),
    optimize_head_and_body(HeadC, BodyC, HeadCC, BodyCC),
    Convert = (HeadCC :- BodyCC),
    fix_equals_in_head(Convert, Converted))).

compile_for_assert_02(HResult, HeadIs, RetType, AsBodyFn, Converted)
 :- is_nsVar(AsBodyFn),
     AsFunction = HeadIs, !,
     must_det_ll((
     Converted = (HeadC :- BodyC), % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
     %funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, _Nth, Head),
     CType = '%Undefined%', Caller = body, Nth = 0,
     f2p(HeadIs, CType, Caller, Nth, RetType, HResult, AsFunction, HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
       funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, _Nth, Head)),
     NextBody = u_assign(4, AsBodyFn, Result),
   optimize_head_and_body(Head, NextBody, HeadC, BodyC),
     cname_var('HEAD_RES', Result))), !.

compile_for_assert_02(HResult, HeadIs, RetType, AsBodyFn, Converted) :- mctrace(compiler44),
    ar2p(HeadIs, RetType, HResult, NewHead),
     AsFunction = HeadIs,
     must_det_ll((
     Converted = (HeadC :- NextBodyC), % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   /*funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, _Nth, Head), */
     CType = '%Undefined%', Caller = body, Nth = 0,
     f2p(NewHead, CType, Caller, Nth, HResult, AsFunction, HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
        funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, _Nth, Head)),
   %verbose_unify(Converted),
     f2p(HeadIs, CType, Caller, Nth, RetType, Result, AsBodyFn, NextBody),
     %RetResult = Converted,
     %RetResult = _,
   optimize_head_and_body(Head, NextBody, HeadC, NextBodyC),
   %fbug([convert(Convert), optimize_head_and_body(HeadC:-NextBodyC)]),
     %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)), fbug(was(Head:-NextBody))),

     cname_var('HEAD_RES', Result))), !.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert_02(HResult, HeadIs, RetType, AsBodyFn, Converted) :-  mctrace(compiler54),
   Result = HResult,
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, _Nth, Head),
   compile_head_args(Head, HeadC, CodeForHeadArgs),
   CType = '%Undefined%', Caller = body, Nth = 0,
   f2p(HeadIs, CType, Caller, Nth, RetType, Result, AsBodyFn, NextBody),
   combine_code(CodeForHeadArgs, NextBody, BodyC), !,
   optimize_head_and_body(HeadC, BodyC, HeadCC, BodyCC), !.


skip_mizer.
/*
*/
%optimize_head_and_body(Head, Body, HeadNewest, BodyNewest):- fail, skip_mizer, !, Head= HeadNewest, Body = BodyNewest, !.
optimize_head_and_body(Head, Body, HeadNewest, BodyNewestO):-
   label_body_singles(Head, Body),
   (merge_and_optimize_head_and_body(Head, Body, HeadNew, BodyNew),
      (((Head, Body)=@=(HeadNew, BodyNew))
      ->  (HeadNew=HeadNewest, BodyNew=BodyNewest)
      ;

  (
   nop((color_g_mesg('#404064', print_pl_source((before(Head) :- Body))),
     color_g_mesg('#404064', print_pl_source((after(HeadNew) :- BodyNew))))),
    optimize_head_and_body(HeadNew, BodyNew, HeadNewest, BodyNewest)
  ))),
  must_optimize_body(HeadNew, BodyNewest, BodyNewestO), !.

continue_opimize(HB, (H:-BB)):- expand_to_hb(HB, H, B), must_optimize_body(HB, B, BB), !.
continue_opimize(Converted, Converted).



merge_and_optimize_head_and_body(Head, Converted, HeadO, Body):- nonvar(Head), Head = (PreHead, True), !,
  merge_and_optimize_head_and_body(PreHead, (True, Converted), HeadO, Body).
merge_and_optimize_head_and_body(AHead, Body, Head, BodyNew):-
   fix_equals_in_head(AHead, Head),
   must_optimize_body(Head, Body, BodyNew).


label_body_singles(Head, Body):-
   term_singletons(Body+Head, BodyS),
   maplist(label_body_singles_2(Head), BodyS).
label_body_singles_2(Head, Var):- sub_var(Var, Head), !.
label_body_singles_2(_, Var):- ignore(Var='$VAR'('_')).



metta_predicate(u_assign(_NN, evaluable, eachvar)).
metta_predicate(eval_true(matchable)).
metta_predicate(with_space(space, matchable)).
metta_predicate(limit(number, matchable)).
metta_predicate(findall(template, matchable, listvar)).
metta_predicate(match(space, matchable, template, eachvar)).



filter_head_arg(H, F):- var(H), !, H=F.
filter_head_arge(H, F):- H = F.

code_callable(Term, _CTerm):- var(Term), !, fail.
code_callable(Term, CTerm):- current_predicate(_, Term), !, Term=CTerm.
%code_callable(Term, CTerm):- current_predicate(_, Term), !, Term=CTerm.

compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, If, Then, Else, Converted):-
  HeadIs = headIs,
  f2p(HeadIs, CType, Caller, Nth, RetType, ThenResult, Then, ThenCode),
  f2p(HeadIs, CType, Caller, Nth, RetType, ElseResult, Else, ElseCode),
  Converted=(If*->(ThenCode, ThenResult=RetResult);
                  (ElseCode, ElseResult=RetResult)).

:- discontiguous(compile_flow_control/8).
:- discontiguous(f2q/8).



%dif_functors(HeadIs, _):- var(HeadIs), !, fail.
dif_functors(_, _):-!.
dif_functors(HeadIs, _):- \+ compound(HeadIs), !.
dif_functors(HeadIs, Convert):- compound(HeadIs), compound(Convert),
  compound_name_arity(HeadIs, F, A), compound_name_arity(Convert, F, A).

is_compiled_and(AND):- member(AND, [ /*(', '), ('and'), */ ('and2'), ('progn')]).

flowc.

no_lists(Args):- maplist(not_a_function_in_arg, Args).

not_a_function_in_arg(Arg):- is_ftVar(Arg), !.
not_a_function_in_arg(Arg):- \+ is_list(Arg), !.



f2p(HeadIs, CType, Caller, Nth, RetResult, Convert, Converted):-
  f2p(HeadIs, CType, Caller, Nth, _ANY_, RetResult, Convert, Converted), !.


f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
   %Depth2 is Depth-1,
  ss_unfreeze_vars(f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted)),
  with_ss_unify(Convert, (f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converting))),!,
  convert_fromi(Converting, Converted), !.

f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
   %Depth2 is Depth-1,
  ss_unfreeze_vars(f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted)),
  with_ss_unify(Convert, must_det_lls(f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converting))),
  convert_fromi(Converting, Converted), !.

f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
   debug_info(always(compiler_failed), eval_missed(Convert)),
   trace,
   rtrace(with_ss_unify(Convert, must_det_lls(f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted)))), !.

f2p(_HeadIs,_CType,_Caller,_Nth, RetType, RetResult, Convert, (info(eval_missed(RetType, Convert)), unify_with_occurs_check(Convert, RetResult))):- !.

convert_fromi(Converted, Converted):-!.
convert_fromi(Converted, Converted):- is_ftVar(Converted), !.
convert_fromi(Converted, Converted):- \+ compound(Converted), !.
%convert_fromi( u_assign(NN, E, R), UA):-  !, u_assign(NN, E, R)=UA.
convert_fromi((A, B), (AA, BB)):- !, convert_fromi(A, AA), convert_fromi(B, BB).
convert_fromi(Converting, Converted):- is_list(Converting), !, maplist(convert_fromi(), Converting, Converted).
convert_fromi(Converting, Converted):- compound_name_arguments(Converting, F, Args), !,
   maplist(convert_fromi(), Args, NewVarL), !,
   compound_name_arguments(Converted, F, NewVarL).

%convert_fromi(Converting, Converted):- compile_flow_control(Converting, Converted).
is_fqVar(Var2):- is_ftVar(Var2), !.
is_fqVar(Var2):- symbol(Var2), !.

compile_flow_control(HeadIs, _CType,_Caller,_Nth, RetType, RetResult, Convert, Converted):- fail,
  wdmsg(cfc(HeadIs, RetType, RetResult, Convert, Converted)), fail.

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, RetVar, Convert, u_assign(6, Convert, RetVar)) :-
    is_ftVar(Convert), !.% Check if Convert is a variable

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, _RetResult, Convert, _Converted):- \+ compound(Convert), !, fail.
compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, _RetResult, [Convert|_], _Converted):- \+ callable(Convert), !, fail.

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _, Var1, Var2, flow_control_v_v((Var1=Var2))):- is_fqVar(Var1), is_fqVar(Var2), !.



compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted):-
    Convert=~ ['call-fn!', Fn|Args], !, append([call, Fn|Args], [RetResult], CallFnArgs),
    must_det_lls(Converted=..CallFnArgs).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
   Convert=~'while!'(Test, Then),
   f2p(HeadIs, CType, Caller, Nth, 'Bool', BoolValue, Test, BoolCode),
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Then, ThenCode),
   Converted =
     ( FinalResult=result_holder([]),
       repeat,
        ((BoolCode->is_True(BoolValue))
             ->(((ThenCode, nb_setarg(1, FinalResult, RetResult))), fail)
              ;(!, true)),
        arg(1, FinalResult, RetResult)).



compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, _RetResult, Convert, Converted):-
   Convert==[empty], !, Converted= fail.

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, _RetResult, u_assign(NN, E, R), UA):-  !, u_assign(NN, E, R)=UA.

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted) :- % HeadIs, RetType\=@=Convert,
     is_arity_0(Convert, F), !, Converted = u_assign(7, [F], RetResult), !.



compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, eval_true(Convert)):-
   %nl, print(Convert), nl,
   as_functor_args(Convert, F, A, Args),
   \+ maplist(is_list, Args),
   is_absorbed_return_value(F, A, _Bool, RResult), RResult=RetResult.


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- %dif_functors(HeadIs, Convert),
    Convert =~ 'match'(ESpace, Pattern, Template), !,
  must_det_ll((
    f2p(HeadIs, CType, Caller, Nth, 'SpaceType', SpaceV, ESpace, Code),
    %term_variables(Template, TemplateVars),
    compile_pattern(HeadIs, SpaceV, Pattern, SpacePatternCode),
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Template, TemplateCode),
    combine_code((Code, SpacePatternCode), TemplateCode, Converted))).

compile_pattern(_HeadIs, Space, Pattern, PatternCode):- is_ftVar(Pattern),
  PatternCode = metta_atom_iter(Space, Pattern).
compile_pattern(HeadIs, Space, [C|Pattern], PatternCode):- C ==', ',
   maplist(compile_pattern(HeadIs, Space), Pattern, PatternCodeL),
   list_to_conjuncts(PatternCodeL, PatternCode).
compile_pattern(HeadIs, Space, [C|Pattern], PatternCode):- C ==';',
   maplist(compile_pattern(HeadIs, Space), Pattern, PatternCodeL),
   list_to_disjuncts(PatternCodeL, PatternCode).
compile_pattern(_HeadIs, Space, Pattern, PatternCode):-
  PatternCode = metta_atom_iter(Space, Pattern).


interpet_this(_Convert):-!, fail.

interpet_this(Convert):- as_functor_args(Convert, F, A, Args), interpet_this(Convert, F, A, Args).
interpet_this(_, F, _, _):- \+ symbolic(F), !.
interpet_this(_, F, _, _):- compile_this_s(F), !, fail.
interpet_this(_, F, _, _):- interpet_this_f(F), !.
% stable workarround until the '=~' bug is fixed for numbers
interpet_this(Convert, F, A, Args):- compile_this(Convert, F, A, Args), !, fail.
interpet_this(_, _, _, _).

interpet_this_f(F):- buffer_src_isa(F, 'Compiled'), !, fail.
interpet_this_f(F):- buffer_src_isa(F, 'Interpreted'), !.
interpet_this_f(F):- op_decl(F, [ 'Number', 'Number' ], 'Number').

compile_this(Convert):- as_functor_args(Convert, F, A, Args), compile_this(Convert, F, A, Args).
compile_this(_, F, _, _):- \+ symbolic(F), !, fail.
compile_this(_, F, _, _):- compile_this_f(F), !.

compile_this_f(F):- buffer_src_isa(F, 'Compiled').
compile_this_f(F):- interpet_this_f(F), !, fail.
compile_this_f(F):- compile_this_s(F), !.
compile_this_f(F):- buffer_src([':', F, [Ar|_]]), Ar=='->', !.
compile_this_s('superpose').
compile_this_s('match').
compile_this_s('do').
compile_this_s('do-all').


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail, dif_functors(HeadIs, Convert),
  copy_term(Convert, ConvertCopy), get_inline_def(Convert, NewDef), Convert=@=ConvertCopy,
  debug_info(compiler_inlined, get_inline_def(Convert, NewDef)),
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, NewDef, Converted))), !.

/*
compile_flow_control(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, eval_late_for(Convert, RetType, RetResult)) :-
  Convert =~ [Var|_], is_ftVar(Var), !.
*/

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, do(Converted)) :- fail, % dif_functors(HeadIs, Convert),
  ignore(RetResult='Empty'),
  Convert =~ ['do', Body], !,
  f2p(HeadIs, CType, Caller, Nth, RetType, _RetResult, Body, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, _RetTypeD, RetResult, Convert, (doall(Converted), RetResult='Empty')) :- fail, % dif_functors(HeadIs, Convert),
  Convert =~ ['do-all', Body], !,
  f2p(HeadIs, CType, Caller, Nth, _RetTypeB, _RetResultB, Body, Converted).


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-  % dif_functors(HeadIs, Convert),
  Convert =~ ['let', LVar, Value1, Body], !,
  compile_chain(HeadIs, CType, Caller, Nth, RetType, RetResult, chain(Value1, LVar, Body), Converted).

compile_chain(HeadIs, CType, Caller, Nth, RetType, RetResult, chain(Value1, LVar, Body), Converted):-
    f2p(HeadIs, CType, Caller, Nth, '%Undefined%', Var, Value1, CodeForValue1),
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode),
    (nonvar(LVar) -> combine_code((CodeForValue1, unify_with_occurs_check(Var, LVar), BodyCode), Converted);
       (Var=LVar, combine_code(CodeForValue1, BodyCode, Converted))).


is_Nil(Nil):- Nil==[], !.
is_Nil(Nil):- Nil=='Nil', !.
is_Nil(Nil):- Nil=='()', !.

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- %dif_functors(HeadIs, Convert),
  Convert =~ ['let*', Nil, Body], is_Nil(Nil), !,
   must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, Converted))).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- %dif_functors(HeadIs, Convert),
  Convert =~ ['let*', AAAA, Body], AAAA=~[VE|Bindings], VE=~[V, E],
  compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, ['let', V, E, ['let*', Bindings, Body]], Converted).


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail, dif_functors(HeadIs, Convert),
  Convert =~ ['let*', Bindings, Body], !,
   must_det_ll((
    maplist(compile_let_star(HeadIs, CType, Caller, Nth, RetType), Bindings, CodeList),
    list_to_conjuncts(CodeList, BindingCode),
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode),
    combine_code(BindingCode, BodyCode, Converted))).

compile_let_star(HeadIs, CType, Caller, Nth, RetType, NV, Converted):-
 must_det_ll((NV =~ [Expression, Var],
 (var(Var)-> f2p(HeadIs, CType, Caller, Nth, RetType, Var, Expression, Converted);
 (var(Expression)-> f2p(HeadIs, CType, Caller, Nth, RetType, Expression, Var, Converted);
 (f2p(HeadIs, CType, Caller, Nth, RetType, Eval1Result, Expression, Code),
  into_equals(l_s1, Eval1Result, Var, Eval1ResultVar),
  combine_code(Code, Eval1ResultVar, Converted)))))).

/*
  compile_flow_control_disabled(HeadIs, RetType, RetResult, Convert, Converted) :-
   Convert =~ ['sequential'|ValueL],
   ReConvert =~ ['superpose'|ValueL], !,

   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, ReConvert, Converted).
*/

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, (Converted)) :-
    Convert =~ ['sequential', ValueL], is_list(ValueL), !,
    %maybe_unlistify(UValueL, ValueL, URetResult, RetResult),
    maplist(
         f2p(HeadIs, CType, Caller, Nth, RetType), RetResultL, ValueL, CodeForValueL),
    last(RetResultL, RetResult),
    list_to_conjuncts(CodeForValueL, Converted), !.

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, (Converted)) :-
    Convert =~ ['superpose', ValueL], is_list(ValueL), !,
    %maybe_unlistify(UValueL, ValueL, URetResult, RetResult),
    must_det_lls(( (cname_var('SP_Ret_', RetResult)),
    maplist(f2p(HeadIs, CType, Caller, Nth, RetType, RetResult), ValueL, CodeForValueL),
    list_to_disjuncts(CodeForValueL, Converted))), !.

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, (Converted)) :-
    Convert =~ ['superpose', ValueL], % is_nsVar(ValueL), !,
    %maybe_unlistify(UValueL, ValueL, URetResult, RetResult),
    Converted = me('superpose', ValueL, RetResult),
    cname_var('MeTTa_SP_', ValueL).

:- op(700, xfx, =~).

uneval(UEval1, Eval1):- is_ftVar(UEval1), !, UEval1=Eval1.
uneval(UEval1, Eval1):-
    UEval1 =~ 'eval'(Eval1), !.
uneval(Eval1, Eval1).


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
   Convert =~ 'chain'(UEval1, Result, Eval2), !, uneval(UEval1, Eval1),
   compile_chain(HeadIs, CType, Caller, Nth, RetType, RetResult,  chain(Eval1, Result, Eval2), Converted).

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, RetType, ResValue, Convert, call_for_value_type(Value1, RetType, Converted, ResValue)) :- % dif_functors(HeadIs, Convert),
   Convert =~ ['call-for!', Value1|SProlog],
   ignore(if_t((var(Value1);var(ResValue)), Value1=ResValue)),
   compile_as_prolog(SProlog, Converted).

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, ResValue, Convert, call_for_unit(Converted, ResValue)) :- % dif_functors(HeadIs, Convert),
   (Convert =~ ['call-unit!'|SProlog]), % copy_term(CT, TA), protect(CT), trace, TA,
   compile_as_prolog(SProlog, Converted).

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, ResValue, Convert, call_for_unit(Converted, ResValue)) :- % dif_functors(HeadIs, Convert),
   (Convert =~ [A|SProlog]), A=='@', % copy_term(CT, TA), protect(CT), trace, TA,
   compile_as_prolog(['@'|SProlog], Converted).

compile_flow_control(_HeadIs, _CType,_Caller,_Nth, _RetType, ResValue, Convert, call_for_unit(Converted, ResValue)) :- % dif_functors(HeadIs, Convert),
   (Convert =~ [A|SProlog]), A=='or', % copy_term(CT, TA), protect(CT), trace, TA,
   locally(nb_setval(prolog_interp_mode, true), compile_as_prolog(['or'|SProlog], Converted)).

compile_as_prolog(Var, call_as_prolog(Var)):- var(Var), !.
compile_as_prolog(V, call_as_prolog2(V)):- \+ is_list(V), \+ compound(V).
compile_as_prolog([Pred|Args], Apply):- var(Pred), !, cmp_p_args(Args, PlArgs), make_apply(Pred, PlArgs, Apply).
compile_as_prolog(V, call_as_prolog3(V)):- \+ is_list(V), compound(V).
compile_as_prolog(['@', Pred|Args], Apply):- \+ is_list(Pred), cmp_p_args(Args, PlArgs), make_apply(Pred, PlArgs, Apply).
compile_as_prolog(['@', Pred|SProlog], Conjs):- is_list(Pred), s_list_to_prolog([Pred|SProlog], Conjs).
compile_as_prolog([', '|SProlog], Conjs):-   s_list_to_prolog(SProlog, Conjs).
compile_as_prolog(['and'|SProlog], Conjs):- s_list_to_prolog(SProlog, Conjs).
compile_as_prolog([';'|SProlog], Conjs):-   s_list_to_prolog_or(SProlog, Conjs).
compile_as_prolog(['or'|SProlog], Conjs):-  s_list_to_prolog_or(SProlog, Conjs).
compile_as_prolog([Pred|Args], Apply):- \+ is_list(Pred), cmp_p_args(Args, PlArgs), make_apply(Pred, PlArgs, Apply), !.
compile_as_prolog([Pred|SProlog], Conjs):-  is_list(Pred), s_list_to_prolog([Pred|SProlog], Conjs).
compile_as_prolog([Pred|Args], apply4(call, PlArgs)):- cmp_p_args([Pred|Args], PlArgs), !.


make_apply(Pred, PlArgs, Apply):-atom(Pred), is_list(PlArgs), Apply=..[Pred|PlArgs], !.
make_apply(Pred, PlArgs, Apply):-is_ftVar(Pred), is_list(PlArgs), Apply=..[call, Pred|PlArgs], !.
make_apply(Pred, PlArgs, apply(Pred, PlArgs)):- \+ is_list(Pred), !.

s_list_to_prolog(SList, Prolog):- is_list(SList), maplist(compile_as_prolog, SList, PrologL), !, list_to_conjuncts(PrologL, Prolog).
s_list_to_prolog_or(SList, Prolog):- is_list(SList), maplist(compile_as_prolog, SList, PrologL), !, list_to_disjuncts(PrologL, Prolog).

cmp_p_args(Args, PlArgs):- var(Args), !, PlArgs= Args.
cmp_p_args([A|Args], [P|PlArgs]):- cmp_as_arg(A, P), !, cmp_p_args(Args, PlArgs).
cmp_p_args(Args, PlArgs):- PlArgs= Args.

cmp_as_arg(L, A):- is_list(L), !, compile_as_prolog(L, A).
cmp_as_arg(A, A).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
  once(Convert =~ 'if'(Cond, Then, Else);Convert =~ 'If'(Cond, Then, Else)),
  !, Test = is_True(CondResult),
  f2p(HeadIs, CType, Caller, Nth, 'Bool', CondResult, Cond, CondCode),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (CondCode, Test), Then, Else, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- dif_functors(HeadIs, Convert),
  once(Convert =~ 'if'(Cond, Then);Convert =~ 'If'(Cond, Then)),
  f2p(HeadIs, CType, Caller, Nth, 'Bool', CondResult, Cond, CondCode),
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Then, ThenCode),
    Converted = ((CondCode, is_True(CondResult)), ThenCode).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
   Convert =~ 'if-error'(Value, Then, Else), !, Test = is_Error(ValueResult),
  f2p_verbatum(HeadIs, RetType, ValueResult, Value, ValueCode),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (ValueCode, Test), Then, Else, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
   Convert =~ 'if-empty'(Value, Then, Else), !, Test = is_Empty(ValueResult),
  f2p_verbatum(HeadIs, RetType, ValueResult, Value, ValueCode),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (ValueCode, Test), Then, Else, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
  (Convert =~ 'if-non-empty-expression'(Value, Then, Else)), !,
  (Test = ( \+ is_Empty(ValueResult))),
  f2p_verbatum(HeadIs, RetType, ValueResult, Value, ValueCode),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (ValueCode, Test), Then, Else, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
    Convert =~ ['if-equal', Value1, Value2, Then, Else], !, Test = equal_enough(ResValue1, ResValue2),
    f2p_verbatum(HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    f2p_verbatum(HeadIs, RetType, ResValue2, Value2, CodeForValue2),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (CodeForValue1, CodeForValue2, Test), Then, Else, Converted).

f2p_verbatum(_, _RetType, Value, Value, info(f2p_verbatum(Value))):-!.

as_variable_name(Sym,Named):- as_variable_name0(Sym,Name),!,ground(Name),Name=Named.

as_variable_name0(Nil,Named):- Nil==[],!,Named='Nil'.
as_variable_name0(NuL,Named):- NuL=='',!,Named='NuL'.
as_variable_name0(Sym,Named):- atom(Sym),!,svar_fixname(Sym,Named),!. %Named=Sym.
as_variable_name0(Sym,Named):- var(Sym),pretty_clauses:variable_name(Sym,Named),!,nonvar(Named).
as_variable_name0(Sym,_Named):- var(Sym),!,fail.
as_variable_name0('$VAR'(Sym),Named):- \+ integer(Sym),!, as_variable_name(Sym,Named).
as_variable_name0('$VAR'(Sym),Named):- format(atom(Named),'~w',['$VAR'(Sym)]),!.
as_variable_name0(Sym,Named):- string(Sym),name(Named,Sym),!.
as_variable_name0(List,Named):- is_list(List),!,maplist(as_variable_name,List,NamedL),atomic_list_concat(NamedL,'_',Named).
%as_variable_name0([Sym|Nil],Named):- Nil==[],!,as_variable_name(Sym,Named).
%as_variable_name0([N|Sym],Named):- as_variable_name(N,A),as_variable_name(Sym,B),append_names(A,B,Named),!.
as_variable_name0(Sym,Named):- compound(Sym),compound_name_arguments(Sym,F,Args),as_variable_name([F|Args],Named),!.
as_variable_name0(Sym,Named):- compound(Sym),sub_term(Sub,Sym),as_variable_name(Sub,Named),!.
as_variable_name0(Sym,Named):- format(atom(Atom),'~w',[Sym]),as_variable_name(Atom,Named),!.


append_names(Sym,Name,Result):- atom_contains(Name,Sym),!,Result=Name.
append_names(Sym,Name,Result):- atom_contains(Sym,Name),!,Result=Sym.
append_names(Sym,Name,Result):- atomic_list_concat([Sym,Name],Result).

append_varname(Sym,Expr):- \+ atom(Sym),!,as_variable_name(Sym,Name),!,append_varname(Name, Expr).
append_varname(Sym,Expr):- nonvar(Expr),!,Expr='$VAR'(N),as_variable_name(N,Name),append_names(Sym,Name,Result),svar_fixname_u(Result,Fixed), gensym(Fixed,Gen),setarg(1,Expr,Gen),!.
append_varname(Sym,Expr):- as_variable_name(Expr,Named2),!,as_variable_name(Sym,Named),append_names(Named,Named2,Result),svar_fixname_u(Result,Fixed), gensym(Fixed,Gen), %debug_var(Gen,Expr),
   put_attr(Expr, vn, Gen),!.
append_varname(Sym,Expr):- gensym(Sym,Gen),put_attr(Expr, vn, Gen),!.

cname_var_unnamed(_Sy, Expr):- attvar(Expr),get_attr(Expr,vn,_),!.
cname_var_unnamed(Sym, Expr):- cname_var(Sym, Expr).

svar_fixname_u(Result,FixedU):- svar_fixname(Result,Fixed),ensure_ends_with_u(Fixed,FixedU).
ensure_ends_with_u(Fixed,FixedU):- atom_concat(_,'_',Fixed),!,Fixed=FixedU.
ensure_ends_with_u(Fixed,FixedU):- atom_concat(Fixed,'_',FixedU),!.

cname_var(Sym, Expr):- \+ atom(Sym),as_variable_name(Sym,Name),cname_var(Name, Expr),!.
cname_var(Sym, _Src):- var(Sym),get_attrs(Sym,Attrs),ppt(Sym=Attrs),trace.
cname_var(Sym,_Expr):- \+ atom(Sym),nop(trace).
cname_var(Sym, Expr):- append_varname(Sym,Expr),!.
%cname_var(Sym, Expr):- ignore((atomic(Sym), gensym(Sym, ExprV), append_varname(ExprV,Expr))),!.
%cname_var(Sym, Expr):- var(Expr), gensym(Sym, ExprV), put_attr(Expr, vn, ExprV), !. %ignore(Expr='$VAR'(ExprV)), debug_var(ExprV, Expr).
%cname_var(Sym, Src):-  Src='$VAR'(_), !, must_det_ll((gensym(Sym, SrcV), nb_setarg(1, Src, SrcV))).
cname_var(_Sym, _Src).

gname_var(Sym, Src):-  must_det_lls((ignore((gensym(Sym, SrcV), ignore(Src='$VAR'(SrcV)))))).

fname_var(Name=Var):- cname_var(Name, Var).
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['assertEqual', Value1, Value2], !,
    cname_var('Src_', Src),
    cname_var('FA_', ResValue1),
    cname_var('FA_', ResValue2),
    cname_var('FARL_', L1),
    cname_var('FARL_', L2),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue1, Value1, CodeForValue1),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue2, Value2, CodeForValue2),
    Converted =
              (Src = Convert,
               loonit_assert_source_tf_empty(src(Src), L1, L2,
                (findall_ne(ResValue1, CodeForValue1, L1),
                 findall_ne(ResValue2, CodeForValue2, L2)),
                 equal_enough_for_test(L1, L2), RetResult)).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['assertEqualToResult', Value1, Value2], !,
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue1, Value1, CodeForValue1),
    Src = Convert,
    Goal = findall_ne(ResValue1, CodeForValue1, L1),
    Converted = (
                loonit_assert_source_tf_empty(src(Src), L1, Value2,
                     Goal,
                     equal_enough_for_test(L1, Value2), RetResult)).

maybe_unlistify([UValueL], ValueL, RetResult, [URetResult]):- fail, is_list(UValueL), !,
  maybe_unlistify(UValueL, ValueL, RetResult, URetResult).
maybe_unlistify(ValueL, ValueL, RetResult, RetResult).

list_to_disjuncts([], false).
list_to_disjuncts([A], A):- !.
list_to_disjuncts([A|L], (A;D)):-  list_to_disjuncts(L, D).


f2p_assign_args(CType, _RetType, Symbol, HeadIs, ParamTypes, ParamL, Args, ConvertedL):-
  must_det_lls((length(Args, Len), length(ParamTypes, Len), findall(N, between(1, Len, N), IntList))),
  maplist(must_f2p_assign(HeadIs, CType, Symbol), IntList, ParamTypes, ParamL, Args, ConvertedL).

must_f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, Converted):-
  must_det_lls(f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, Converted)).

is_evaled(Value):- var(Value), !, fail.
is_evaled([Value|List]):- is_list(List), nonvar(Value), is_evaled_f(Value).
is_evaled_f(chain).
is_evaled_f('@').
is_evaled_f(superpose).
is_evaled_f(if).
is_evaled_f(S):- symbol(S), from_pfc_true(returnType(_, S, _, _)).
is_evaled_f(S):- from_pfc_true(compiled_clauses(_, S, _)).

:- dynamic(returnType/4).
from_pfc_true(G):- !,  call(G).
from_pfc_true(G):- catch(clause(G, true, _), _, fail).

%f2p_assign(_HeadIs, CType, _Caller, Nth, _RetType, V, Value, is_True(V)):- Value=='True'.
% /*info(is_non_eval_kind(var(Value), RetType)))*/
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, true ):- is_ftVar(Value), is_ftVar(ValueR),
   is_non_eval_kind(RetType), Value=ValueR, !. %, ValueR=Value, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, eval_for_var(RetType, Value, ValueR) ):- is_ftVar(Value),
   is_non_eval_kind(RetType), !. %, ValueR=Value, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, true ):- \+ callable(Value), is_non_eval_kind(RetType), ValueR=Value, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, true ):- atom(Value), is_non_eval_kind(RetType), ValueR=Value, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, true ):- \+ is_list(Value), is_non_eval_kind(RetType), ValueR=Value, !.


f2p_assign(_HeadIs, _CType, Caller, Nth, RetType, ValueR, Value, info(is_non_eval_kind_lst(Caller, Nth, RetType))):-
      is_non_eval_kind(RetType),
      is_list(Value), \+ is_evaled(Value), ValueR=Value, !.

f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueR, SrcValue, info(is_non_eval_kind_eval(Caller, Nth, RetType, NewRetType))):-
 fail,
  is_non_eval_kind(RetType),
  is_list(SrcValue), is_evaled(SrcValue),
  ValueR=as_p1(SrcValue, Code, NewValueR),
  f2p(HeadIs, CType, Caller, Nth, NewRetType, NewValueR, SrcValue, Code).

%f2p_assign(_HeadIs, CType, _Caller, _Nth, RetType, ValueR, Value, info(is_non_eval_kind(Value, RetType))):- is_non_eval_kind(RetType), ValueR=Value, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, _RetType, ValueR, Value, true/*info(is_nsVar(Value))*/):- is_nsVar(Value), Value=ValueR, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, eval_for3(RetType, Value, ValueR)):- is_nsVar(Value), !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, RetType, ValueR, Value, eval_for4(RetType, Value, ValueR)):- is_ftVar(Value), !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, _RetType, ValueR, Value, true/*info(\+ callable(Value))*/):- \+ callable(Value), Value=ValueR, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, _RetType, ValueR, Value, info(\+ compound(Value))):- \+ compound(Value), Value=ValueR, !.
f2p_assign(_HeadIs, _CType, _Caller, _Nth, _RetType, ValueR, Value, info(\+ is_list(Value))):- \+ is_list(Value), Value=ValueR, !.
%f2p_assign(_HeadIs, CType, _Caller, Nth, _RetType, ValueR, Value, ValueR=Value):- \+ compound(Value), !.
%f2p_assign(_HeadIs, CType, _Caller, Nth, _RetType, ValueR, Value, ValueR=Value):- \+ is_list(Value), !.


f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueR, SrcValue, as_p1(RetType, NewRetType, SrcValue, Code, NewValueR, ValueR)):-
 fail,
  is_non_eval_kind(RetType),
  is_list(SrcValue), is_evaled(SrcValue),
  f2p(HeadIs, CType, Caller, Nth, NewRetType, NewValueR, SrcValue, Code), !.

f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueR, SrcValue, as_e1(RetType, NewRetType, SrcValue, Code, NewValueR, ValueR)):-
 fail,
  is_list(SrcValue), is_evaled(SrcValue),
  f2p(HeadIs, CType, Caller, Nth, NewRetType, NewValueR, SrcValue, Code), !.

f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, Done):-
   f2p([op(Caller, Nth)|HeadIs], CType, Caller, Nth, RetType, ValueResult, Value, CodeForValue),
   adorn_co4value(CodeForValue, f2p_thunk, Done).

f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, f2p_thunk(CodeForValue)):-
   f2p([op(Caller, Nth)|HeadIs], CType, Caller, Nth, RetType, ValueResult, Value, CodeForValue), !.



f2p_assign(_HeadIs, _CType, _Caller, _Nth, _RetType, ValueResult, Value, ar2p_thunk(Converted)):-
  ar2p(Value, ValueResult, Converted), !.



f2p_assign(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, Converted):-
   f2p([op(Caller, Nth)|HeadIs], CType, Caller, Nth, RetType, ValueResultRFree, Value, CodeForValue),
   %into_equals(NN, ValueResultR, ValueResult, ValueResultRValueResult),
   ValueResultRValueResult = f2p_freed(ValueResultRFree=ValueResult),
   combine_code(CodeForValue, ValueResultRValueResult, Converted).

adorn_co4value(CodeForValue, _Inf, CodeForValue):- CodeForValue\=(_, _), !.
adorn_co4value(CodeForValue,  Inf, (info(true(Inf)))):- CodeForValue==true, !.
adorn_co4value(CodeForValue,  Inf, (info(Inf), CodeForValue)):-!.


%compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, keep(Converted)) :-
%  Convert =~ ['case', Value, PNil], []==PNil, !, Converted = (ValueCode, RetResult=[]),
%      f2p(HeadIs, CType, Caller, Nth, RetType, _ValueResult, Value, ValueCode), !.

/*
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, (ValueCode, Converted)) :-
  Convert =~ ['case', Value|Options], \+ is_nsVar(Value), !,
  cname_var('CASE_VAR_', ValueResult),
  compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, ['case', ValueResult|Options], Converted),
  f2p(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, ValueCode).
*/
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
  Convert = [Switch, Value, Options], Switch=='switch', is_list(Options), !,
   must_det_ll((
    maplist(compile_case_bodies(HeadIs, CType, Caller, Nth, RetType, []), Options, Cases),
    cname_var('SWITCH_CASE', AllCases),
    cname_var('SWITCH_RESULT_', RetResult),
    Converted =
           ( AllCases = Cases,
             select_case(AllCases, Value, RetResult)))).

select_case(AllCases, Value, RetResult):-
  member(caseOption(Match, BodyResult, _ExtraVars, BodyCode), AllCases),
  Value=Match,!,call(BodyCode),RetResult=BodyResult.


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
  Convert = [Case, ValueSrc, Options], Case=='case', is_list(Options), !,
   must_det_ll((
    term_variables(ValueSrc, ExtraVars),
    %gensym('caseHead_', HeadMatcher),
    %HEAD=..[HeadMatcher, Value, RetResult|ExtraVars],
    maplist(compile_case_bodies(HeadIs, CType, Caller, Nth, RetType, ExtraVars), Options, Cases),

    cname_var('ValueRetType_',ValueRetType),
    f2p(HeadIs, CType, Caller, Nth, ValueRetType, Value, ValueSrc, ValueCode),
    cname_var('CASE_SWITCH_', AllCases),
    cname_var('CASE_RESULT_', RetResult),

    Converted =
           ( ValueCode,
             AllCases = Cases,
             select_case(AllCases, Value, RetResult)))).

compile_case_bodies(HeadIs, CType, Caller, Nth, RetType, ExtraVars, [Match, Body], caseOption(_, BodyResult, ExtraVars, BodyCode)):- Match == '%void%', !,
      f2p(HeadIs, CType, [case_bod|Caller], Nth, RetType, BodyResult, Body, BodyCode).
compile_case_bodies(HeadIs, CType, Caller, Nth, RetType, ExtraVars, [Match, Body], caseOption(Match, BodyResult, ExtraVars, BodyCode)):- !,
      f2p(HeadIs, CType, [case_bod|Caller], Nth, RetType, BodyResult, Body, BodyCode).
/*
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
  Convert = [Case, ValueSrc, Options], Case=='case', is_list(Options), !,
  %remove_attrs(cant_bind, RetResult+ValueSrc+Convert),
  %if_t((attvar(ValueSrc), get_attr(ValueSrc, vn, _), get_var_name(ValueSrc, Named)), Value = '$VAR'(Named)),
   must_det_lls((
    if_t(var(Value), cname_var('CASE_VALUE_', Value)),
                    cname_var('CASE_SWITCH_', AllCases),
                    cname_var('CASE_RESULT_', RetResult),
    %if_t(RetResult=='$VAR'('CASE_RESULT_45'), (bt, trace)),
    %if_t(RetResult=='$VAR'('CASE_RESULT_46'), (bt, trace)),
    term_variables(ValueSrc, ExtraVars),
    gensym('caseHead_', HeadMatcher))),
    %numbervars(ExtraVars, 66, _, [attvar(skip)]),
    must_det_lls((HEAD=..[HeadMatcher, Value, RetResult|ExtraVars],
    must_det_lls((maplist(compile_case_bodies(HEAD, RetResult, HeadIs, RetType), Options, Cases))), !,
    %remove_attrs(cant_bind, RetResult+ValueSrc+Convert),
    f2p(HeadIs, CType, Caller, Nth, _ValueRetType, Value, ValueSrc, ValueCode),
    combine_code(AllCases = Cases, call(ValueCode), CC),
    combine_code(CC, (call_case_body(HEAD, AllCases)), Converted))), !.

compile_case_bodies(HEAD, RetResult, HeadIs, RetType, [Match, Body], (VHEAD:-BodyCode)):- Match == '%void%', !,
      duplicate_term(HEAD, VHEAD), setarg(1, VHEAD, _), f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode).
compile_case_bodies(HEAD, RetResult, HeadIs, RetType, [Match, Body], (VHEAD:-BodyCode)):-
      duplicate_term(HEAD, VHEAD), setarg(1, VHEAD, Match), f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode).

call_case_body(HEAD, AllCases):-
       member((HEAD:-BodyCode), AllCases),
       %unify_enough(Value, MatchValue),
       !,
       rtrace_on_error(BodyCode).
*/

compound_equals(COL1, COL2):- COL1=@=COL2, !, COL1=COL2.
compound_equals(COL1, COL2):- compound_equals1(COL1, COL2).
compound_equals1(COL1, COL2):- is_nsVar(COL1), !, is_nsVar(COL2), ignore(COL1=COL2), !.
compound_equals1(COL1, COL2):- compound(COL1), !, compound(COL2), COL1=COL2.


compile_flow_control(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Converted) :-
    Convert =~ ['collapse', Value1], var(Value1),!,
    Converted = (findall_ne(ResValue1, eval(Value1,ResValue1), RetResult)).


compile_flow_control(HeadIs, CType, Caller, Nth, _RetType, RetResult, Convert, Converted) :-
    Convert =~ ['collapse', Value1],
    f2p(HeadIs, CType, Caller, Nth, '%Undefined%', ResValue1, Value1, CodeForValue1),
    Converted = (findall_ne(ResValue1, CodeForValue1, RetResult)).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['collapse-bind', Value1], !,
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue1, Value1, CodeForValue1),
    Converted = (findall_ne(ResValue1, CodeForValue1, RetResult)).


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
  Convert =~ ['unify', Value1, Value2, Then, Else], !, Test = metta_unify(ResValue1, ResValue2),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue1, Value1, CodeForValue1),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue2, Value2, CodeForValue2),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (CodeForValue1, CodeForValue2, Test), Then, Else, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
  Convert =~ ['if-unify', Value1, Value2, Then, Else], !, Test = metta_unify(ResValue1, ResValue2),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue1, Value1, CodeForValue1),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResValue2, Value2, CodeForValue2),
  compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (CodeForValue1, CodeForValue2, Test), Then, Else, Converted).




compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),

   Convert =~ ['if-decons', Atom, Head, Tail, Then, Else], !,
    Test = unify_cons(AtomResult, ResHead, ResTail),
    f2p(HeadIs, CType, Caller, Nth, RetType, AtomResult, Atom, AtomCode),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResHead, Head, CodeForHead),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResTail, Tail, CodeForTail),
    compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (AtomCode, CodeForHead, CodeForTail, Test), Then, Else, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
   Convert =~ ['if-decons', Atom, Head, Tail, Then], !,
    Test = unify_cons(AtomResult, ResHead, ResTail),
    f2p(HeadIs, CType, Caller, Nth, RetType, AtomResult, Atom, AtomCode),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResHead, Head, CodeForHead),
    f2p(HeadIs, CType, Caller, Nth, RetType, ResTail, Tail, CodeForTail),
    compile_test_then_else(HeadIs, CType, Caller, Nth, RetType, RetResult, (AtomCode, CodeForHead, CodeForTail, Test), Then, [empty], Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, _RetResult, Convert, Converted) :-
   Convert   =~ return(MeTTaValue),
   f2p(HeadIs, CType, Caller, Nth, RetType, AtomResult, MeTTaValue, AtomCode),
   Converted = (AtomCode, throw(metta_return(AtomResult))).


% !(compile-body! (function 1))
% !(compile-body! (function (throw 1)))
% !(compile-body! (superpose ((throw 1) (throw 2))))
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, CodeForExpr) :- % dif_functors(HeadIs, Convert),
   Convert =~ ['eval', Expr],
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Expr, CodeForExpr).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, (CodeForSpace, Converted)) :- % dif_functors(HeadIs, Convert),
   Convert =~ ['evalc', Expr, Space],
   f2p(HeadIs, CType, Caller, Nth, 'SpaceType', RetSpace, Space, CodeForSpace),
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Expr, CodeForExpr),
   Converted = with_space(RetSpace, CodeForExpr).

compile_flow_control(HeadIs, CType, Caller, Nth, _RetTypeIn, RetResult, Convert, (CodeForSpace, Converted)) :- % dif_functors(HeadIs, Convert),
   Convert =~ ['metta', Expr, RetType, Space],
   f2p(HeadIs, CType, Caller, Nth, 'SpaceType', RetSpace, Space, CodeForSpace),
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Expr, CodeForExpr ),
   Converted = with_space(RetSpace, CodeForExpr).


compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
   Convert =~ ['function', Body], !,
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode),
   Converted = catch(BodyCode, metta_return(FunctionResult), FunctionResult=RetResult), !.

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
   Convert =~ ['function'| BodyR], !, trace,
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, ['and2'(function, RetType)|BodyR], BodyCode),
   Converted = catch(BodyCode, metta_return(FunctionResult), FunctionResult=RetResult), !.

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, BodyCode) :-
   Convert =~ ['progn'| BodyR], !,
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, ['and2'('progn', RetType)|BodyR], BodyCode).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, BodyCode) :-
   Convert =~ [AND| BodyR], is_compiled_and(AND2), AND == AND2, !,
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, ['and2'(AND2, RetType)|BodyR], BodyCode).

is_and_2(AND, AND2, AndRetType):- compound(AND), !, AND='and2'(AND2, AndRetType).

compile_flow_control(_HeadIs, _CType, _Caller, _Nth, RetType, RetResult, Convert, BodyCode) :-
   Convert =~ [AND|Nil], Nil == [], is_and_2(AND, AND2, AndRetType), !,
   combine_types(AndRetType, RetType, RRType),
   BodyCode = res_ok_and2(AND2, combine_types(AndRetType, RetType, RRType), RetResult).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, AndBodyCode) :-
   Convert =~ [AND, Body|Nil], Nil == [], is_and_2(AND, AND2, AndRetType), !,
   combine_types(AndRetType, RetType, RRType),
   f2p(HeadIs, CType, Caller, Nth, AndRetType, RetResult, Body, BodyCode),
   combine_code(BodyCode, res_ok_and2(AND2, combine_types(AndRetType, RetType, RRType), RetResult), AndBodyCode).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
   Convert =~ [AND, Body1|BodyR], is_and_2(AND, AND2, AndRetType), !,
   %    B1Type = [->],
   ignore(AndRetType=B1Type),
   f2p(HeadIs, CType, Caller, Nth, B1Type, B1Res, Body1, Body1Code),
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, [AND|BodyR], Body2Code),
   combine_code((Body1Code, res_ok_and2(AND2, B1Type, B1Res), Body2Code), Converted).


%is_p2_bang(WTL):- metta_params_and_return_type(KB, WTL, 2, _, _RetType), get_type_src(WTL, 'MeTTaLog').
is_p2_bang('limit!'). is_p2_bang('offset!').
is_p2_bang('max-time!').
is_p2_bang('call-for!').
is_p2_bang('unique-by!').
is_p2_bang('with-output-to!').
wtl_type_pred(WTL, P2):- atom(WTL), atom_concat(P2, '!', WTL), is_p2_bang(P2).
wtl_type_pred(WTL, Arg1Type, P2):- wtl_type_pred(WTL, P2), get_farg_type(WTL, 1, Arg1Type).
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
   Convert=~ [WTL, Test, Then], nonvar(WTL),
   wtl_type_pred(WTL, Arg1Type, P2), % 'limit!' 'with-time-limit!' etc
   f2p(HeadIs, CType, Caller, Nth, Arg1Type, Arg1Value, Test, Arg1Code),
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Then, ThenCode),
   Converted =
     ( Arg1Code, call(P2, Arg1Value, ThenCode)).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
   Convert=~ [Call, Then], nonvar(Call), is_call_wrapper(Call, P1), % once!  rtrace! , etc
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Then, ThenCode),
   Converted =.. [P1, ThenCode], !.





% If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, SOR, or(AsPredO, Converted)) :-
  SOR =~ or(AsPredI, Convert),
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsPredI, AsPredO),
               f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted))), !.
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, or(AsPredI, Convert), (AsPredO *-> true; Converted)) :- fail, !,
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsPredI, AsPredO),
               f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted))).
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, (AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsPredI, AsPredO),
               f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted))).

'True'(X):- ignore(is_True(X)).
'False'(X):- is_False(X).

get_inline_case_list(HeadDef, Quot, CaseList):-
   findall([HeadDef, NewDef], get_inline_def1(HeadDef, NewDef), DefList), DefList\==[],
   findall([Quot, NewDef], member([HeadDef, NewDef], DefList), CaseList).

get_inline_def(HeadDef, NewDef):-fail,
   findall(NewDef, get_inline_def1(HeadDef, NewDef), EachDef), EachDef\==[],
   disj_def(EachDef, NewDef).



get_inline_def1(HeadDef, NewDef):-
   into_list_args(HeadDef, UHeadDef),
   copy_term(UHeadDef+HeadDef, CopyUHeadDef+HeadDefC),
   [UHead|_UArgs] = UHeadDef, nonvar(UHead),
   buffer_src([Eq, UHeadDef|Body]), is_metta_decl_f(Eq), once(xform_body(Body, NewDef)),
   (UHeadDef+HeadDef =@= CopyUHeadDef+HeadDefC).

%xform_body([Body], Body):-!.
%xform_body(Items, [progn|Body]).

xform_body(Var, Var):-is_ftVar(Var), !.
xform_body([], call(true)):-!.
xform_body([Body], Body):-!.
xform_body([Body1, Body2], (Body1, Body2)):-!.
xform_body([Body1|Body2L], (Body1, Body2)):-xform_body(Body2L, Body2).

disj_def(Var, Var):-is_ftVar(Var), !.
disj_def([], call(fail)):-!.
disj_def([Body], Body):-!.
disj_def([Body1, Body2], (Body1;Body2)):-!.
disj_def([Body1|Body2L], (Body1;Body2)):-disj_def(Body2L, Body2).


/*
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, transpose(Convert), Converted, Code) :- !,
   maplist(each_result(HeadIs, RetType, RetResult), Convert, Converted),
   list_to_disjuncts(Converted, Code).
*/
/*
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, once(u_assign(NN, Body, RetResult))) :-
  Convert=~ first_of(Body), is_ftVar(Body), !.
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
  Convert=~ first_of(Body),
  must_det_ll((as_functor_args(Body, F, A, Args),
  as_functor_args(Quot, quot, A, NewVarL),
  as_functor_args(QConvert, quot, A, Args))),
  get_inline_case_list([F|NewVarL], Quot, DefList), !,
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, [case, QConvert, DefList], Converted))).*/
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, once(Converted)) :-
  Convert=~ first_of(Body), !, f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, catch(BodyCode, Ex, HandlerCode)) :-
    Convert=~ catch(Body, E, Handler), !, s2p(E, Ex),
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode),
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Handler, HandlerCode).


%compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, rtrace(BodyCode)) :- Convert=~ 'rtrace!'(Body), !, f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode).
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, call(P1, BodyCode)) :-
    Convert=~ [CW, Body], is_call_wrapper(CW, P1), !,
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, call_cleanup(BodyCode, HandlerCode)) :-
    Convert=~ finally(Body, Handler), !,
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Body, BodyCode),
    f2p(HeadIs, CType, Caller, Nth, '%Undefined%', _DontCare, Handler, HandlerCode).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail,
   protect(Convert = [switch, Atom | Args ]),
   ReConvert = [case, [eval, Atom]| Args ], !,
   compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, ReConvert, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail, dif_functors(HeadIs, Convert),
  get_inline_def(Convert, InlineDef), !,
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, InlineDef, Converted))).

% If Convert is a ":-" (if) function, we convert it to the equivalent ":-" (if) predicate.
compile_flow_control(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Converted) :- Convert =(H:-B), !,
  RetResult=(H:-B), Converted = true.

% If Convert is a ", " (and) function, we convert it to the equivalent ", " (and) predicate.
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, SOR, [Comma, AsPredO, Converted]) :-
      SOR =~ [Comma, AsPredI, Convert], ', ' == Comma,
      must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsPredI, AsPredO),
                   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted))), !.

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, SOR, (AsPredO, Converted)) :-
      SOR =~ [Comma, AsPredI, Convert], 'and' == Comma,
            must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsPredI, AsPredO),
                   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted))), !.


% If Convert is a "not" function, we convert it to the equivalent ";" (or) predicate.
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, \+ eval_true(AsPredO)) :- !,
  Convert =~ 'not'(AsPredI),
  must_det_ll(f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsPredI, AsPredO)).

each_result(HeadIs, RetType, RetResult, Convert, Converted):-
   CType = '%Undefined%', Caller = body, Nth = 0,
   f2q(HeadIs, CType, Caller, Nth, RetType, OneResult, Convert, Code1),
   into_equals(ear, OneResult, RetResult, Code2),
   combine_code(Code1, Code2, Converted).


/*
compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
   Convert =~ if(Cond, Then), !,
   f2p(HeadIs, CType, Caller, Nth, RetType, CondResult, Cond, CondCode),
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Then, ThenCode),
   Converted = ((CondCode, is_True(CondResult)), ThenCode).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Converter, Converted):-
   de_eval(Converter, Convert), !,
   f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted).

compile_flow_control(HeadIs, CType, Caller, Nth, RetType, _Result, Convert, Converted)
  :- fail,
  as_functor_args(Convert, Func, PA),
   functional_predicate_arg(Func, PA, Nth),
   Convert =~ [Func|PredArgs],
   nth1(Nth, PredArgs, Result, FuncArgs),
   RetResult = Result,
   AsFunct =~ [Func|FuncArgs],
  f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, AsFunct, Converted).
*/
/*

f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Value, RetType)):- number(Value), Var=Value,    !, add_type_to(RetType, 'Number').
f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Var, RetType)):-  var(Value), Var=Value, !.
f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Value=Var, RetType)):- var(Value), !.
%into_equality_fct(_OpN, _Nth, RetType, Var, Value, RetType=Value):- is_value_obj(Value), !, add_type_to(RetType, 'PlainObj').
f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Value, RetType)):- is_value_obj(Value), Var=Value, !, add_type_to(RetType, val(Value)).
%into_equality_fct(OpN, Nth, RetType, Var, Convert, Code):- Var=@='$VAR'('_'), !,
%   into_equality_fct(OpN, Nth, _, Convert, Code), !.

f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Var, RetType)):- \+ is_list(Value), Var=Value, !.
f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Value=Var, RetType)):- \+ is_list(Value), !.
f2q(HeadIs, CType, Caller, Nth, RetType, Var, List, 'S'(Var=List, RetType)):- Var=List, is_value_list(List), !, add_type_to(RetType, 'Expression').



f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, true) :-
   is_ftVar(Convert), %sub_var(Convert, HeadIs), % Check if Convert is a variable in Head
   is_ftVar(RetResult),
   RetResult = Convert, !.
*/

% If Convert is a variable, the corresponding predicate is just u_assign(NN, Convert, RetResult)
%f2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, tvf_of_value(RetType, Convert, RetResult)) :- is_ftVar(RetResult), \+ var(RetResult), !. % \+ callable(Convert), !.
%f2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, tff_of_value(RetType, Convert, RetResult)) :- \+ var(RetResult), \+ callable(RetResult), !. % \+ callable(Convert), !.


%f2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, tvf_of_value(RetType, Convert, RetResult)) :- is_ftVar(RetResult), \+ var(RetResult), !. % \+ callable(Convert), !.
%f2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, tvf_of_value(RetType, Convert, RetResult)) :-!. % \+ callable(Convert), !.


f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, _Code) :- var(RetType), \+ attvar(RetType),
    dppt(var_rt(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert)),
    trace, %trace
    fail.


f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Code) :- \+ var(RetResult), \+ is_ftVar(RetResult),
   f2p(HeadIs, CType, Caller, Nth, RetType, Var, Convert, ProducedCode),
   Code = (ProducedCode,unify_value(Var, RetResult)),!.

f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Code) :- \+ var(RetResult), is_ftVar(RetResult), RetResult = '$VAR'(Named),
   cname_var(Named,Var) ,!,
   f2p(HeadIs, CType, Caller, Nth, RetType, Var, Convert, ProducedCode),
   (var(Var) -> (cname_var(Named,Var) , Code=ProducedCode) ; Code = (ProducedCode,unify_value_var(Var,RetResult))),!.

f2q(_HeadIs, _CType, _Caller, _Nth,  RetType, Var, Value, true):- number(Value), Var=Value,    !, add_type_to(RetType, 'Number').
f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, get_value(Value, Var)):- \+ var(Value), is_src_bound(Value), !.
f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, true):- atom(Value), Var=Value, !.
f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, true):- string(Value), Var=Value, !.
f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, info(is_value_obj(Value))):- \+ var(Value), \+ is_list(Value), is_value_obj(Value), Var=Value, !.
f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, true):- \+ var(Value), \+ is_list(Value), Var=Value, !.
f2q(_HeadIs, _CType, _Caller, _Nth,  RetType, Var, Value, unify_value(RetType, Value, Var)):- \+ var(Value), \+ is_list(Value), \+ Var=Value, !.
% If Convert is a number or an symbol, it is considered as already converted.

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, (RetResult =~ Convert)) :-
     as_is_data_term(Convert), !, RetResult = Convert. % Check if Convert is a data_term

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, RetResult = Convert) :- % HeadIs, RetType\=@=Convert,
    once(number(Convert); symbol(Convert); data_term(Convert)), % Check if Convert is a number or an symbol
    !.  % Set RetResult to Convert as it is already in predicate form

f2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, _Converted) :-
   dppt(f2q:-[ctype(CType),caller(Caller), Nth, ret_type(RetType), ret_result(RetResult), convert(Convert)]),
   %'is-function-type'
   %if_t(Caller==[if,let,case_bod,case,let|body],rtrace),
   fail.


f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code):-
    is_non_eval_kind(RetType), \+ is_code_kind(RetType),
    f2q_AtomTerm(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code),!.

f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code):-
    is_non_eval_kind(RetType),  is_code_kind(RetType),
    f2q_AtomCode(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code),!.

f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code):-
    is_pro_eval_kind(RetType), \+ is_code_kind(RetType),
    f2q_EvalTerm(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code),!.

f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code):-
    is_pro_eval_kind(RetType),  is_code_kind(RetType),
    f2q_EvalCode(HeadIs, CType, Caller, Nth, RetType, Var, Value, Code),!.

f2q_AtomTerm(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, true):-  var(Value),var(Var), must_det_lls(Var=Value), !.
f2q_AtomTerm(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, true):-  var(Value), must_det_lls(Var=Value), !.
f2q_AtomCode(_HeadIs, _CType, _Caller, _Nth,  RetType, Var, Value, pass_code(RetType, Value, Var)):-  var(Value),var(Var), !.
f2q_EvalTerm(_HeadIs, _CType, _Caller, _Nth, _RetType, Var, Value, true):-  var(Value),var(Var), Value=Var,!.
f2q_EvalCode(_HeadIs, _CType, _Caller, _Nth,  RetType, Var, Value, eval_code(RetType, Value, Var)):-  var(Value),var(Var), !.


f2q(_HeadIs, _CType, _Caller, _Nth, RetType, Var, Value,  eval_term(RetType, Value, Var)):-  var(Value),var(Var),!.


/*
f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Value=Var, RetType)):- var(Value), !.
%into_equality_fct(_OpN, _Nth, RetType, Var, Value, RetType=Value):- is_value_obj(Value), !, add_type_to(RetType, 'PlainObj').
f2q(HeadIs, CType, Caller, Nth, RetType, Var, Value, 'S'(Value, RetType)):- is_value_obj(Value), Var=Value, !, add_type_to(RetType, val(Value)).
%into_equality_fct(OpN, Nth, RetType, Var, Convert, Code):- Var=@='$VAR'('_'), !,
%   into_equality_fct(OpN, Nth, _, Convert, Code), !.

f2q(_HeadIs, CType, Caller, Nth, _RetType, RetResult, Convert, true) :-
     number(Convert), !, RetResult = Convert. % Check if Convert is a number

f2q(_HeadIs, CType, Caller, Nth, _RetType, RetResult, Convert, true) :-
     \+ callable(Convert), RetResult = Convert, !. % Check if Convert is uncallable

f2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, eval_for2(RetType, Convert, RetResult)):-
   interpet_this(Convert), !.
*/

%Rf2q(_HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, tvv_of_value(RetType, Convert, RetResult)) :- is_ftVar(Convert), !.% Check if Convert is a variable
     %into_equals(NN, RetResult, Convert, RetResultConverted).
% Converted = u_assign(NN, Convert, RetResult).  % Set Converted to u_assign(NN, Convert, RetResult)


pre_specialize_code(Specialize, Args, SArgs, SpecialCode):- Specialize==[], !, Args=SArgs, SpecialCode=true.
pre_specialize_code(Specialize, Args, SArgs, SpecialCode):-
    SpecialCode = specialize_args(Specialize, Args, SArgs).

protect(GG):-compound_name_arguments(GG, Op, [P, L]),
  copy_term(P, PC, _), copy_term_nat(PC, C),
  call(Op, C, L), C=@=PC, C=P, !.


f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Code):-
   Convert =~ ['intersection-atom',Arg1,Arg2],
   Code = intersection(Arg1,Arg2,RetResult).

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Code):-
   Convert =~ ['superpose-atom',Arg],
   Code = member(RetResult,Arg).


f2q(_HeadIs, _CType, _Caller, _Nth, RetType, RetResult, Convert, Eval):-
   protect(Convert =~ [['py-atom'|SymSpecialize]|Args]),
   listify(SymSpecialize, [Sym|Specialize]),
   pre_specialize_code(Specialize, Args, SArgs, SpecialCode),
   combine_code(SpecialCode, py_call_method_and_args_sig(RetType, Specialize, Sym, SArgs, RetResult), Eval), !.

f2q(_HeadIs, _CType, _Caller, _Nth, RetType, RetResult, Convert, Eval):-
   protect(Convert =~ [['py-dot', Arg1, Arg2|Specialize]|Args]),
   pre_specialize_code(Specialize, Args, SArgs, SpecialCode),
   combine_code((SpecialCode, make_py_dot(Arg1, Arg2, Specialize, SymRes), py_call_method_and_args_sig(RetType, Specialize, SymRes, SArgs, RetResult)), Eval), !.


symbol_impl(_, 'println!', 1, _Args, _ParamL, _ConvertedL, 'println_impl', _, _).
%symbol_impl(Self, Symbol, mi(Symbol)):- metta_type(Self, Symbol, [Arr|_]), Arr=='->', !.

%symbol_impl(_Self, Symbol, Len, Args, ParamL, _ConvertedL, ml(Symbol,Nth1,Type), ParamTypes, _):- nth1(Nth1,Args,List), is_list(List), nth1(Nth1,ParamL,Param), List==Param,!,nth1(Nth1,ParamTypes,Type).

symbol_impl(Self, Symbol, Len, _Args, _ParamL, _ConvertedL, mi(Symbol), _ParamTypes, RetType):- from_pfc_true(returnType(Self, Symbol, Len, RetType)).
symbol_impl(Self, Symbol, Len, _Args, _ParamL, _ConvertedL, me(Symbol), _ParamTypes, _):- \+ \+ (metta_defn(Self, [Symbol|DeclArgs], _), length(DeclArgs, DeclLen), DeclLen==Len).
symbol_impl(_, Symbol, _Len, _Args, _ParamL, _ConvertedL, 's'(Symbol), _ParamTypes, _RetType).

ensure_code(EC,B,A):- list(EC),EC=[Ar|_],Ar=='->', !, B=A,!.
ensure_code(_,B,A):-!,B=A.

f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, SrcValue, Converted):-
      (is_non_eval_kind(RetType)/*;is_non_eval_kind(CType)*/),
      is_code_kind(RetType),
      %is_evaled(SrcValue),
      Converted = (RetResult = as_p1(CType, RetType, NewRetType, NewValueR, SrcValue, Code)),
      cname_var('RetTypeNotCd', NewRetType),
      %NewRetType = RetType,
      cname_var('P1_CODE_', RetResult),
      cname_var('P1_CODE_RESULT_', NewValueR),
      f2p(HeadIs, cr(RetType,CType), Caller, Nth, NewRetType, NewValueR, SrcValue, Code), !.

% check if this is a flow control operation
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
  compound(Convert), \+ compound_name_arity(Convert, _, 0),
  Convert =~ [Symbol|_Args],
  compile_flow_control(HeadIs, CType, [Symbol|Caller], Nth, RetType, RetResult, Convert, Converted), !.

f2q(HeadIs, CType, Caller, Nth, RetType, Var, List,  Code):- f2q_preempt(HeadIs, CType, Caller, Nth, RetType, Var, List,  Code),!.
/*
f2q(HeadIs, CType, Caller, Nth, RetTypeO, RetResult, Convert, Converted) :-
  Convert =~ [Symbol|Args], callable(Symbol), \+ is_conz(Symbol), compiler_self(Self),
   length(Args, Len),
   (((get_operator_typedef_cmp(Self, Symbol, Len, ParamTypes, RetType),
     RetTypeO = RetType))*->true;get_operator_typedef_cmp(Self, Symbol, Len, ParamTypes, RetType)),
     combine_types(RetType, RetTypeO, RetTypeC),
     f2p_assign_args(RetTypeO, RetTypeC, Symbol, HeadIs, ParamTypes, ParamL, Args, ConvertedL), !,
     must_det_lls((
      symbol_impl(Self, Symbol, Len, Args, PL, ParamTypes, RetType),
      list_to_conjuncts(ConvertedL, Conjs),
      %maplist_p(not(is_list), ParamL),
      append(ParamL, [RetResult], ArgsWRes),
      append_termlist(PL, ArgsWRes, Code),
      combine_code(Conjs, note(f2p_assign_args, Code), Converted))), !.
*/
f2q_preempt(HeadIs, CType, Caller, Nth, RetTypeO, RetResult, Convert, Converted) :-
  Convert =~ [Symbol|Args], callable(Symbol), \+ is_conz(Symbol), compiler_self(Self),
   length(Args, Len),
   (((get_operator_typedef_cmp(Self, Symbol, Len, ParamTypes, RetType),
     RetTypeO = RetType))*->true;get_operator_typedef_cmp(Self, Symbol, Len, ParamTypes, RetType)),
     combine_types(RetType, RetTypeO, RetTypeC),
     dppt(f2q_preempt(combine_types(RetType, RetTypeO, RetTypeC),fa(Symbol/Len),args(Args/ParamTypes -> RetType),CType, Caller, Nth, RetResult, Convert, Converted)),
     maplist_nth(must(f2q(HeadIs, CType, [Symbol|Caller])), 1, ParamTypes, ParamL, Args, ConvertedL),
     must_det_lls((
      symbol_impl(Self, Symbol, Len, Args, ParamL, ConvertedL, PL, ParamTypes, RetType),
      is_list(ConvertedL))),
      list_to_conjuncts(ConvertedL, Conjs),
      %maplist_p(not(is_list), ParamL),
      must_det_lls(( append(ParamL, [RetResult], ArgsWRes),
                     append_termlist(PL, ArgsWRes, Code),
                     combine_code(Conjs, Code, Converted))), !.



get_first_p1(_, Cmpd, _):- \+ compound(Cmpd), !, fail.
get_first_p1(E, Cmpd, set_nth1(N1, Cmpd)):- is_list(Cmpd), nth1(N1, Cmpd, E).
get_first_p1(E, Cmpd, Result)           :- is_list(Cmpd), !, member(Ele, Cmpd), get_first_p1(E, Ele, Result).
get_first_p1(_, Cmpd, _)                :- is_conz(Cmpd), !, fail.
get_first_p1(E, Cmpd, set_arg(N1, Cmpd)) :- arg(N1, Cmpd, E).
get_first_p1(E, Cmpd, Result)           :- arg(_, Cmpd, Ele), !, get_first_p1(E, Ele, Result).

non_simple_arg(E):- compound(E), !, \+ is_ftVar(E).


f2q_11(HeadIs, CType, Caller, Nth, RetType, RetResult, Converting, (PreArgs, Converted)):- fail,
     as_functor_args(Converting, F, A, Args),
        \+ \+ (member(E, Args), non_simple_arg(E)),
          cname_var('Self', Self),
          %Self = '$VAR'('RetType'),
          maplist(type_fit_childs('=', Self), _RetTypes1, ArgsCode, Args, NewVarL),
            list_to_conjunction(ArgsCode, PreArgs),
            nop(non_compat_io(color_g_mesg('magenta',
              ((write_src(type_fit_childs('=', F, _RetTypes2, PreArgs, Args, NewVarL)), nl))))),
        as_functor_args(Convert, F, A, NewVarL),
        \+ (member(E, NewVarL), non_simple_arg(E)), !,
        f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted).



%   f2q(_HeadIs, CType, Caller, Nth, _RetType, _RetVal, Convert, Convert) :- compound(Convert), (Convert= (_, _)), !.

f2q_12(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail,
 must_det_ll((
  as_functor_args(Convert, F, A, Args),
  as_functor_args(Quot, quot, A, NewVarL),
  as_functor_args(QConvert, quot, A, Args))),
  get_inline_case_list([F|NewVarL], Quot, DefList), !,
  must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, case(QConvert, DefList), Converted))).

is_non_evaluatable(S):- \+ compound(S), !.
is_non_evaluatable(S):- is_ftVar(S), !.
is_non_evaluatable([H|_]):- \+ symbol(H), \+ is_non_evaluatable(H).

f2q(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted) :- fail, is_non_evaluatable(Convert),
   Converted = (Convert=RetResult), !.

f2q(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
   Convert =~ 'bind!'(Var, Value), is_ftVar(Value), !,
   Converted = u_assign(8, 'bind!'(Var, Value), RetResult).

f2q(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
    Convert =~ 'bind!'(Var, Value), Value =~ 'new-space'(), !,
    Converted = u_assign(9, 'bind!'(Var, Value), RetResult).

f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
    Convert =~ 'bind!'(Var, Value), !,
    f2p(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, ValueCode),
    Eval = u_assign(15, 'bind!'(Var, ValueResult), RetResult),
   combine_code(ValueCode, Eval, Converted).

f2q(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted) :-
     Convert =~ 'add-atom'(Where, What), !,
     =(What, WhatP),
     Converted = as_tf('add-atom'(Where, WhatP), RetResult).

f2q(_HeadIs, _CType,_Caller,_Nth, _RetType, RetResult, Convert, Converted) :-
     Convert =~ 'add-atom'(Where, What, RetResult), !,
     =(What, WhatP),
     Converted = as_tf('add-atom'(Where, WhatP), RetResult), !.


f2q_13(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, CodeForValueConverted) :-
    Convert =~ [Plus, N, Value], symbol(Plus), current_predicate(Plus/3), number(N),
    \+ number(Value), \+ is_nsVar(Value), !,
    f2p(HeadIs, CType, Caller, Nth, RetType, ValueResult, Value, CodeForValue), !,
    must_det_llu(Converted =.. [Plus, N, ValueResult, RetResult]),
    combine_code(CodeForValue, Converted, CodeForValueConverted).

f2q(HeadIs, CType, _Caller, _Nth, RetType, AtomsVar, Convert, Converted) :- \+ is_non_eval_kind(RetType), \+ is_non_eval_kind(CType),
    Convert=~ 'get-atoms'(Space), Pattern = AtomsVar,
    compile_pattern(HeadIs, Space, Pattern, Converted), !.

metta_atom_iter(Space, Match):-
  metta_atom_iter('=', 10, Space, Space, Match).

make_with_space(Space, MatchCode, MatchCode):- Space=='&self', !.
make_with_space(Space, MatchCode, with_space(Space, MatchCode)):- Space\=='&self'.

% If Convert is a Value, and RetResult is a Variable bind them together and mark the compiler used them
f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, _RetResult, Convert , f2q_info(A =~ B)) :- compound(Convert), Convert = (A =~ B),!.


% If Convert is an "u_assign" function, we convert it to the equivalent "is" predicate.
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, EvalConvert, Converted):-
 EvalConvert =~ eval(Convert), !,
   must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted))).


f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted):-
    compound(Convert), Convert = u_assign(NN, C, Var), compound_non_cons(C), into_list_args(C, CC), !,
    f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, u_assign(NN, CC, Var), Converted).

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, _RetResult, Convert, Converted):-
    compound(Convert),
    Convert = u_assign(16, C, _Var),
    is_list(C), Converted = Convert, !.


f2q_17(HeadIs, _CType, _Caller, _Nth, RetType, RetResult, Convert, Converted) :-
     symbol(Convert), functional_predicate_arg(Convert, Nth, Nth2),
      Nth==1, Nth2==1,
      HeadIs, RetType\=@=Convert,
      Convert = F, !,
      must_det_ll((
        do_predicate_function_canonical(FP, F),
        compound_name_list(Converted, FP, [RetResult]))).

% If Convert is an "is" function, we convert it to the equivalent "is" predicate.
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Converting, (Converted, is(RetResult, Result))):- compound(Converting), is(Convert)=Converting, !,
   must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, Result, Convert, Converted))).


f2q_18(_HeadIs, _CType,_Caller,_Nth, _RetType, _RetResult, N=V, Code) :- !, into_equals(eq, N, V, Code).

f2q(_HeadIs, _CType,_Caller,_Nth, _RetType, RetVar, Data, '=~'(Data, RetVar)) :- fail,
    as_functor_args(Data, F, A, _),
    is_data_functor(F, A), !.

into_equals(NN, Eval, Result, Code):-
  into_u_assign(NN, Eval, Result, Code).

%into_u_assign(NN, Eval, Result, true):- is_nsVar(Eval), is_nsVar(Result), Eval=Result, !.
into_u_assign(NN, Eval, Result, Code):- var(Eval), \+ var(Result), !, into_u_assign(NN, Result, Eval, Code).
into_u_assign(NN, Eval, Result, Code):- is_nsVar(Eval), is_nsVar(Result), !, Code = e_assign(NN, Result, Eval).
%into_u_assign(NN, Eval, Result, Code):- Result=='True', r2p(Eval, _Result, Code), !.
%into_u_assign(NN, Eval, Result, Code):- ar2p(Eval, Result, Code), !.
into_u_assign(NN, Eval, Result, Code):- \+ compound(Eval), !, Code = u_assign(NN, Eval, Result).
into_u_assign(NN, Eval, Result, Code):- % mctrace(compiler),
   Code = u_assign(NN, Eval, Result).


not_funcall(A):- \+ is_list(A), !.
not_funcall([A|_]):- \+ callable(A), !.

f2q(_HeadIs, _CType, _Caller, _Nth, RetType, RetResult, ConvertL, Converted) :- var(RetResult), is_list(ConvertL),
 (RetType=='Atom';RetType=='Expression'), !,
   Converted= true, RetResult= ConvertL, !.

/*
f2q(_HeadIs, CType, Caller, Nth, _RetType, RetResult, ConvertL, Converted) :- % dif_functors(HeadIs, Convert),
    is_list(ConvertL)
    Converted = true,
    Convert=RetResult, !.
*/
%f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, RetResultConverted):-
%  compile_flow_control(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, RetResultConverted), !.

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
    Convert =~ [H|_], \+ symbol(H), \+ is_non_evaluatable(H),
    Converted = true,
    Convert=RetResult, !.

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Converted) :- % dif_functors(HeadIs, Convert),
    Convert =~ [H|_], \+ symbol(H), \+ is_non_evaluatable(H),
    mctrace(compilerF2Q),
    Converted = true,
    Convert=RetResult, !.



% argtypes of the symbol are known
f2q_20(HeadIs,_CType, _Caller, _Nth, RetType, RetResult, Convert, Converted) :- is_list(Convert), %not_funcall(RetResult),
  Convert = [Symbol|Args], symbol(Symbol),
  compiler_self(Self),
  length(Args, Len), get_operator_typedef_cmp(Self, Symbol, Len, ParamTypes, RType), !,
     must_det_lls(( combine_types(RType, RetType, RRType),
      f2p_assign_args(RType, RRType, Symbol, HeadIs, ParamTypes, ParamL, Args, ConvertedL),
      list_to_conjuncts(ConvertedL, Conjs),
      append(ParamL, [RetResult], ArgsWRes),
      must_det_lls(Code =.. [mi, Symbol|ArgsWRes]),
      combine_code(Conjs, Code, Converted))).

compiler_self(Self):- current_self(Self).
compiler_self('&corelib'):- current_self(Self), '&corelib' \== Self.
compiler_self('&top'):- current_self(Self), '&top' \== Self.


include_varnames(N=Var):- atomic(N), put_attr(Var, vn, N).
include_varnames(N=Var):- compound(N), arg(N, 1, Name), !, include_varnames(Name=Var).
include_varnames(_).




buffer_file_src(Filename, Expr):- metta_file_buffer(0, _Ord, _Kind, Expr, NamedVarsList, Filename, _LineCount),
  maplist(include_varnames, NamedVarsList).


buffer_src_any(Expr):-  metta_file_buffer(_, _Ord, _Kind, Expr, _NamedVarsList, _Filename, _LineCount).

buffer_src(Atom):- woct(metta_atom(Atom)).
buffer_src(Expr):- woct(metta_file_buffer(0, _Ord, _Kind, Expr, _NamedVarsList, _Filename, _LineCount)).

buffer_src_isa(I, T):- woct(metta_type(_KB, I, T)).
buffer_src_isa(I, T):- ((buffer_src([Colon, Op, T]), Op == I, (Colon == ':'; Colon == 'iz'))).

buffer_src_isa_u(I, T):- buffer_src([Colon, Op, T]), Op = I, (Colon == ':'; Colon == 'iz').

get_operator_typedef_cmp_nd(Self, Symbol, Len, ParamTypes, RetType):- get_operator_typedef1(Self, Symbol, Len, ParamTypes, RetType), !.
get_operator_typedef_cmp_nd(_Self, Symbol, Len, ParamTypes, RetType):-
  quietly((buffer_src_isa(Symbol, ArTypeDecl), arrow_type(ArTypeDecl, ParamTypes, RetType),
  length(ParamTypes, Len))).

get_operator_typedef_cmp_nd(_Self, Symbol, Len, ParamTypes, RetType):- quietly((buffer_src([Colon, [Op | Info], RetType]),
   Colon == ':', Symbol==Op, length(Info, Len), all_atom(Len, ParamTypes, _))), !.

get_operator_typedef_cmp_nd(_Self, Symbol, Len, ParamTypes, RetType):- quietly((buffer_src_isa(Symbol, T), nonvar(T), all_atom_type(T))), !, all_atom(Len, ParamTypes, RetType).
get_operator_typedef_cmp_nd(_Self, Symbol, Len, ParamTypes, RetType):- quietly((buffer_src_isa(Symbol, T), nonvar(T), all_eval_arg_type(T))), !, all_eval_args(Len, ParamTypes, RetType).
get_operator_typedef_cmp_nd(_Self, _Symbol, Len, ParamTypes, 'Any'):- Len ==0, !, all_atom(Len, ParamTypes, _).
get_operator_typedef_cmp_nd(_Self, _Symbol, Len, ParamTypes, RetType):- all_eval_args(Len, ParamTypes, RetType).



get_operator_typedef_cmp(Self, F, Len, ParamTypes, RetType):-
  get_operator_typedef_cmp_nd(Self, F, Len, ParamTypes, RetType),
   maplist_nth(maybe_remember_types(F),0,[RetType|ParamTypes]).

maybe_remember_types(_F,_N,Type):- nonvar(Type),!.
maybe_remember_types( F, N,Type):- add_type_to(Type,type(F,N)),!,ignore(cname_var_unnamed(['Type',F,N,'__'],Type)),!.



all_atom(Len, ParamTypes, RetType):- (integer(Len);is_list(ParamTypes)), !, length(ParamTypes, Len), maplist('='('Atom'), ParamTypes), RetType='Atom'.
all_eval_args(Len, ParamTypes, RetType):- (integer(Len);is_list(ParamTypes)), !, length(ParamTypes, Len), maplist('='('%Undefined%'), ParamTypes), RetType='%Undefined%'.

all_atom_type('MinimalMeTTaHelper').
all_atom_type('MeTTaLog').
all_atom_type('DataFunctor').
all_atom_type('EvalNoArgs').
all_atom_type('NoEvalArgs').
all_eval_arg_type('EvalArgs').

combine_2types( X, Undefined, X):- Undefined == '%Undefined%',!.
combine_2types( X, Atom,      X):-      Atom == 'Atom',!.
combine_2types( X, CodeAtom,  X):-  CodeAtom == 'CodeAtom',!.


combine_2types_r(A, B, C):- combine_2types(A, B, C).
combine_2types_r(A, B, C):- combine_2types(B, A, C).
combine_types(A, B, C):- A==B, !, C=A.
combine_types(A, B, C):- nonvar(A), nonvar(B), combine_2types_r(A, B, C), !.
combine_types(A, B, C):- narrow_types(A, B, C), !.

f2q_21(HeadIs, CType, _Caller, _Nth, RetType, RetResult, Convert, Converted) :- is_list(Convert), %not_funcall(RetResult),
  Convert = [Symbol|Args], symbol(Symbol),
  compiler_self(Self),
  length(Args, Len), trace, get_operator_typedef_cmp(Self, Symbol, Len, ParamTypes, RType), !,
     must_det_lls(( combine_types(RType, RetType, RRetType),
      f2p_assign_args(CType, RRetType, Symbol, HeadIs, ParamTypes, ParamL, Args, ConvertedL),
      list_to_conjuncts(ConvertedL, Conjs),
      %maplist(not_list, ParamL),
      append(ParamL, [RetResult], ArgsWRes),
      must_det_lls(Code =.. [mee, Symbol|ArgsWRes]),
      combine_code(Conjs, Code, Converted))).

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Converted) :- is_list(Convert), %not_funcall(RetResult),
  Convert = [Symbol|Args], symbol(Symbol), maplist(not_funcall, Args),
  append(Args, [RetResult], ArgsWRes),
  must_det_lls(Converted =.. [mie, Symbol|ArgsWRes]), !.

% argtypes of the symbol are unknown
f2q(HeadIs, CType, _Caller, _Nth, RetType, RetResult, Convert, Converted) :- is_list(Convert), %not_funcall(RetResult),
  Convert = [Symbol|Args], symbol(Symbol),
  compiler_self(Self),
  trace, length(Args, Len), get_operator_typedef1(Self, Symbol, Len, ParamTypes, RType), !,
     must_det_lls(( combine_types(RType, RetType, RRetType),
      f2p_assign_args(CType, RRetType, Symbol, HeadIs, ParamTypes, ParamL, Args, ConvertedL),
      list_to_conjuncts(ConvertedL, Conjs),
      append(ParamL, [RetResult], ArgsWRes),
      must_det_lls(Code =.. [mei, Symbol|ArgsWRes]),
      combine_code(Conjs, Code, Converted))).


% If Convert is a list, we convert it to its termified form and then proceed with the functs_to_preds conversion.
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail, is_list(Convert),
   once((sexpr_s2p(Convert, IS), \+ IS=@=Convert)), !, % Check if Convert is a list and not in predicate form
   must_det_ll((f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, IS, Converted))).  % Proceed with the conversion of the predicate form of the list.


% list not headed by s symbol
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, ConvertL, Converted) :- is_list(ConvertL), %mctrace(compiler),
   not_funcall(ConvertL), nonvar(RetType), is_pro_eval_kind(RetType), is_nonspecific_type(RetType),
   ConvertL=[First|_Args],
   \+ not_funcall(First),
   maplist(f2p(HeadIs, CType, Caller, Nth, RetType), RetResultL, ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL, Conjs),
   into_equals(13, RetResultL, RetResult, Code),
   combine_code(Conjs, Code, Converted).

f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, ConvertL, Converted) :- is_list(ConvertL), %mctrace(compiler),
   not_funcall(ConvertL), nonvar(RetType), is_pro_eval_kind(RetType), is_nonspecific_type(RetType),
   ConvertL=[First|_Args],
   not_funcall(First),
   maplist(f2p(HeadIs, CType, Caller, Nth, RetType), RetResultL, ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL, Conjs),
   into_equals(14, RetResultL, RetResult, Code),
   combine_code(Conjs, Code, Converted).



/*
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, ConvertL, (ConvertedL, ParamL=RetResult)) :- is_list(ConvertL),
  RetResult == '%Undefined%', length(ConvertL, Len), length(ParamTypes, Len), maplist(=(RetResult), ParamTypes),
  f2p_assign_args(CType, RetType, list, HeadIs, ParamTypes, ParamL, ConvertL, ConvertedL).
*/
f2q(_HeadIs, CType, _Caller, _Nth, RetType, RetResult, ConvertL, Converted) :- is_list(ConvertL),
   Converted= eval_for1(CType, RetType, ConvertL, RetResult), !.

eval_for1(_CType, _RetType, ConvertL, RetResult):-
   eval(ConvertL, RetResult).


f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-
   ppt(unhandled(HeadIs, ctype(CType), caller(Caller), Nth, ret_type(RetType), ret_result(RetResult), convert(Convert), Converted)),
   trace, %trace
   fail.


/* MAYBE USE ?
% If Convert is a compound term, we need to recursively convert its arguments.
f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail,
    compound(Convert), !,
    Convert =~ [Functor|Args], % Deconstruct Convert to functor and arguments
    maplist(convert_argument, Args, ConvertedArgs), % Recursively convert each argument
    Converted =~ [Functor|ConvertedArgs], % Reconstruct Converted with the converted arguments
    (callable(Converted) -> f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, Converted, _); true).  % If Converted is callable, proceed with its conversion
% Helper predicate to convert an argument of a compound term
convert_argument(Arg, ConvertedArg) :-
    (callable(Arg) -> ftp(_, _, Arg, ConvertedArg); ConvertedArg = Arg).
*/

%f2q(_HeadIs, CType, Caller, Nth, _RetType, ResultVar, 'cdr-atom'(Atom), 'cdr-atom'(Atom, ResultVar)) :- !.
%f2q(_HeadIs, CType, Caller, Nth, _RetType, ResultVar, 'car-atom'(Atom), 'car-atom'(Atom, ResultVar)) :- !.
% convert Funtion
% f2q(HeadIs, CType, Caller, Nth, RetType, ResultVar, Convert, Converted) :- ar2p(Convert, ResultVar, Converted).


/*
f2q(_HeadIs, CType, Caller, Nth, _RetType, RetResult, AsPred, Converted):-
   compound(AsPred),
   as_functor_args(AsPred, F, A, Args),
   no_lists(Args),
   always_predicate_in_src(F, A),
   was_predicate(AsPred, RetResult, Converted).

f2q(_HeadIs, CType, Caller, Nth, _RetType, RetResult, AsPred, Converted):-
   compound(AsPred),
   as_functor_args(AsPred, F, A, Args),
   no_lists(Args),
   always_function_in_src(F, A),
   was_predicate(AsPred, RetResult, Converted).
*/

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, _RetResult, u_assign(NN, Convert, Res), e(assign(NN), Convert, Res)):-!.

% The catch-all If no specific case is matched, consider Convert as already converted.

f2q(_HeadIs, _CType, _Caller, _Nth, _RetType, RetResult, Convert, Code):- into_u_assign(999, Convert, RetResult, f(Code)).


data_term(Convert):-
 self_eval(Convert), !,
 (iz_conz(Convert) ;  \+ compound(Convert)).


de_eval(u_assign(_NN, X), X):- compound(X), !.

call1(G):- call(G).
call2(G):- call(G).
call3(G):- call(G).
call4(G):- call(G).
call5(G):- call(G).

trace_break:- mctrace(compilerTB), break.

:- table(u_assign/2).
u_assign(_NN, FList, R):- is_list(FList), !, eval_args(FList, R).
u_assign(NN, FList, R):- var(FList), nonvar(R), !, u_assign(NN, R, FList).
u_assign(_NN, FList, R):- FList=@=R, !, FList=R.
u_assign(_NN, FList, R):- number(FList), var(R), !, R=FList.
u_assign(_NN, FList, R):- self_eval(FList), var(R), !, R=FList.
u_assign(NN, FList, R):- var(FList), !, /*mctrace(compiler), */freeze(FList, u_assign(NN, FList, R)).
u_assign(_NN, FList, R):- \+ compound(FList), var(R), !, R=FList.
u_assign(_NN, [F|List], R):- F == ':-', !, trace_break, as_tf(clause(F, List), R).
u_assign(_NN, FList, RR):- (compound_non_cons(FList), u_assign_c(FList, RR))*->true;FList=~RR.
u_assign(_NN, FList, RR):-
  u_assign_list1(FList, RR)*->true;u_assign_list2(FList, RR).

u_assign_list1([F|List], R):- u_assign(_NN, [F|List], R), nonvar(R), R\=@=[F|List].
u_assign_list2([F|List], R):- symbol(F), append(List, [R], ListR),
  catch(quietly(apply(F, ListR)), error(existence_error(procedure, F/_), _),
     catch(quietly(as_tf(apply(F, List), R)), error(existence_error(procedure, F/_), _),
        quietly(catch(u_assign(333, [F|List], R), _, R=[F|List])))).

%u_assign(NN, [V|VI], [V|VO]):- nonvar(V), is_metta_data_functor(_Eq, V), !, maplist(u_assign, VI, VO).

u_assign_c((F:-List), R):- !, R = (F:-List).
u_assign_c(FList, RR):-
  as_functor_args(FList, F, _),
  (catch(quietlY(call(FList, R)), error(existence_error(procedure, F/_), _),
     catch(quietlY(as_tf(FList, R)), error(existence_error(procedure, F/_), _),
        quietlY((p2m(FList, [F|List]), catch(u_assign(u_c, [F|List], R), _, R=~[F|List])))))), !, R=RR.
u_assign_c(FList, RR):- as_tf(FList, RR), !.
u_assign_c(FList, R):- compound(FList), !, FList=~R.

quietlY(G):- call(G).

call_fr(G, Result, FA):- current_predicate(FA), !, call(G, Result).
call_fr(G, Result, _):- Result=G.

% This predicate is responsible for converting functions to their equivalent predicates.
% It takes a function 'AsFunction' and determines the predicate 'AsPred' which will be
% equivalent to the given function, placing the result of the function at the 'Nth' position
% of the predicate arguments. The 'Result' will be used to store the result of the 'AsFunction'.
%
% It handles cases where 'AsFunction' is a variable and when it's an symbol or a compound term.
% For compound terms, it decomposes them to get the functor and arguments and then reconstructs
% the equivalent predicate with the 'Result' at the 'Nth' position.
%
% Example:
% funct_with_result_is_nth_of_pred(HeadIs, RetType, +(1, 2), Result, 3, +(1, 2, Result)).

into_callable(Pred, AsPred):- is_ftVar(Pred), !, AsPred=holds(Pred).
into_callable(Pred, AsPred):- Pred=AsPred, !.
into_callable(Pred, AsPred):- iz_conz(Pred), !, AsPred=holds(Pred).
into_callable(Pred, AsPred):- Pred=call_fr(_, _, _), !, AsPred=Pred.
into_callable(Pred, AsPred):- Pred =~ Cons, !, AsPred=holds(Cons).


r2p(MeTTa, Result, IsPred):- r2p(_, MeTTa, Result, IsPred).
r2p(What, MeTTa, Result, IsPred):- ar2p(What, MeTTa, Result, IsPred), !.
r2p(What, MeTTa, Result, IsPred):- ar2q(What, MeTTa, Result, IsPred), !.


ar2p(MeTTa, Result, IsPred):- ar2p(_, MeTTa, Result, IsPred).

absorbed_default(_, _, _):-!, fail.
absorbed_default('Bool', _AsPred, 'True').
absorbed_default(_, _AsPred, _).

is_absorbed_return_value(_, _, _):-!, fail.
is_absorbed_return_value(F, A, Result):-
  is_absorbed_return(F, A, Bool),
  absorbed_default(Bool, _AsPred, Result).

is_absorbed_return_value(_, _, _, _):- !, fail.
is_absorbed_return_value(F, A, Bool, Result):-
  is_absorbed_return(F, A, Bool),
  nonvar(Bool),
  absorbed_default(Bool, _AsPred, Result).

ar2p('Bool', AsPred, Result, IsPred):- fail,
    as_functor_args(AsPred, F, A, Args),
    is_absorbed_return(F, A, Bool),
    nonvar(Bool), absorbed_default(Bool, AsPred, Result),
    must_det_llu(IsPred=..[F|Args]).

ar2p(pred, AsPred, Result, IsPred):- fail,
    as_functor_args(AsPred, F, A, Args),
    is_absorbed_return(F, A, Bool),
    nonvar(Bool), absorbed_default(Bool, AsPred, Result),
    must_det_llu(IsPred=..[F|Args]).

ar2p(W, Data, Result, IsPred):- fail,
    W\== 'Bool',
    as_functor_args(Data, F, A, _Args),
    is_data_functor(F, AA), !,
    (AA==A
       -> (IsPred = u_assign(_NN, Data, Result))
       ; was_predicate(Data, Result, IsPred)).
ar2p(pred, AsPred, Result, IsPred):-
    as_functor_args(AsPred, F, A, Args),
    always_predicate_in_src(F, A), !,
    once(functional_predicate_arg(F, A, Nth);Nth=A),
    \+ is_absorbed_return(F, _, _Bool),
    nth1(Nth, Args, Result),
    must_det_llu(IsPred=..[F|Args]).
ar2p(func, AsFunction, Result, IsPred):-
   as_functor_args(AsFunction, F, A0, FArgs),
   \+ is_absorbed_return(F, A0, _Bool),
   always_function_in_src(F, A0), !, A is A0 + 1,
   once(functional_predicate_arg(F, A, Nth);Nth=A),
   nth1(Nth, Args, Result, FArgs),
   must_det_lls(IsPred=..[F|Args]).

ar2q(MeTTa, Result, IsPred):- ar2q(_, MeTTa, Result, IsPred).
ar2q(pred, AsPred, Result, IsPred):-
  as_functor_args(AsPred, F, A, Args),
  once(functional_predicate_arg(F, A, Nth);Nth=A),
  nth1(Nth, Args, Result),
  \+ is_absorbed_return(F, _, _Bool),
  must_det_llu(IsPred=..[F|Args]).
ar2q(funct, AsFunction, Result, IsPred):-
  as_functor_args(AsFunction, F, A0, FArgs), A is A0 + 1,
  \+ is_absorbed_return(F, _, _Bool),
  once(functional_predicate_arg(F, A, Nth);Nth=A),
  nth1(Nth, Args, Result, FArgs),
  must_det_llu(IsPred=..[F|Args]).
ar2q(pred, AsPred, Result, IsPred):-
    as_functor_args(AsPred, F, A, Args),
    is_absorbed_return(F, A, Bool),
    nonvar(Bool), absorbed_default(Bool, AsPred, Result),
    must_det_llu(IsPred=..[F|Args]).

was_predicate(AsPred, Result, IsPred):-
    as_functor_args(AsPred, F, A, Args),
    is_absorbed_return(F, A, Bool),
    nonvar(Bool), absorbed_default(Bool, AsPred, Result), !,
    must_det_llu(IsPred=..[F|Args]), !.

was_predicate(AsPred, Result, IsPred):-
    as_functor_args(AsPred, F, A, Args),
    once(functional_predicate_arg(F, A, Nth);Nth=A),
    \+ is_non_absorbed_return(F, A, _Bool),
    nth1(Nth, Args, Result),
    must_det_llu(IsPred=..[F|Args]).

was_function(AsFunction, Result, IsPred):-
    as_functor_args(AsFunction, F, A0, FArgs),
    ( ( \+ is_absorbed_return(F, A0, _)) ; is_non_absorbed_return(F, A0, _)),
    A is A0 + 1,
  once(functional_predicate_arg(F, A, Nth);Nth=A),
  nth1(Nth, Args, Result, FArgs),
  must_det_llu(IsPred=..[F|Args]).


funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, Nth, AsPred):-
  var(AsPred), !,
  funct_with_result_is_nth_of_pred0(HeadIs, RetType, AsFunction, Result, Nth, Pred),
  into_callable(Pred, AsPred).

funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, Nth, AsPred):-
  var(AsFunction), !,
  funct_with_result_is_nth_of_pred0(HeadIs, RetType, Function, Result, Nth, AsPred),
  into_callable(Function, AsFunction).

funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, Nth, AsPred):-
  funct_with_result_is_nth_of_pred0(HeadIs, RetType, AsFunction, Result, Nth, AsPred).

% Handles the case where AsFunction is a variable.
% It creates a compound term 'AsPred' and places the 'Result' at the 'Nth' position
% of the predicate arguments, and the 'AsFunction' represents the functional form with
% arguments excluding the result.
funct_with_result_is_nth_of_pred0(_HeadIs, _RetType, AsFunction, Result, Nth, AsPred) :-
    is_nsVar(AsFunction), !,
   compound(AsPred),
    compound_name_list(AsPred, FP, PredArgs),
    nth1(Nth, PredArgs, Result, FuncArgs),
    do_predicate_function_canonical(FP, F),
    AsFunction =~ [F, FuncArgs].

% Handles the case where 'AsFunction' is not a variable.
% It decomposes 'AsFunction' to get the functor and arguments (FuncArgs) of the function
% and then it constructs the equivalent predicate 'AsPred' with 'Result' at the 'Nth'
% position of the predicate arguments.
funct_with_result_is_nth_of_pred0(HeadIs, RetType, AsFunctionO, Result, Nth, (AsPred)) :-
   de_eval(AsFunctionO, AsFunction), !, funct_with_result_is_nth_of_pred0(HeadIs, RetType, AsFunction, Result, Nth, AsPred).

funct_with_result_is_nth_of_pred0(HeadIs, RetType, AsFunction, Result, _Nth, AsPred) :-
   nonvar(AsFunction),
   compound(AsFunction),
   \+ is_arity_0(AsFunction, _),
   as_functor_args(AsFunction, F, A),
   RetType\=@=AsFunction,
   \+ (compound(HeadIs, RetType), (is_arity_0(HeadIs, HF);as_functor_args(HeadIs, HF, _))-> HF==F),
   (into_u_assign(345, AsFunction, Result, AsPred)
       -> true
       ; (AA is A+1,
           (FAA=(F/AA)),
           \+ current_predicate(FAA), !,
           AsPred = call_fr(AsFunction, Result, FAA))).


funct_with_result_is_nth_of_pred0(_HeadIs, _RetType, AsFunction, Result, Nth, (AsPred)) :-
   (symbol(AsFunction)->AsFunction =~ [F | FuncArgs]; compound_name_list(AsFunction, F, FuncArgs)),
   ignore(var(Nth) -> is_function(AsFunction, Nth); true),
    nth1(Nth, PredArgs, Result, FuncArgs), % It places 'Result' at the 'Nth' position
    AA is Nth+1, \+ current_predicate(F/AA),
    do_predicate_function_canonical(FP, F),
    AsPred =~ [FP | PredArgs]. % It forms the predicate 'AsPred' by joining the functor with the modified arguments list.



funct_with_result_is_nth_of_pred0(_HeadIs, _RetType, AsFunction, Result, Nth, (AsPred)) :-
    nonvar(AsFunction),
    AsFunction =~ [F | FuncArgs],
    do_predicate_function_canonical(FP, F),
    length(FuncArgs, Len),
   ignore(var(Nth) -> is_function(AsFunction, Nth); true),
   ((number(Nth), Nth > Len + 1) -> (mctrace(compiler7), throw(error(index_out_of_bounds, _))); true),
   (var(Nth)->(between(1, Len, From1), Nth is Len-From1+1);true),
    nth1(Nth, PredArgs, Result, FuncArgs),
    AsPred =~ [FP | PredArgs].

% optionally remove next line
funct_with_result_is_nth_of_pred0(_HeadIs, _RetType, AsFunction, _, _, _) :-
    var(AsFunction),
    throw(error(instantiation_error, _)).

% The remove_funct_arg/3 predicate is a utility predicate that removes
% the Nth argument from a predicate term, effectively converting a
% predicate to a function. The first argument is the input predicate term,
% the second is the position of the argument to be removed, and the third
% is the output function term.
remove_funct_arg(AsPred, Nth, AsFunction) :-
    % Decompose AsPred into its functor and arguments.
    AsPred =~ [F | PredArgs],
    % Remove the Nth element from PredArgs, getting the list FuncArgs.
    nth1(Nth, PredArgs, _Result, FuncArgs),
    % Construct AsFunction using the functor and the list FuncArgs.
    do_predicate_function_canonical(F, FF),
    compound_name_list(AsFunction, FF, FuncArgs).

% deep_lhs_sub_sterm/2 predicate traverses through a given Term
% and finds a sub-term within it. The sub-term is unifiable with ST.
% This is a helper predicate used in conjunction with others to inspect
% and transform terms.

deep_lhs_sub_sterm(ST, Term):- deep_lhs_sub_sterm0(ST, Term), ST\=@=Term.
deep_lhs_sub_sterm0(_, Term):- never_subterm(Term), !, fail.
deep_lhs_sub_sterm0(ST, Term):- Term =~ if(Cond, _Then, _Else), !, deep_lhs_sub_sterm0(ST, Cond).
deep_lhs_sub_sterm0(ST, Term):- Term =~ 'if-error'(Cond, _Then, _Else), !, deep_lhs_sub_sterm0(ST, Cond).
deep_lhs_sub_sterm0(ST, Term):- Term =~ 'if-decons'(Cond, _Then, _Else), !, deep_lhs_sub_sterm0(ST, Cond).
deep_lhs_sub_sterm0(ST, Term):- Term =~ 'chain'(Expr, _Var, _Next), !, deep_lhs_sub_sterm0(ST, Expr).
deep_lhs_sub_sterm0(ST, Term):-
    % If Term is a list, it reverses the list and searches for a member
    % in the reversed list that is unifiable with ST.
    is_list(Term), !, member(E, Term), deep_lhs_sub_sterm0(ST, E).
deep_lhs_sub_sterm0(ST, Term):-
    % If Term is a compound term, it gets its arguments and then recursively
    % searches in those arguments for a sub-term unifiable with ST.
    compound(Term), compound_name_list(Term, _, Args), deep_lhs_sub_sterm0(ST, Args).
deep_lhs_sub_sterm0(ST, ST):-
    % If ST is non-var, not an empty list, and callable, it unifies
    % ST with Term if it is unifiable.
    nonvar(ST), ST\==[], callable(ST).

never_subterm(Term):- is_ftVar(Term).
never_subterm([]).
never_subterm('Nil').
%never_subterm(F):- symbol(F), not_function(F, 0).

% rev_member/2 predicate is a helper predicate used to find a member
% of a list. It is primarily used within deep_lhs_sub_sterm/2 to
% traverse through lists and find sub-terms. It traverses the list
% from the end to the beginning, reversing the order of traversal.
rev_member(E, [_|L]):- rev_member(E, L).
rev_member(E, [E|_]).

% Continuing from preds_to_functs/2
% Converts a given predicate representation to its equivalent function representation
preds_to_functs(Convert, Converted):-
  % Verbose_unify/1 here may be used for debugging or to display detailed unification information
  verbose_unify(Convert),
  % Calls the auxiliary predicate preds_to_functs0/2 to perform the actual conversion
  preds_to_functs0(Convert, Converted).

% if Convert is a variable, Converted will be the same variable
preds_to_functs0(Convert, Converted) :-
    is_ftVar(Convert), !,
    Converted = Convert.

% Converts the rule (Head :- Body) to its function equivalent
preds_to_functs0((Head:-Body), Converted) :- !,
  % The rule is converted by transforming Head to a function AsFunction and the Body to ConvertedBody
  (
   pred_to_funct(Head, AsFunction, Result),
   cname_var('HEAD_RES', Result),
   conjuncts_to_list(Body, List),
   reverse(List, RevList), append(Left, [BE|Right], RevList),
   compound(BE), arg(Nth, BE, ArgRes), sub_var(Result, ArgRes),
   remove_funct_arg(BE, Nth, AsBodyFunction),
   append(Left, [u_assign(hed_rez, AsBodyFunction, Result)|Right], NewRevList),
   reverse(NewRevList, NewList),
   list_to_conjuncts(NewList, NewBody),
   preds_to_functs0(NewBody, ConvertedBody),
   % The final Converted term is constructed
   into_equals(333, AsFunction, ConvertedBody, Converted)).

% Handles the case where Convert is a conjunction, and AsPred is not not_function.
% It converts predicates to functions inside a conjunction
preds_to_functs0((AsPred, Convert), Converted) :-
    \+ not_function(AsPred),
    pred_to_funct(AsPred, AsFunction, Result),
    sub_var(Result, Convert), !,
    % The function equivalent of AsPred replaces Result in Convert
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs0(Converting, Converted).

% Handles the special case where u_assign/2 is used and returns the function represented by the first argument of u_assign/2
preds_to_functs0(u_assign(_NN, AsFunction, _Result), AsFunction) :- !.

% Handles the general case where Convert is a conjunction.
% It converts the predicates to functions inside a conjunction
preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

% Handles the case where AsPred is a compound term that can be converted to a function
preds_to_functs0(AsPred, u_assign(pr2f, AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.

% any other term remains unchanged
preds_to_functs0(X, X).

% Converts a given predicate AsPred to its equivalent function term AsFunction
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), % Checks if AsPred is a compound term
    as_functor_args(AsPred, F, A), % Retrieves the functor F and arity A of AsPred
    functional_predicate_arg(F, A, Nth), !, % Finds the Nth argument where the result should be
    arg(Nth, AsPred, Result), % Retrieves the result from the Nth argument of AsPred
    remove_funct_arg(AsPred, Nth, AsFunction). % Constructs the function AsFunction by removing the Nth argument from AsPred

% If not found in functional_predicate_arg/3, it tries to construct AsFunction by removing the last argument from AsPred
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    as_functor_args(AsPred, _, Nth),
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

% body_member/4 is utility predicate to handle manipulation of body elements in the clause, but the exact implementation details and usage are not provided in the given code.
body_member(Body, BE, NewBE, NewBody):-
   conjuncts_to_list(Body, List),
   reverse(List, RevList), append(Left, [BE|Right], RevList),
   append(Left, [NewBE|Right], NewRevList),
   reverse(NewRevList, NewList),
   list_to_conjuncts(NewList, NewBody).
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
    as_functor_args(LeastHead, F, A), as_functor_args(NewHead, F, A),
    % Transform and combine bodies according to the new head found
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)),
    \+ \+ (
     Print=[converting=HeadBodiesList, newHead=NewHead],
     numbervars(Print, 0, _, []), fbug(Print),
        nop(in_cmt(print_pl_source(( NewHead :- NewCombinedBodies))))), !.

% Predicate to find the least general unified head (LGU) among the given list of heads.
% Heads is a list of head terms, and LeastGeneralHead is the least general term that unifies all terms in Heads.
least_general_head(Heads, LeastGeneralHead) :-
    lgu(Heads, LeastGeneralHead).

% the LGU of a single head is the head itself.
lgu([Head], Head) :- !.
% find the LGU of the head and the rest of the list.
lgu([H1|T], LGU) :-
    lgu(T, TempLGU),
    % Find generalization between head H1 and temporary LGU
    generalization(H1, TempLGU, LGU).

% generalization/3 finds the generalization of two heads, Head1 and Head2, which is represented by GeneralizedHead.
% This predicate is conceptual and will require more complex processing depending on the actual structures of the heads.
generalization(Head1, Head2, GeneralizedHead) :-
    % Ensure the functor names and arities are the same between Head1 and Head2.
    as_functor_args(Head1, Name, Arity),
    as_functor_args(Head2, Name, Arity),
    as_functor_args(GeneralizedHead, Name, Arity),
    % Generalize the arguments of the heads.
    generalize_args(Arity, Head1, Head2, GeneralizedHead).

% no more arguments to generalize.
generalize_args(0, _, _, _) :- !.
% generalize the corresponding arguments of the heads.
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
    Clauses = [(Head :- FirstBody)|RestClauses], % Decompose the list into the first clause and the rest.
    combine_bodies(RestClauses, FirstBody, Body).  % Combine the bodies of all the clauses.

% Helper predicate to combine the bodies of a list of clauses.
% The base case is when there are no more clauses to combine; the combined body is the current body.
combine_bodies([], Body, Body).
combine_bodies([(Head :- CurrentBody)|RestClauses], PrevBody, Body) :-
    % Combine the current body with the previous body using a conjunction (, ).
    combine_two_bodies(PrevBody, CurrentBody, CombinedBody),
    % Recursively combine the rest of the bodies.
    combine_bodies(RestClauses, CombinedBody, Body).

% Predicate to combine two bodies.
% Handles the combination of different Prolog constructs like conjunctions, disjunctions, etc.
combine_two_bodies((A, B), (C, D), (A, B, C, D)) :- !.  % Combine conjunctions.
combine_two_bodies((A; B), (C; D), (A; B; C; D)) :- !.  % Combine disjunctions.
combine_two_bodies(A, B, (A, B)).  % Combine simple terms using conjunction.
*/

% if there are no more bodies, the accumulated Combined is the final CombinedBodies.
combine_bodies([], _, Combined, Combined).
% combine the transformed body with the accumulated bodies.
combine_bodies([(Head:-Body)|T], NewHead, Acc, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    % Create a disjunction between the accumulated bodies and the transformed body.
    NewAcc = (Acc;TransformedBody),
    combine_bodies(T, NewHead, NewAcc, CombinedBodies).

% combine_code/3 combines Guard and Body to produce either Guard, Body, or a conjunction of both, depending on the values of Guard and Body.
combine_code(Guard, Body, Guard) :- assumed_true([], Body), !.
combine_code(Guard, Body, Body) :- assumed_true([], Guard), !.
combine_code(Guard, Body, Out):- var(Guard), !, combine_code(varcode(Guard), Body, Out).
combine_code(Guard, Body, Out):- var(Body), !, combine_code(Guard, varcode(Body), Out).
combine_code((A, B, C), Body, Out):- combine_code(C, Body, CBody), combine_code(B, CBody, BCBody), combine_code(A, BCBody, Out).
combine_code((AB, C), Body, Out):- combine_code(C, Body, CBody), combine_code(AB, CBody, Out).
combine_code(Guard, Body, (Guard, Body)).


lexical_true(V):- \+ ground(V), !, fail.
lexical_true(true).
lexical_true(u_assign(_, Nil, True)):- Nil==[], True=='True'.
lexical_true(u_assign(_, True, Var)):- var(Var), True=='True'.

combine_code(Body, varcode(Body)):- var(Body), !.
combine_code(Body, Body):- \+ compound(Body), !.
combine_code([A|Nil], O):- Nil==[], !, combine_code(A, O).
combine_code([A|B], O):- \+ is_list(B), combine_code(A, AA), combine_code(B, BB), !,
  combine_code([AA, BB], O).
combine_code([A, B|C], O):- \+ is_list(B),
  combine_code(A, AA), combine_code(B, BB), !,
  combine_code(AA, BB, AB),
  combine_code([AB|C], O), !.
combine_code((A;O), (AA;OO)):- !, combine_code(A, AA), combine_code(O, OO).
combine_code((A, B), O):- !, combine_code(A, B, O), !.
combine_code(AO, AO).



% create_unifier/3 creates a unification code that unifies OneHead with NewHead.
% If OneHead and NewHead are structurally equal, then they are unified and the unification Guard is 'true'.
% Otherwise, the unification code is 'metta_unify(OneHead, NewHead)'.

create_unifier(OneHead, NewHead, Guard):- OneHead=@=NewHead, OneHead=NewHead, !, Guard=true.
create_unifier(OneHead, NewHead, Guard):- compound(OneHead),
  compound_name_list(OneHead, _, Args1),
  compound_name_list(NewHead, _, Args2),
  create_unifier_goals(Args1, Args2, Guard), !.
create_unifier(OneHead, NewHead, u(OneHead, NewHead)).

create_unifier_goals([V1], [V2], u(V1, V2)):-!.
create_unifier_goals([V1|Args1], [V2|Args2], RightGuard):-!,
  create_unifier_goals(Args1, Args2, Guard),
  combine_code(u(V1, V2), Guard, RightGuard).
create_unifier_goals([], [], true).


% transform/4 combines unification code with Body to produce NewBody according to the transformations required by NewHead.
% It uses create_unifier/3 to generate the unification code between OneHead and NewHead.
transform(OneHead, NewHead, Body, NewBody):- create_unifier(OneHead, NewHead, Guard),
   combine_code(Guard, Body, NewBody).



    into_arg_code([], true):-!.
    into_arg_code(H, TT):- \+ iz_conz(H), TT = H.
    into_arg_code([H, T], TT):- H==true, !, into_arg_code(T, TT).
    into_arg_code([T, H], TT):- H==true, !, into_arg_code(T, TT).
    into_arg_code([H, T], ', '(HH, TT)):- !, into_arg_code(H, HH), into_arg_code(T, TT).
    into_arg_code([H|T], TT):- H==true, !, into_arg_code(T, TT).
    into_arg_code([H|T], ', '(HH, TT)):- !, into_arg_code(H, HH), into_arg_code(T, TT).
    into_arg_code(TT, TT).
    into_arg_code([H|T], next(H, TT)):- into_arg_code(T, TT).


    % reduce args to match types even inside atoms
    type_fit_childs(_Eq, _Depth, _Self, _RetType, true, X, Y):- is_ftVar(X), !, Y=X.
    type_fit_childs(_Eq, _Depth, _Self, _RetType, true, X, Y):- symbolic(X), !, Y=X.
    type_fit_childs(Eq, Depth, Self, RetType, CodeForArg, X, Y):- compound_non_cons(X), !,
       into_list_args(X, XX), !, type_fit_childs(Eq, Depth, Self, RetType, CodeForArg, XX, Y).
    type_fit_childs(_Eq, _Depth, _Self, _RetType, true, X, Y):- \+ is_list(X), iz_conz(X), mctrace(unknown), !, Y=X.
    type_fit_childs(_Eq, _Depth, _Self, _RetType, true, X, Y):- self_eval(X), !, Y=X.

    type_fit_childs(_Eq, _Depth, _Self, _RetType, true, [H|Args], [H|Args]):- (H=='eval_args';H=='eval_args-for'), !.

    type_fit_childs(Eq, Depth, Self, RetType, CodeForArg, ['let*', Lets, Body], RetVal):- !,
        expand_let_star(Lets, Body, NewLet), !,
            type_fit_childs(Eq, Depth, Self, RetType, CodeForArg, NewLet, RetVal).

    /*                                e, CodeForCond, ['If', Cond, Then, Else],
        pe_fit_childs(Eq, Depth, Self, RetType, CodeForCond, ['If', Cond, Then, Else],
                ['If', ConVal, (CodeForThen), CodeForElse]):-
        type_fit_childs(Eq, Depth, Self, 'Bool', CodeForCond, Cond, ConVal).
        type_fit_childs(Eq, Depth, Self, RetType, CodeForThen, Then, ThenVal).
        type_fit_childs(Eq, Depth, Self, RetType, CodeForElse, Else, ElseVal).
    */

    type_fit_childs(Eq, Depth, Self, RetType, FullCodeForArgs, [H|Args], Y):- H\==':',
       ignore(get_operator_typedef1(Self, H, ParamTypes, RType)),
       ignore(eager_for_type(RType, RetType)), !,
       must_det_ll((maplist(type_fit_childs(Eq, Depth, Self), ParamTypes, CodeForArgs, Args, NewVarL),
       into_arg_code(CodeForArgs, MCodeForArgs),
       into_arg_code([MCodeForArgs, 'eval_args'(XX, Y)], FullCodeForArgs),

       XX = [H|NewVarL],
       Y = _)).
       %eval_args(Eq, RetType, CodeForArg, Depth, Self, XX, Y).

    type_fit_childs(Eq, Depth, Self, RetType, FullCodeForArgs, [H|Args], Y):-
       must_det_ll((ignore(get_operator_typedef1(Self, H, ParamTypes, RetType)),
       maplist(type_fit_childs(Eq, Depth, Self), ParamTypes, CodeForArgs, Args, NewVarL),
       into_arg_code(CodeForArgs, FullCodeForArgs),
       Y = [H|NewVarL])).
    type_fit_childs(_Eq, _Depth, _Self, _RetType, true, X, Y):-!, must_det_ll((X=Y)).

    eager_for_type(_RType, 'Atom'):- !, fail.
    eager_for_type(_RType, 'Type'):- !, fail.
    eager_for_type(RType, RetType):- RType==RetType, !.
    eager_for_type(RType, 'Expression'):- !, RType=='Expression'.
    eager_for_type('Atom', 'Expression'):- !, fail.
    eager_for_type('Symbol', 'Expression'):- !, fail.
    eager_for_type(RType, Var):- var(Var), !, RType=Var.
    eager_for_type(_RType, _):-!.
    %eager_for_type(_RType, 'Any'):- !.
    %eager_for_type(_RType, 'Number').
    %eager_for_type(_RType, 'Nat').


    eval_evals(_Eq, _Depth, _Self, _RetType, X, Y):-self_eval(X), !, Y=X.
    eval_evals(_Eq, _Depth, _Self, _RetType, X, Y):- \+ is_list(X), !, Y=X.
    eval_evals(Eq, Depth, Self, RetType, [Eval, X], Y):- Eval == 'eval_args', !,
      eval_evals(Eq, Depth, Self, RetType, X, XX),
      eval_args(Eq, RetType, Depth, Self, XX, Y).
    eval_evals(Eq, Depth, Self, RetType, [Eval, SomeType, X], Y):- Eval == 'eval_args-for', !,
      eval_evals(Eq, Depth, Self, RetType, X, XX),
      eval_args(Eq, SomeType, Depth, Self, XX, Y).
    eval_evals(Eq, Depth, Self, RetType, [H|Args], Y):-
       ignore(get_operator_typedef1(Self, H, ParamTypes, RetType)),
       maplist(eval_evals(Eq, Depth, Self), ParamTypes, Args, NewVarL),
       XX = [H|NewVarL], Y=XX.
    eval_evals(_Eq, _Depth, _Self, _RetType, X, X):-!.


allow_type_violations(Goal):-
   locally(nb_setval(suspend_type_unificaton, true),
     Goal).



outputing_to_pl(MeTTaFile, PlFile):- wrote_pl_file(MeTTaFile, PlFile), !.

calc_precompiled_types(_Value):- !. %suspend for now
calc_precompiled_types(Value):-
   compute_precompiled_types(Value, Out), !,
   if_t(outputing_to_pl(_MeTTaFile, PlFile),
       send_to_txt_file(PlFile,
           call(in_cmt(call(do_show_precompiled_types(Out)))))).




compute_precompiled_types(Value, Out):-
 allow_type_violations(must_det_lls((
    pl_equality(Value, Out),
    expand_src_cns(Out)))).


show_precompiled_types(Var):- is_ftVar(Var), !, in_cmt(ppt(show_precompiled_types(Var))).
show_precompiled_types(Cmt):-  extract_cmmt(Cmt, Str), !, in_cmt(writeln(Str)).
show_precompiled_types(Value):-
   show_doing_src(Value),
   compute_precompiled_types(Value, Out),
   in_cmt(in_color(do_show_precompiled_types(Out))).

do_show_precompiled_types(Out):-
  maybe_write_info((call(show_prcompiled_types_now(Out)))).

show_prcompiled_types_now(Out):-
 \+ \+ allow_type_violations(must_det_lls((
  copy_term(Out, OutC, VS), Out=OutC,
  %extract_constraints(Out, VS),
  renumvars(Out+VS, COut+CVS),
  %CVS = VS, COut = Out,
  pp_se(COut),
  maplist(ppt_attr(cns), CVS)))).

cns_value(Val, Val):- \+ compound(Val), !.
cns_value(_=List, List):-!.
cns_value(Val, Val).
ppt_attr(Attr, put_attr(Var, Cns, Val)):- \+ \+  ((cns_value(Val, List), if_t(Cns=Attr, ppt(Var=List)))), !.
ppt_attr(_, _).

expand_src_cns(Out):- \+ compound(Out), !.
expand_src_cns(Out):- ground(Out), !.
expand_src_cns(Out):- compound_name_arguments(Out, _, [E]), !, expand_src_cns(E).
expand_src_cns(Out):-
   walk_src_for_constraints(Out),
   term_attvars(Out, VS),
   maplist(expand_cns, VS).

expand_cns(VS):- nonvar(VS), !.
expand_cns(VS):- \+ attvar(VS), !.
expand_cns(VS):- get_types_of(VS, More),
   findall(TT, (member(T, More), arg_type_hints_plus(VS, T, TT), nonvar(TT)), EvenMore),
   add_type_to_too(VS, EvenMore).

arg_type_hints_plus(_, T, TT):-  arg_type_hints(T, TT).
arg_type_hints_plus(_, Var, _):- var(Var), !, fail.
arg_type_hints_plus(VS, val(A), TT):- var(A), A\==VS, !, get_types_of(A, More), member(TT, More).
arg_type_hints_plus(_, arg(F, A), TT):- get_farg_type(F, A, TT).

add_type_to_too(V, T):- is_list(T), !, maplist(add_type_to_too(V), T).
add_type_to_too(V, T):- T == val(V), !.
add_type_to_too(V, T):- add_type_to(V, T).

put_arg_n_except(A, _, Nth, _):-A==Nth, !.
put_arg_n_except(_, F, Nth, arg(F, Nth)).

%delete_constraints(Out):- remove_attrs(cns, Out).

%call_fn_ize2(Value):- pl_equality(Value, Out),

pl_equality(Value, Value):- not_compound(Value), !.
pl_equality(Cmt, Out):- extract_cmmt(Cmt, Str), !, Out = in_cmt(Str).
pl_equality(exec(Eval), (for_exec(Var) :- Out)):- !,
    must_det_lls((into_equality_fct([body], 2, _RetType, Var, Eval, Out))).
pl_equality([Eq, H, B], Out):- is_metta_decl_f(Eq), !,
   must_det_lls((into_equality_fct([head], 1, RetType, Var, H, HH),
   into_equality_fct([body], 2, Var, RetType, B, BB),
   %renumvars((HH:-BB), Out).
   =((HH:-BB), Out))).
pl_equality(H, Out):-
   must_det_lls((into_equality_fct([head], 1, _RetType, _Var, H, HH),
   %renumvars((HH:-BB), Out).
   =((HH:-true), Out))).
pl_equality(Expr, metta_atom_asserted(Self, Expr)):- ignore(current_self(Self)), !.

arg_equality(_OpN, _Nth, Value, Out):- not_compound(Value), Value=Out, !.
arg_equality( OpN, Nth, Value, Out):-
  into_equality_call_fct(OpN, Nth, _RetType, Var, Value, Code),
  Out = type_value_code(Var, Code), !.
arg_equality(_OpN, _Nth, Value, Value).

has_arg_types(S):- has_returnType(S).
has_returnType(S):- ground(S), callable(S).

label_each_arg_type(_OpN, _Nth, _, _, _, []):-!.
label_each_arg_type( OpN, Nth, Var, F, N, [A|Args]):-
  label_each_arg_n_type(OpN, Nth, Var, F, N, A), N2 is N+1,
  label_each_arg_type(OpN, Nth, Var, F, N2, Args).

label_each_arg_n_type(_OpN, _Nth, Type, F, N, A):-
   if_t(has_arg_types(F), label_arg_e_type(F, N, A)),
   if_t(F=='..maybe superpose..', add_type_to_var(Type, A)).


% label_arg_n_type(F, 0, A):- !, label_type_assignment(A, F).
label_arg_e_type(F, N, A):- compound(F), functor_chkd(F, Fn, Add), Is is Add+N, !, label_arg_n_type(Fn, Is, A).
label_arg_e_type(F, N, A):- add_type_to(A, arg(F, N)), !.


skip_some([At|ListIn], List, 1, 1):- At=='@', !, append(List, [_], ListIn), !.
skip_some([At|ListIn], ListIn, 1, 0):- nonvar(At), skip_me_f(At), !.
skip_some(List, List, 0, 0).

add_types_into(_OpN, _Nth, Return, [F], [F], [F], [F], 0, 0):- if_t(var(Return), if_t(has_returnType(F),
   add_type_to_var(arg(F, 0), Return))), !.
add_types_into( OpN, Nth, Return, ListIn, VarL, NewVarL, List, SS, SE):-
 must_det_lls((
   skip_some(ListIn, List, SS, SE),
   length(LL, SS), length(LR, SE), append([LL, List, LR], ListIn),
   length(VL, SS), length(VR, SE), append([VL, NewVarL, VR], VarL),
   LL=VL, ignore(LR=VR),
   List = [F|Args],
   if_t(has_returnType(F), NewVarL = [F|_]),
   if_t(var(Return), if_t(has_returnType(F), add_type_to_var(arg(F, 0), Return))),
   label_each_arg_type(OpN, Nth, Return, F, 1, Args),
   if_t( \+ has_arg_types(F), maplist(add_type_to_var(ele(Return)), List)))).


into_equality_option(OpN, Nth, RetType, Var, Body, Code):-
  into_equality(OpN, Nth, RetType, BodyVar, Body, BodyCode),
  ((var(BodyVar), var(Var), BodyVar=Var)
             -> Code=BodyCode
             ;  combine_code(BodyCode, assign_now(BodyVar, Var), Code)).

into_equality_call(_, _, X, X, true):- atom(X), \+ is_src_bound(X).
into_equality_call(OpN, Nth, RetType, Var, Else, Code):-
  %if_t(is_value_obj(Else), must_det_lls(add_type_to(Var, val(Else)))),
  %if_t(is_value_obj(Var), must_det_lls(add_type_to(Else, val(Var)))),
  must_det_lls(into_equality(OpN, Nth, RetType, Var, Else, Code)).

:- discontiguous(into_equality/6).


has_function(KB,Op,Len):- between(0,10,Len),metta_function_asserted(KB,[Op|Args],_),length(Args,Len).

is_value_list(L):- notrace(quietly(is_value_list0(L))).
is_value_list0(L):- \+ is_list(L), !, fail.
is_value_list0([]):- !.
is_value_list0([QUOTE, Arg]):- QUOTE=='quote', !, \+ is_src_bound(Arg), !.
is_value_list0([Colon, A, B]):- Colon==':', !, is_value_obj(A), is_value_obj(B).
is_value_list0([F|L]):- \+ callable(F), is_value_obj(F), !, maplist(is_value_obj, L).

is_src_bound_nd(Var):- buffer_src_any(([BIND, Var|_])), (BIND=='bind!';BIND=='import!').

is_src_bound(Symbol):- notrace(is_src_bound0(Symbol)).

is_src_bound0(Symbol):- var(Symbol),!,fail.
is_src_bound0(Symbol):- quietly(( is_src_bound_nd(Var), Symbol==Var)), !.
is_src_bound0(Symbol):- atom(Symbol), atom_concat('&', _, Symbol).

is_value_obj(L):- notrace(quietly(is_value_obj0(L))).
is_value_obj0(L):- var(L), !, fail.
is_value_obj0(L):- atom(L), !, \+ is_src_bound(L).
is_value_obj0(L):- \+ callable(L), !.
is_value_obj0(L):- is_value_list(L).
is_value_obj0(L):- as_is_data_term(L), !.
is_value_obj0(L):- self_eval(L), !.
is_value_obj0(L):- not_compound(L), !.

into_equality(_OpN, _Nth, _RetType, Var, Value, true):- var(Value), Var=Value, !.
into_equality(_OpN, _Nth, _RetType, Var, Value, get_value(Value, Var)):- is_src_bound(Value), !, add_type_to(Var, bind(Value)).
into_equality(_OpN, _Nth, _RetType, Var, Value, get_value(Var, Value)):- is_src_bound(Var), !.
into_equality(_OpN, _Nth, _RetType, Var, Value, true):- is_value_obj(Value), Var=Value, !.

into_equality(_OpN, _Nth, _RetType, Var, Value, Var=Value):- number(Value),    !, add_type_to(Var, 'Number').
into_equality(_OpN, _Nth, _RetType, Var, List, Var= List):- is_value_list(List), !, add_type_to(Var, 'Expression').
%into_equality(_OpN, _Nth, RetType, Var, Value, Var=Value):- is_value_obj(Value), !, add_type_to(Var, 'PlainObj').
into_equality(_OpN, _Nth, _RetType, Var, Value, Var=Value):- is_value_obj(Value), !, add_type_to(Var, val(Value)).
%into_equality(OpN, Nth, RetType, Var, Convert, Code):- Var=@='$VAR'('_'), !,
%   into_equality(OpN, Nth, _, Convert, Code), !.

into_equality(_OpN, _Nth, _RetType, Var, Convert, Code):- Convert==[empty], !, Code=fail,
   add_type_to(Var, 'LazyEval'), add_type_to(Var, val('Empty')), !.

maybe_cond(_OpN, _Nth, [EQ, X, Y]):- EQ == '==', !,
   maybe_share_types([EQ], X, Y).

maybe_share_types(OpN, X, Y):-
   into_equality_call(OpN, 0, VarY, Y, _CodeY),
   into_equality_call(OpN, 0, VarX, X, _CodeX),
   get_types_of(VarX, TypesX),
   get_types_of(VarY, TypesY),
   add_type_to(VarX, TypesY),
   add_type_to(VarY, TypesX),
   add_type_to(X, TypesY),
   add_type_to(Y, TypesX), !.

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ [IF, Cond, Then, Else], IF=='if', !,
  ignore(maybe_cond(OpN, Nth, Cond)),
  into_equality_call(OpN, Nth, CondVar, Cond, CondCode),
  add_type_to(CondVar, 'Bool'),
  into_equality_option(OpN, Nth, RetType, Var, Then, ThenCode),
  into_equality_option(OpN, Nth, RetType, Var, Else, ElseCode),
  % Code =  ((CondCode *-> (is_True(CondVar)->ThenCode;ElseCode) ; Var = Convert)).
  Code =  ((CondCode, ((is_True(CondVar)->ThenCode;ElseCode)))).

into_equality(HeadIs, RetType, BodyResult, Convert, Converted) :-
  Convert = [Case, ValueSrc, Options], Case=='case', is_list(Options), !,
   must_det_ll((
    term_variables(ValueSrc, ExtraVars),
    into_equality_call(HeadIs, _ValueRetType, Value, ValueSrc, ValueCode),
    maplist(equals_case_bodies(HeadIs, RetType, ExtraVars, Value, BodyResult), Options, Cases),
    Converted =
           ( AllCases = Cases,
             call(ValueCode),
             select_case(AllCases, Value, BodyResult)))).

equals_case_bodies(HeadIs, RetType, ExtraVars, _CaseVar, BodyResult, [Match, Body], caseOption(_, BodyResult, ExtraVars, BodyCode)):- Match == '%void%', !,
      into_equality_option(HeadIs, RetType, BodyResult, Body, BodyCode).
equals_case_bodies(HeadIs, RetType, ExtraVars, CaseVar, BodyResult, [Match, Body], caseOption(Match, BodyResult, ExtraVars, BodyCode)):- !,
      add_type_to(CaseVar, val(Match)),
      into_equality_option(HeadIs, RetType, BodyResult, Body, BodyCode).

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ [CHAIN, Eval, LVar, Body], CHAIN=='chain', !,
  into_equality_call(OpN, Nth, RetType, Var, ['let', LVar, Eval, Body], Code).

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ [PROGN|Bodies], PROGN=='progn', !,
  append(Each, [Last], Bodies),
  make_dont_care(DontCare),
  maplist(into_equality(OpN, Nth, RetType, DontCare), Each, BodiesCode),
  into_equality_call(OpN, Nth, RetType, Var, Last, LastCode),
  append(BodiesCode, [LastCode], ListCode),
  list_to_conjuncts(ListCode, Code).


make_dont_care('$VAR'('_')).

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ [LET, LVar, Eval, Body], LET=='let', !,
  into_equality_call(let, 1, LVar, Eval, EvalCode),
  into_equality_call(OpN, Nth, RetType, Var, Body, BodyCode),
  combine_code(EvalCode, BodyCode, Code).
into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert =~ [LET_STAR, AAAA, Body], LET_STAR=='let*', AAAA =~ [VE|Bindings], VE=~[V, E], !,
  into_equality_call(OpN, Nth, RetType, Var, ['let', V, E, ['let*', Bindings, Body]], Code).

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
   Convert =~ [LET_STAR, AAAA, Body], LET_STAR=='let*', AAAA == [], !,
   into_equality_call(OpN, Nth, RetType, Var, Body, Code).


build_new_call(MightF, Nth, New, Old, Use):- (use_old_arg(MightF, Nth, Old) -> Use = Old ; Use = New).

use_old_arg( MightF, _Nth, _Old):- \+ symbol(MightF), !, fail.
use_old_arg(_MightF, Nth, _Old):- \+ integer(Nth), !, fail.
use_old_arg( 'bind!', 1, _Old).
use_old_arg('import!', 1, _Old).
use_old_arg( MightF, A, Old):- get_farg_type(MightF, A, Type), Type=='Symbol', symbol(Old), !.
%use_old_arg('set-random-seed', 1, _Old):- !.
use_old_arg( MightF, A, _Old):- get_farg_type(MightF, A, Type), Type=='Variable', !.


into_equality(OpN, Nth, RetType, Var, Convert, Code):-
   Convert=~ [At, B|Bodies], At=='@', is_list(B), !,
  progn_into_equality(OpN, Nth, RetType, Var, [B|Bodies], Code).

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ [At|PredAndArgs], At=='@', !,
  progn_into_equality(OpN, Nth, RetType, Var, [PredAndArgs], Code).

into_equality(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ ['call-for!', Var|Bodies], !,
  make_dont_care(DontCare),
  progn_into_equality(OpN, Nth, RetType, DontCare, Bodies, Code).

into_equality(OpN, Nth, RetType, Var, Convert, unit_or_error(Code, Var)):-
  Convert=~ ['call-unit!'|Bodies], !,
  make_dont_care(DontCare),
  progn_into_equality(OpN, Nth, RetType, DontCare, Bodies, Code).

into_equality(OpN, Nth, RetType, TF, Convert, as_tf(Code, TF)):-
  pl_mode, Convert=~ [and|Bodies],
  make_dont_care(DontCare),
  maplist(into_equality(OpN, Nth, RetType, DontCare), Bodies, CodeL),
    list_to_conjuncts(CodeL, Code).

pl_mode:- nb_current(prolog_interp_mode, true).
progn_into_equality(OpN, Nth, RetType, Var, Bodies, Code):-
    locally(nb_setval(prolog_interp_mode, true),
    into_equality(OpN, Nth, RetType, Var, ['progn'|Bodies], Code)).


into_equality(OpN, Nth, RetType, DontCare, Convert, Code):-
   Convert=~ ['or'|Bodies],
   maplist(into_equality(OpN, Nth, RetType, DontCare), Bodies, CodeL),
   list_to_disjuncts(CodeL, Code).

into_equality(OpN, Nth, RetType, Var, List, Code):- is_list(List), pl_mode, !,
  must_det_lls((
   length(List, FullLen),
   length(VarL, FullLen),
   length(NewCall, FullLen),
   maplist_nth(into_equality_call([arg(Nth)|OpN]), 1, VarL, List, CodeL),
   add_types_into(OpN, Nth, RetType, Var, List, VarL, [MightF|NewVarL], [_|Args], SS, SE),
   ignore(maybe_cond(OpN, Nth, [MightF|Args])),
   %writeln(List->[MightF|Args]),
   length([MightF|Args], SM), length(LL, SS), length(LR, SE), append([LL, [MightF|Args], LR], List),
   length([MightF|NewMiddle], SM),                     append([LL, [MightF|NewMiddle], LR], NewCall),
   (((is_list(VarL), VarL=[V], var(V), fail)) -> (NewCall=V, Var=NewCall, Call=true) ; (maplist_nth(build_new_call(MightF), 1, NewVarL, Args, NewMiddle),
     re_comp(NewCall, ReComp), Call='='(ReComp, Var))),
   if_t((has_returnType(MightF), is_list(NewVarL)), add_types_into(OpN, Nth, RetType, Var, [MightF|NewVarL], [MightF|Args], _, _, _, _)),
   if_t(has_returnType(MightF), add_type_to(Var, arg(MightF, 0))),
   append(CodeL, [Call], LLL),
   list_to_conjuncts(LLL, Code))).

into_equality(OpN, Nth, RetType, Var, List, Code):- is_list(List), !,
  must_det_lls((
   length(List, FullLen),
   length(VarL, FullLen),
   length(NewCall, FullLen),
   maplist_nth(into_equality_call([arg(Nth)|OpN]), 1, VarL, List, CodeL),
   add_types_into(OpN, Nth, RetType, Var, List, VarL, [MightF|NewVarL], [_|Args], SS, SE),
   ignore(maybe_cond(OpN, Nth, [MightF|Args])),
   %writeln(List->[MightF|Args]),
   length([MightF|Args], SM), length(LL, SS), length(LR, SE), append([LL, [MightF|Args], LR], List),
   length([MightF|NewMiddle], SM),                     append([LL, [MightF|NewMiddle], LR], NewCall),
   (((is_list(VarL), VarL=[V], var(V), fail)) -> (NewCall=V, Var=NewCall, Call=true) ; (maplist_nth(build_new_call(MightF), 1, NewVarL, Args, NewMiddle),
     re_comp(NewCall, ReComp), Call='='(ReComp, Var))),
   if_t((has_returnType(MightF), is_list(NewVarL)), add_types_into(OpN, Nth, RetType, Var, [MightF|NewVarL], [MightF|Args], _, _, _, _)),
   if_t(has_returnType(MightF), add_type_to(Var, arg(MightF, 0))),
   append(CodeL, [Call], LLL),
   list_to_conjuncts(LLL, Code))).
into_equality(OpN, Nth, _RetType, _Var, Comp, Code):- compound_name_arguments(Comp, F, Args), maplist_nth(arg_equality([F, arg(Nth)|OpN]), 1, Args, DFArgs), compound_name_arguments(Code, F, DFArgs).

de_comp(DeComp, Comp):- is_list(DeComp), !, Comp=DeComp.
de_comp(DeComp, Comp):- is_ftVar(DeComp), !, Comp=DeComp.
de_comp(DeComp, Comp):- \+ compound(DeComp), !, Comp=DeComp.
de_comp(DeComp, Comp):- compound_name_arguments(DeComp, 'S', [Comp]), !.
de_comp(DeComp, Comp):- Comp=DeComp.

re_comp(Comp, ReComp):- compound_name_arguments(ReComp, 'S', [Comp]), !.


walk_src_for_constraints(Out):- walk_src_for_constraints([], 0, Out).
walk_src_for_constraints(F, A, Var):- var(Var), !, ignore((atom(F), integer(A), add_type_to(Var, arg(F, A)))).
walk_src_for_constraints(_, _, Out):- not_compound(Out), !.
walk_src_for_constraints(_, _, Out):- ground(Out), !.
walk_src_for_constraints(_, _, assign_now(BodyVar, Var)):- !, add_type_to(Var, val(BodyVar)).

walk_src_for_constraints(_, _, '='(DeComp, Var)):- de_comp(DeComp, Comp), var(Var), is_list(Comp), Comp=[MightF|Args], !,
   if_t(callable(MightF), maplist_nth(walk_src_for_constraints(MightF), 1, Args)).

walk_src_for_constraints(_, _, Comp):- compound_name_arguments(Comp, F, Args), maplist_nth(walk_src_for_constraints([F|Args]), 1, Args).

not_compound(C):- is_ftVar(C), !.
not_compound(C):- \+ compound(C), !, \+ is_src_bound(C).

% One List
maplist_nth(_, _, []):-!.
maplist_nth(P, Nth, [Arg1|List1]):- (call_ps(P, Nth, Arg1)), succ(Nth, Mth), maplist_nth(P, Mth, List1).
% Two Lists
maplist_nth(_, _, [], []):-!.
maplist_nth(P, Nth, [Arg1|List1], [Arg2|List2]):- (call_ps(P, Nth, Arg1, Arg2)), succ(Nth, Mth), maplist_nth(P, Mth, List1, List2).
% Three Lists
maplist_nth(_, _, [], [], []):-!.
maplist_nth(P, Nth, [Arg1|List1], [Arg2|List2], [Arg3|List3]):- (call_ps(P, Nth, Arg1, Arg2, Arg3)), succ(Nth, Mth), maplist_nth(P, Mth, List1, List2, List3).
% Three Lists
maplist_nth(_, _, [], [], [], []):-!.
maplist_nth(P, Nth, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4]):- (call_ps(P, Nth, Arg1, Arg2, Arg3, Arg4)), succ(Nth, Mth), maplist_nth(P, Mth, List1, List2, List3, List4).


maplist_p(_, []) :- !.
maplist_p(P, [Arg1|List1]) :- call_ps(P, Arg1), maplist_p(P, List1).

maplist_p(_, [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2]) :- call_ps(P, Arg1, Arg2), maplist_p(P, List1, List2).

maplist_p(_, [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3]) :- call_ps(P, Arg1, Arg2, Arg3), maplist_p(P, List1, List2, List3).

maplist_p(_, [], [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4]) :- call_ps(P, Arg1, Arg2, Arg3, Arg4), maplist_p(P, List1, List2, List3, List4).

maplist_p(_, [], [], [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4], [Arg5|List5]) :- call_ps(P, Arg1, Arg2, Arg3, Arg4, Arg5), maplist_p(P, List1, List2, List3, List4, List5).

maplist_p(_, [], [], [], [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4], [Arg5|List5], [Arg6|List6]) :- call_ps(P, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), maplist_p(P, List1, List2, List3, List4, List5, List6).

maplist_p(_, [], [], [], [], [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4], [Arg5|List5], [Arg6|List6], [Arg7|List7]) :- call_ps(P, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7), maplist_p(P, List1, List2, List3, List4, List5, List6, List7).

maplist_p(_, [], [], [], [], [], [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4], [Arg5|List5], [Arg6|List6], [Arg7|List7], [Arg8|List8]) :- call_ps(P, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8), maplist_p(P, List1, List2, List3, List4, List5, List6, List7, List8).

maplist_p(_, [], [], [], [], [], [], [], [], []) :- !.
maplist_p(P, [Arg1|List1], [Arg2|List2], [Arg3|List3], [Arg4|List4], [Arg5|List5], [Arg6|List6], [Arg7|List7], [Arg8|List8], [Arg9|List9]) :- call_ps(P, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9), maplist_p(P, List1, List2, List3, List4, List5, List6, List7, List8, List9).

call_ps(P, A1) :- apply_ps(P, [A1]).
call_ps(P, A1, A2) :- apply_ps(P, [A1, A2]).
call_ps(P, A1, A2, A3) :- apply_ps(P, [A1, A2, A3]).
call_ps(P, A1, A2, A3, A4) :- apply_ps(P, [A1, A2, A3, A4]).
call_ps(P, A1, A2, A3, A4, A5) :- apply_ps(P, [A1, A2, A3, A4, A5]).
call_ps(P, A1, A2, A3, A4, A5, A6) :- apply_ps(P, [A1, A2, A3, A4, A5, A6]).
call_ps(P, A1, A2, A3, A4, A5, A6, A7) :- apply_ps(P, [A1, A2, A3, A4, A5, A6, A7]).
call_ps(P, A1, A2, A3, A4, A5, A6, A7, A8) :- apply_ps(P, [A1, A2, A3, A4, A5, A6, A7, A8]).
call_ps(P, A1, A2, A3, A4, A5, A6, A7, A8, A9) :- apply_ps(P, [A1, A2, A3, A4, A5, A6, A7, A8, A9]).

apply_ps(copy(P), List):- !, copy_term(P, S), apply_ps(S, List).
apply_ps(ignore(P), List):- !, ignore(apply_ps(P, List)).
apply_ps(not(P), List):- !, \+ apply_ps(P, List).
apply_ps( \+ P, List):- !, \+ apply_ps(P, List).
apply_ps(rtrace(P), List):- !, rtrace(apply(P, List)).
apply_ps(must(P), List):- !, must_det_lls(apply_ps(P, List)).
apply_ps(P, List):- apply(P, List).


arg_equality_fct(Out):- arg_equality_fct([top, arg(0)], 0, Out).
arg_equality_fct([F|_], A, Var, Var):- var(Var), !, ignore((add_type_to(Var, arg(F, A)))).
arg_equality_fct(_, _, Out, Out):- not_compound(Out), !.

arg_equality_fct(_, _, 'S'(E, R), 'S'(E, R)):-!.

arg_equality_fct(F, A, WasList, 'S'(IsList, _)):- is_list(WasList), maplist(arg_equality_fct(F, A), WasList, IsList).

arg_equality_fct(_, _, did(Comp), did(Comp)):-!.
arg_equality_fct(Before, N, Comp, did(Out)):- compound_name_arguments(Comp, F, Args), maplist_nth(arg_equality_fct([F, arg(N)|Before]), 1, Args, NewArgs), compound_name_arguments(Out, F, NewArgs).


%into_equality_fct_option(OpN, Nth, RetType, Var, Body, Code):- into_equality_option(OpN, Nth, RetType, Var, Body, Code).
into_equality_fct_option(OpN, Nth, RetType, Var, Body, Code):-
   into_equality_fct(OpN, Nth, RetType, Var, Body, Code), !.


into_equality_fct_option(OpN, Nth, RetType, Var, Body, Code):-
  into_equality_fct(OpN, Nth, RetType, BodyVar, Body, BodyCode),
  ((var(BodyVar), var(Var), BodyVar=Var)
             -> Code=BodyCode
             ;  combine_code(BodyCode, assign_now(BodyVar, Var), Code)).

%into_equality_fct_call(OpN, Nth, RetType, Var, Else, Code):- into_equality_call(OpN, Nth, RetType, Var, Else, Code).
into_equality_fct_call(_OpN, _Nth, _RetType, _Var, X, X):- atom(X), \+ is_src_bound(X), !.
into_equality_fct_call( OpN,  Nth,  RetType,  Var, Else, Code):-
  %if_t(is_value_obj(Else), must_det_lls(add_type_to(Var, val(Else)))),
  %if_t(is_value_obj(Var), must_det_lls(add_type_to(Else, val(Var)))),
  into_equality_fct(OpN, Nth, RetType, Var, Else, Code), !.
into_equality_fct_call(_OpN, _Nth, _RetType, _Var, X, X):- !.

:- discontiguous(into_equality_fct/6).

into_equality_fct(_OpN, _Nth, RetType, Var, Convert, 'S'(Var, ((Var='Empty';Var=[empty]), Code), RetType)):- Convert==[empty], !, Code=fail, add_type_to(RetType, 'LazyEval'), add_type_to(RetType, val('Empty')), !.
into_equality_fct(_OpN, _Nth, RetType, Var, Symbol, 'S'(Var, get_value(Symbol, Var), RetType)):- symbol(Symbol), is_src_bound(Symbol), !, add_type_to(RetType, bind(Symbol)).

/*
into_equality_fct(_OpN, _Nth, RetType, Var, VRetType^Value, VRetType^Value):- VRetType==RetType, !.
into_equality_fct(_OpN, _Nth, RetType, Var, VRetType^Value, VRetType^Value):- VRetType=RetType, !.
into_equality_fct(_OpN, _Nth, RetType, Var, Value, RetType^Value):- var(Value), RetType=Value, !.
into_equality_fct(_OpN, _Nth, RetType, Var, Value, Value):- RetType=@=Value, !, ignore(RetType=Value).
*/
%into_equality_fct(_OpN, _Nth, RetType, Var, Value, RetType^get_value(RetType, Value)):- is_src_bound(RetType), !.
%into_equality_fct(_OpN, _Nth, RetType, Var, Value, Value):- is_value_obj(Value), RetType=Value, !.

into_equality_fct(_OpN, _Nth, RetType, Var, Value, 'S'(Value, true, RetType)):- number(Value), Var=Value,    !, add_type_to(RetType, 'Number').
into_equality_fct(_OpN, _Nth, RetType, Var, Value, 'S'(Var, true, RetType)):-  var(Value), Var=Value, !.
into_equality_fct(_OpN, _Nth, RetType, Var, Value, 'S'(Var, Value=Var, RetType)):- var(Value), !.
%into_equality_fct(_OpN, _Nth, RetType, Var, Value, RetType=Value):- is_value_obj(Value), !, add_type_to(RetType, 'PlainObj').
into_equality_fct(_OpN, _Nth, RetType, Var, Value, 'S'(Value, true, RetType)):- is_value_obj(Value), ignore(Var=Value), !, add_type_to(RetType, val(Value)).
%into_equality_fct(OpN, Nth, RetType, Var, Convert, Code):- Var=@='$VAR'('_'), !,
%   into_equality_fct(OpN, Nth, _, Convert, Code), !.

into_equality_fct(_OpN, _Nth, RetType, Var, Value, 'S'(Var, true, RetType)):- \+ is_list(Value), Var=Value, !.
into_equality_fct(_OpN, _Nth, RetType, Var, Value, 'S'(Var, Value=Var, RetType)):- \+ is_list(Value), !.
into_equality_fct(_OpN, _Nth, RetType, Var, List, 'S'(Var, Var=List, RetType)):- Var=List, is_value_list(List), !, add_type_to(RetType, 'Expression').

into_equality_fct(_OpN, _Nth, RetType, Var, Convert, 'S'(Var, Code, RetType)):-
  Convert=~ [IF, Cond, Then, Else], IF=='if', !,
 must_det_ll((
  ignore(maybe_cond(OpN, Nth, Cond)),
  into_equality_fct_call(OpN, Nth, 'Bool', CondVar, Cond, CondCode),
  add_type_to(CondVar, 'Bool'),
  into_equality_fct_option(OpN, Nth, RetType, Var, Then, ThenCode),
  into_equality_fct_option(OpN, Nth, RetType, Var, Else, ElseCode),
  % Code =  ((CondCode *-> (is_True(CondVar)->ThenCode;ElseCode) ; Var = Convert)).
  Code =  (((is_True(CondCode)->ThenCode;ElseCode))))).

into_equality_fct(OpN, Nth, RetType, BodyResult, Convert, 'S'(BodyResult, Converted, RetType)):-
  Convert = [Case, ValueSrc, Options], Case=='case', is_list(Options), !,
   must_det_ll((
    term_variables(ValueSrc, ExtraVars),
    into_equality_fct_call(OpN, Nth, _ValueRetType, Value, ValueSrc, ValueCode),
    maplist(equals_case_bodies_fct(OpN, Nth, RetType, ExtraVars, Value, BodyResult), Options, Cases),
    Converted =
           ( AllCases = Cases,
             call(ValueCode),

             select_case(AllCases, Value, BodyResult)))).

equals_case_bodies_fct(OpN, Nth, RetType, ExtraVars, _CaseVar, BodyResult, [Match, Body], caseOption(_, BodyResult, ExtraVars, BodyCode)):- Match == '%void%', !,
      into_equality_fct_option(OpN, Nth, RetType, BodyResult, Body, BodyCode).
equals_case_bodies_fct(OpN, Nth, RetType, ExtraVars, CaseVar, BodyResult, [Match, Body], caseOption(Match, BodyResult, ExtraVars, BodyCode)):- !,
      add_type_to(CaseVar, val(Match)),
      into_equality_fct_option(OpN, Nth, RetType, BodyResult, Body, BodyCode).

into_equality_fct(OpN, Nth, RetType, Var, Convert, Code):-
  Convert=~ [CHAIN, Eval, LVar, Body], CHAIN=='chain', !,
  into_equality_fct_call(OpN, Nth, RetType, Var, ['let', LVar, Eval, Body], Code).


into_equality_fct(OpN, Nth, RetType, Var, Convert, Code):-
  Convert =~ [LET_STAR, AAAA, Body], LET_STAR=='let*', AAAA =~ [VE|Bindings], VE=~[V, E], !,
  into_equality_fct_call(OpN, Nth, RetType, Var, ['let', V, E, ['let*', Bindings, Body]], Code).

into_equality_fct(OpN, Nth, RetType, Var, Convert, Code):-
   Convert =~ [LET_STAR, AAAA, Body], LET_STAR=='let*', AAAA == [], !,
   into_equality_fct_call(OpN, Nth, RetType, Var, Body, Code).

into_equality_fct(OpN, Nth, RetType, Var, Convert, ProgCode):-
   Convert=~ [At, B|Bodies], At=='@', is_list(B), !,
  progn_into_equality_fct(OpN, Nth, RetType, Var, [B|Bodies], ProgCode).

into_equality_fct(OpN, Nth, RetType, Var, Convert, ProgCode):-
  Convert=~ [At|PredAndArgs], At=='@', !,
  progn_into_equality_fct(OpN, Nth, RetType, Var, [PredAndArgs], ProgCode).

into_equality_fct(OpN, Nth, RetType, Var, Convert, 'S'(Var, ProgCode, RetType)):-
  Convert=~ ['call-for!', Var|Bodies], !,
  make_dont_care(DontCare),
  progn_into_equality_fct(OpN, Nth, RetType, DontCare, Bodies, ProgCode).

into_equality_fct(OpN, Nth, RetType, Var, Convert, 'S'(Var, ProgCode, RetType)):- ignore(Var=[]),
  Convert=~ ['call-unit!'|Bodies], !,
  make_dont_care(DontCare),
  progn_into_equality_fct(OpN, Nth, RetType, DontCare, Bodies, ProgCode).

into_equality_fct(OpN, Nth, RetType, Var, Convert, 'S'(Var, EvalBodyCode, RetType)):- ignore(Var=[]),
  Convert=~ [LET, LVar, Eval, Body], LET=='let', !,
  into_equality_fct_call(let, 1, _LRetType, LVar, Eval, EvalCode),
  into_equality_fct_call(OpN, Nth, RetType, Var, Body, BodyCode),
  combine_code(EvalCode, BodyCode, EvalBodyCode).


into_equality_fct(OpN, Nth, RetType, Var, Convert, 'S'(Var, Code, RetType)):-
  Convert=~ [PROGN|Bodies], PROGN=='progn', !,
  append(Each, [Last], Bodies),
  make_dont_care(DontCare),
  maplist(into_equality_fct(OpN, Nth), _RetType, [DontCare|_], Each, BodiesCode),
  into_equality_fct_call(OpN, Nth, RetType, Var, Last, LastCode),
  append(BodiesCode, [LastCode], ListCode),
  list_to_conjuncts(ListCode, Code).

progn_into_equality_fct(OpN, Nth, RetType, Var, Bodies, Code):-
    locally(nb_setval(prolog_interp_mode, true),
    into_equality_fct(OpN, Nth, RetType, Var, ['progn'|Bodies], Code)).

into_equality_fct(OpN, Nth, RetType, Var, Convert, 'S'(Var, Code, RetType)):-
  pl_mode, Convert=~ [and|Bodies],
  make_dont_care(DontCare),
  maplist(into_equality_fct(OpN, Nth, RetType, DontCare), Bodies, CodeL),
    list_to_conjuncts(CodeL, Code).

into_equality_fct(OpN, Nth, RetType, Var, Convert, 'S'(Var, Code, RetType)):-
   Convert=~ ['or'|Bodies],
   maplist(into_equality_fct(OpN, Nth, RetType, Var), Bodies, CodeL),
   list_to_disjuncts(CodeL, Code).

into_equality_fct(OpN, Nth, RetType, Var, List, 'S'(Var, Code, RetType)):- is_list(List), !,
  must_det_lls((
   length(List, FullLen),
   length(VarL, FullLen),
   length(ParamTypes, FullLen),
   length(NewCall, FullLen),
   maplist_nth(into_equality_fct_call([arg(Nth)|OpN]), 1, ParamTypes, VarL, List, CodeL),
   add_types_into(OpN, Nth, RetType, List, VarL, [MightF|NewVarL], [_|Args], SS, SE),
   ignore(maybe_cond(OpN, Nth, [MightF|Args])),
   %writeln(List->[MightF|Args]),
   length([MightF|Args], SM), length(LL, SS), length(LR, SE), append([LL, [MightF|Args], LR], List),
   length([MightF|NewMiddle], SM),                     append([LL, [MightF|NewMiddle], LR], NewCall),
   (((is_list(VarL), VarL=[V], var(V), fail)) -> (NewCall=V, Var=NewCall, Call=true) ; (maplist_nth(build_new_call(MightF), 1, NewVarL, Args, NewMiddle),
     re_comp_fct(NewCall, Var, Call))),
   if_t((has_returnType(MightF), is_list(NewVarL)), add_types_into(OpN, Nth, RetType, [MightF|NewVarL], [MightF|Args], _, _, _, _)),
   if_t(has_returnType(MightF), add_type_to(Var, arg(MightF, 0))),
   append(CodeL, [Call], LLL),
   list_to_conjuncts(LLL, Code))).

re_comp_fct(NewCall, Var, 'S'(NewCall, Var)).

into_equality_fct(OpN, Nth, RetType, Var, Comp, 'S'(Var, Code, RetType)):-
  compound_name_arguments(Comp, F, Args), maplist_nth(arg_equality_fct([F, arg(Nth)|OpN]), 1, Args, DFArgs), compound_name_arguments(Code, F, DFArgs).








:- endif.


end_of_file.



    % If any sub-term of Convert is a control flow imperative, convert that sub-term and then proceed with the conversion.
    f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-   fail,
        % Get the least deepest sub-term AsFunction of Convert
        get_first_p1(AsFunction, Convert, N1Cmpd),
        arg(2, N1Cmpd, Cmpd),
        Cmpd \= ( ==(_, _) ),
        (Cmpd = [EE, _, _] -> (EE \== '==') ; true ),
        AsFunction\=@= Convert,
        callable(AsFunction), % Check if AsFunction is callable
        %Depth2 is Depth -0,
        % check that that is is a control flow imperative
        f2q(HeadIs, CType, Caller, Nth, RetType, Result, AsFunction, AsPred),
        HeadIs\=@=AsFunction, !,
        subst(Convert, AsFunction, Result, Converting), % Substitute AsFunction by Result in Convert
        f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, (AsPred, Result==AsFunction, Converting), Converted).  % Proceed with the conversion of the remaining terms


        % If any sub-term of Convert is a control flow imperative, convert that sub-term and then proceed with the conversion.
    f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :-   fail,
            deep_lhs_sub_sterm(AsFunction, Convert),
            AsFunction\=@= Convert,
            % Get the deepest sub-term AsFunction of Convert
          %  sub_term(AsFunction, Convert), AsFunction\==Convert,
            callable(AsFunction), % Check if AsFunction is callable
        %Depth2 is Depth -0,
            f2q(HeadIs, CType, Caller, Nth, RetType, Result, AsFunction, AsPred),
            HeadIs\=@=AsFunction, !,
            subst(Convert, AsFunction, Result, Converting), % Substitute AsFunction by Result in Convert
            f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

    % If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
    f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail,
        deep_lhs_sub_sterm(AsFunction, Convert), % Get the deepest sub-term AsFunction of Convert
        AsFunction\=@= Convert,
        callable(AsFunction), % Check if AsFunction is callable
        %is_function(AsFunction, Nth), % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
        HeadIs\=@=AsFunction,
        funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, _Nth, AsPred), % Convert AsFunction to a predicate AsPred
        subst(Convert, AsFunction, Result, Converting), % Substitute AsFunction by Result in Convert
        f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms
    /*
    % If AsFunction is a recognized function, convert it to a predicate.
    f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, AsFunction, AsPred):- % HeadIs, RetType\=@=AsFunction,
       is_function(AsFunction, Nth), % Check if AsFunction is a recognized function and get the position Nth where the result is stored/retrieved
       funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, RetResult, Nth, AsPred),
       \+ ( compound(AsFunction), arg(_, AsFunction, Arg), is_function(Arg, _)), !.
    */

    % If any sub-term of Convert is an u_assign/2, convert that sub-term and then proceed with the conversion.
    f2q(HeadIs, CType, Caller, Nth, RetType, RetResult, Convert, Converted) :- fail,
        deep_lhs_sub_sterm0(ConvertFunction, Convert), % Get the deepest sub-term AsFunction of Convert
        callable(ConvertFunction), % Check if AsFunction is callable
        ConvertFunction = u_assign(NN, AsFunction, Result),
        ignore(is_function(AsFunction, Nth)),
        funct_with_result_is_nth_of_pred(HeadIs, RetType, AsFunction, Result, Nth, AsPred), % Convert AsFunction to a predicate AsPred
        subst(Convert, ConvertFunction, Result, Converting), % Substitute AsFunction by Result in Convert
        f2p(HeadIs, CType, Caller, Nth, RetType, RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms







