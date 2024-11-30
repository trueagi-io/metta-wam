
% File that is loaded from LSP server and transpiled MeTTa files so all the options such as not starting the REPL and things happen
:- (prolog_load_context(directory, Value);Value='.'), absolute_file_name('../packs/',Dir,[relative_to(Value)]),
    atom_concat(Dir,'predicate_streams',PS),
    atom_concat(Dir,'logicmoo_utils',LU),
    attach_packs(Dir,[duplicate(replace),search(first)]),
    pack_attach(PS,[duplicate(replace),search(first)]),
    pack_attach(LU,[duplicate(replace),search(first)]).

:- set_prolog_flag(mettalog_rt, true),
   set_prolog_flag(mettalog_rt_args, []),
   set_prolog_flag(metta_argv, []).

:- set_prolog_flag(argv,[]),
   set_prolog_flag(os_argv,[swipl]).


:- % user:ensure_loaded(metta_compiler_lib),!,
   user:ensure_loaded(metta_interp),!,
   user:loon.


metta_runtime_write_answers(List):- write('['),write_answers_aux(List), write(']'),nl.
write_answers_aux(List):- List == [], !.
write_answers_aux([H|List]):- List == [], !, write_src_woi(H).
write_answers_aux([H|List]):- write_src_woi(H), write(', '), write_answers_aux(List).


do_file_top(Var,Call):-
  with_output_to(user_error, findall(Var,Call,List)),
  with_output_to(user_error, metta_runtime_write_answers(List)),
  write_answer_output.

begin_metta_runtime:- write_answer_output.

end_metta_runtime:- write_answer_output.


