

:- dynamic(transpiler_depends_on/4).
:- volatile(transpiler_depends_on/4).
:- discontiguous(transpiler_depends_on/4).
:- multifile(transpiler_depends_on/4).

:- dynamic(transpiler_stored_eval/3).
:- volatile(transpiler_stored_eval/3).
:- discontiguous(transpiler_stored_eval/3).
:- multifile(transpiler_stored_eval/3).


:- dynamic(transpiler_predicate_store/7).
:- discontiguous(transpiler_predicate_store/7).
:- multifile(transpiler_predicate_store/7).

:- dynamic(transpiler_predicate_nary_store/9).
:- discontiguous(transpiler_predicate_nary_store/9).
:- multifile(transpiler_predicate_nary_store/9).

:- dynamic(transpiler_clause_store/9).
:- discontiguous(transpiler_clause_store/9).
:- multifile(transpiler_clause_store/9).

:- dynamic(arg_type_n/4).
:- volatile(arg_type_n/4).
:- discontiguous(arg_type_n/4).


:-dynamic(pred_uses_fallback/2).
:-dynamic(pred_uses_impl/2).

:- dynamic(metta_function_asserted/3).
:- multifile(metta_function_asserted/3).
:- dynamic(metta_other_asserted/2).
:- multifile(metta_other_asserted/2).
:- dynamic(metta_function_asserted/3).
:- multifile(metta_function_asserted/3).
:- dynamic(metta_atom_asserted/2).
:- multifile(metta_atom_asserted/2).
:- dynamic(metta_atom_deduced/2).
:- multifile(metta_atom_deduced/2).
:- dynamic(metta_atom_in_file/2).
:- multifile(metta_atom_in_file/2).
:- dynamic(metta_atom_asserted_last/2).
:- multifile(metta_atom_asserted_last/2).


