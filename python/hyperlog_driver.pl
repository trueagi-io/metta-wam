% mettalog.pl


%:- ensure_loaded(library(mettalog_rt)).

% Basic S-expression parser (stub)
parse_sexpr_for_py(StringExpr, Parsed) :- facaded,!, StringExp= Parsed,!.
parse_sexpr_for_py(StringExpr, Parsed) :- parse_sexpr(StringExpr, Parsed).

% Simple evaluator (replace with real MeTTa interpreter)
eval_sexpr_for_py(StringExpr, Parsed) :- facaded,!, StringExp= Parsed,!.
eval_sexpr_for_py(StringExpr, Parsed) :- eval_args(StringExpr, Parsed).

% Entry point
metta_eval(StringExpr, Result) :-
    parse_sexpr(StringExpr, Parsed),
    eval_sexpr_for_py(Parsed, Result).
