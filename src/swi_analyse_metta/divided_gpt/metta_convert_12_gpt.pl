% maybe_call_directive/2
% This predicate checks the stream and calls a corresponding directive based on the argument.
% It either calls use_module or sets the stream encoding.
% 
% @param Stream The stream being operated on.
% @param Directive The directive to be executed.
maybe_call_directive(_Stream, use_module(library(W))) :- 
    % This checks if the directive is a use_module directive.
    trans_mod:use_module(library(W)).  % Calls use_module from trans_mod

maybe_call_directive(Stream, encoding(Enc)) :-
    % If the directive is about encoding, set the stream encoding.
    set_stream(Stream, encoding(Enc)).

% is_directive/1
% Checks if a term is a directive by verifying if it is prefixed with ':-'.
% 
% @param Term The term to be checked.
% @example is_directive((:- encoding(utf8))).
is_directive((:- _)).  % A directive starts with :-, if so, it is considered a directive.

% push_term_ctx/1
% Updates the current term context using a term if it's not already set.
% Handles non-compound terms, terms with bodies, and compound terms by their functor name.
% 
% @param X The term to push into the context.
push_term_ctx(X) :- 
    \+ compound(X), !, % If X is not compound, process directly
    (nb_current(term_ctx, Was) -> true ; Was = []), % Retrieve the current term context or set it as an empty list
    (Was =@= X -> true ; (nb_setval(term_ctx, X), nl)).  % If X is already the context, do nothing, otherwise update it.

push_term_ctx((X :- _)) :- 
    !, % If X is part of a clause, push X's context.
    push_term_ctx(X).

push_term_ctx(X) :- 
    compound_name_arity(X, F, _A),  % Extract the functor F of compound term X
    push_term_ctx(F).  % Push the functor as the context

% print_directive/1
% Prints a Prolog directive in a specific format after processing it.
% 
% @param Directive The directive to print.
print_directive((:- Directive)) :-
    push_term_ctx(exec), % Pushes 'exec' as the current term context.
    p2m([':-'], Directive, STerm),  % Converts the directive into a metaterm (STerm).
    write_pl_metta(exec(STerm)).  % Writes the metaterm in a specific format.

% write_pl_metta/1
% Wrapper to write a metaterm (STerm) ensuring that it's processed only once.
% 
% @param STerm The structured term to write.
write_pl_metta(STerm) :-
    \+ \+ write_pl_metta_0(STerm).  % Ensures the term is only written once.

% write_pl_metta_0/1
% Handles writing the structured term after handling variables within it.
% 
% @param STerm The structured term to write.
write_pl_metta_0(STerm) :-
    numbervars(STerm, 0, _, [singletons(true), attvar(skip)]),  % Number variables, handling singleton variables and skipping attributed variables.
    write_src(STerm).  % Write the structured term to the source.

% Directive to ensure the following files are loaded:
% - metta_compiler: Compiler functionalities
% - metta_convert: Conversion utilities
% - metta_types: Type definitions and checks
% - metta_space: Metta space utilities
% - metta_testing: Testing framework for metta
% - metta_utils: General utilities for metta
% - metta_printer: Printing functionalities
% - metta_eval: Evaluation functionalities
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_eval).

/* previously: Some code related to other metta modules was skipped as it may no longer be relevant.
   The directives that follow are kept for loading utility and evaluation modules.
*/