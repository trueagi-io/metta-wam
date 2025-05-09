/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%*********************************************************************************************
% PROGRAM FUNCTION: provides predicates for pretty-printing and formatting MeTTa
% expressions and other terms, with various options for controlling output style, indentation, etc.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

% ===============================
%       PRINTERS
% ===============================
% 'ppc' and 'ppc1' rules pretty-print original terms and convert them to metta if different,
% printing the converted forms as well.

%!  ppc(+Msg, +Term) is det.
%
%   Pretty-prints the original Term, and if a different MeTTa form exists, converts
%   and prints that form as well.
%
%   This predicate calls `ppc1/2` to print the original Term with the provided
%   Msg as context. It then converts Term to MeTTa form using `p2m/2`, and if
%   the converted form (MeTTa) is distinct from Term, it invokes `ppc1/2` again to
%   print the MeTTa form.
%
%   @arg Msg  A message or label to be associated with the output for context.
%   @arg Term The term to be printed and potentially converted to MeTTa form.
%
%   @example
%     % Pretty-print and convert the term if applicable.
%     ?- ppc('Original Term', some_term).
%
ppc(Msg, Term) :-
    % Call ppc1 to print the original Term.
    ppc1(Msg, Term),
    % Convert Term to MeTTa form.
    p2m(Term, MeTTa),
    !,
    % If MeTTa differs from Term, print the converted form with ppc1.
    (MeTTa \== Term -> ppc1(p2m(Msg), MeTTa) ; true).

%!  ppc1(+Msg, +Term) is det.
%
%   Handles the primary pretty-printing functionality for a given Term with a
%   specific Msg label.
%
%   This predicate attempts to print Term in a structured format. It calls `ppct/2`
%   for terms that match specific patterns, or uses a generalized pretty-printing
%   structure if none of the patterns apply. Additional information such as source
%   representation (`write_src/1`) and conversion trees (`ppt/1`) is also
%   displayed to enhance readability.
%
%   @arg Msg  A message or label associated with this printout for context.
%   @arg Term The term to be printed.
%
%   @example
%     % Pretty-print the term with specified structure.
%     ?- ppc1('Some Message', example_term).
%
ppc1(Msg, Term) :-
    % Attempt to print Term with ppct, handling specific term structures.
    undo_bindings(ppct(Msg, Term)), !.
ppc1(Msg, Term) :-
    % Generalized pretty-printing block.
    undo_bindings((
        ignore(guess_pretty(Term)),  % Use heuristic to pretty-print if available.
        writeln('---------------------'),
        write(p(Msg)), write(':'), nl,
        portray_clause(Term),  % Print Term in clause form.
        writeln('---------------------'),
        % Display the conversion tree if available.
        undo_bindings(ppt(?-show_cvts(Term))), nl,
        writeln('---------------------'),
        write(s(Msg)), write(':'), nl,
        write_src(Term), nl  % Display source representation.
    )).

undo_bindings(G):- \+ \+ woc(call(G)).

dont_numbervars(_,_,_,_).

%!  ppct(+Msg, +Term) is det.
%
%   Specific pretty-print handler for terms that are lists, clauses, or equations.
%
%   This predicate provides specialized handling for terms that are lists, clauses,
%   or equations, formatting each type according to its structure. For lists, variables
%   are numbered to avoid conflicts; for clauses and equations, the appropriate display
%   functions are used, with additional formatting for readability.
%
%   @arg Msg  A message or label associated with the printout for context.
%   @arg Term The term to be printed.
%
ppct(Msg, Term) :-
    % If Term is a list, apply list-specific formatting.
    is_list(Term), !,
    writeln('---------------------'),
    dont_numbervars(Term, 666, _, [attvar(skip)]),  % Bind variables for display.
    write((Msg)), write(':'), nl,
    write_src(Term), nl.
ppct(Msg, Term) :-
    % If Term is a clause (Head :- Body), format accordingly.
    Term = (_ :- _), !,
    writeln('---------------------'),
    write((Msg)), write(':'), nl,
    portray_clause(Term), nl.
ppct(Msg, Term) :-
    % If Term is an equation (Left = Right), format with variable numbering.
    Term = (_ = _), !,
    writeln('---------------------'),
    write((Msg)), write(':'), nl,
    dont_numbervars(Term, 444, _, [attvar(skip)]),
    write_src(Term), nl.
ppct(Msg, Term) :-
    % For clauses with specific formatting needs, include variable numbering and tree display.
    Term = (_ :- _), !,
    writeln('---------------------'),
    write((Msg)), write(':'), nl,
    dont_numbervars(Term, 222, _, [attvar(skip)]),
    ppt(Term), nl.

%!  pp_metta(+P) is det.
%
%   Pretty-prints a MeTTa term, handling variable numbers for readability.
%
%   The predicate first applies `pretty_numbervars/2` to convert any variables
%   within the term `P` into a standardized format `PP` for clarity. It then
%   calls `pp_fb/1` within the context of the `concepts=false` option, which
%   adjusts how the term is displayed (e.g., omitting certain conceptual details
%   for cleaner output).
%
%   @arg P The MeTTa term to be pretty-printed.
%
%   @example
%     % Pretty-print a MeTTa term with variable formatting.
%     ?- pp_metta(example_metta_term).
%
pp_metta(P) :-
    % Standardize variable names in P for readability.
    %pretty_numbervars(P, PP),
    P=PP,
    % Pretty-print PP with the `concepts=false` option.
    with_option(concepts=false, pp_fb(PP)).

%!  string_height(+Pt1, -H1) is det.
%
%   Calculates the height of a multi-line string `Pt1`, returning the number
%   of lines as `H1`.
%
%   The predicate splits the input string `Pt1` by newline characters to count
%   the number of lines. Any whitespace or tab characters are treated as delimiters.
%   The result `H1` represents the line count of `Pt1`.
%
%   @arg Pt1 The input string for which height (line count) is calculated.
%   @arg H1  Unifies with the line count of `Pt1`.
%
%   @example
%     % Calculate the height (number of lines) of a string.
%     ?- string_height("line1\nline2\nline3", Height).
%     Height = 3.
%
string_height(Pt1, H1) :-
    % Split Pt1 by line breaks and count the resulting list to get line height.
    split_string(Pt1, "\r\n", "\s\t\n\n", L),
    length(L, H1).

% 'dynamic' allows modification of the predicate at runtime.
:- dynamic(just_printed/1).

%!  print_pl_source(+P) is det.
%
%   Prints the source of a Prolog term `P` by running the `print_pl_source0/1`
%   predicate in a controlled environment.
%
%   This predicate is responsible for displaying the source code representation of
%   the term `P`. It does so by invoking `print_pl_source0/1` through `run_pl_source/1`,
%   which handles potential errors during execution.
%
%   @arg P The Prolog term whose source representation is to be printed.
%
%   @example
%     % Print the source representation of a term.
%     ?- print_pl_source((some_term :- some_goal)).
%
print_pl_source(P) :-
    % Run the primary source-printing predicate within `run_pl_source/1`.
    run_pl_source(print_pl_source0(P)).

%pnotrace(G):- !, call(G).
pnotrace(G):- notrace(G).
%pnotrace(G):- quietly(G).

%!  run_pl_source(+G) is det.
%
%   Executes a goal `G` safely, catching any errors and attempting a retry with tracing if needed.
%
%   This predicate tries to run the goal `G`. If `G` raises an error, it catches
%   the error, writes out the error for debugging, and retries `G` with `rtrace/1`
%   for tracing and debugging purposes.
%
%   @arg G The goal to be executed.
%
run_pl_source(G) :-
    % Fail silently on error.
    catch(G, E, (
        fail,
        % Log the error with the goal.
        write_src_uo(G = E),
        % Retry the goal with tracing enabled.
        rtrace(G)
    )).

%!  print_pl_source0(+P) is det.
%
%   Core logic for printing the source of a Prolog term `P`.
%
%   This predicate applies various display methods to `P` depending on its structure.
%   It checks if the term `P` has already been printed recently using `just_printed/1`
%   to avoid redundant printing. If `P` is a clause, directive, or other structure,
%   specific pretty-printing functions are applied (e.g., `portray_clause/1`).
%   Otherwise, the term is printed using a combination of methods to produce a clear
%   representation.
%
%   @arg P The Prolog term to be printed.
%
print_pl_source0(_) :- fail,
    % Do not print if compatibility mode is enabled.
    pnotrace(is_compatio), !.
print_pl_source0(_) :-
    % Do not print if silent loading mode is enabled.
    pnotrace(silent_loading), !.

print_pl_source0(P) :- fail,!,
    format('~N'),
    ppt(P),
    format('~N'), !.

print_pl_source0(P) :-
    % Check if P was just printed (avoid redundant printing).
    pnotrace((just_printed(PP), PP =@= P)), !.
print_pl_source0((A :- B)) :-
    % For rules (A :- B), display using portray_clause for readability.
    !,portray_clause((A :- B)).
print_pl_source0((:- B)) :-
    % For directives (:- B), display using portray_clause.
    !,portray_clause((:- B)).
print_pl_source0(P) :-
    % Default printing using `ppt/1`.
    format('~N'),
    ppt(P),
    format('~N'), !.
print_pl_source0(P) :-
    % Try different display actions and choose the output with the least height.
    Actions = [ppt, portray_clause, pp_fb1_e],
    % For each action, produce output and measure its height.
    findall(
        H - Pt,
        (
            member(Action, Actions),
            must_det_ll((
                % Run Action and capture output in string form.
                run_pl_source(with_output_to(string(Pt), call(Action, P))),
                % Attempt to determine string height; default to 0 on failure.
                catch(string_height(Pt, H), _, H = 0)
            ))
        ),
        HeightsAndOutputs
    ),
    % Sort outputs by height and display the shortest one.
    sort(HeightsAndOutputs, Lst),
    last(Lst, _ - Pt),
    writeln(Pt),
    % Update just_printed/1 to mark P as printed.
    retractall(just_printed(_)),
    assert(just_printed(P)),
    !.

%!  pp_fb1_a(+P) is det.
%
%   Pretty-prints a term `P` using a sequence of formatting options.
%
%   This predicate formats and displays the term `P` by numbering its variables
%   uniquely with `numbervars_w_singles/1`, followed by calling `pp_fb1_e/1` to apply
%   multiple pretty-printing functions in sequence. The term is enclosed in line breaks
%   for clearer output, and output is flushed at the end.
%
%   @arg P The Prolog term to be printed.
%
%   @example
%     % Print a term with numbered variables and multiple formats.
%     ?- pp_fb1_a(example_term).
%
pp_fb1_a(P) :-
    % Begin with a newline for cleaner output.
    format("~N "),
    % Apply variable numbering to P.
    undo_bindings(numbervars_w_singles(P),
    % Pretty-print P using `pp_fb1_e/1`.
    pp_fb1_e(P)),
    % End with a newline and flush output.
    format("~N "), flush_output.

%!  pp_fb1_e(+P) is det.
%
%   Applies multiple pretty-printing methods to a term `P`.
%
%   This predicate attempts various printing functions on `P` to find the most
%   suitable representation. It calls each function through `pp_fb2/2`, ensuring that
%   only available predicates are invoked. The sequence includes tree and clause
%   representations, as well as debugging and formatting helpers.
%
%   @arg P The Prolog term to be printed.
%
pp_fb1_e(P) :-
    % Attempt to print P using `ppt`.
    pp_fb2(ppt, P).
pp_fb1_e(P) :-
    % Attempt to print P using `pp_ilp`.
    pp_fb2(pp_ilp, P).
pp_fb1_e(P) :-
    % Attempt to print P using `pp_as`.
    pp_fb2(pp_as, P).
pp_fb1_e(P) :-
    % Attempt to print P as a clause.
    pp_fb2(portray_clause, P).
pp_fb1_e(P) :-
    % Attempt to print P with standard `print/1`.
    pp_fb2(print, P).
pp_fb1_e(P) :-
    % Attempt to print P with `fbdebug1`.
    pp_fb2(fbdebug1, P).
pp_fb1_e(P) :-
    % Attempt to format P directly.
    pp_fb2(fmt0(P)).

%!  pp_fb2(+F, +P) is nondet.
%
%   Applies a printing function `F` to a term `P`, if `F/1` is defined.
%
%   This predicate checks if the specified function `F` is available as a predicate
%   in the current context. If so, it calls `F/1` on `P`, printing `P` according
%   to the function's output format.
%
%   @arg F The name of the predicate to call.
%   @arg P The term to be passed to the predicate.
%
%   @example
%     % Apply the available formatting function to the term.
%     ?- pp_fb2(print, example_term).
%
pp_fb2(F, P) :-
    % Ensure F is an atom and a defined predicate.
    atom(F), current_predicate(F / 1),
    % Call F on P.
    call(F, P).

%!  pp_sax(+S) is det.
%
%   Formats and prints a term `S` based on its type or characteristics, with
%   specific handling for final writes, concept permissions, string-like terms,
%   and recognized identifiers.
%
%   This predicate applies different formatting actions depending on the
%   properties of `S`. It checks if `S` is suitable for final output, if concepts
%   are allowed, if `S` resembles an English term, or if `S` is of a specific type.
%   Based on these checks, it formats `S` as a concept, type, or value node
%   representation.
%
%   @arg S The term to be formatted and printed.
%
%   @example
%     % Print a term based on its identified format.
%     ?- pp_sax("example_string").
%
pp_sax(V) :-
    % If V is designated for final output, terminate here.
    is_final_write(V), !.
pp_sax(S) :-
    % If concepts are disallowed, use `write_src/1` for direct output.
    \+ allow_concepts, !, write_src(S).
pp_sax(S) :-
    % If S resembles an English word, print as a "StringValue".
    is_englishy(S), !,
    print_concept("StringValue", S).
pp_sax(S) :-
    % If S is a single-symbol term, format it as a "StringValue".
    symbol_length(S, 1), symbol_string(S, SS), !,
    print_concept("StringValue", SS).
pp_sax(S) :-
    % If S is identified as a specific argument type, print as a "TypeNode".
    is_an_arg_type(S, T), !,
    print_concept("TypeNode", T).
pp_sax(S) :-
    % If S has a recognized type, format as a "(TypeValueNode)".
    has_type(S, T), !,
    format('(~wValueNode "~w")', [T, S]).
pp_sax(S) :-
    % If S matches a FlyBase identifier, format based on type.
    sub_atom(S, 0, 4, Aft, FB), flybase_identifier(FB, Type), !,
     (Aft>0->format('(~wValueNode "~w")',[Type,S]);'format'('(TypeNode "~w")',[Type])).
pp_sax(S) :-
    % Default case: print S as a generic "ConceptNode".
    print_concept("ConceptNode", S).

%!  print_concept(+CType, +V) is det.
%
%   Prints a concept `V` with a specified concept type `CType`, if concepts are allowed.
%
%   If concept output is enabled (`allow_concepts/0` succeeds), the predicate
%   formats `V` within parentheses alongside its concept type `CType`.
%   Otherwise, it simply outputs `V` using `write_src/1`.
%
%   @arg CType The type of the concept to be printed (e.g., "StringValue", "TypeNode").
%   @arg V     The concept value to be printed.
%
%   @example
%     % Print a concept with the type "StringValue".
%     ?- print_concept("StringValue", "example_value").
%

%print_concept( CType,V):- allow_concepts, !, write("("),write(CType),write(" "),ignore(with_concepts(false,write_src(V))),write(")").
print_concept(_CType, V) :-
    % If concepts are not allowed, simply write V.
    ignore(write_src(V)).

%!  write_val(+V) is det.
%
%   Writes a value `V` in a specific format based on its type.
%
%   This predicate determines the type of `V` and applies appropriate formatting:
%   it directly outputs final values, writes numbers and compound terms as-is, and
%   wraps other types of terms (typically atoms or strings) in double quotes.
%
%   @arg V The value to be written.
%
%   @example
%     % Write a value based on its type.
%     ?- write_val(123).
%     123
%     ?- write_val(my_atom).
%     "my_atom"
%
write_val(V) :-
    % If V is a final write term, terminate here.
    is_final_write(V), !.
write_val(V) :-
    % If V is a number, write it directly.
    number(V), !, write_src(V).
write_val(V) :-
    % If V is a compound term, write it directly.
    compound(V), !, write_src(V).
write_val(V) :-
    % For other cases, wrap V in double quotes and write it.
    write('"'), write(V), write('"').

%!  is_final_write(+V) is nondet.
%
%   Handles the final write formatting for special cases of variables, `$VAR`
%   structures, Python objects, and specific list structures.
%
%   This predicate customizes the final output formatting of `V` depending on
%   its type. It deals with standard Prolog variables, `$VAR` structures,
%   special character representations, Python objects, and some list structures.
%   If none of these cases match, other predicates like `write_val/1` or `print_concept/2`
%   may handle further output.
%
%   @arg V The term to be formatted and written.
%
%   @example
%     % Final formatting for a Prolog variable.
%     ?- is_final_write('$VAR'(example_variable)).
%

is_final_write(V) :- current_printer_override(P1), catch(call(P1, V),_,fail),!.
% If V is an unbound variable, write it with `write_dvar/1`.
is_final_write(V) :- var(V), !,  write_dvar(V), !.
% For '$VAR' structures, write the variable name S.
is_final_write('$VAR'(S)) :- !,  write_dvar(S), !.
% For special character format `#\S`, write S in single quotes.
is_final_write('#\\'(S)) :- !, format("'~w'", [S]).
% For special character format `#\S`, write S in single quotes.
is_final_write('rng'(Id,_,_)) :- !, format("~w", [Id]).
is_final_write('rng'(Id,_)) :- !, format("~w", [Id]).
% Big number use underscores betten them
is_final_write(V) :- integer(V), V>900_000, catch(format('~I',[V]),_,fail),!.
% If Python mode is enabled and V is a Python object, format with `py_ppp/1`.
is_final_write(V) :- py_is_enabled, notrace(catch((py_is_py(V), !, py_ppp(V)),_,fail)), !.
% For lists like ['$VAR', Value], write the variable if the tail is empty.
is_final_write([VAR, V | T]) :- '$VAR' == VAR, T == [], !, write_dvar(V).
% For '[|]' (empty list), represent it as 'Cons'.
is_final_write('[|]') :- write('Cons'), !.
% For an empty list, write it as '()'.
is_final_write([]) :- !, write('()').

is_final_write(A:B) :- write_src(A),write(':'),write_src(B),!.

%is_final_write([]):- write('Nil'),!.
is_final_write(A) :- fail, \+ is_list(A), compound(A), A \= exec(_),
  \+ ((sub_term_safely(E,A), is_list(E))),
  catch(portray_clause(A),_,fail), !.


current_printer_override(P1):- nb_current('printer_override', P1),!,P1\==[].
with_write_override(P1, Goal):-
  locally(b_setval('printer_override',P1), Goal).
:- thread_initialization(nb_setval('printer_override',[])).


%!  write_dvar(+S) is det.
%
%   Writes a dynamic variable `S` in a standardized format based on its type.
%
%   This predicate handles the output formatting for variables, special symbols,
%   and specific atoms or strings prefixed with an underscore. The formatting
%   generally prepends a `$` symbol to `S` or a derived name, and calls `write_dname/1`
%   to output the result. If none of the specific cases apply, it simply writes `S`
%   as-is.
%
%   @arg S The variable or symbol to be formatted and written.
%
%   @example
%     % Format a dynamic variable for output.
%     ?- write_dvar('__').
%     $
%
write_dvar(S):- mvar_str(S,N),write_dname(N) ,!.

mvar_str(S,N) :- % If S is an underscore, output the name directly.
    S == '_', !, sformat(N,'~w',['_']).
mvar_str(S,N) :- % If S is a double underscore, write `$` to represent it.
    S == '__', !, sformat(N,'~w',['']).
mvar_str(S,N) :- % If S is an atom prefixed with an underscore, write its name.
    atom(S), symbol_concat('_', NS, S), sformat(N,'~w',[NS]).
mvar_str(S,N) :- % If S is a string prefixed with an underscore, write its name.
    string(S), symbol_concat('_', NS, S), sformat(N,'~w',[NS]).
mvar_str(S,N) :- % For an unbound variable, get its name and write it.
   var(S), get_snamed(S, Name), nonvar(Name), !, mvar_str(Name,N).
mvar_str(S,N) :- % For an unbound variable without a name, format it as `$<variable>`.
   attvar(S), get_attr(S,vn,Name),!, mvar_str(Name,N).
mvar_str(S,N) :- % For an unbound variable without a name, format it as `$<variable>`.
   attvar(S), notrace,ignore(nortrace),
   with_output_to(string(SS),ignore((get_attrs(S,Attrs),display(Attrs)))),
   %trace,
   sformat(N,'a-~w-~w', [SS,S]). % bou
mvar_str(S,N) :- % For an unbound variable without a name, format it as `$<variable>`.
    var(S), !, sformat(N,'~p', [S]).
mvar_str('$VAR'(S),N) :- number(S), !, sformat(N,'~p', ['$VAR'(S)]).
mvar_str('$VAR'(S),N) :- !, mvar_str(S,N).
%mvar_str(S,N):- number(S,N), sformat(N,S,N).
mvar_str(S,N) :- % Default case: write the name directly.
    sformat(N,'~w',[S]).

%!  write_dname(+S) is det.
%
%   Writes a variable name `S` prefixed by `$`.
%
%   This predicate simply prepends `$` to `S` and outputs the result. It is
%   intended to standardize the display of dynamic variable names.
%
%   @arg S The name or value to be printed with a `$` prefix.
%
%   @example
%     % Write a variable name with a `$` prefix.
%     ?- write_dname("var_name").
%     $var_name
%
write_dname(S) :-
    % Prefix S with `$` and write it.
    write('$'), write(S).

%!  pp_as(+V) is det.
%
%   Pretty-prints a value `V` using `pp_sex/1` and flushes the output buffer.
%
%   This predicate applies `pp_sex/1` to format and display `V`, then flushes
%   the output to ensure immediate display.
%
%   @arg V The term to be printed.
%
pp_as(V) :-
    % Apply `pp_sex/1` to format and print V, flushing output afterward.
    undo_bindings(pp_sex(V), flush_output).

%!  pp_sex_nc(+V) is det.
%
%   Pretty-prints a value `V` without quoting symbols, using `pp_sex/1`.
%
%   This predicate temporarily disables quoting symbols by using `with_no_quoting_symbols/2`
%   to format `V` with `pp_sex/1`, then re-enables quoting symbols afterward.
%
%   @arg V The term to be printed.
%
pp_sex_nc(V) :-
    % Temporarily disable quoting symbols and apply `pp_sex/1`.
    with_no_quoting_symbols(true, pp_sex(V)), !.

%!  unlooped_fbug(+Mesg) is det.
%
%   Sends a debug message `Mesg` to the fbug message hook, ensuring it doesn't
%   loop back to itself.
%
%   This predicate directs `Mesg` to the `fbug_message_hook/2` if it is set,
%   preventing recursive calls to itself.
%
%   @arg Mesg The message to be sent to the debug message hook.
%
unlooped_fbug(Mesg) :-
    % Send `Mesg` to `fbug_message_hook/2`, ensuring no recursion.
    fbug_message_hook(fbug_message_hook, fbug(Mesg)).

%!  into_hyphens(+D, -U) is det.
%
%   Converts the input term `D` into a hyphenated format `U`, transforming
%   functors and atoms to use hyphens.
%
%   If `D` is an atom, it is transformed directly via `always_dash_functor/2`.
%   Otherwise, `descend_and_transform/3` applies the transformation recursively
%   throughout complex terms.
%
%   @arg D The input term to be transformed.
%   @arg U The resulting term with hyphens.
%
into_hyphens(D, U) :-
    % If D is an atom, apply `always_dash_functor/2`.
    atom(D), !, always_dash_functor(D, U).
into_hyphens(D, U) :-
    % For complex terms, recursively apply the transformation.
    descend_and_transform(into_hyphens, D, U), !.

%!  unlooped_fbug(+W, +Mesg) is det.
%
%   Sends a debug message `Mesg` to `print/1` while managing a non-backtracking
%   variable `W` to avoid looping.
%
%   This predicate checks if `W` is already active to prevent looping; if not,
%   it prints `Mesg` and triggers a break. Otherwise, it uses `setup_call_cleanup/3`
%   to safely handle the variable state during the message output.
%
%   @arg W    The non-backtracking variable used to track the debugging state.
%   @arg Mesg The message to be printed.
%
unlooped_fbug(W, Mesg) :-
    % Check if W is already set to avoid looping; if so, print Mesg and break.
    nb_current(W, true), !,
    writeq(Mesg), nl, bt, break.
unlooped_fbug(W, Mesg) :-
    % Otherwise, safely set W and execute Mesg once, cleaning up afterward.
    setup_call_cleanup(
        nb_setval(W, true),
        once(Mesg),
        nb_setval(W, false)
    ),
    nb_setval(W, false).

% 'dynamic' allows modification of the predicate at runtime.
:- dynamic(py_is_enabled/0).

%!  py_is_enabled is det.
%
%   Asserts a flag to indicate that Python functionality is enabled if the
%   predicate `py_ppp/1` is defined.
%
%   This predicate checks whether `py_ppp/1` exists in the current environment
%   by verifying if it is a defined predicate. If `py_ppp/1` is available, it
%   dynamically asserts a clause for `py_is_enabled/0` that immediately succeeds,
%   effectively caching the check result for subsequent calls.
%
%   This mechanism avoids redundant lookups, as the assertion makes future
%   invocations of `py_is_enabled/0` instantaneous.
%
%   @example
%     % Enable Python functionality if py_ppp/1 is defined.
%     ?- py_is_enabled.
%
py_is_enabled :-
    % Check if `py_ppp/1` is defined in the current environment.
    predicate_property(py_ppp(_), defined),
    % If defined, assert `py_is_enabled/0` to cache the enabled state.
    asserta((py_is_enabled :- !)).

%write_src(V):-  !, undo_bindings(quietly(pp_sex(V))),!.

%!  write_src_wi(+V) is det.
%
%   Writes the source of a term `V` with indentation enabled.
%
%   This predicate sets indentation mode on by calling `with_indents/2` with `true`,
%   and then writes the source of `V` using `write_src/1`. The use of `pnotrace/1`
%   ensures that tracing is disabled during this operation.
%
%   @arg V The term whose source is to be written with indentation.
%
write_src_wi(V) :-
    % Enable indentation and write the source of V.
    pnotrace((with_indents(true, write_src(V)))).

%!  write_src(+V) is det.
%
%   Writes the source of a term `V` after guessing Metta variables.
%
%   This predicate first tries to identify variables in `V` using `guess_metta_vars/1`
%   and then formats `V` for output using `pp_sex/1`. The `pnotrace/1` wrapper
%   ensures tracing is turned off.
%
%   @arg V The term to be written as source.
%
write_src(V) :-
 % display(v=V),
 no_type_unification((
    % Guess variables in V and pretty-print using `pp_sex/1`.

    undo_bindings(pnotrace((
               term_variables(V,Vars),
               copy_term(V+Vars,CV+CVars,_),
               maplist(transfer_varname,Vars,CVars),
               ((CVars=@=Vars)
              -> pp_sex(V);
                (gather_src_and_goal(V,Nat,NatGoals),
                 unify_with_occurs_check(CV,Nat),
                 write_src_ng(CV,NatGoals)))))))),!.

write_src_ng(Nat,NatGoals):- pp_sex(Nat),
   if_t(NatGoals\==[],if_t(nb_current('$write_goals',true),maybe_write_goals(NatGoals))).

bou:attr_portray_hook(Val,_V):- copy_term(Val,_,Info),!,format(' bou ~w ',[Info]),!. %writeq(break_on_unify(V,Val)),write(' ').
bou:attr_unify_hook(_,_):- bt,!,trace,break.
bou:attribute_goals(V) --> {get_attr(V,bou,Val)},[break_on_unify(V,Val)].

break_on_unify(_,_):-!. % disables `break_on_unify`
break_on_unify(GG,G):- var(G),!,put_attr(G,bou, (GG)).
break_on_unify(GG,G):-
    term_variables(G,Vs),
    maplist(break_on_unify(GG),Vs).

print_compounds_special:- true.
src_vars(V,I):- nb_current(suspend_type_unificaton, true),!,V=I.
%src_vars(V,I):- var(V),!,I=V.
src_vars(V,O):- %ignore(guess_metta_vars(V)),
             no_type_unification((((must_det_lls((
              guess_varnames(V,I),
              nop(ignore(dont_numbervars(I,400,_,[singleton(true),attvar(skip)]))),
              materialize_vns(I,IO),
             pre_guess_varnames(IO,O),call(IO=O))))))).
pre_guess_varnames(V,I):- \+ compound(V),!,I=V.
pre_guess_varnames(V,I):- ground(V),!,I=V.
%pre_guess_varnames(V,I):- copy_term_nat(V,VC),compound_name_arity(V,F,A),compound_name_arity(II,F,A), metta_file_buffer(_, _, _, II, Vs, _,_), Vs\==[], copy_term_nat(II,IIC), VC=@=IIC, II=I,maybe_name_vars(Vs),!.
%pre_guess_varnames(V,I):- is_list(V),!,maplist(pre_guess_varnames,V,I).
%pre_guess_varnames(C,I):- compound_name_arguments(C,F,V),!,maplist(pre_guess_varnames,V,VV),compound_name_arguments(I,F,VV),!.
pre_guess_varnames(V,V).

wsv:- cls, parse_sexpr("!($X)",P),rtrace(write_src(P)).

gather_src_and_goal(V,Nat,NatGoals):- ground(V),!,Nat= V, NatGoals=[].
gather_src_and_goal(V,Nat,NatGoals):-
    number_src_vars(V, I, Goals),
    copy_term_nat(I+Goals,Nat+NatGoals),
    break_on_unify(V,Nat+NatGoals).

write_w_attvars(Term):- undo_bindings(no_type_unification((write_w_attvars0(Term)))).

write_w_attvars0(Term):-
   number_src_vars(Term,PP,Goals),
   copy_term_nat(PP+Goals,Nat+NatGoals),
   writeq(Nat),
   maybe_write_goals(NatGoals), !.

number_src_vars(V, I, Goals):- ground(V),!, I= V, Goals=[].
number_src_vars(Term,TermC,Goals):-
 no_type_unification((
  must_det_lls((
    term_variables(Term,TermVars),
    src_vars(Term,PP),
    copy_term(Term+TermVars,TermC+TermVarsC,Goals),
    % copy_term(Goals,CGoals,GoalsGoals),
    PP = TermC,
    must(PP = Term),
    maplist(transfer_varname,TermVars,TermVarsC),
    %materialize_vns(PP),
    %materialize_vns(TermVarsC),
    nop(ignore(dont_numbervars(PP,260,_,[singleton(true),attvar(skip)]))),

    nop(ignore(dont_numbervars(PP,26,_,[singleton(true),attvar(bind)]))))))).

transfer_varname(V,C):- nonvar(V),!,V=C.
transfer_varname(V,C):- nonvar(C),!,V=C.
transfer_varname(V,C):- ignore((catch((get_attr(V,vn,Named),put_attr(C,vn,Named)),E,(trace,writeln(E))))).

once_writeq_nl_now(P) :-
    undo_bindings(pnotrace((no_type_unification((
             format('~N'),
             write_w_attvars(P),
             format('~N')))))).

:- thread_initialization(nb_setval('suspend_type_unificaton',[])).
no_type_unification(G):-
  locally(b_setval(suspend_type_unificaton, true),G).


:- thread_initialization(nb_setval('$write_goals',[])).

with_written_goals(Call):-
   locally(b_setval('$write_goals',true),Call).

maybe_write_goals(Goals):- Goals==[],!.
maybe_write_goals(_):- \+ nb_current('$write_goals',true), !.
maybe_write_goals(Goals):- once(exclude(is_f_nv,Goals,LGoals)),if_t(LGoals\==[],format(' {<~q>} ', [LGoals])).

is_f_nv(P):- compound(P), functor(P,F,A,_), !, is_f_nv(F,A).
is_f_nv(break_on_unify,2). is_f_nv(name_variable,2).


% Standardize variable names in `P` and print it using `ansi_format`.
% Use `nb_setval` to store the printed term in `$once_writeq_ln`.
once_writeq_nl_now(Color,P) :- w_color(Color,once_writeq_nl_now(P)).

%!  write_src_nl(+Src) is det.
%
%   Prints a source line followed by a newline.
%
%   @arg Src The source line to print.
%
write_src_nl(Src) :-
    % Print a newline, the source line, and another newline.
    undo_bindings(must_det_ll((
               (format('~N'), write_src(Src),
                format('~N'))))).


w_color(Color,Goal):-
    undo_bindings(must_det_ll((
           wots(Text,Goal),
           with_output_to(user_error,ansi_format([fg(Color)], '~w', [Text]))))).

materialize_vns(Term,NewTerm):- term_attvars(Term,List), materialize_vnl(List,Term,MidTerm),!,term_variables(MidTerm,MList),materialize_vnl(MList,MidTerm,NewTerm),!.
materialize_vnl([],IO,IO):-!.
materialize_vnl([Var|List],Term,NewTerm):- get_vnamed(Var,VNamed), subst001(Term,Var,VNamed,MidTerm),!,materialize_vnl(List,MidTerm,NewTerm).
materialize_vnl([_|List],Term,NewTerm):- materialize_vnl(List,Term,NewTerm).

get_snamed(Var, SNamed):- attvar(Var), get_attr(Var,vn,NN), atom_string(NN,SNamed),!.
get_snamed(Var, SNamed):- var(Var), get_var_name(Var, N), atom_string(N,SNamed),!.
get_snamed(Var, SNamed):- var(Var), variable_name(Var, N), atom_string(N,SNamed),!.
get_snamed(Var, SNamed):- var(Var), sformat(N,'~q',[Var]), atom_string(N,SNamed),!.

get_vnamed(Var, VNamed):- get_snamed(Var, Name), into_vnamed(Name,VNamed),!.
get_vnamed(Var, _Named):- \+ compound(Var),!,fail.
get_vnamed('$VAR'(N), VNamed):- into_vnamed(N,VNamed),!.
get_vnamed('$'(N), VNamed):- into_vnamed(N,VNamed),!.

into_vnamed(S,'$VAR'(NS)):- mvar_str(S,N),sformat(NS,'_~w',[N]).

%!  write_src_woi(+Term) is det.
%
%   Writes the source of a term `Term` with indentation disabled.
%
%   This predicate calls `with_indents/2` with `false` to disable indentation,
%   and then writes `Term` using `write_src/1`. The `pnotrace/1` wrapper ensures
%   that tracing is disabled.
%
%   @arg Term The term to be written without indentation.
%
write_src_woi(Term) :-
    % Disable indentation and write the source of Term.
    pnotrace((with_indents(false, write_src(Term)))).

%!  write_src_woi_nl(+X) is det.
%
%   Writes the source of a term `X` without indentation, adding newlines before and after.
%
%   This predicate sets indentation mode off and adds newline formatting
%   around the output for readability. It calls `guess_metta_vars/1` to
%   identify variables in `X`, and then uses `write_src_woi/1` to write the source.
%
%   @arg X The term to be written without indentation and with surrounding newlines.
%
write_src_woi_nl(X) :-
    % Guess variables in X, add newlines, and write without indentation.
    undo_bindings(pnotrace((
        format('~N'), write_src_woi(X), format('~N')
    ))).

%!  pp_sex(+V) is det.
%
%   Primary predicate for pretty-printing the source of `V`.
%
%   This predicate calls `pp_sexi/1` to handle different cases based on the
%   type and structure of `V`, and ensures that the operation succeeds
%   deterministically.
%
%   @arg V The term to be pretty-printed.
%
pp_sex(V) :-
    % Call `pp_sexi/1` to handle various cases of term `V`.
    pp_sexi(V), !.

% Various 'write_src' and 'pp_sex' rules are handling the writing of the source,
% dealing with different types of values, whether they are lists, atoms, numbers, strings, compounds, or symbols.

%!  pp_sexi(+V) is det.
%
%   Helper predicate to handle various cases for pretty-printing term `V`.
%
%   This predicate checks the structure and type of `V` and applies specific
%   formatting rules. It handles dictionaries, modules, numbers, strings, symbols,
%   atoms, and complex terms differently, and adjusts output based on indentation
%   settings.
%
%   @arg V The term to be printed.
%
pp_sexi(V) :-
    % If `V` is a final write term, handle it and terminate.
    is_final_write(V), !.
pp_sexi(V) :-
    % If `V` is a dictionary, print it directly.
    is_dict(V), !, write_term(V,[]).
pp_sexi((USER:Body)) :- fail,
    % If `V` is in the format `user:Body`, process `Body` directly.
    USER == user, !, pp_sex(Body).
pp_sexi(V) :-
    % If concepts are allowed, disable concept formatting and print `V`.
    allow_concepts, !, with_concepts('False', pp_sex(V)), flush_output.
pp_sexi('Empty') :- nb_current(may_skip_printing_empty, true),% fail,
    % If `V` is the atom 'Empty', do not print anything.
    !.
pp_sexi('') :-
    % If `V` is an empty string, print quoted empty string.
    !, writeq('').
% Handling more cases for 'pp_sex', when the value is a number, a string, a symbol, or a compound.
%pp_sex('') :- format('(EmptyNode null)',[]).
pp_sexi(V) :-
    % If `V` is a number, print it quoted.
    number(V), !, writeq(V).
pp_sexi(V) :-
    % If `V` is a string, print it quoted.
    string(V), !, writeq(V).
pp_sexi(S) :-
    % If `S` is a string, print as a "StringValue" concept.
    string(S), !, print_concept('StringValue', S).
pp_sexi(V) :-
    % If `V` is a symbol and should be quoted, convert to string and quote.
    symbol(V), should_quote(V), !, symbol_string(V, S), write("'"), write(S), write("'").
% Base case: atoms are printed as-is.
%pp_sexi(S):- symbol(S), always_dash_functor(S,D), D \=@= S, pp_sax(D),!.
pp_sexi(V) :-
    % If `V` is a symbol, print it directly.
    symbol(V), !, write(V).
pp_sexi(V) :-
    % If `V` is a number or a dictionary, print as a "ValueAtom".
    (number(V); is_dict(V)), !, print_concept('ValueAtom', V).
%pp_sex((Head:-Body)) :- !, print_metta_clause0(Head,Body).
%pp_sex(''):- !, write('()').
% Continuing with 'pp_sex', 'write_mobj', and related rules,
% handling different cases based on the value type and structure, and performing the appropriate writing action.
% Lists are printed with parentheses.
pp_sexi(V) :-
    % If `V` is not a compound term, print it as a Prolog term.
    \+ compound(V), !, format('~p', [V]).
%pp_sexi(V):-  is_list(V),!, pp_sex_l(V).
%pp_sex(V) :- (symbol(V),symbol_number(V,N)), !, print_concept('ValueAtom',N).
%pp_sex(V) :- V = '$VAR'(_), !, format('$~p',[V]).
pp_sexi(V) :-
    % If indentation is disabled, apply compact printing.
    no_src_indents, !, pp_sex_c(V).
pp_sexi(V) :-
    % If indentation is enabled, apply proper indentation for pretty-printing.
    w_proper_indent(2, w_in_p(pp_sex_c(V))).

%!  write_mobj(+H, +Args) is det.
%
%   Writes a structured term `H` with its arguments `Args` in a specific format,
%   based on the head functor or symbol of `H`.
%
%   This predicate handles various structures by matching `H` to specific symbols
%   (e.g., `$VAR`, `exec`, `$OBJ`, `{}`, and `[ ... ]`) and applying specialized
%   formatting for each case. If `H` is not a recognized symbol, the predicate
%   fails or defaults to a generic output format.
%
%   @arg H     The head functor or symbol of the term to be written.
%   @arg Args  The arguments associated with `H` for formatting.
%
%   @example
%     % Write an object structure with arguments.
%     ?- write_mobj('$VAR', [example_var]).
%
write_mobj(H, _) :- % If `H` is not a recognized symbol, fail.
    \+ symbol(H), !, fail.
write_mobj(_, T) :- \+ is_list(T),!,fail.
write_mobj('$VAR', [S]) :-
    % If `H` is `$VAR`, write the variable `S` using `write_dvar/1`.
    write_dvar(S).
write_mobj(exec, [V]) :-
    % For `exec`, print `V` prefixed by an exclamation mark.
    !, write('!'), write_src(V).
write_mobj('$OBJ', [_, S]) :-
    % For `$OBJ`, print `S` enclosed in square brackets.
    write('['), write_src(S), write(' ]').
write_mobj('{}', [S]) :-
    % For `{}`, print `S` enclosed in braces.
    write('{'), write_src(S), write(' }').
write_mobj('{...}', [S]) :-
    % For `{...}`, print `S` in brace notation, ignoring additional symbols.
    write('{'), write_src(S), write(' }').
write_mobj('[...]', [S]) :-
    % For `[ ... ]`, print `S` in bracket notation, ignoring additional symbols.
    write('['), write_src(S), write(' ]').
write_mobj('$STRING', [S]) :-
    % For `$STRING`, quote and print `S`.
    !, writeq(S).
write_mobj(F, Args) :-
    % Fallback clause using `mlog_sym/1` and `pp_sex_c/1` for custom formatting.
    fail, mlog_sym(K), !, pp_sex_c([K, F | Args]).
%write_mobj(F,Args):- pp_sex_c([F|Args]).

%!  pp_sex_l(+V) is det.
%
%   Primary predicate for pretty-printing a list `V` in S-expression format.
%
%   This predicate delegates the formatting of `V` to `pp_sexi_l/1`, which handles
%   various list structures and applies the appropriate formatting based on the
%   list’s content.
%
%   @arg V The list to be printed.
%
pp_sex_l(V) :-
    % Delegate to `pp_sexi_l/1` for detailed list formatting.
    pp_sexi_l(V), !.

%!  pp_sexi_l(+V) is det.
%
%   Helper predicate for pretty-printing different types of lists in S-expression format.
%
%   This predicate applies various formatting rules based on the content and structure
%   of `V`, handling lists of symbols, specific structures, and default cases.
%   Certain structures, such as `[If | T]`, trigger indentation and S-expression formatting.
%
%   @arg V The list to be printed.
%
pp_sexi_l(V) :-
    % If `V` is marked for final output, handle it and terminate.
    is_final_write(V), !.
%pp_sexi_l([F|V]):- integer(F), is_codelist([F|V]),!,format("|~s|",[[F|V]]).


pp_sexi_l([F | V]) :-
    % If `F` is a symbol and `V` is a list, format using `write_mobj/2`.
    symbol(F), is_list(V), write_mobj(F, V), !.
pp_sexi_l([H | T]) :- T == [], !,  % If `T` is an empty list, print `H` in parentheses.
    write_start(list), pp_sex_nc(H), write_end(list).
pp_sexi_l([H, H2| T]) :- T ==[], !,
    % If `V` has two elements, print `H` followed by `H2` in S-expression format.
    write_start(list), pp_sex_nc(H), write(' '), with_indents(false, write_args_as_sexpression(list,[H2])),
    write_end(list), !.

pp_sexi_l([H| T]) :- \+ is_list(T),!, pp_sexi_lc([H | T]).

pp_sexi_l([H, S]) :-
    % If `H` is `'[...]'`, print `S` enclosed in square brackets.
    H == '[...]', write('['), write_args_as_sexpression(S), write(' ]').
pp_sexi_l([H, S]) :-
    % If `H` is `'{...}'`, print `S` enclosed in curly braces.
    H == '{...}', write('{'), write_args_as_sexpression(S), write(' }').
%pp_sex_l(X):- \+ compound(X),!,write_src(X).
%pp_sex_l('$VAR'(S))):-
pp_sexi_l([Eq, H, B]) :- Eq == '=',
    % For lists in the format `[=, H, B]`, format using `pp_sexi_hb/2`.
    pp_sexi_hb(H, B), !.

pp_sexi_l([H | T]):- pp_sexi_lc([H | T]).


pp_sexi_lc([H | T]) :- \+ is_list(T),!,
    %write('#{'), writeq([H|T]), write('}.').
    print_compound_type(0, cmpd, [H|T]),!,
    assertion(\+ is_list(T)).

pp_sexi_lc([H | T]) :-
    % If `V` has more than two elements, print `H` followed by `T` in S-expression format.
    write_start(list), pp_sex_nc(H), write(' '), write_args_as_sexpression(list, T), write_end(list), !.
pp_sexi_lc([H | T]) :-
    % If `H` is a control structure and indents are enabled, apply proper indentation.
    \+ no_src_indents, symbol(H), member(H, ['If', 'cond', 'let', 'let*']), !,
    with_indents(true, w_proper_indent(2, w_in_p(pp_sex([H | T])))).

pp_sexi_lc([H | T]) :-
    % If `T` has 2 or fewer elements, format as S-expression or apply indentation based on length.
    is_list(T), length(T, Args), Args =< 2, fail,
    wots(SS, ((with_indents(false, (write_start(list), pp_sex_nc(H), write(' '), write_args_as_sexpression(list,T), write_end(list)))))),
    ((symbol_length(SS, Len), Len < 20) -> write(SS);
        with_indents(true, w_proper_indent(2, w_in_p(pp_sex_c([H | T]))))), !.


/*

pp_sexi_l([H|T]) :- is_list(T),symbol(H),upcase_atom(H,U),downcase_atom(H,U),!,
   with_indents(false,(write('('), pp_sex_nc(H), write(' '), write_args_as_sexpression(T), write(')'))).

%pp_sex([H,B,C|T]) :- T==[],!,
%  with_indents(false,(write('('), pp_sex(H), write_args_as_sexpression([B,C]), write(')'))).
*/

%!  pp_sexi_hb(+H, +B) is det.
%
%   Formats and prints an equality structure `(= H B)` in S-expression format.
%
%   This predicate writes an equality structure where `H` is printed as the left-hand side
%   and `B` as the right-hand side. If `B` is a list of lists, it is printed inline using
%   `write_src_inl/1` for each sublist. Otherwise, `B` is printed as a single expression
%   using `pp_sex/1`. Indentation settings are adjusted depending on the structure of `B`.
%
%   @arg H The left-hand side of the equality to be printed.
%   @arg B The right-hand side, which may be a list of lists or a single expression.
%
pp_sexi_hb(H, B) :-
    % Begin the equality structure with `(= ` and print `H` inline.
    write('(= '), with_indents(false, pp_sex(H)), write('  '),
    % Check if `B` is a list of lists, and print each sublist inline if true.
    ((is_list(B), maplist(is_list, B))
        -> with_indents(true, maplist(write_src_inl, B))
        % Otherwise, print `B` as a single S-expression.
        ; with_indents(true, pp_sex(B))),
    % Close the equality structure.
    write(')').

%!  write_src_inl(+B) is det.
%
%   Writes a single line of `B` with indentation for inline S-expression formatting.
%
%   This predicate outputs `B` with a newline and a specific indentation level,
%   then prints `B` using `pp_sex/1` to maintain inline formatting within an
%   equality structure.
%
%   @arg B The list item or sublist to be printed on a new line with indentation.
%
%   @example
%     % Print a list item with inline formatting.
%     ?- write_src_inl([bar]).
%         bar
%
write_src_inl(B) :-
    % Start a new line with indentation, then print `B` inline.
    nl, write('    '), pp_sex(B).

%!  pp_sex_c(+V) is det.
%
%   Main predicate for pretty-printing compound terms `V` in S-expression format.
%
%   This predicate delegates to `pp_sexi_c/1`, which handles various cases based on the
%   structure and type of `V`, applying specific formatting for different compound terms.
%
%   @arg V The compound term or list to be printed.
%
pp_sex_c(V) :-
    % Delegate to `pp_sexi_c/1` for detailed formatting of `V`.
    pp_sexi_c(V), !.

%!  pp_sexi_c(+V) is det.
%
%   Helper predicate for pretty-printing compound terms and lists in S-expression format.
%
%   This predicate checks the type and structure of `V`, handling special cases for
%   user modules, execution terms, equality structures, lists, and custom compound terms.
%   Various Prolog structures are transformed into S-expressions with appropriate formatting.
%
%   @arg V The compound term, list, or special structure to be printed.
%
pp_sexi_c(V) :-
    % If `V` is marked for final output, handle it and terminate.
    is_final_write(V), !.
pp_sexi_c((USER:Body)) :- fail,
    % If `V` is in the format `user:Body`, process `Body` directly.
    USER == user, !, pp_sex(Body).
pp_sexi_c(exec(S)) :-
    % For `exec([H | T])` with a list `T`, print as `!` followed by `H` and `T`.
     !, write('!'), pp_sex(S).
pp_sexi_c('!'(S)) :-
    % For `!([H | T])` with a list `T`, print as `!` followed by `H` and `T`.
     !, write('!'), pp_sex(S).
%pp_sexi_c([H|T]) :- is_list(T),!,unlooped_fbug(pp_sexi_c,pp_sex_l([H|T])).
pp_sexi_c([H | T]) :-
    % If `V` is a list starting with `H`, print it as an S-expression list.
    is_list(T), !, pp_sex_l([H | T]).
pp_sexi_c([H | T]) :-
    % If `V` is a list starting with `H`, print it as an S-expression list.
    \+ is_list(T), !, pp_sex_l([H | T]).
%pp_sexi_c(V) :- print(V),!.
pp_sexi_c(=(H, B)) :- fail,
    % If `V` is an equality structure `H = B`, print with `pp_sexi_hb/2`.
    !, pp_sexi_hb(H, B), !.
pp_sexi_c(V) :-
    % For other compound terms, use `write_mobj/2` to print `V`.
    compound_name_list(V, F, Args), write_mobj(F, Args), !.
% Compound terms.
%pp_sex(Term) :- compound(Term), Term =.. [Functor|Args], write('('),format('(~w ',[Functor]), write_args_as_sexpression(Args), write(')').
%pp_sex(Term) :- Term =.. ['=',H|Args], length(Args,L),L>2, write('(= '),  pp_sex(H), write('\n\t\t'), maplist(pp_sex(2),Args).
pp_sexi_c(V) :-
    % If `V` is a simple term or list, delegate to `pp_sex/1`.
    (\+ compound(V) ; is_list(V)), !, pp_sex(V).
pp_sexi_c(listOf(S, _)) :-
    % For `listOf(S, _)`, print using `write_mobj/1`.
    !, write_mobj(listOf(S)).
pp_sexi_c(listOf(S)) :-
    % For `listOf(S)`, print as `(ListValue S)`.
    !, format('(ListValue ~@)', [pp_sex(S)]).
pp_sexi_c('!'(V)) :-
    % For `!(V)`, print `V` prefixed by `!`.
    write('!'), !, pp_sex(V).
%pp_sex_c('exec'(V)) :- write('!'),!,pp_sex(V).
pp_sexi_c('='(N, V)) :-
    % For `N = V`, print in concept format if allowed.
    allow_concepts, !, format("~N;; ~w == ~n", [N]), !, pp_sex(V).
%pp_sex_c(V):- writeq(V).
pp_sexi_c(Term) :- fail,
    % For `Term` with zero arity, format as a single symbol.
    compound_name_arity(Term, F, 0), !, pp_sex_c([F]).


pp_sexi_c(V) :- print_compounds_special,
    compound(V), V\=[_|_], V\=exec(_),
    % If `V` is an empty string, print quoted empty string.
    !, compound_name_arguments(V,Functor,Args),
    always_dash_functor(Functor, DFunctor),
    (symbol_glyph(Functor) -> ExtraSpace = ' ' ; ExtraSpace = ''),
    write_start(cmpd), write(ExtraSpace), write_args_as_sexpression(cmpd, [DFunctor|Args]), write(ExtraSpace), write_end(cmpd), !.
    %maybe_indent_in(Lvl),write('{'), write_args_as_sexpression([F|Args]), write('}'),maybe_indent_out(Lvl).

pp_sexi_c(Term) :-
    % For general compound terms, convert functor to dashed format and print as S-expression.
    Term =.. [Functor | Args],
    always_dash_functor(Functor, DFunctor),
    format('(~w ', [DFunctor]), write_args_as_sexpression(Args), write(')'), !.
pp_sexi_c(Term) :-
    % If concepts are allowed, print `Term` in EvaluationLink format.
    allow_concepts, Term =.. [Functor | Args],
    format('(EvaluationLink (PredicateNode "~w") (ListLink ', [Functor]),
    write_args_as_sexpression(Args), write('))'), !.
pp_sexi_c(Term) :-
    % Default compound term formatting with dashed functor name.
    Term =.. [Functor | Args],
    always_dash_functor(Functor, DFunctor),
    format('(~w ', [DFunctor]), write_args_as_sexpression(Args), write(')'), !.

%!  pp_sexi(+IndentLevel, +Result) is det.
%
%   Pretty-prints `Result` with a specified indentation level.
%
%   This clause handles formatting `Result` with two levels of indentation
%   by adding two tab characters before printing `Result` as an S-expression.
%
%   @arg IndentLevel The level of indentation to be applied (2 in this case).
%   @arg Result The term to be printed.
%
%   @example
%     % Print a result with two levels of indentation.
%     ?- pp_sexi(2, my_term).
%         my_term
%
pp_sexi(2, Result) :-
    % For indentation level 2, print `Result` with tabbed formatting.
    write('\t\t'), pp_sex(Result).

%!  current_column(-Column) is det.
%
%   Retrieves the current column position of the output stream.
%
%   This predicate attempts to obtain the column position from `line_position/2`
%   if available; otherwise, it retrieves it from `stream_position_data/3`.
%
%   @arg Column The current column position of the output stream.
%
%   @example
%     % Get the current column position.
%     ?- current_column(Column).
%
current_column(Column) :-
    % Try getting the column using `line_position/2` if available.
    current_output(Stream),
    line_position(Stream, Column), !.
current_column(Column) :-
    % Fallback method to get column using `stream_position_data/3`.
    stream_property(current_output, position(Position)),
    stream_position_data(column, Position, Column).

%!  min_indent(+Sz) is det.
%
%   Ensures the output is indented to at least `Sz` spaces.
%
%   This predicate calculates the required spaces to reach the minimum
%   indentation level `Sz`, adding spaces or moving to a new line if necessary.
%
%   @arg Sz The minimum indentation level required.
%
%   @example
%     % Ensure at least 10 spaces of indentation.
%     ?- min_indent(10).
%
min_indent(Sz) :-
    % If current column exceeds `Sz`, start a new line and indent.
    current_column(Col), Col > Sz, nl, indent_len(Sz).
min_indent(Sz) :-
    % If indentation is needed, calculate the spaces required and indent.
    current_column(Col), Need is Sz - Col, indent_len(Need), !.
min_indent(Sz) :-
    % Start a new line and indent to `Sz` if starting fresh.
    nl, indent_len(Sz).

%!  indent_len(+Need) is det.
%
%   Writes `Need` number of spaces to the output.
%
%   This predicate ensures precise indentation by writing a specified
%   number of spaces.
%
%   @arg Need The number of spaces to be written.
%
%   @example
%     % Write 5 spaces for indentation.
%     ?- indent_len(5).
%
indent_len(Need) :-
    % Write `Need` spaces by iterating from 1 to `Need`.
    forall(between(1, Need, _), write(' ')).

%!  w_proper_indent(+N, :G) is det.
%
%   Executes a goal `G` with adjusted indentation set to `N` spaces.
%
%   Sets the indentation for `G` based on the current `w_in_p` flag, which
%   indicates nesting depth. The total indentation is calculated as `(X*2)+N`.
%
%   @arg N The additional indentation to be applied.
%   @arg G The goal to be executed with specified indentation.
%
%   @example
%     % Run a goal with 4 spaces of indentation.
%     ?- w_proper_indent(4, writeln('Indented line')).
%
w_proper_indent(N, G) :-
    % Calculate indentation based on the `w_in_p` flag.
    flag(w_in_p, X, X),
    XX is (X * 2) + N,
    % Apply the calculated indentation and execute `G`.
    setup_call_cleanup(min_indent(XX), G, true).

%!  w_in_p(:G) is det.
%
%   Executes a goal `G` with incremented indentation level.
%
%   Increments the `w_in_p` flag, which adjusts indentation depth, then
%   executes `G` and restores the flag.
%
%   @arg G The goal to execute with increased indentation.
%
%   @example
%     % Run a goal with one level of increased indentation.
%     ?- w_in_p(writeln('Indented by level')).
%
w_in_p(G) :-
    % Increment indentation flag, execute `G`, then reset flag.
    setup_call_cleanup(flag(w_in_p, X, X + 1), G, flag(w_in_p, _, X)).

%!  always_dash_functor(+A, -B) is det.
%
%   Ensures `B` is the dashed version of functor `A` if transformation is applicable.
%
%   Attempts to transform `A` to a dashed format using `dash_functor/2`, ensuring
%   that `B` differs from `A` when possible. If transformation is not applicable, `A`
%   is returned unchanged.
%
%   @arg A The original functor.
%   @arg B The functor with dashes if applicable, otherwise `A`.
%
%   @example
%     % Transform an underscore-based functor to dashes.
%     ?- always_dash_functor(my_functor, DFunctor).
%
always_dash_functor(A, B) :-
    % Try transforming `A` with `dash_functor/2`, ensuring `B` is distinct from `A`.
    once(dash_functor(A, B)),
    A \=@= B, !.
always_dash_functor(A, A).

%!  dash_functor(+A, -C) is det.
%
%   Attempts to convert `A` to a dashed functor `C` based on specific conditions.
%
%   This predicate applies a variety of transformations based on symbol structure,
%   prefixes, or existing formats. If `A` does not meet transformation conditions,
%   it returns `A` unchanged. Specific cases include symbols with underscores or
%   special prefixes (e.g., `$` or `%`).
%
%   @arg A The original functor to be converted.
%   @arg C The resulting functor with dashes if transformation applies.
%
%   @example
%     % Attempt to transform a symbol containing underscores.
%     ?- dash_functor(my_functor_with_underscores, DFunctor).
%
dash_functor(A, C) :-
    % If `A` is not a symbol, return `A` unchanged.
    \+ symbol(A), !, C = A.
% dash_functor(A, C):- p2m(A, B), A \== B, !, always_dash_functor(B, C).
dash_functor(ASymbolProc, O) :-
    % Attempt transformation if `ASymbolProc` contains underscores and `atom`.
    fail, symbol_contains(ASymbolProc, '_'),
    symbol_contains(ASymbolProc, 'atom'),
    current_predicate(system:ASymbolProc/_),
    symbolic_list_concat(LS, 'atom', ASymbolProc),
    symbolic_list_concat(LS, 'symbol', SymbolProc),
    always_dash_functor(SymbolProc, O), !.
dash_functor(ASymbolProc, O) :-
    % For symbols prefixed with `$`, replace with `%`.
    symbol_concat('$', LS, ASymbolProc), !,
    symbol_concat('%', LS, SymbolProc),
    always_dash_functor(SymbolProc, O).
dash_functor(Functor, DFunctor) :-
    % For functors with underscores, replace with dashes if appropriate.
    fail,
    symbolic_list_concat(L, '_', Functor), L \= [_],
    symbolic_list_concat(L, '-', DFunctor).

%!  write_args_as_sexpression(+List) is det.
%
%   Prints each element of `List` in S-expression format.
%
%   This predicate handles list elements by iterating through each element, printing
%   them in sequence. For multi-element lists, spaces separate the elements, while
%   single-element lists are printed directly.
%
%   @arg List The list to be printed in S-expression format.
%
%   @example
%     % Print a list as an S-expression.
%     ?- write_args_as_sexpression([a, b, c]).
%     a b c
%
%!  write_args_as_sexpression(+X) is det.
%
%   Prints a list of items `X` in an S-expression format if `X` is a list;
%   otherwise, it prints `X` using the default source format.
%
%   This predicate checks if `X` is a list. If so, it calls `write_args_as_sexpression/1`
%   to display it in S-expression style. If `X` is not a list, it falls back to
%   printing `X` with `write_src/1`.
%
%   @arg X The term or list to be printed.
%
%   @example
%     % Print a list as an S-expression.
%     ?- write_args_as_sexpression([a, b, c]).
%     (a b c)
%
%     % Print a non-list item.
%     ?- write_args_as_sexpression(atom).
%     atom
%
write_args_as_sexpression(Args):- write_args_as_sexpression('|',Args).

write_args_as_sexpression(_,Nil):- Nil == [], !.
%write_args_as_sexpression(H):- w_proper_indent(pp_sex(H)),!.
write_args_as_sexpression(M,Var):- \+ compound(Var),!, write_middle(M),write(' '),pp_sex(Var).
% Print a single-element list directly.
write_args_as_sexpression(_,[H|T]):- T==[], !, pp_sex(H).
% For multi-element lists, print each element separated by a space.
write_args_as_sexpression(M,[H|T]):- pp_sex(H), write(' '), write_args_as_sexpression(M,T).


%!  with_indents(+TF, :Goal) is det.
%
%   Temporarily modifies the `src_indents` option value during the execution of `Goal`.
%
%   This predicate sets the `src_indents` option to `TF` and then executes `Goal` with
%   the modified setting. After `Goal` completes, the option reverts to its previous value.
%
%   @arg TF   The desired value for the `src_indents` option (e.g., `true` or `false`).
%   @arg Goal The goal to be executed with the specified `src_indents` setting.
%
%   @example
%     % Execute a goal with `src_indents` set to true.
%     ?- with_indents(true, writeln('Indented output')).
%
with_indents(TF, Goal) :-
    % Set the value of the `src_indents` option to TF and then execute the Goal.
    as_tf(TF, Value),
    with_option(src_indents, Value, Goal).

%!  no_src_indents is nondet.
%
%   Succeeds if the `src_indents` option is disabled.
%
%   This predicate checks the value of `src_indents` and succeeds if it is set to
%   `'False'`. It is typically used to determine if indentation should be suppressed.
%
%   @example
%     % Check if `src_indents` is disabled.
%     ?- no_src_indents.
%
no_src_indents :-
    % Check if `src_indents` is set to `False`.
    option_else(src_indents, TF, true), !, TF == 'False'.

%!  no_quoting_symbols is nondet.
%
%   Succeeds if the `no_quoting_symbols` option is enabled.
%
%   This predicate checks the value of `no_quoting_symbols` and succeeds if it is
%   set to `'True'`. It is used to suppress quoting symbols during output.
%
%   @example
%     % Check if `no_quoting_symbols` is enabled.
%     ?- no_quoting_symbols.
%
no_quoting_symbols :-
    % Check if `no_quoting_symbols` is set to `True`.
    option_else(no_quoting_symbols, TF, true), !, TF == 'True'.

%!  with_no_quoting_symbols(+TF, :Goal) is det.
%
%   Temporarily modifies the `no_quoting_symbols` option during the execution of `Goal`.
%
%   This predicate sets the `no_quoting_symbols` option to `TF`, executes `Goal`,
%   and then restores the previous setting of `no_quoting_symbols`.
%
%   @arg TF   The desired value for the `no_quoting_symbols` option (e.g., `true` or `false`).
%   @arg Goal The goal to be executed with the specified `no_quoting_symbols` setting.
%
%   @example
%     % Execute a goal with `no_quoting_symbols` set to true.
%     ?- with_no_quoting_symbols(true, writeln('Unquoted output')).
%
with_no_quoting_symbols(TF, Goal) :-
    % Set the value of the `no_quoting_symbols` option to TF and then execute the Goal.
    with_option(no_quoting_symbols, TF, Goal).

% The predicate allow_concepts/0 checks whether the use of concepts is allowed.
% It does this by checking the value of the concepts option and ensuring it is not false.

%!  allow_concepts is nondet.
%
%   Checks whether the use of concepts is allowed.
%
%   This predicate verifies that the `concepts` option is enabled by checking
%   that it is not set to `'False'`. If `concepts` is allowed, this predicate succeeds;
%   otherwise, it fails.
%
%   @example
%     % Check if concepts are allowed.
%     ?- allow_concepts.
%
allow_concepts :-
    % This clause is currently set to fail immediately, with the code below inactive.
    !, fail,
    % Check if the option `concepts` is not set to `False`.
    option_else(concepts, TF, 'False'),
    \+ TF == 'False'.

%!  with_concepts(+TF, :Goal) is det.
%
%   Temporarily enables or disables the use of concepts during the execution of `Goal`.
%
%   This predicate sets the `concepts` option to `TF`, which should be `true` or `false`,
%   and then executes `Goal` with the specified `concepts` setting. Once `Goal` completes,
%   the `concepts` option reverts to its prior state.
%
%   @arg TF   A Boolean indicating whether to enable (`true`) or disable (`false`) concepts.
%   @arg Goal The goal to be executed with the given concepts setting.
%
%   @example
%     % Execute a goal with concepts enabled.
%     ?- with_concepts(true, writeln('Concepts are enabled')).
%
with_concepts(TF, Goal) :-
    % Set the value of the `concepts` option to TF and then execute the Goal.
    with_option(concepts, TF, Goal).

%!  dont_quote(+Atom) is nondet.
%
%   Determines if a symbol `Atom` does not need to be quoted in MeTTa.
%
%   This predicate checks if `Atom` meets specific conditions where quoting is unnecessary:
%   it either has a single character and is punctuation, or is a symbol with consistent
%   uppercase or lowercase casing.
%
%   @arg Atom The symbol to be checked for quoting requirements.
%
%   @example
%     % Check if a symbol does not need quoting.
%     ?- dont_quote('.').
%
dont_quote(Atom) :-
    % Single character symbols that are punctuation do not need quotes.
    symbol_length(Atom, 1), !, char_type(Atom, punct).
dont_quote(Atom) :-
    % Uppercase or lowercase symbols do not need quotes.
    symbol(Atom), upcase_atom(Atom, Atom), downcase_atom(Atom, Atom).

%!  should_quote(+Atom) is nondet.
%
%   Determines if a symbol `Atom` should be quoted in MeTTa.
%
%   This predicate checks if `Atom` requires quoting based on specific conditions.
%   If `Atom` does not meet conditions in `dont_quote/1`, it checks for special
%   characters in `Atom` by breaking it into character lists.
%
%   @arg Atom The symbol to check for quoting requirements.
%
%   @example
%     % Check if a symbol needs quoting.
%     ?- should_quote("My Symbol").
%
should_quote(Atom) :-
    % Fail if `Atom` is neither a symbol nor a string.
    \+ symbol(Atom), \+ string(Atom), !, fail.
should_quote(Atom) :-
    % Check if `Atom` requires quoting based on characters.
    \+ dont_quote(Atom),
    symbol_chars(Atom, Chars),
    once(should_quote_chars(Chars); should_quote_symbol_chars(Atom, Chars)).

%!  contains_unescaped_quote(+Chars) is nondet.
%
%   Checks if a list of characters `Chars` contains an unescaped double-quote.
%
%   This predicate detects unescaped double-quote characters in `Chars`, which
%   may require the entire symbol to be quoted.
%
%   @arg Chars The list of characters to check for unescaped double-quotes.
%
%   @example
%     % Check if a list of characters contains an unescaped double-quote.
%     ?- contains_unescaped_quote(['a', '\\', '"', 'b']).
%
contains_unescaped_quote(['"']) :-
    % Ignore single ending quote.
    !, fail.
contains_unescaped_quote(['"' | _]) :-
    % Unescaped quote found at the start.
    !.
contains_unescaped_quote(['\\', '"' | T]) :-
    % Skip escaped quote and continue checking.
    !, contains_unescaped_quote(T).
contains_unescaped_quote([_ | T]) :-
    % Continue checking the rest of the list.
    contains_unescaped_quote(T).

%!  should_quote_chars(+Chars) is nondet.
%
%   Checks if a list of characters `Chars` contains any characters that
%   would require quoting.
%
%   This predicate checks for specific characters (e.g., spaces, quotes)
%   in `Chars` to determine if quoting is necessary.
%
%   @arg Chars The list of characters to check for quoting requirements.
%
%   @example
%     % Check if characters in a list should be quoted.
%     ?- should_quote_chars(['a', ' ', 'b']).
%
should_quote_chars([]).
should_quote_chars(['"' | Chars]) :-
    % Check for unescaped quotes if starting with a double-quote.
    !, contains_unescaped_quote(Chars).
should_quote_chars(Chars) :-
    % Check if `Chars` contains specific characters that require quoting.
    member('"', Chars);         % Contains double quote
    member(' ', Chars);         % Contains space
    member('''', Chars);        % Contains single quote
    %  member('/', Chars);         % Contains slash
    member(',', Chars);         % Contains comma
    (fail, member('|', Chars)). % Contains pipe

%!  should_quote_symbol_chars(+Atom, +Chars) is nondet.
%
%   Checks if a symbol `Atom` should be quoted based on its character list `Chars`.
%
%   This predicate verifies that `Chars` does not start with a digit or contain
%   numeric-like structures that would otherwise prevent quoting.
%
%   @arg Atom  The symbol to be checked.
%   @arg Chars The list of characters in the symbol.
%
%   @example
%     % Check if a symbol's characters require quoting.
%     ?- should_quote_symbol_chars('123abc', ['1', '2', '3', 'a', 'b', 'c']).
%
should_quote_symbol_chars(Atom, [Digit | _]) :-
    % Prevent quoting if Atom starts with a digit and is not a recognized symbol.
    fail, char_type(Digit, digit), \+ symbol_number(Atom, _).

% Example usage:
% ?- should_quote('123abc').
% true.
% ?- should_quote('123.456').
% false.

% ensure_loaded is a built-in Prolog directive that loads a source file if it hasn't been loaded already.
% Its main purpose is to prevent multiple loadings of the same file, which helps avoid duplicate definitions
% and wasted resources.
/*
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_printer).
%:- ensure_loaded(metta_eval).
*/



% Check if a term is a cons cell (not necessarily a proper list)
is_lcons(X) :- compound(X), X = [_|_].

% Determine if a list is complex based on nesting depth
is_complex_list(N, _) :- N > 1, !.  % Consider complex if nesting exceeds 2
is_complex_list(C, [H|T]) :-
    is_list(H), !, succ(C, CL), is_complex_list(CL, H);
    is_list(T), is_complex_list(C, T).
is_complex_list(_, _) :- !, fail.

simple_print_expr:- true.

% Predicate to print expressions in Lisp-like format
print_sexpr(Expr) :- simple_print_expr,!, pp_sex(Expr), !.
print_sexpr(Expr) :-
    print_sexpr(Expr, 0),  % Start with zero indentation
    nop(nl).  % Ensure a newline after the complete expression for cleaner output

% Base case for empty lists
print_sexpr(T, Indent) :- compound(Indent), catch(Is is Indent,_,fail), !, print_sexpr(T, Is).
print_sexpr(T, Indent) :- T == [], !, print_indent(Indent), write('()').
print_sexpr(exec([H | T]), Indent) :-
        % For `exec([H | T])` with a list `T`, print as `!` followed by `H` and `T`.
        is_list(T), !, write('!'), print_sexpr([H | T], Indent).

print_sexpr(Expr, _) :- simple_print_expr,!, pp_sex(Expr), !.
print_sexpr(Expr, Indent) :- \+ compound(Expr), !,
    print_indent(Indent),
    pp_sex(Expr).

% If Expr is not a cons cell (Print a single element)
print_sexpr(Expr, Indent) :- is_ftVar(Expr), !,
    print_indent(Indent),
    pp_sex(Expr).

% Handling for cons lists
print_sexpr(Expr, Indent) :- is_lcons(Expr),
    (Indent > 0 -> nl, print_indent(Indent); true),
    print_compound_type(Indent, list, Expr),
    (Indent == 0 -> nl; true).

% If Expr is non cons compound
print_sexpr(Expr, Indent) :- compound(Expr),
    once(conjuncts_to_list(Expr,List)), [Expr]\=@=List, is_list(List),
    (Indent > 0 -> nl, print_indent(Indent); true),
    print_compound_type(Indent, cmpd, List),
    (Indent == 0 -> nl; true).

print_sexpr((IF->THEN;ELSE), Indent):- !,
    print_sexpr(if_t_then_else(IF,THEN,ELSE), Indent).
print_sexpr((IF*->THEN;ELSE), Indent):- !,
  print_sexpr(if_then_or_else(IF,THEN,ELSE), Indent).
print_sexpr((IF->THEN), Indent):- !,
  print_sexpr(if_t_then(IF,THEN), Indent).
print_sexpr((IF*->THEN), Indent):- !,
  print_sexpr(if_then(IF,THEN), Indent).
print_sexpr((THEN;ELSE), Indent):- !,
    print_sexpr('or'(THEN,ELSE), Indent).

print_sexpr((Expr :- Body), Indent):- Body==true, !,
  print_sexpr((Expr), Indent).
print_sexpr((M:Expr :- Body), Indent):- atom(M), \+ number(Expr), print_module(M,Indent),!, print_sexpr((Expr :- Body), Indent).
print_sexpr(M:Expr, Indent):- atom(M), \+ number(Expr), print_module(M,Indent),!,print_sexpr(Expr, Indent).
print_sexpr(M:Expr, Indent):- atom(M), number(Expr), print_sexpr(M, Indent),write(':'),!,write(Expr).

print_sexpr((Expr :- Body), Indent):-
  print_sexpr((Expr), Indent),
  write(' '),
  print_sexpr((':-'), Indent+2),
  print_sexpr((Body), Indent+2).

% If Expr is non cons compound
print_sexpr(Expr, Indent) :- compound(Expr),
    (Indent > 0 -> nl, print_indent(Indent); true),
    compound_name_arguments(Expr, H, T),
    print_compound_type(Indent, cmpd, [H|T]),
    (Indent == 0 -> nl; true).

% If Expr is not a cons cell (Print a single element)
print_sexpr(Expr, Indent) :-
    print_indent(Indent),
    pp_sex(Expr).

print_module(M, _Indent):- M == user,!,nl.
print_module(M, _Indent):- write('&'), print_sexpr(M, 0),write(' :\n'),!.

last_item(Item,Item):- \+ is_lcons(Item),!.
last_item([_|T],Last):- T \== [], !, last_item(T,Last).
last_item([Item],Item).

write_start(Type):- current_printer_override(P1),call(P1,'$write_start'(Type)),!.
write_start(Char):- atom_length(Char, 1),write(Char),!.
write_start(Type):- compound_type_s_m_e(Type,L,_,_),write(L),!.
write_middle(Type):- current_printer_override(P1),call(P1,'$write_middle'(Type)),!.
write_middle(Char):- atom_length(Char, 1),write(Char),!.
write_middle(Type):- compound_type_s_m_e(Type,_,M,_),write(M),!.
write_end(Type):- current_printer_override(P1),call(P1,'$write_end'(Type)),!.
write_end(Char):- atom_length(Char, 1),write(Char),!.
write_end(Type):- compound_type_s_m_e(Type,_,_,R),write(R),!.

print_compound_type(Indent, Type, [H|T] ):-
    last_item([H|T],Last),
    write_start(Type),
    (symbol_glyph(H)->write(" ");true),
    NextIndent is Indent + 1,
    print_sexpr(H, 0),
    print_rest_elements(Type, T, NextIndent),
    (symbol_glyph(Last)->write(" ");true),
    write_end(Type),!.

symbol_glyph(A):- atom(A), upcase_atom(A,U),downcase_atom(A,D),!,U==D.


compound_type_s_m_e(list,'(','.',')').
compound_type_s_m_e(cmpd,S,E,M):- prolog_term_start(S),compound_type_s_m_e(ocmpd,S,E,M),!.
compound_type_s_m_e(ocmpd,'#(','.',')').
compound_type_s_m_e(ocmpd,'[','|',']').
compound_type_s_m_e(ocmpd,'{','|','}').
compound_type_s_m_e(ocmpd,'(','@',')').

prolog_term_start('[').

paren_pair_functor('(',')',_).
paren_pair_functor('{','}','{...}').
paren_pair_functor('[',']','[...]').


% Print the rest of the elements in the list, ensuring spacing
print_rest_elements(_,T, _) :- T==[], !.
print_rest_elements(M, T, Indent) :- \+ is_lcons(T), !, write(' '), write(M), write(' '), print_sexpr(T, Indent).
print_rest_elements(M, [H|T], Indent) :-
    write(' '),  % Space before each element after the first
    print_sexpr(H, Indent),
    (is_lcons(H) -> NextIndent is Indent + 0 ; NextIndent is Indent + 0),
    print_rest_elements(M, T, NextIndent).

% Helper predicate to print indentation spaces

print_indent(NewIndent):- current_column(C), C < 4,  !, print_indent_now(NewIndent).
print_indent(NewIndent):- current_column(C), C > 90,  nl, !, min_indent(NewIndent+2).
print_indent(_).
% Helper predicate to print indentation spaces
%print_indent(N) :- !, min_indent(N).
print_indent_now(Indent) :-
    Indent > 0, write(' '),
    NewIndent is Indent - 1, !,
    print_indent_now(NewIndent).
print_indent_now(_).

% Example usage
/*
:- write_src(['parent', ['john', 'mary'], ['likes', 'ice-cream']]).
:- write_src(['level1', ['level2', ['level3a', 'level3b'], 'level2b'], 'level1b']).
:- write_src(['mix', 'of', ['atoms', 'and', ['sub', 'lists']], 'with', 'more', 'atoms']).
:- write_src(['improper', ['list', 'without'] | 'ending properly']).

:- write_src(((
        print_sexpr(Lvl, Expr, Indent) :-
            is_lcons(Expr), Expr = [H|T],
            succ(Lvl, Lvl2),
            min_indent(Indent), write("("),
            NextIndent is Indent + 4,
            print_sexpr(Lvl2, H, NextIndent),
            print_rest_elements(M, Lvl2, T, 0),
            write("("),
            (Indent == 0 -> nl; true)))).
%:- halt.
*/
/*
:- ensure_loaded(library(xlisting)).
:- abolish(xlisting_console:portray_hbr/3).
xlisting_console:portray_hbr(H, B, _R):- !, writeq(H:-B),!,nl.
xlisting_console:portray_hbr(H, B, _R):- B==true, !, write_src_nl(H).
xlisting_console:portray_hbr(H, B, _R):- write_src_nl(H:-B).
*/
