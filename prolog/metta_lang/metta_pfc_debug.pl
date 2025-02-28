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

/*
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/

%*********************************************************************************************
% PROGRAM FUNCTION:  Implements forward chaining, tracks changes, and provides proofs of safety.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(expect_pfc_file, unknown).

% =======================================================
/*
%
%= predicates to examine the state of pfc
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/pfc_list_triggers.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).

%!  pfc_listing_module is det.
%
%   Defines a module `pfc_listing` with a list of exported predicates.
%
%   This predicate is used to define the `pfc_listing` module, which exports a variety of predicates
%   related to PFC (Prolog Forward Chaining) operations. These predicates are responsible for tasks such as
%   listing triggers, printing facts, rules, and handling logic operations. Some predicates related to PFC
%   tracing and debugging are commented out.
%
%   This module is conditionally included based on the status of the `logicmoo_include` flag, which
%   controls whether this specific code should be loaded.
%
%   @example
%   % Define the `pfc_listing` module with several utility predicates:
%   ?- pfc_listing_module.
%
pfc_listing_module :- nop(module(pfc_listing,
          [ draw_line/0,
            loop_check_just/1,
            pinfo/1,
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            pfc_list_triggers_types/1,
            pfc_list_triggers_nlc/1,
            pfc_list_triggers_1/1,
            pfc_list_triggers_0/1,
            pfc_list_triggers/1,
            pfc_contains_term/2,
            pfc_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %pfcWhyBrouse/2,
            %pfcWhy1/1,
            %pfcWhy/1,
            %pfcWhy/0,
            pp_rules/0,
            pfcPrintSupports/0,
            pfcPrintTriggers/0,
            print_db_items/1,
            print_db_items/2,
            print_db_items/3,
            print_db_items/4,
            print_db_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            pfc_listing_file/0
          ])).

%:- include('pfc_header.pi').

:- endif.

%   Operator declarations
%
%   This section defines custom operators to be used in the program.
%
%   - `~` (fx, precedence 500): Unary negation operator.
%   - `==>` (xfx, precedence 1050): Defines an implication or rule operator used in logic programming.
%   - `<==>` (xfx, precedence 1050): Represents bi-conditional equivalence.
%   - `<-` (xfx, precedence 1050): Represents a backward implication or reverse rule.
%   - `::::` (xfx, precedence 1150): A specialized operator often used in Prolog for custom logic.
%
%   These operator declarations define how terms with these symbols are parsed and processed
%   by the Prolog interpreter.
% Operator declarations
:- op(500, fx, '~').                % Unary negation operator
:- op(1050, xfx, ('==>')).          % Implication operator
:- op(1050, xfx, '<==>').           % Bi-conditional equivalence operator
:- op(1050, xfx, ('<-')).           % Backward implication operator
:- op(1100, fx, ('==>')).           % Implication operator (fx variant)
:- op(1150, xfx, ('::::')).         % Specialized operator

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).

%   The `multifile/1` directive allows the specified predicates to have clauses spread across multiple files.
%   This is particularly useful in modular Prolog programs where different components may define or extend the
%   same predicates. The following predicates are declared as multifile in the `user` module:
%
:- multifile((
              user:portray/1,
              user:prolog_list_goal/1,
              user:prolog_predicate_name/2,
              user:prolog_clause_name/2)).

%  `user:portray/1` can be modified (asserted or retracted) during runtime.
:- dynamic user:portray/1.

%:- dynamic(whybuffer/2).

%!  lqu is nondet.
%
%   Lists all clauses of the predicate `que/2`.
%
%   The `lqu/0` predicate uses the built-in `listing/1` predicate to display all clauses
%   currently defined for the predicate `que/2`. It helps in inspecting the facts or rules
%   related to `que/2` that are loaded in the program.
%
%   @example
%   % List all clauses of the predicate que/2:
%   ?- lqu.
%   % Expected output: All defined clauses of que/2.
%
lqu :- listing(que/2).

% Ensure that the file `metta_pfc_base` is loaded.
:- ensure_loaded(metta_pfc_base).
%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author : Dave Matuszek, dave@prc.unisys.com
%   Author : Douglas R. Miles, dmiles@teknowledge.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugging
%   for Pfc.

%  The following predicates can be modified (asserted or retracted) during runtime.
:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic pfcWarnings/1.

%!  pfcDefault(+Option, +DefaultValue) is det.
%
%   Set a default value for a PFC (Prolog Forward Chaining) option.
%
%   This directive sets a default value for the specified PFC option if it has not been defined yet.
%   In this case, it ensures that the `pfcWarnings/1` option has a default value of `true`,
%   which likely enables warnings during PFC operations.
%
%   - `pfcWarnings(_)`: The option related to enabling or disabling PFC warnings.
%   - `pfcWarnings(true)`: Sets the default value for `pfcWarnings/1` to `true`, enabling warnings.
%
%   @arg Option The PFC option to configure.
%   @arg DefaultValue The default value to set if no value is already set.
%
%   @example
%   % Set the default value of pfcWarnings to true:
%   :- pfcDefault(pfcWarnings(_), pfcWarnings(true)).
%
:- pfcDefault(pfcWarnings(_), pfcWarnings(true)).

%!  pfcQueue is nondet.
%
%   Lists all clauses of the predicate `pfcQueue/1`.
%
%   This predicate lists all the clauses currently defined for `pfcQueue/1`,
%   allowing inspection of the Pfc queue contents.
%
%   @example
%   % List all clauses of pfcQueue/1:
%   ?- pfcQueue.
%
pfcQueue :- listing(pfcQueue/1).

%!  pfcPrintDB is nondet.
%
%   Prints the entire Pfc database, including facts, rules, triggers, and supports.
%
%   This predicate calls several sub-predicates to print all facts, rules, triggers,
%   and supports in the Pfc database. It provides a complete overview of the current
%   Pfc knowledge base.
%
%   @example
%   % Print the entire Pfc database:
%   ?- pfcPrintDB.
%
pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports, !.

%!  printLine is nondet.
%
%   Draws a line in the console output for formatting purposes.
%
%   This predicate prints a separator line to the console using ANSI formatting,
%   which can be used for visual separation of output sections.
%
%   @example
%   % Print a separator line:
%   ?- printLine.
%
printLine :- ansi_format([underline], "~N=========================================~n", []).

%!  pfcPrintFacts is nondet.
%
%   Prints all facts in the Pfc database.
%
%   This predicate prints all facts currently in the Pfc database by calling
%   `pfcPrintFacts/2` with a wildcard pattern and a flag to show all facts.
%
%   @example
%   % Print all facts in the Pfc database:
%   ?- pfcPrintFacts.
%
pfcPrintFacts :- pfcPrintFacts(_, true).

%!  pfcPrintFacts(+Pattern) is nondet.
%
%   Prints all facts in the Pfc database that match a given pattern.
%
%   This predicate prints all facts that match the given `Pattern` in the Pfc database.
%   The pattern can be used to filter facts for specific queries.
%
%   @arg Pattern The pattern to match facts against.
%
%   @example
%   % Print facts matching a specific pattern:
%   ?- pfcPrintFacts(my_predicate(_)).
%
pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern, true).

%!  pfcPrintFacts(+Pattern, +Condition) is nondet.
%
%   Prints all facts in the Pfc database that match a given pattern and condition.
%
%   This predicate retrieves facts from the Pfc database that match the given `Pattern`
%   and satisfy the specified `Condition`. The facts are classified into user-added facts
%   and Pfc-added facts, and then printed accordingly. The predicate uses auxiliary
%   predicates to classify and print the facts.
%
%   @arg Pattern   The pattern to match facts against.
%   @arg Condition The condition used to filter facts.
%
%   @example
%   % Print facts matching a pattern and a condition:
%   ?- pfcPrintFacts(my_predicate(_), true).
%
pfcPrintFacts(P, C) :-
  pfcFacts(P, C, L),
  pfcClassifyFacts(L, User, Pfc, _Rule),
  printLine,
  pfcPrintf("User added facts:~n", []),
  pfcPrintitems(User),
  printLine,
  pfcPrintf("MettaLog-Pfc added facts:~n", []),
  pfcPrintitems(Pfc),
  printLine, !.

%!  pfcPrintitems(+List) is det.
%
%   Prints a list of items.
%
%   This predicate prints each item in the provided `List`. It uses `pretty_numbervars/2`
%   to standardize variable names and `portray_clause_w_vars/1` to format and display the items.
%   Note that this predicate modifies its arguments during execution, so care should be taken.
%
%   @arg List The list of items to print.
%
%   @example
%   % Print a list of facts:
%   ?- pfcPrintitems([fact1, fact2]).
%
pfcPrintitems([]).
pfcPrintitems([H|T]) :- \+ \+ ( pretty_numbervars(H, H1), format(" ", []), portray_clause_w_vars(H1)),pfcPrintitems(T).

%!  pfcClassifyFacts(+Facts, -UserFacts, -PfcFacts, -RuleFacts) is det.
%
%   Classifies a list of facts into user-added facts, Pfc-added facts, and rule facts.
%
%   This predicate takes a list of `Facts` and classifies them into three categories:
%   `UserFacts` (facts added by the user), `PfcFacts` (facts added by the Pfc system),
%   and `RuleFacts` (facts that are rules). The classification is based on the type of
%   each fact and its associated support structure.
%
%   @arg Facts      The list of facts to classify.
%   @arg UserFacts  The list of user-added facts.
%   @arg PfcFacts   The list of Pfc-added facts.
%   @arg RuleFacts  The list of rule facts.
%
%   @example
%   % Classify a list of facts:
%   ?- pfcClassifyFacts([fact1, fact2, rule1], User, Pfc, Rule).
%
pfcClassifyFacts([], [], [], []).
pfcClassifyFacts([H|T], User, Pfc, [H|Rule]) :- pfcType(H, rule),!,pfcClassifyFacts(T, User, Pfc, Rule).
pfcClassifyFacts([H|T], [H|User], Pfc, Rule) :- matches_why_UU(UU),pfcGetSupport(H, UU),!,pfcClassifyFacts(T, User, Pfc, Rule).
pfcClassifyFacts([H|T], User, [H|Pfc], Rule) :- pfcClassifyFacts(T, User, Pfc, Rule).

%!  pfcPrintRules is nondet.
%
%   Prints all rules in the Pfc database.
%
%   This predicate prints all the rules currently defined in the Pfc database. It uses
%   `bagof_or_nil/3` to retrieve rules that match different formats (`==>`, `<==>`, and `<-`)
%   and then prints them using `pfcPrintitems/1`. Each set of rules is preceded and followed by
%   a separator line for formatting purposes.
%
%   @example
%   % Print all rules in the Pfc database:
%   ?- pfcPrintRules.
%
pfcPrintRules :-
  printLine,
  pfcPrintf("Rules:...~n", []),
  bagof_or_nil((P==>Q), clause((P==>Q), true), R1),
  pfcPrintitems(R1),
  bagof_or_nil((P<==>Q), clause((P<==>Q), true), R2),
  pfcPrintitems(R2),
  bagof_or_nil((P<-Q), clause((P<-Q), true), R3),
  pfcPrintitems(R3),
  printLine.

%!  pfcGetTrigger(-Trigger) is nondet.
%
%   Retrieves a trigger from the Pfc database.
%
%   This predicate retrieves a trigger from the Pfc database using `pfc_call/1`. The trigger
%   is nondeterministically returned, meaning multiple triggers can be retrieved through
%   backtracking. The retrieved `Trigger` can be any of the types used within the Pfc framework.
%
%   @arg Trigger The retrieved trigger from the Pfc database.
%
%   @example
%   % Retrieve a trigger from the Pfc database:
%   ?- pfcGetTrigger(Trigger).
%
pfcGetTrigger(Trigger) :- pfc_call(Trigger).

%!  pfcPrintTriggers is nondet.
%
%   Pretty prints all triggers in the Pfc database.
%
%   This predicate prints the positive, negative, and goal triggers in the Pfc database.
%   Each set of triggers is printed with a heading and followed by the respective triggers
%   using `print_db_items/2`. Triggers are categorized as positive (`'$pt$'/2`), negative
%   (`'$nt$'/3`), and goal triggers (`'$bt$'/2`).
%
%   @example
%   % Print all triggers in the Pfc database:
%   ?- pfcPrintTriggers.
%
pfcPrintTriggers :-
  print_db_items("Positive triggers", '$pt$'(_, _)),
  print_db_items("Negative triggers", '$nt$'(_, _, _)),
  print_db_items("Goal triggers", '$bt$'(_, _)).

%!  pp_triggers is nondet.
%
%   A shorthand predicate to pretty print all triggers in the Pfc database.
%
%   This predicate is an alias for `pfcPrintTriggers/0`. It provides a shorter way to invoke
%   the trigger printing functionality.
%
%   @example
%   % Pretty print all triggers using the alias:
%   ?- pp_triggers.
%
pp_triggers :- pfcPrintTriggers.

%!  pfcPrintSupports is nondet.
%
%   Pretty prints all supports in the Pfc database.
%
%   This predicate prints all support relationships in the Pfc database. It retrieves the
%   support information using `pfcGetSupport/2` and then pretty-prints the results, filtering
%   out predicates based on the conditions defined in `pp_filtered/1`.
%
%   @example
%   % Print all supports in the Pfc database:
%   ?- pfcPrintSupports.
%
pfcPrintSupports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n", []),
  setof_or_nil((P =< S), (pfcGetSupport(P, S), \+ pp_filtered(P)), L),
  pp_items('Support', L),
  draw_line, !.

%!  pp_supports is nondet.
%
%   Alias for `pfcPrintSupports/0`.
%
%   This predicate serves as a shorthand alias for `pfcPrintSupports/0`, which prints all
%   support relationships in the Pfc database.
%
pp_supports :- pfcPrintSupports.

%!  pp_filtered(+Predicate) is nondet.
%
%   Checks if a predicate should be filtered out from pretty-printing.
%
%   This predicate determines whether a given `Predicate` should be filtered out from
%   pretty-printing during support or fact displays. It filters out certain system predicates,
%   such as those using `pfc_prop/2`.
%
%   @arg Predicate The predicate to check.
%
pp_filtered(P) :- var(P), !, fail.
pp_filtered(_:P) :- !, pp_filtered(P).
pp_filtered(P) :- safe_functor(P, F, A), F \== (/), !, pp_filtered(F/A).
pp_filtered(F/_) :- F == pfc_prop.

%!  pfcFact(+Predicate) is nondet.
%
%   Checks if a fact was asserted into the database via `pfcAdd/2`.
%
%   This predicate checks whether the given `Predicate` was asserted into the Pfc database
%   using `pfcAdd/2`. It uses `pfcFact/2` with a default condition of `true`.
%
%   @arg Predicate The fact to check.
%
pfcFact(P) :- pfcFact(P, true).

%!  pfcFact(+Predicate, +Condition) is nondet.
%
%   Checks if a fact was asserted into the database via `pfcAdd/2` and a condition is satisfied.
%
%   This predicate checks whether the given `Predicate` was asserted into the Pfc database
%   and whether the provided `Condition` holds. The `Condition` can be any logical check
%   on the predicate.
%
%   @arg Predicate The fact to check.
%   @arg Condition The condition to check.
%
%   @example
%   % Check if a fact was asserted and a condition is satisfied:
%   ?- pfcFact(X, pfcUserFact(X)).
%
pfcFact(F, C) :-
  filter_to_pattern_call(F, P, Call),
  pfcFact1(P, C),
  pfcCallSystem(Call).

%!  pfcFact1(+Predicate, +Condition) is nondet.
%
%   Helper predicate for `pfcFact/2`.
%
%   This predicate is a helper for `pfcFact/2`. It checks whether the given `Predicate`
%   satisfies the `Condition` and whether it is a fact in the Pfc database.
%
%   @arg Predicate The fact to check.
%   @arg Condition The condition to check.
%
pfcFact1(P, C) :-
  pfcGetSupport(P, _),
  pfcType(P, fact(_)),
  pfcCallSystem(C).

%!  pfcFacts(-ListofPfcFacts) is det.
%
%   Returns a list of facts added to the Pfc database.
%
%   This predicate returns a list of all facts currently in the Pfc database.
%
%   @arg ListofPfcFacts The list of facts.
%
pfcFacts(L) :- pfcFacts(_, true, L).

%!  pfcFacts(+Pattern, -ListofPfcFacts) is det.
%
%   Returns a list of facts added to the Pfc database that match a given pattern.
%
%   This predicate returns a list of facts in the Pfc database that match the specified `Pattern`.
%
%   @arg Pattern The pattern to match facts against.
%   @arg ListofPfcFacts The list of facts.
%
pfcFacts(P, L) :- pfcFacts(P, true, L).

%!  pfcFacts(+Pattern, +Condition, -ListofPfcFacts) is det.
%
%   Returns a list of facts added to the Pfc database that match a given pattern and condition.
%
%   This predicate returns a list of facts in the Pfc database that match the specified `Pattern`
%   and satisfy the `Condition`.
%
%   @arg Pattern The pattern to match facts against.
%   @arg Condition The condition to filter facts.
%   @arg ListofPfcFacts The list of facts.
%
pfcFacts(P, C, L) :- setof_or_nil(P, pfcFact(P, C), L).

%!  brake(+Predicate) is det.
%
%   Calls a system predicate and breaks execution.
%
%   This predicate calls the specified `Predicate` using `pfcCallSystem/1` and then breaks execution
%   by invoking `ibreak/0` (used for debugging).
%
%   @arg Predicate The predicate to call before breaking.
%
brake(X) :- pfcCallSystem(X), ibreak.

%!  pfcTraceAdd(+Predicate) is det.
%
%   Adds a predicate to the Pfc trace for debugging and monitoring purposes.
%   This allows tracing of the given predicate in the Prolog Forward Chaining (Pfc) system.
%
%   @arg Predicate The predicate to be added to the trace.
%
%   This implementation includes backward compatibility by calling an internal
%   version of `pfcTraceAdd/2`, which may eventually be deprecated.
%
%   @example Adding a predicate to the trace:
%       ?- pfcTraceAdd(my_predicate).
%
pfcTraceAdd(P) :-
    % This is here for upward compatibility with older versions.
    % Should be removed once no longer needed.
    pfcTraceAdd(P, (o, o)).

%!  pfcTraceAdd(+Trigger, +Support) is det.
%
%   Adds a trigger and its support to the Pfc trace for monitoring and debugging purposes.
%   This predicate enables tracing of triggers in the Prolog Forward Chaining (Pfc) system
%   based on the provided trigger and its support. Certain triggers, such as positive and
%   negative triggers, are excluded from tracing.
%
%   @arg Trigger The trigger to be traced. Certain internal triggers (`$pt$` and `$nt$`)
%                are excluded from the trace.
%   @arg Support The support of the trigger, typically providing additional context for
%                the trace operation.
%
%   @example Tracing a custom trigger:
%       ?- pfcTraceAdd(my_trigger, some_support).
%
pfcTraceAdd('$pt$'(_, _), _) :-
    % Never trace positive triggers. These are skipped for tracing.
    !.
pfcTraceAdd('$nt$'(_, _), _) :-
    % Never trace negative triggers. These are skipped for tracing.
    !.
pfcTraceAdd(P, S) :-
    % Print the trigger and its support to the trace output.
    pfcTraceAddPrint(P, S),
    % Optionally break into the debugger for detailed inspection.
    pfcTraceBreak(P, S).

%!  pfcTraceAddPrint(+Predicate, +Support) is det.
%
%   Prints a predicate being added to the Pfc trace if tracing is enabled for the predicate.
%   This predicate checks whether tracing is active for the provided predicate and, if so,
%   prints the predicate and its support information in a formatted manner.
%
%   @arg Predicate The predicate to be printed. If the predicate is being traced, it is printed
%                  with relevant support information.
%   @arg Support The support of the predicate, which can influence how the message is formatted.
%
%   @example Printing a traced predicate:
%       ?- pfcTraceAddPrint(my_predicate, some_support).
%
pfcTraceAddPrint(P, S) :-
    % Check if tracing is enabled for the predicate.
    pfcIsTraced(P),
    !,
    % Create a copy of the predicate with number variables handled.
    \+ \+ (pretty_numbervars(P, Pcopy),
           % Old code to number variables manually is commented out.
           % numbervars(Pcopy,0,_),
           % Check if there is a match for unknown reasons (matches_why_UU/1).
           matches_why_UU(UU),
           % If the support matches, print the predicate with "(u)", otherwise normally.
           (S = UU
            -> pfcPrintf("Adding (u) ~@", [fmt_cl(Pcopy)])
            ;  pfcPrintf("Adding ~@", [fmt_cl(Pcopy)]))).
pfcTraceAddPrint(_, _). % Default case: Do nothing if tracing is not enabled or conditions are not met.

%!  pfcTraceBreak(+Predicate, +Support) is det.
%
%   Breaks the execution if the predicate is spied in the Pfc trace. This is used to
%   halt the program for inspection when a particular predicate is being traced, allowing
%   the user to debug or inspect the state at that point.
%
%   @arg Predicate The predicate to check for spying. If the predicate is spied,
%                  execution breaks.
%   @arg Support The support of the predicate, though it is not used in this case.
%
%   @example Breaking on a spied predicate:
%       ?- pfcTraceBreak(my_predicate, some_support).
%
pfcTraceBreak(P, _S) :-
    % If the predicate is spied, proceed with breaking execution.
    pfcSpied(P, +) ->
       (pretty_numbervars(P, Pcopy),
        % Old code to number variables manually is commented out.
        % numbervars(Pcopy,0,_),
        % Print a message indicating the break and predicate involved.
        pfcPrintf("Breaking on pfcAdd(~p)", [Pcopy]),
        % Trigger the debugger or break the execution for inspection.
        ibreak)
    ; true.

%!  pfcTraceRem(+Trigger) is det.
%
%   Removes a trigger from the Pfc trace, stopping any further tracing for the given trigger.
%   Positive and negative triggers (`$pt$` and `$nt$`) are never traced, and thus are excluded.
%
%   @arg Trigger The trigger to remove from tracing.
%
%   @example Removing a trigger from the trace:
%       ?- pfcTraceRem(my_trigger).
%
pfcTraceRem('$pt$'(_, _)) :-
    % Never trace positive triggers. Simply succeed.
    !.
pfcTraceRem('$nt$'(_, _)) :-
    % Never trace negative triggers. Simply succeed.
    !.
pfcTraceRem(P) :-
    % Check if the predicate is currently being traced.
    (pfcIsTraced(P)
       % If traced, print a message indicating it is being removed.
       -> pfcPrintf("Removing: ~p.", [P])
       ; true),
    % If the predicate is spied, break execution for inspection.
    (pfcSpied(P, -)
       -> (pfcPrintf("Breaking on pfcRem(~p)", [P]),
           ibreak)
       ; true).

%!  pfcIsTraced(+Predicate) is nondet.
%
%   Checks if a predicate is currently being traced in the Pfc trace.
%
%   @arg Predicate The predicate to check. If tracing is active for this predicate,
%                  the predicate succeeds; otherwise, it fails.
%
%   @example Checking if a predicate is traced:
%       ?- pfcIsTraced(my_predicate).
%
pfcIsTraced(P) :-
    % Check if the predicate is not being traced, and fail if so.
    pfcIsNotTraced(P),!, fail.
pfcIsTraced(P) :-
    % Check if the first argument of the compound predicate is being traced.
    compound_eles(1, P, Arg),pfcTraced(Arg).

%!  pfcIsNotTraced(+Predicate) is nondet.
%
%   Checks if a predicate is currently *not* being traced.
%
%   @arg Predicate The predicate to check. If it is not being traced, this predicate succeeds.
%
%   @example Checking if a predicate is not traced:
%       ?- pfcIsNotTraced(my_predicate).
%
pfcIsNotTraced(P) :-
    % Check if the first argument of the compound predicate is ignored.
    compound_eles(1, P, Arg), pfcIgnored(Arg).

%  The pfcIgnored/1 can be modified (asserted or retracted) during runtime.
:- dynamic(pfcIgnored/1).

%!  compound_eles(+Level, +Compound, -Element) is det.
%
%   Extracts elements from a compound term by traversing its structure based on the specified level.
%   The predicate handles variables, compound terms, and attributes.
%
%   @arg Level The level of extraction, determining how deep to traverse the compound term structure.
%   @arg Compound The compound term from which elements are extracted. It can also be a variable with attributes.
%   @arg Element The element that is extracted from the compound term or variable.
%
%   This predicate is used to recursively navigate through compound terms, potentially accessing
%   nested structures depending on the specified level.
%
%   @example Extracting elements from a compound term:
%       ?- compound_eles(1, foo(bar, baz), E).
%       E = foo ;
%       E = bar ;
%       E = baz.
%
%   @example Extracting elements from a term at a deeper level:
%       ?- compound_eles(2, foo(bar(baz)), E).
%       E = baz.
%
compound_eles(Lvl, P, Arg) :-
    % If P is a variable, retrieve its attribute and treat it as a compound term.
    var(P),!,get_attr(P, A, AV),compound_eles(Lvl, attvar(A, AV), Arg).
compound_eles(Lvl, P, Arg) :-
    % If P is not a compound term or the level is less than 1, return P as the result.
    (\+ compound(P); Lvl < 1),!,Arg = P.
compound_eles(Lvl, P, Arg) :-
    % Decrease the level by 1 and recurse into the substructure of P.
    LvlM1 is Lvl - 1,compound_eles(P, E),compound_eles(LvlM1, E, Arg).

%!  compound_eles(+Compound, -Element) is det.
%
%   Extracts elements from a compound term or list by iterating through the list elements
%   or the functor and arguments of a compound term.
%
%   @arg Compound The compound term or list to be decomposed.
%   @arg Element The extracted element, either a member of the list or the functor/arguments
%                of the compound term.
%
%   This predicate handles both lists and compound terms. For lists, it returns each member
%   of the list, and for compound terms, it returns the functor followed by the arguments.
%
%   @example Extracting elements from a list:
%       ?- compound_eles([a, b, c], E).
%       E = a ;
%       E = b ;
%       E = c.
%
%   @example Extracting the functor and arguments from a compound term:
%       ?- compound_eles(foo(bar, baz), E).
%       E = foo ;
%       E = bar ;
%       E = baz.
%
compound_eles(P, E) :-
    % If P is a list, extract each element as E.
    is_list(P),!,member(E, P).
compound_eles(P, E) :-
    % If P is a compound term, extract its functor and arguments.
    compound(P),compound_name_arguments(P, F, Args),!,member(E, [F | Args]).

%!  mpred_trace_exec is det.
%
%   Enables tracing and watching in the Prolog Forward Chaining (Pfc) system.
%   This predicate activates both the `pfcWatch` and `pfcTrace` functionalities,
%   allowing for detailed observation of rule executions and predicate tracing.
%
%   @example Enabling tracing and watching:
%       ?- mpred_trace_exec.
%
mpred_trace_exec :- pfcWatch,pfcTrace.

%!  mpred_notrace_exec is det.
%
%   Disables tracing and watching in the Prolog Forward Chaining (Pfc) system.
%   This predicate deactivates both `pfcTrace` and `pfcWatch` functionalities,
%   stopping the detailed observation of rule executions and predicate tracing.
%
%   @example Disabling tracing and watching:
%       ?- mpred_notrace_exec.
%
mpred_notrace_exec :-
    pfcNoTrace,
    pfcNoWatch.

%!  pfcTrace is det.
%
%   Enables global tracing in the Pfc system. This allows for tracking the execution of
%   rules and predicates without specifying a particular form.
%
%   @example Enabling global tracing:
%       ?- pfcTrace.
%
pfcTrace :-
    pfcTrace(_).

%!  pfcTrace(+Form) is det.
%
%   Enables tracing for a specific form in the Pfc system. This allows for targeted
%   tracing, where only the specified form will be traced during execution.
%
%   @arg Form The form to trace.
%
%   @example Enabling tracing for a specific form:
%       ?- pfcTrace(my_form).
%
pfcTrace(Form) :-
    assert(pfcTraced(Form)).

%!  pfcTrace(+Form, +Condition) is det.
%
%   Enables tracing for a specific form under a given condition in the Pfc system.
%   This allows for conditional tracing, where the form is only traced if the condition holds true.
%
%   @arg Form The form to trace.
%   @arg Condition The condition under which the form will be traced.
%
%   @example Enabling conditional tracing for a form:
%       ?- pfcTrace(my_form, my_condition).
%
pfcTrace(Form, Condition) :-
    assert((pfcTraced(Form) :- Condition)).

%!  pfcSpy(+Form) is det.
%
%   Adds a form to the Pfc spy list, allowing the form to be monitored during execution.
%   By default, the form is spied with both the addition (`+`) and removal (`-`) modes.
%
%   @arg Form The form to spy on.
%
%   @example Spying on a form:
%       ?- pfcSpy(my_form).
%
pfcSpy(Form) :-
    pfcSpy(Form, [+,-], true).

%!  pfcSpy(+Form, +Modes) is det.
%
%   Adds a form to the Pfc spy list with specific modes. The modes determine whether
%   to spy on the form during addition (`+`), removal (`-`), or both.
%
%   @arg Form The form to spy on.
%   @arg Modes The modes to use for spying, e.g., `+` for addition or `-` for removal.
%
%   @example Spying on a form with specific modes:
%       ?- pfcSpy(my_form, [+]).
%
pfcSpy(Form, Modes) :-
    pfcSpy(Form, Modes, true).

%!  pfcSpy(+Form, +Modes, +Condition) is det.
%
%   Adds a form to the Pfc spy list with specific modes and a condition. The form
%   will only be spied upon if the specified condition holds true, providing fine-grained
%   control over when to spy on the form.
%
%   @arg Form The form to spy on.
%   @arg Modes The modes to use for spying, e.g., `+`, `-`.
%   @arg Condition The condition under which to spy on the form.
%
%   @example Spying on a form with modes and a condition:
%       ?- pfcSpy(my_form, [+], my_condition).
%
pfcSpy(Form, [H|T], Condition) :-
    % Recursively process each mode in the list.
    !,
    pfcSpy1(Form, H, Condition),
    pfcSpy(Form, T, Condition).

pfcSpy(Form, Mode, Condition) :-
    % Process the single mode for the form.
    pfcSpy1(Form, Mode, Condition).

%!  pfcSpy1(+Form, +Mode, +Condition) is det.
%
%   Helper predicate for `pfcSpy/3`. It asserts that the form is spied with the
%   given mode and condition, adding the spy rule to the Pfc system.
%
%   @arg Form The form to spy on.
%   @arg Mode The mode to use for spying, e.g., `+` or `-`.
%   @arg Condition The condition under which to spy on the form.
%
pfcSpy1(Form, Mode, Condition) :-
    assert((pfcSpied(Form, Mode) :- Condition)).

%!  pfcNospy is det.
%
%   Removes all forms from the Pfc spy list. This predicate clears all spy points
%   that have been set, stopping any further spying on forms.
%
%   @example Removing all spy points:
%       ?- pfcNospy.
%
pfcNospy :-
    pfcNospy(_,_,_).

%!  pfcNospy(+Form) is det.
%
%   Removes a specific form from the Pfc spy list, effectively stopping spying
%   on the given form.
%
%   @arg Form The form to remove from the spy list.
%
%   @example Removing a specific form from the spy list:
%       ?- pfcNospy(my_form).
%
pfcNospy(Form) :-
    pfcNospy(Form,_,_).

%!  pfcNospy(+Form, +Mode, +Condition) is det.
%
%   Removes a specific form from the Pfc spy list, considering the given mode and condition.
%   This predicate erases the spy points that match the provided form, mode, and condition.
%
%   @arg Form The form to remove from the spy list.
%   @arg Mode The mode to remove (`+`, `-`, or both).
%   @arg Condition The condition under which the form was being spied.
%
%   @example Removing a form with a specific mode and condition:
%       ?- pfcNospy(my_form, +, my_condition).
%
pfcNospy(Form, Mode, Condition) :-
    % Find and erase the matching spy clause.
    clause(pfcSpied(Form, Mode), Condition, Ref),
    erase(Ref),
    fail.

% Ensure pfcNospy succeeds even when no more spy points exist.
pfcNospy(_,_,_).

%!  pfcNoTrace is det.
%
%   Disables all tracing in the Pfc system. This stops any tracing that was
%   previously enabled for forms.
%
%   @example Disabling all tracing:
%       ?- pfcNoTrace.
%
pfcNoTrace :- pfcUntrace.

%!  pfcUntrace is det.
%
%   Untraces all forms in the Pfc system, removing any trace points that were set.
%
%   @example Untracing all forms:
%       ?- pfcUntrace.
%
pfcUntrace :- pfcUntrace(_).

%!  pfcUntrace(+Form) is det.
%
%   Untraces a specific form in the Pfc system, stopping any further tracing for that form.
%
%   @arg Form The form to untrace.
%
%   @example Untracing a specific form:
%       ?- pfcUntrace(my_form).
%
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

%!  pfcTraceMsg(+Message) is det.
%
%   Traces a message in the Pfc system. This is used to output debug or trace messages.
%   By default, it traces a single message without any additional arguments.
%
%   @arg Message The message to trace.
%
%   @example Tracing a simple message:
%       ?- pfcTraceMsg('This is a trace message').
%
pfcTraceMsg(Msg) :-
    pfcTraceMsg('~p', [Msg]).

%!  pfcTraceMsg(+Message, +Arguments) is det.
%
%   Traces a formatted message with arguments in the Pfc system. The message is output
%   if tracing is enabled or if any of the arguments is traced.
%
%   @arg Message The formatted message to trace.
%   @arg Arguments The arguments to include in the message.
%
%   @example Tracing a message with arguments:
%       ?- pfcTraceMsg('Tracing predicate ~p with arg ~p', [my_pred, my_arg]).
%
pfcTraceMsg(Msg, Args) :-
    % If tracing is enabled, print the message with the given arguments.
    pfcTraceExecution,
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(Msg, Args) :-
    % If any argument is traced, print the message.
    member(P, Args), pfcIsTraced(P),
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(_, _).

%!  pfcPrintf(+Message, +Arguments) is det.
%
%   Prints a formatted message to the default output in the Pfc system.
%
%   @arg Message The formatted message to print.
%   @arg Arguments The arguments for the message.
%
%   @example Printing a formatted message:
%       ?- pfcPrintf('Outputting ~p', [my_value]).
%
pfcPrintf(Msg, Args) :- pfcPrintf(user_output, Msg, Args).

%!  pfcPrintf(+Where, +Message, +Arguments) is det.
%
%   Prints a formatted message to a specified output location in the Pfc system.
%
%   @arg Where The output location (e.g., `user_output`).
%   @arg Message The formatted message to print.
%   @arg Arguments The arguments for the message.
%
%   @example Printing a message to user_output:
%       ?- pfcPrintf(user_output, 'Message: ~p', [my_message]).
%
pfcPrintf(Where, Msg, Args) :-
    % Ensure the output is formatted with a newline.
    format(Where, '~N', []),with_output_to(Where, color_g_mesg_ok(blue, format(Msg, Args))).

%!  pfcWatch is det.
%
%   Enables execution tracing in the Pfc system. This will trace the execution
%   of rules and predicates in the system.
%
%   @example Enabling execution tracing:
%       ?- pfcWatch.
%
pfcWatch :- clause(pfcTraceExecution, true),!.
pfcWatch :- assert(pfcTraceExecution).

%!  pfcNoWatch is det.
%
%   Disables execution tracing in the Pfc system. This stops tracing of rule
%   and predicate execution.
%
%   @example Disabling execution tracing:
%       ?- pfcNoWatch.
%
pfcNoWatch :- retractall(pfcTraceExecution).

%!  pfcError(+Message) is det.
%
%   Prints an error message in the Pfc system.
%
%   @arg Message The error message to print.
%
%   @example Printing a simple error message:
%       ?- pfcError('An error occurred').
%
pfcError(Msg) :- pfcError(Msg, []).

%!  pfcError(+Message, +Arguments) is det.
%
%   Prints a formatted error message with arguments in the Pfc system.
%
%   @arg Message The formatted error message.
%   @arg Arguments The arguments to include in the message.
%
%   @example Printing an error message with arguments:
%       ?- pfcError('Error with predicate ~p', [my_predicate]).
%
pfcError(Msg, Args) :- format("~N~nERROR/Pfc: ", []),format(Msg, Args).

% %
% %  These control whether or not warnings are printed at all.
% %    pfcWarn.
% %    nopfcWarn.
% %
% %  These print a warning message if the flag pfcWarnings is set.
% %    pfcWarn(+Message)
% %    pfcWarn(+Message,+ListOfArguments)
% %

%!  pfcWarn is det.
%
%   Enables warning messages in the Pfc system. This predicate sets a flag that allows
%   warning messages to be printed during execution.
%
%   @example Enabling warning messages:
%       ?- pfcWarn.
%
pfcWarn :- retractall(pfcWarnings(_)),assert(pfcWarnings(true)).

%!  nopfcWarn is det.
%
%   Disables warning messages in the Pfc system. This predicate sets a flag that prevents
%   warning messages from being printed during execution.
%
%   @example Disabling warning messages:
%       ?- nopfcWarn.
%
nopfcWarn :- retractall(pfcWarnings(_)),assert(pfcWarnings(false)).

%!  pfcWarn(+Message) is det.
%
%   Prints a warning message in the Pfc system. This is used to output a simple warning message.
%
%   @arg Message The warning message to print.
%
%   @example Printing a warning message:
%       ?- pfcWarn('This is a warning').
%
pfcWarn(Msg) :- pfcWarn('~p', [Msg]).

%!  pfcWarn(+Message, +Arguments) is det.
%
%   Prints a formatted warning message with arguments in the Pfc system. The message will only
%   be printed if warning messages are enabled.
%
%   @arg Message The formatted warning message to print.
%   @arg Arguments The arguments to include in the message.
%
%   @example Printing a warning message with arguments:
%       ?- pfcWarn('Warning for predicate ~p', [my_predicate]).
%
pfcWarn(Msg, Args) :-
    % If warnings are enabled, print the warning message.
    pfcWarnings(true),
    !,
    ansi_format([underline, fg(red)], "~N==============WARNING/Pfc================~n", []),
    ansi_format([fg(yellow)], Msg, Args),
    printLine.
pfcWarn(_, _).

%!  pfcWarnings is det.
%
%   Enables warning messages in the Pfc system by setting the internal flag.
%   This flag causes all Pfc warning messages to be printed.
%
%   @example Enabling warnings:
%       ?- pfcWarnings.
%
pfcWarnings :- retractall(pfcWarnings(_)),assert(pfcWarnings(true)).

%!  pfcNoWarnings is det.
%
%   Disables warning messages in the Pfc system by clearing the internal flag.
%   This flag stops Pfc warning messages from being printed.
%
%   @example Disabling warnings:
%       ?- pfcNoWarnings.
%
pfcNoWarnings :- retractall(pfcWarnings(_)).

%!  pp_facts is nondet.
%
%   Pretty prints all facts in the Pfc database. This predicate outputs a formatted list
%   of all facts currently stored in the Pfc database.
%
%   @example Pretty printing all facts:
%       ?- pp_facts.
%
pp_facts :- pp_facts(_, true).

%!  pp_facts(+Pattern) is nondet.
%
%   Pretty prints facts in the Pfc database that match a given pattern. Only facts that
%   match the specified pattern are printed.
%
%   @arg Pattern The pattern to match facts against.
%
%   @example Pretty printing facts that match a pattern:
%       ?- pp_facts(my_pattern).
%
pp_facts(Pattern) :- pp_facts(Pattern, true).

%!  pp_facts(+Pattern, +Condition) is nondet.
%
%   Pretty prints facts in the Pfc database that match a given pattern and condition.
%   The facts are categorized into user-added facts and system (MettaLog-Pfc) added facts
%   before being printed.
%
%   @arg Pattern The pattern to match facts against.
%   @arg Condition The condition used to filter facts.
%
%   @example Pretty printing facts matching a pattern and condition:
%       ?- pp_facts(my_pattern, my_condition).
%
pp_facts(P, C) :-
    % Retrieve the list of facts that match the pattern and condition.
    pfcFacts(P, C, L),
    % Classify the facts into user-added and Pfc-added categories.
    pfc_classify_facts(L, User, Pfc, _Rule),
    % Draw a line and print user-added facts.
    draw_line,
    fmt("User added facts:", []),
    pp_items(user, User),
    % Draw a separator line.
    draw_line,
    draw_line,
    % Print system-added (MettaLog-Pfc) facts.
    fmt("MettaLog-Pfc added facts:", []),
    pp_items(system, Pfc),
    % Final line to close the output.
    draw_line.

%!  pp_deds is nondet.
%
%   Pretty prints all deduced facts in the Pfc database. Deduced facts are those
%   generated by the system (MettaLog-Pfc) during reasoning.
%
%   @example Pretty printing all deduced facts:
%       ?- pp_deds.
%
pp_deds :-
    pp_deds(_, true).

%!  pp_deds(+Pattern) is nondet.
%
%   Pretty prints deduced facts in the Pfc database that match a given pattern.
%   This predicate filters the deduced facts based on the specified pattern.
%
%   @arg Pattern The pattern to match deduced facts against.
%
%   @example Pretty printing deduced facts matching a pattern:
%       ?- pp_deds(my_pattern).
%
pp_deds(Pattern) :- pp_deds(Pattern, true).

%!  pp_deds(+Pattern, +Condition) is nondet.
%
%   Pretty prints deduced facts in the Pfc database that match a given pattern and condition.
%   The facts are filtered by both the pattern and the condition before being printed.
%
%   @arg Pattern The pattern to match deduced facts against.
%   @arg Condition The condition to filter deduced facts.
%
%   @example Pretty printing deduced facts with a pattern and condition:
%       ?- pp_deds(my_pattern, my_condition).
%
pp_deds(P, C) :-
    % Retrieve the list of deduced facts that match the pattern and condition.
    pfcFacts(P, C, L),
    % Classify the facts, extracting only the Pfc (deduced) facts.
    pfc_classify_facts(L, _User, Pfc, _Rule),
    % Draw a line and print system-added deduced facts.
    draw_line,fmt("MettaLog-Pfc added facts:", []),pp_items(system, Pfc),draw_line.

%!  show_deds_w(+Pattern) is nondet.
%
%   Shows deduced facts in the Pfc database that match a given pattern. This predicate
%   is a wrapper for `pp_deds/2`, focused on displaying deduced facts.
%
%   @arg Pattern The pattern to match deduced facts against.
%
%   @example Showing deduced facts that match a pattern:
%       ?- show_deds_w(my_pattern).
%
show_deds_w(F) :- pp_deds(F).

%!  show_info(+Pattern) is nondet.
%
%   Shows information about facts in the Pfc database that match a given pattern.
%   This predicate retrieves facts that match the specified pattern and classifies
%   them into user-added and system-added (MettaLog-Pfc) facts before displaying them.
%
%   @arg Pattern The pattern to match facts against.
%
%   @example Showing information about facts matching a pattern:
%       ?- show_info(my_pattern).
%
show_info(F) :-
    % Retrieve all facts in the Pfc database.
    pfcFacts(_, true, L),
    % Filter the facts that match the given pattern.
    include(sub_functor(F), L, FL),
    % Classify the filtered facts into user-added and system-added (Pfc) facts.
    pfc_classify_facts(FL, User, Pfc, _Rule),
    % Draw a line and print user-added facts that match the pattern.
    draw_line, fmt("User added facts with ~q:", [F]), pp_items(user, User),
    % Draw separator lines and print system-added (Pfc) facts that match the pattern.
    draw_line, draw_line, fmt("MettaLog-Pfc added facts with ~q:", [F]), pp_items(system, Pfc),
    % Final line to close the output.
    draw_line.

%!  maybe_filter_to_pattern_call(+Pattern, +Predicate, -Condition) is det.
%
%   Converts a pattern and a predicate into a condition for filtering. This predicate creates
%   a condition based on the pattern and predicate provided, which can then be used to filter
%   facts or other data.
%
%   @arg Pattern The pattern used for filtering.
%   @arg Predicate The predicate to apply the filtering to.
%   @arg Condition The resulting condition used for filtering.
%
%   @example Filtering based on a pattern:
%       ?- maybe_filter_to_pattern_call(my_pattern, my_predicate, Condition).
%
maybe_filter_to_pattern_call(F, _, true) :- var(F), !, fail.
maybe_filter_to_pattern_call(F, P, true) :- atom(F), !, (P = F ; freeze(P, (P \== F, sub_functor(F, P)))).
maybe_filter_to_pattern_call(F, P, true) :- \+ compound(F), !, P = _ ; freeze(P, (P \== F, sub_functor(F, P))).
maybe_filter_to_pattern_call(F/A, P, true) :- !, freeze(P, (P \== F, sub_functor(F/A, P))).
% maybe_filter_to_pattern_call(F, P, true) :- P = F.

%!  filter_to_pattern_call(+Pattern, +Predicate, -Condition) is det.
%
%   Converts a pattern and a predicate into a condition for filtering, with alternative handling
%   if the primary method fails. This predicate first attempts to apply `maybe_filter_to_pattern_call/3`
%   and, if it fails, it falls back to `alt_filter_to_pattern_call/3`.
%
%   @arg Pattern The pattern to filter.
%   @arg Predicate The predicate to filter.
%   @arg Condition The resulting condition used for filtering.
%
%   @example Filtering with fallback:
%       ?- filter_to_pattern_call(my_pattern, my_predicate, Condition).
%
filter_to_pattern_call(F, P, Call) :- maybe_filter_to_pattern_call(F, P, Call) *-> true; alt_filter_to_pattern_call(F, P, Call).

%!  alt_filter_to_pattern_call(+Pattern, +Predicate, -Condition) is det.
%
%   Alternative handling for `filter_to_pattern_call/3` in case the primary method fails.
%   It simply checks if the pattern and predicate are the same.
%
%   @arg Pattern The pattern to filter.
%   @arg Predicate The predicate to filter.
%   @arg Condition The resulting condition.
%
%   @example Alternative filtering:
%       ?- alt_filter_to_pattern_call(my_pattern, my_predicate, Condition).
%
alt_filter_to_pattern_call(P, P, true).

%!  sub_functor(+Functor, +Term) is nondet.
%
%   Checks if a term contains a specific functor. This predicate searches through the term
%   to determine if it contains the functor or a similar term with the specified arity.
%
%   @arg Functor The functor to check for.
%   @arg Term The term to check.
%
%   @example Checking if a term contains a functor:
%       ?- sub_functor(my_functor/2, my_term).
%
sub_functor(F-UnF, Term) :- !, sub_functor(F, Term), \+ sub_functor(UnF, Term).
sub_functor(F, Term) :- var(F), !, sub_var_safely(F, Term), !.
sub_functor(F/A, Term) :- !, sub_term_safely(E, Term), compound(E), compound_name_arity(E, F, A).
sub_functor(F, Term) :- sub_term_safely(E, Term), E =@= F, !.
sub_functor(F, Term) :- sub_term_safely(E, Term), compound(E), compound_name_arity(E, FF, AA), (AA == F ; FF == F).

%!  pp_items(+Type, +Items) is nondet.
%
%   Pretty prints a list of items. Each item in the list is printed according to its type.
%
%   @arg Type The type of items (e.g., `user`, `system`) which affects how they are printed.
%   @arg Items The list of items to print.
%
%   @example Pretty printing a list of items:
%       ?- pp_items(user, [item1, item2]).
%
pp_items(_Type, []) :- !.
pp_items(Type, [H|T]) :- ignore(pp_item(Type, H)), !, pp_items(Type, T).
pp_items(Type, H) :- ignore(pp_item(Type, H)).

% Declares `print_mode/1` as a thread-local predicate, meaning each thread can have its
% own value for `print_mode/1`. This is useful in multithreaded environments where
% different threads may need different printing modes without affecting each other.
:- thread_local t_l:print_mode/1.

%!  pp_item(+Mode, +Item) is nondet.
%
%   Pretty prints a single item based on the given printing mode. It handles various types of items,
%   including rules, triggers, and facts, and applies specific formatting rules depending on the mode.
%
%   @arg Mode The mode for printing (e.g., `user`, `system`, or custom modes like `html`).
%   @arg Item The item to print, which can be a fact, rule, or other data structure.
%
%   @example Pretty printing a fact:
%       ?- pp_item(user, my_fact).
%
pp_item(_M, H) :- pp_filtered(H), !.
pp_item(MM, (H :- B)) :- B == true, pp_item(MM, H).
pp_item(MM, H) :- flag(show_asserions_offered, X, X+1), find_and_call(get_print_mode(html)), (\+ \+ if_defined(pp_item_html(MM, H))), !.
pp_item(MM, '$spft$'(W0, U, ax)) :- W = (_KB:W0), !, pp_item(MM, U:W).
pp_item(MM, '$spft$'(W0, F, U)) :- W = (_KB:W0), atom(U), !, fmt('~N%~n', []), pp_item(MM, U:W), fmt('rule: ~p~n~n', [F]), !.
pp_item(MM, '$spft$'(W0, F, U)) :- W = (_KB:W0), !, fmt('~w~nd:       ~p~nformat:    ~p~n', [MM, W, F]), pp_item(MM, U).
pp_item(MM, '$nt$'(Trigger0, Test, Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger(-): ~p~ntest: ~p~nbody: ~p~n', [MM, Trigger, Test, Body]).
pp_item(MM, '$pt$'(F0, Body)) :- F = (_KB:F0), !, fmt('~w p-trigger(+):~n', [MM]), pp_item('', (F:-Body)).
pp_item(MM, '$bt$'(F0, Body)) :- F = (_KB:F0), !, fmt('~w b-trigger(?):~n', [MM]), pp_item('', (F:-Body)).
pp_item(MM, U:W) :- !, format(string(S), '~w  ~w:', [MM, U]), !, pp_item(S, W).
pp_item(MM, H) :- \+ \+ (get_clause_vars_for_print(H, HH), fmt("~w ~p~N", [MM, HH])).

%!  get_clause_vars_for_print(+Clause, -ClauseWithVars) is det.
%
%   Prepares a clause for printing by handling variables. If the clause contains variables,
%   it generates a copy of the clause with variables properly numbered for readability.
%   Ground clauses are returned unchanged.
%
%   @arg Clause The clause to prepare for printing.
%   @arg ClauseWithVars The clause with variables prepared for printing.
%
%   @example Preparing a clause for printing:
%       ?- get_clause_vars_for_print(my_clause(X, Y), ClauseWithVars).
%
get_clause_vars_for_print(HB, HB) :- ground(HB), !.
get_clause_vars_for_print(I, I) :- is_listing_hidden(skipVarnames), fail.
get_clause_vars_for_print(H0, MHB) :- get_clause_vars_copy(H0, MHB), H0 \=@= MHB, !.
get_clause_vars_for_print(HB, HB) :- numbervars(HB, 0, _, [singletons(true), attvars(skip)]), !.

%!  pfc_classify_facts(+Facts, -UserFacts, -PfcFacts, -Rules) is det.
%
%   Classifies a list of facts into user-added facts, system (Pfc) deductions, and rules.
%   This is used to differentiate between facts added directly by the user, those inferred
%   by the Pfc system, and rules.
%
%   @arg Facts The list of facts to classify.
%   @arg UserFacts The list of facts added by the user.
%   @arg PfcFacts The list of facts deduced by the system.
%   @arg Rules The list of classified rules.
%
%   @example Classifying facts:
%       ?- pfc_classify_facts([fact1, fact2, rule1], UserFacts, PfcFacts, Rules).
%
pfc_classify_facts([],[],[],[]).
pfc_classify_facts([H|T],User,Pfc,[H|Rule]) :- pfcType(H,rule), !,pfc_classify_facts(T,User,Pfc,Rule).
pfc_classify_facts([H|T],[H|User],Pfc,Rule) :- pfcGetSupport(H,(mfl4(_VarNameZ,_,_,_),ax)), !,
    pfc_classify_facts(T,User,Pfc,Rule).
pfc_classify_facts([H|T],User,[H|Pfc],Rule) :- pfc_classify_facts(T,User,Pfc,Rule).

%=

%!  print_db_items(+T, +I) is nondet.
%
%   Prints database items with a given title or label. This predicate is used to format and
%   display items from the database, surrounded by separator lines for clarity.
%
%   @arg T The title or label for the items being printed.
%   @arg I The items or goals to be printed.
%
%   @example Printing database items:
%       ?- print_db_items('Facts', [fact1, fact2, fact3]).
%
print_db_items(T, I):-
    draw_line, % Draw a separator line before printing.
    fmt("~N~w ...~n", [T]), % Print the title.
    print_db_items(I), % Print the database items.
    draw_line, % Draw a separator line after printing.
    !.


%=

%!  print_db_items(+I) is nondet.
%
%   Prints database items based on the provided predicate or item. This predicate checks if
%   the input is a valid functor/arity pair or a specific clause and prints matching database
%   entries accordingly.
%
%   @arg I The predicate or item to be printed.
%
%   @example Printing all clauses for a predicate:
%       ?- print_db_items(my_predicate/2).
%
print_db_items(F/A):-
    number(A),!, % Check if A is a number, ensuring F/A is a valid functor/arity pair.
    safe_functor(P,F,A),!, % Safely create a functor from F and A.
    print_db_items(P). % Print the functor.
print_db_items(H):-
    bagof(H, clause(H,true), R1), % Collect all clauses matching H into a list R1.
    pp_items((':'), R1), % Pretty print the collected items.
    R1 \== [], !. % Succeed if the list is non-empty.
print_db_items(H):-
    \+ current_predicate(_,H),!. % Succeed if H is not a current predicate.
print_db_items(H):-
    catch(('$find_predicate'(H,_), call_u(listing(H))), _, true),!, % Try to list the predicate, catching any errors.
    nl, nl. % Print two newlines after listing.

%=

%!  pp_rules is nondet.
%
%   Pretty prints various types of rules and facts from the database. This predicate organizes
%   and prints different rule types (forward, bidirectional, implication, etc.) along with
%   facts (positive and negative).
%
%   @example Pretty printing rules and facts:
%       ?- pp_rules.
%
pp_rules :-
   print_db_items("Forward Rules",(_ ==> _)), % Print forward rules.
   print_db_items("Bidirectional Rules",(_ <==> _)), % Print bidirectional rules.
   print_db_items("Implication Rules",=>(_ , _)), % Print implication rules.
   print_db_items("Bi-conditional Rules",<=>(_ , _)), % Print bi-conditional rules.
   print_db_items("Backchaining Rules",(_ <- _)), % Print backchaining rules.
   print_db_items("Positive Facts",(==>(_))), % Print positive facts.
   print_db_items("Negative Facts",(~(_))). % Print negative facts.

%=

%!  draw_line is nondet.
%
%   Draws a line separator in the console output. This predicate is useful for
%   visually separating different sections of printed information. It only
%   operates in the main thread.
%
%   @example Drawing a line separator:
%       ?- draw_line.
%
draw_line:-
    \+ thread_self_main,!. % Do nothing if not in the main thread.
draw_line:- printLine,!. % Attempt to use printLine to draw a line.
draw_line:-
    (t_l:print_mode(H)->true;H=unknown), % Get the current print mode or set to unknown.
    fmt("~N% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %~n",[]), % Draw the line using format.
    H=H.

:- meta_predicate loop_check_just(0).

%=

%!  loop_check_just(:GoalG) is nondet.
%
%   Performs a loop check for a given goal in the context of justifications.
%   This is used to prevent infinite loops in recursive reasoning or backtracking.
%
%   @arg GoalG The goal to check for loops.
%
%   @example Checking a goal for loops:
%       ?- loop_check_just(my_goal).
%
loop_check_just(G):-
    loop_check(G, ignore(arg(1, G, []))). % Perform loop check, ignoring goals with an empty first argument.

%=

%!  show_pred_info(?PI) is nondet.
%
%   Shows information about a predicate. This includes displaying instances where the functor
%   is a certain type and additional information if the predicate is not imported from another module.
%
%   @arg PI The predicate indicator (F/A) for which information is to be shown.
%
%   @example Showing information about a predicate:
%       ?- show_pred_info(my_predicate/2).
%
show_pred_info(PI):-
   ((
       pi_to_head_l(PI,Head), % Convert predicate indicator to head.
       % doall(show_call(why,call_u(isa(Head,_)))),
        safe_functor(Head,F,_), % Extract the functor from the head.
        doall(show_call(why,call_u(isa(F,_)))), % Show all instances where F is a certain type.
       ((current_predicate(_,M:Head), (\+ predicate_property(M:Head,imported_from(_))))
          -> show_pred_info_0(M:Head); % Show predicate info if not imported.
             wdmsg_pretty(cannot_show_pred_info(Head))))),!. % Display a message if unable to show info.

%=

%!  show_pred_info_0(?Head) is nondet.
%
%   Primary helper for displaying information about a predicate. This predicate shows all properties
%   of the predicate and lists its clauses or a general listing if clauses exist.
%
%   @arg Head The head of the predicate for which information is to be shown.
%
%   @example Showing detailed information for a predicate:
%       ?- show_pred_info_0(my_predicate).
%
show_pred_info_0(Head):-
    doall(show_call(why, predicate_property(Head, _))), % Show all properties of the predicate.
    (has_cl(Head) -> doall((show_call(why, clause(Head, _)))) ; quietly((listing(Head)))), !. % List predicate clauses or show listing.

% ===================================================
% Pretty Print Formula
% ===================================================

%=

%!  print_db_items(?Title, ?Mask, ?What) is nondet.
%
%   Prints database items matching a given mask or pattern, along with a title. This predicate
%   handles the organization and printing of matched database entries under the specified title.
%
%   @arg Title The title to be printed, describing the type of items being displayed.
%   @arg Mask The mask or pattern used to filter the items.
%   @arg What The items to be printed.
%
%   @example Printing database items:
%       ?- print_db_items('Rules', _ ==> _, _).
%
print_db_items(Title, Mask, What):-
    print_db_items(Title, Mask, Mask, What). % Print items with the given title, mask, and what parameters.

%=


%!  print_db_items(+Title, +Mask, +Show, +What) is nondet.
%
%   Prints database items based on a mask, show predicate, and a condition. This predicate filters
%   items matching the mask and the given condition, then applies the show predicate to print them
%   under the specified title.
%
%   @arg Title The title describing the items to be printed.
%   @arg Mask The mask used to filter items (predicate pattern).
%   @arg Show The show predicate that determines how the items are displayed.
%   @arg What The condition to filter items.
%
%   @example Printing database items based on a mask and condition:
%       ?- print_db_items('Rules', _ ==> _, pp_item, _).
%
print_db_items(Title, Mask, Show, What0) :-
     get_pi(Mask, H), get_pi(What0, What),
     format(atom(Showing), '~p for ~p...', [Title, What]),
     statistics(cputime, Now), Max is Now + 2, !,
     gripe_time(1.0,
         doall((once(statistics(cputime, NewNow)), NewNow < Max, clause_or_call(H, B),
             quietly(pfc_contains_term(What, (H:-B))),
             flag(print_db_items, LI, LI+1),
             ignore(quietly(pp_item(Showing, Show)))))),
     ignore(pp_item(Showing, done)), !.

%!  pfc_contains_term(+Term, +Inside) is nondet.
%
%   Checks if a term contains another term. This is used to determine if a specific term
%   is part of a larger structure within another term.
%
%   @arg Term The term to check.
%   @arg Inside The term to look for inside the main term.
%
pfc_contains_term(What, _) :- is_ftVar(What), !.
pfc_contains_term(What, Inside) :- compound(What), !, (\+ \+ ((copy_term_nat(Inside, Inside0), snumbervars(Inside0), occurs:contains_term(What, Inside0)))), !.
pfc_contains_term(What, Inside) :- (\+ \+ once((subst(Inside, What, foundZadooksy, Diff), Diff \=@= Inside ))), !.

%!  hook_pfc_listing(+What) is nondet.
%
%   Hook for Pfc listing. This hook allows for custom listing of items based on the condition `What`.
%
%   @arg What The condition used to filter items for listing.
%
:- current_prolog_flag(pfc_shared_module, BaseKB),
   assert_if_new((BaseKB:hook_pfc_listing(What) :- on_x_debug(pfc_list_triggers(What)))).

% Declares `pfc_list_triggers_disabled/0` as a thread-local predicate, meaning that
% each thread can have its own instance of this predicate. This is useful in
% multithreaded environments where enabling or disabling trigger listing can be
% controlled independently for each thread.
:- thread_local t_l:pfc_list_triggers_disabled/0.

% listing(L):-locally(t_l:pfc_list_triggers_disabled,listing(L)).

%!  pfc_list_triggers(+What) is nondet.
%
%   Lists triggers in the Pfc database that match a given condition. If the predicate
%   `pfc_list_triggers_disabled/0` is set, listing is skipped for that thread.
%
%   @arg What The condition used to filter triggers.
%
%   @example Listing triggers based on a condition:
%       ?- pfc_list_triggers(my_condition).
%
pfc_list_triggers(_) :- t_l:pfc_list_triggers_disabled, !.
pfc_list_triggers(What) :- loop_check(pfc_list_triggers_nlc(What)).

%!  pfc_list_triggers_nlc(+What) is nondet.
%
%   Lists triggers in the Pfc database without performing a loop check. This is a helper
%   predicate for `pfc_list_triggers/1`.
%
%   @arg What The condition used to filter triggers.
%
:-meta_predicate(pfc_list_triggers_nlc(?)).
pfc_list_triggers_nlc(MM:What) :- atom(MM), !, MM:pfc_list_triggers(What).
pfc_list_triggers_nlc(What) :- loop_check(pfc_list_triggers_0(What), true).

%!  pfc_list_triggers_0(+What) is nondet.
%
%   Primary helper for listing triggers in the Pfc database. It handles various patterns for
%   listing triggers.
%
%   @arg What The condition used to filter triggers.
%
pfc_list_triggers_0(What) :- get_pi(What, PI), PI \=@= What, pfc_list_triggers(PI).
pfc_list_triggers_0(What) :- nonvar(What), What = ~(Then), !, \+ \+ pfc_list_triggers_1(Then), \+ \+ pfc_list_triggers_1(What).
pfc_list_triggers_0(What) :- \+ \+ pfc_list_triggers_1(~(What)), \+ \+ pfc_list_triggers_1(What).

%!  pfc_list_triggers_types(-TriggerType) is nondet.
%
%   Lists various trigger types in the Pfc database. This predicate is used to identify
%   different types of triggers available in the system.
%
%   @arg TriggerType The trigger type to list.
%
pfc_list_triggers_types('Triggers').
pfc_list_triggers_types('Instances').
pfc_list_triggers_types('Subclasses').
pfc_list_triggers_types('ArgTypes').
pfc_list_triggers_types('Arity').
pfc_list_triggers_types('Forward').
pfc_list_triggers_types('Bidirectional').
pfc_list_triggers_types('Backchaining').
pfc_list_triggers_types('Negative').
pfc_list_triggers_types('Sources').
pfc_list_triggers_types('Supports').
pfc_list_triggers_types('Edits').

%!  print_db_items_and_neg(+Title, +Fact, +What) is nondet.
%
%   Prints database items and their negations. It prints both the items and their negations
%   based on the specified condition.
%
%   @arg Title The title for the items being printed.
%   @arg Fact The fact to check.
%   @arg What The condition used to filter items.
%
%   @example Printing items and their negations:
%       ?- print_db_items_and_neg('Facts', my_fact, my_condition).
%
print_db_items_and_neg(Title, Fact, What) :- print_db_items(Title, Fact, What).
print_db_items_and_neg(Title, Fact, What) :- print_db_items(Title, ~(Fact), What).

%!  pfc_list_triggers_1(+What) is nondet.
%
%   Secondary helper for listing triggers in the Pfc database. This predicate handles a wide variety of
%   Pfc constructs such as facts, rules, instances, subclasses, and argument types, and organizes them
%   for display based on the specified condition `What`.
%
%   @arg What The condition used to filter triggers.
%
%   @example Listing Pfc triggers:
%       ?- pfc_list_triggers_1(my_condition).
%
pfc_list_triggers_1(What) :- var(What), !.
pfc_list_triggers_1(~(What)) :- var(What), !.
pfc_list_triggers_1(~(_What)) :- !.
pfc_list_triggers_1(What) :-
   print_db_items('Supports User', spft_precanonical(P, mfl4(VarNameZ, _, _, _), ax), '$spft$'(P, mfl4(VarNameZ, _, _, _), ax), What),
   print_db_items('Forward Facts', (nesc(F)), F, What),
   print_db_items('Forward Rules', (_==>_), What),
 ignore((What\= ~(_), safe_functor(What, IWhat, _),
   print_db_items_and_neg('Instance Of', isa(IWhat, _), IWhat),
   print_db_items_and_neg('Instances: ', isa(_, IWhat), IWhat),
   print_db_items_and_neg('Subclass Of', genls(IWhat, _), IWhat),
   print_db_items_and_neg('Subclasses: ', genls(_, IWhat), IWhat))),
   forall(suggest_m(M), print_db_items('PFC Watches', pfc_prop(M, _, _, _), What)),
   print_db_items('Triggers Negative', '$nt$'(_, _, _, _), What),
   print_db_items('Triggers Goal', '$bt$'(_, _, _), What),
   print_db_items('Triggers Positive', '$pt$'(_, _, _), What),
   print_db_items('Bidirectional Rules', (_<==>_), What),
   dif(A, B), print_db_items('Supports Deduced', spft_precanonical(P, A, B), '$spft$'(P, A, B), What),
   dif(G, ax), print_db_items('Supports Nonuser', spft_precanonical(P, G, G), '$spft$'(P, G, G), What),
   print_db_items('Backchaining Rules', (_<-_), What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits', is_edited_clause(_, _, _), What),
   print_db_items('Instances', isa(_, _), What),
   print_db_items('Subclasses', genls(_, _), What),
   print_db_items('Negative Facts', ~(_), What),
   print_db_items('ArgTypes', argGenls(_, _, _), What),
   print_db_items('ArgTypes', argIsa(_, _, _), What),
   print_db_items('ArgTypes', argQuotedIsa(_, _, _), What),
   print_db_items('ArgTypes', meta_argtypes(_), What),
   print_db_items('ArgTypes', predicate_property(G, meta_predicate(G)), What),
   print_db_items('ArgTypes', resultGenls(_, _), What),
   print_db_items('ArgTypes', resultIsa(_, _), What),
   print_db_items('Arity', arity(_, _), What),
   print_db_items('Arity', current_predicate(_), What),
   print_db_items('MetaFacts Predicate', predicate_property(_, _), What),
   print_db_items('Sources', module_property(_, _), What),
   print_db_items('Sources', predicateConventionMt(_, _), What),
   print_db_items('Sources', source_file(_, _), What),
   print_db_items('Sources', _:man_index(_, _, _, _, _), What),
   print_db_items('Sources', _:'$pldoc'(_, _, _, _), What),
   print_db_items('Sources', _:'$pred_option'(_, _, _, _), What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.

%!  pinfo(+Functor_Arity) is nondet.
%
%   Shows information about a predicate for a given functor and arity. This includes listing
%   the predicate definition and displaying its properties.
%
%   @arg Functor_Arity The functor and arity of the predicate (in the form F/A).
%
%   @example Showing information for a predicate:
%       ?- pinfo(my_predicate/2).
%
pinfo(F/A) :-
    listing(F/A), % List the definition of the predicate.
    safe_functor(P, F, A), % Create a functor from F/A.
    findall(Prop, predicate_property(P, Prop), List), % Collect all properties of the predicate.
    wdmsg_pretty(pinfo(F/A) == List), % Display the properties in a formatted way.
    !.

%!  pp_DB is nondet.
%
%   Pretty prints all facts, rules, triggers, and supports in the default module.
%   This predicate iterates through the default module and prints the database content,
%   including facts, rules, and triggers, in a formatted way.
%
%   @example Pretty printing the default module content:
%       ?- pp_DB.
%
%pp_DB:- defaultAssertMt(M),clause_b(mtHybrid(M)),!,pp_DB(M).
%pp_DB:- forall(clause_b(mtHybrid(M)),pp_DB(M)).
pp_DB :- prolog_load_context(module, M), pp_DB(M).

%!  with_exact_kb(+Module, +Goal) is det.
%
%   Executes a goal within the context of a specific module. This predicate ensures that
%   the goal is called with the exact module context provided.
%
%   @arg Module The module context in which the goal will be executed.
%   @arg Goal The goal to execute within the module context.
%
%   @example Executing a goal in a specific module:
%       ?- with_exact_kb(my_module, my_goal).
%
with_exact_kb(M, G) :-
    M:call(G).

%!  pp_DB(+Module) is nondet.
%
%   Pretty prints the Pfc database for a specific module. This includes facts, rules,
%   triggers, and supports stored in the given module.
%
%   @arg Module The module context for which the Pfc database will be printed.
%
%   @example Pretty printing the Pfc database for a module:
%       ?- pp_DB(my_module).
%
pp_DB(M) :-
    with_exact_kb(M, M:must_det_ll((
        pp_db_facts,    % Pretty print facts.
        pp_db_rules,    % Pretty print rules.
        pp_db_triggers, % Pretty print triggers.
        pp_db_supports  % Pretty print supports.
    ))).

%!  pp_db_facts is nondet.
%
%   Pretty prints all facts in the current module Pfc database.
%
%   @example Pretty printing facts in the current module:
%       ?- pp_db_facts.
%
pp_db_facts :- context_module(M),pp_db_facts(M).

%!  pp_db_rules is nondet.
%
%   Pretty prints all rules in the current module Pfc database.
%
%   @example Pretty printing rules in the current module:
%       ?- pp_db_rules.
%
pp_db_rules :- context_module(M),pp_db_rules(M).

%!  pp_db_triggers is nondet.
%
%   Pretty prints all triggers in the current module Pfc database.
%
%   @example Pretty printing triggers in the current module:
%       ?- pp_db_triggers.
%
pp_db_triggers :- context_module(M),pp_db_triggers(M).

%!  pp_db_supports is nondet.
%
%   Pretty prints all supports in the current module Pfc database.
%
%   @example Pretty printing supports in the current module:
%       ?- pp_db_supports.
%
pp_db_supports :- context_module(M),pp_db_supports(M).

% Import and export pp_DB/0 at the system level.
% This allows the predicate to be used across different modules.
:- system:import(pp_DB/0).
:- system:export(pp_DB/0).

%!  pp_db_facts(+Module) is nondet.
%
%   Pretty prints all facts in a specific module Pfc database. This predicate
%   calls the actual printing function while handling any potential errors gracefully.
%
%   @arg Module The module context for which the facts will be printed.
%
pp_db_facts(MM) :- ignore(pp_db_facts(MM, _, true)).

%!  pp_db_facts(+Module, +Pattern) is nondet.
%
%   Pretty prints facts in a specific module Pfc database that match a given pattern.
%
%   @arg Module The module context.
%   @arg Pattern The pattern to match facts against.
%
pp_db_facts(MM, Pattern) :- pp_db_facts(MM, Pattern, true).

%!  pp_db_facts(+Module, +Pattern, +Condition) is nondet.
%
%   Pretty prints facts in a specific module Pfc database that match a given pattern and condition.
%
%   @arg Module The module context.
%   @arg Pattern The pattern to match facts against.
%   @arg Condition The condition to filter facts.
%
pp_db_facts(MM, P, C) :-
  pfc_facts_in_kb(MM, P, C, L),
  pfc_classifyFacts(L, User, Pfc, _ZRule),
  length(User, UserSize), length(Pfc, PfcSize),
  format("~N~nUser added facts in [~w]: ~w", [MM, UserSize]),
  pp_db_items(User),
  format("~N~nSystem added facts in [~w]: ~w", [MM, PfcSize]),
  pp_db_items(Pfc).

%!  pp_db_items(+Items) is det.
%
%   Pretty prints a list of database items.
%
%   @arg Items The list of items to print.
%
pp_db_items(Var) :- var(Var), !, format("~N  ~p", [Var]).
pp_db_items([]) :- !.
pp_db_items([H|T]) :- !,
  % numbervars(H,0,_),
  format("~N  ~p", [H]),
  nonvar(T), pp_db_items(T).
pp_db_items((P >= FT)) :- is_hidden_pft(P, FT), !.
pp_db_items(Var) :- format("~N  ~p", [Var]).

%!  is_hidden_pft(+Predicate, +FactType) is nondet.
%
%   Checks if a fact type should be hidden based on certain criteria.
%
%   @arg Predicate The predicate to check.
%   @arg FactType The fact type to check.
%
is_hidden_pft(_,(mfl4(_VarNameZ, BaseKB, _, _), ax)) :- current_prolog_flag(pfc_shared_module, BaseKB), !.
is_hidden_pft(_,(why_marked(_), ax)).

%!  pp_mask(+Type, +Module, +Mask) is nondet.
%
%   Prints masked items in a module Pfc database.
%
%   @arg Type The type of items.
%   @arg Module The module context.
%   @arg Mask The mask to filter items.
%
pp_mask(Type, MM, Mask) :-
  bagof_or_nil(Mask, lookup_kb(MM, Mask), Nts),
  list_to_set_variant(Nts, NtsSet), !,
  pp_mask_list(Type, MM, NtsSet).

%!  pp_mask_list(+Type, +Module, +List) is nondet.
%
%   Pretty prints a list of masked items.
%
%   @arg Type The type of items.
%   @arg Module The module context.
%   @arg List The list of masked items.
%
pp_mask_list(Type, MM, []) :- !,format("~N~nNo ~ws in [~w]...~n", [Type, MM]).
pp_mask_list(Type, MM, NtsSet) :- length(NtsSet, Size), !,format("~N~n~ws (~w) in [~w]...~n", [Type, Size, MM]),
  pp_db_items(NtsSet).

%!  pfc_classifyFacts(+Facts, -UserFacts, -PfcFacts, -Rules) is det.
%
%   Classifies a list of facts into user facts, Pfc system-added facts, and rule facts.
%
%   @arg Facts The list of facts to classify.
%   @arg UserFacts The output list of user-added facts.
%   @arg PfcFacts The output list of system-added (Pfc) facts.
%   @arg Rules The output list of rule facts.
%
pfc_classifyFacts([], [], [], []).
pfc_classifyFacts([H|T], User, Pfc, [H|Rule]) :-
    pfcType(H, rule(_)), !,
    pfc_classifyFacts(T, User, Pfc, Rule).
pfc_classifyFacts([H|T], [H|User], Pfc, Rule) :-
    get_first_user_reason(H, _UU), !,
    pfc_classifyFacts(T, User, Pfc, Rule).
pfc_classifyFacts([H|T], User, [H|Pfc], Rule) :-
    pfc_classifyFacts(T, User, Pfc, Rule).

%!  pp_db_rules(+Module) is det.
%
%   Pretty prints all types of rules in a specified module. This includes forward rules,
%   bidirectional rules, backchaining rules, implication rules, bi-conditional rules,
%   and negative facts.
%
%   @arg Module The module in which to search for rules.
%
pp_db_rules(MM) :-
   pp_mask("Forward Rule", MM, ==>(_,_)),
   pp_mask("Bidirectional Rule", MM, <==>(_,_)),
   pp_mask("Backchaining Rule", MM, <-(_, _)),
   pp_mask("Implication Rule", MM, =>(_, _)),
   pp_mask("Bi-conditional Rule", MM, <=>(_, _)),
   pp_mask("Negative Fact", MM, (~(_))),
   % Additional rule types can be uncommented if needed.
   % pp_mask("Material-implRule", MM, <=(_, _)),
   % pp_mask("PrologRule", MM, :-( _, _)),
   !.

%!  pp_db_triggers(+Module) is det.
%
%   Pretty prints all triggers in a specific module Pfc database. This includes positive,
%   negative, and goal triggers.
%
%   @arg Module The module context.
%
pp_db_triggers(MM) :-
    pp_mask("Positive trigger(+)", MM, '$pt$'(_, _)),
    pp_mask("Negative trigger(-)", MM, '$nt$'(_, _, _)),
    pp_mask("Goal trigger(?)", MM, '$bt$'(_, _)), !.

%!  pp_db_supports(+Module) is nondet.
%
%   Pretty prints all supports in a specific module Pfc database.
%
%   @arg Module The module context.
%
pp_db_supports(MM) :-
    % temporary hack to print supports
    format("~N~nSupports in [~w]...~n", [MM]),
    with_exact_kb(MM, bagof_or_nil((P >= S), pfcGetSupport(P, S), L)),
    list_to_set_variant(L, LS),
    pp_db_items(LS), !.

%!  list_to_set_variant(+List, -Unique) is det.
%
%   Converts a list to a set, removing variants. Ensures that only unique items are retained.
%
%   @arg List The input list.
%   @arg Unique The output list of unique items.
%
list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique), !.

%!  list_unique_1(+List, +So_far, -Unique) is det.
%
%   Helper predicate for `list_to_set_variant/2`. Iteratively checks each item and builds
%   a list of unique items based on variant equality.
%
%   @arg List The input list.
%   @arg So_far The accumulator for unique items.
%   @arg Unique The output set of unique items.
%
list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far), !,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).

%!  memberchk_variant(+Val, +List) is nondet.
%
%   Checks for membership using =@= (variant equality) rather than unification.
%
%   @arg Val The value to check.
%   @arg List The list in which to check for membership.
%
memberchk_variant(X, [Y|Ys]) :-
    (X =@= Y -> true ; memberchk_variant(X, Ys)).

%!  lookup_kb(+MM, -MHB) is nondet.
%
%   Looks up a clause in the knowledge base for the given module `MM`. The predicate searches
%   for a clause in the specified module and returns the head-body clause `MHB` if found.
%
%   @arg MM The module context to operate within.
%   @arg MHB The head-body clause found.
%
lookup_kb(MM, MHB) :-
 strip_module(MHB,M,HB),
     expand_to_hb(HB, H, B),
      (MM:clause(M:H, B, Ref) *-> true; M:clause(MM:H, B, Ref)),
      %clause_ref_module(Ref),
      clause_property(Ref, module(MM)).

%!  has_cl(+Head) is nondet.
%
%   Checks if a clause exists for a specific predicate head. It uses the `predicate_property/2`
%   to verify if the predicate has any clauses.
%
%   @arg Head The predicate head to check.
%
has_cl(H) :- predicate_property(H, number_of_clauses(_)).

%!  clause_or_call(+H, ?B) is nondet.
%
%   Determines whether a predicate can be called directly or needs to match a clause.
%   The predicate checks if there are more clauses than rules and chooses the appropriate
%   approach to execute or retrieve the clause.
%
%   @arg H The head of the predicate.
%   @arg B The body of the clause or goal to execute.
%
% PFC2.0 clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
% PFC2.0 clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
% PFC2.0 clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H, B) :- clause(src_edit(_Before, H), B).
clause_or_call(H, B) :-
    predicate_property(H, number_of_clauses(C)),
    predicate_property(H, number_of_rules(R)),
    ((R*2 < C) -> (clause(H, B) *-> ! ; fail) ; clause(H, B)).

% PFC2.0 clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).

  /*



% as opposed to simply using clause(H,true).

% %  should_call_for_facts( +H) is nondet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

% %  should_call_for_facts( +VALUE1, ?F, ?VALUE3) is nondet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(pfc_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(pfc_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.

       */

%!  no_side_effects(+Predicate) is nondet.
%
%   Checks if a predicate has no side effects. This is done by checking if side effects are
%   disabled or if the predicate belongs to the list of predicates with side effects.
%
%   @arg Predicate The predicate to check.
%
no_side_effects(P) :- (\+ is_side_effect_disabled -> true;(get_functor(P, F, _), a(prologSideEffects, F))).

%!  pfc_facts_in_kb(+Module, +Pattern, +Condition, -Facts) is det.
%
%   Retrieves facts from a specific module knowledge base.
%
%   @arg Module The module context.
%   @arg Pattern The pattern to match facts against.
%   @arg Condition The condition to filter facts.
%   @arg Facts The retrieved facts.
%
pfc_facts_in_kb(MM, P, C, L) :-
    with_exact_kb(MM, setof_or_nil(P, pfcFact(P, C), L)).

%!  lookup_spft(+Predicate, -Fact, -Type) is nondet.
%
%   Looks up a support fact type for a specific predicate.
%
%   @arg Predicate The predicate to look up.
%   @arg Fact The support fact.
%   @arg Type The support type.
%
lookup_spft(P, F, T) :-
    pfcGetSupport(P, (F, T)).

%!  u_to_uu(+U, -UU) is det.
%
%   Converts a user fact or support to a user fact type (U to UU).
%
%   @arg U The user fact or support.
%   @arg UU The resulting user fact type.
%
u_to_uu(U, (U, ax)) :- var(U), !.
u_to_uu(U, U) :- nonvar(U), U = (_, _), !.
u_to_uu([U|More], UU) :- list_to_conjuncts([U|More], C), !, u_to_uu(C, UU).
u_to_uu(U, (U, ax)) :- !.

%!  get_source_uu(-UU) is det.
%
%   Retrieves the source reference for the current context as a user fact type (UU).
%
%   @arg UU The retrieved source reference.
%
:- module_transparent((get_source_uu)/1).
get_source_uu(UU) :- must_ex((get_source_ref1(U), u_to_uu(U, UU))), !.

%!  get_source_ref1(-U) is det.
%
%   Retrieves the source reference for the current context.
%
%   @arg U The retrieved source reference.
%
get_source_ref1(U) :- quietly_ex((current_why(U), nonvar(U)));ground(U), !.
get_source_ref1(U) :- quietly_ex((get_source_mfl(U))), !.

%!  get_why_uu(-UU) is det.
%
%   Retrieves the current "why" reference as a user fact type (UU).
%
%   @arg UU The retrieved user fact type.
%
:- module_transparent((get_why_uu)/1).
get_why_uu(UU) :- findall(U, current_why(U), Whys),Whys \== [], !,u_to_uu(Whys, UU).
get_why_uu(UU) :- get_source_uu(UU), !.

%!  get_startup_uu(-UU) is det.
%
%   Retrieves the startup "why" reference as a user fact type (UU).
%
%   @arg UU The retrieved user fact type.
%
get_startup_uu(UU) :- prolog_load_context(module, CM),u_to_uu((isRuntime, mfl4(VarNameZ, CM, user_input, _)), UU),
    varnames_load_context(VarNameZ).

%!  is_user_reason(+UserFact) is nondet.
%
%   Checks if a user fact is a valid user reason.
%
%   @arg UserFact The user fact to check.
%
is_user_reason((_, U)) :- atomic(U).

only_is_user_reason((U1, U2)) :- freeze(U2, is_user_reason((U1, U2))).

%!  is_user_fact(+Predicate) is nondet.
%
%   Checks if a predicate is a user-added fact.
%
%   @arg Predicate The predicate to check.
%
is_user_fact(P) :- get_first_user_reason(P, UU),is_user_reason(UU).

%!  get_first_real_user_reason(+Predicate, -UU) is nondet.
%
%   Retrieves the first real user reason for a predicate.
%
%   @arg Predicate The predicate to check.
%   @arg UU The retrieved user reason.
%
get_first_real_user_reason(P, UU) :- nonvar(P), UU = (F, T),
  quietly_ex(((((lookup_spft(P, F, T))), is_user_reason(UU)) *-> true;
    ((((lookup_spft(P, F, T))), \+ is_user_reason(UU)) *-> (!, fail) ; fail))).

%!  get_first_user_reason(+Predicate, -UU) is nondet.
%
%   Retrieves the first user reason for a predicate. This predicate checks various sources
%   (such as lookup tables, asserted clauses, and source locations) to determine the first
%   user reason associated with the given predicate.
%
%   @arg Predicate The predicate to check.
%   @arg UU The retrieved user reason, consisting of a source reference and a term.
%
get_first_user_reason(P, (F, T)) :-
  UU = (F, T),
  ((((lookup_spft(P, F, T))), is_user_reason(UU)) *-> true;
    ((((lookup_spft(P, F, T))), \+ is_user_reason(UU)) *-> (!, fail) ;
       (clause_asserted(P), get_source_uu(UU), is_user_reason(UU)))), !.
get_first_user_reason(_, UU) :- get_why_uu(UU), is_user_reason(UU), !.
get_first_user_reason(_, UU) :- get_why_uu(UU), !.
get_first_user_reason(P, UU) :- must_ex(ignore((get_first_user_reason0(P, UU)))), !.
%get_first_user_reason(_,UU):- get_source_uu(UU),\+is_user_reason(UU). % ignore(get_source_uu(UU)).

%!  get_first_user_reason0(+Predicate, -UU) is nondet.
%
%   Helper predicate for `get_first_user_reason/2`. This predicate retrieves the source
%   reference (user reason) for a given predicate by calling `get_source_mfl/1`.
%
%   @arg Predicate The predicate to check.
%   @arg UU The retrieved user reason, which consists of the source reference and the 'ax' marker.
%
get_first_user_reason0(_, (M, ax)) :- get_source_mfl(M).

%:- export(pfc_at_box:defaultAssertMt/1).
%:- system:import(defaultAssertMt/1).
%:- pfc_lib:import(pfc_at_box:defaultAssertMt/1).

%!  get_source_mfl(-MFL) is det.
%
%   Retrieves the source reference for the current module/file location. This includes
%   the module, file, line number, and variable names.
%
%   @arg MFL The retrieved source reference, typically of the form `mfl4/4`.
%
:- module_transparent((get_source_mfl)/1).
get_source_mfl(M):- current_why(M), nonvar(M) , M =mfl4(_VarNameZ,_,_,_).
get_source_mfl(mfl4(VarNameZ, M, F, L)) :- defaultAssertMt(M), current_source_location(F, L), varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ, M, F, L)) :- defaultAssertMt(M), current_source_file(F:L), varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ, M, F, _L)) :- defaultAssertMt(M), current_source_file(F), varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ, M, _F, _L)) :- defaultAssertMt(M), varnames_load_context(VarNameZ).
%get_source_mfl(M):-(defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_mfl(M):-fail,dtrace,
((defaultAssertMt(M)->!;
(atom(M)->(module_property(M,class(_)),!);
pfcError(no_source_ref(M))))).

%!  is_source_ref1(+Term) is nondet.
%
%   Placeholder predicate to check if a term is a source reference.
%   Currently, it accepts any term but can be expanded for specific logic.
%
is_source_ref1(_).

%!  defaultAssertMt(-Module) is det.
%
%   Retrieves the current module context during loading. This predicate
%   provides the default module where assertions will be made.
%
%   @arg Module The module being loaded, retrieved from the current Prolog context.
%
defaultAssertMt(M) :- prolog_load_context(module, M).

%!  pfc_pp_db_justifications(+Predicate, +Justifications) is det.
%
%   Pretty prints the justifications for a predicate.
%
%   @arg Predicate The predicate to print justifications for.
%   @arg Justifications The list of justifications to print.
%
pfc_pp_db_justifications(P, Js) :-
    show_current_source_location,
    must_ex(quietly_ex((format("~NJustifications for ~p:", [P]),
        pfc_pp_db_justification1('', Js, 1)))).

%!  pfc_pp_db_justification1(+Prefix, +Justifications, +N) is det.
%
%   Helper predicate for `pfc_pp_db_justifications/2`. This predicate recursively
%   prints the justifications with numbered steps.
%
%   @arg Prefix The prefix for printing.
%   @arg Justifications The list of justifications to print.
%   @arg N The current justification number.
%
pfc_pp_db_justification1(_, [], _).
pfc_pp_db_justification1(Prefix, [J|Js], N) :-
    nl,
    pfc_pp_db_justifications2(Prefix, J, N, 1),
    N2 is N + 1,
    pfc_pp_db_justification1(Prefix, Js, N2).

%!  pfc_pp_db_justifications2(+Prefix, +Justification, +JustNo, +StepNo) is det.
%
%   Helper predicate for `pfc_pp_db_justification1/3`. This predicate prints individual
%   steps of a justification, handling sub-justifications as necessary.
%
%   @arg Prefix The prefix for printing.
%   @arg Justification The justification to print.
%   @arg JustNo The current justification number.
%   @arg StepNo The current step number.
%
pfc_pp_db_justifications2(_, [], _, _).
pfc_pp_db_justifications2(Prefix, [C|Rest], JustNo, StepNo) :-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
 ((StepNo==1->fmt('~N~n',[]);true),
  backward_compatibility:sformat(LP,' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(pfcWhy_sub_sub(C))),
  StepNext is 1+StepNo,
  pfc_pp_db_justifications2(Prefix,Rest,JustNo,StepNext))).

%!  pfcWhy_sub_sub(+Predicate) is det.
%
%   Sub-function for `pfcWhy` to handle sub-subjustifications, printing nested justifications
%   as needed.
%
%   @arg Predicate The predicate to check.
%
pfcWhy_sub_sub(P) :-
  justifications(P, Js),
  clear_proofs,
  % retractall_u(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed', P) -> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P, Js)),
   nb_getval('$last_printed', LP),
   ((pfc_pp_db_justification1(LP, Js, 1), fmt('~N~n', [])))))).

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for browsing justifications *****

% Import the lists library for list processing.
:- use_module(library(lists)).

% Declare `t_l:whybuffer/2` as a dynamic predicate, allowing it to be modified during runtime.
:- dynamic(t_l:whybuffer/2).

%!  pfcWhy is nondet.
%
%   Interactively explores Pfc justifications. This predicate calls `pfcWhy/1`
%   with the current predicate stored in `whybuffer`.
%
pfcWhy :-
    t_l:whybuffer(P, _),
    pfcWhy(P).

%!  pfcTF(+Predicate) is nondet.
%
%   Prints the truth value of a predicate. It first checks if the predicate is true using
%   `pfc_call/1`, and then prints its truth value using `pfcTF1/1`.
%
%   @arg Predicate The predicate to check.
%
pfcTF(P) :-
    pfc_call(P) *-> foreach(pfcTF1(P), true); pfcTF1(P).

%!  pfcTF1(+Predicate) is nondet.
%
%   Helper predicate for `pfcTF/1`. This predicate prints the truth value and explores
%   both the truth (`P`) and negation (`~P`) of the predicate interactively.
%
%   @arg Predicate The predicate to check.
%
pfcTF1(P) :-
    ansi_format([underline], "~N=========================================", []),
    (ignore(pfcWhy(P))),
    ignore(pfcWhy(~P)),
    printLine.

%!  pfcWhy(+N) is nondet.
%!  pfcWhy(+Predicate) is nondet.
%
%   Interactively explores the Nth justification for a predicate or explores all justifications
%   for a predicate if no number is provided.
%
%   @arg N The justification number to explore.
%   @arg Predicate The predicate to explore.
%
pfcWhy(N) :- number(N), !,t_l:whybuffer(P, Js),pfcWhyCommand(N, P, Js).
pfcWhy(P) :- justifications(P, Js),retractall(t_l:whybuffer(_,_)),assert(t_l:whybuffer(P, Js)),
    pfcWhyBrouse(P, Js).

%!  pfcWhy1(+Predicate) is nondet.
%
%   Interactively explores the first justification for a predicate.
%
%   @arg Predicate The predicate to explore.
%
pfcWhy1(P) :- justifications(P, Js),pfcWhyBrouse(P, Js).

%!  pfcWhy2(+Predicate, +N) is nondet.
%
%   Interactively explores the Nth justification for a predicate.
%
%   @arg Predicate The predicate to explore.
%   @arg N The justification number to explore.
%
pfcWhy2(P, N) :- justifications(P, Js),pfcShowJustification1(Js, N).

%!  pfcWhyBrouse(+Predicate, +Justifications) is nondet.
%
%   Interactively explores justifications for a predicate. This predicate shows the
%   justifications for the given predicate and waits for the user to input a command
%   to continue exploring or modify the exploration.
%
%   @arg Predicate The predicate to explore.
%   @arg Justifications The justifications to explore.
%
pfcWhyBrouse(P, Js) :-
  % rtrace(pfc_pp_db_justifications(P,Js)),
  pfcShowJustifications(P, Js),
  nop((pfcAsk(' >> ', Answer),
  pfcWhyCommand(Answer, P, Js))).

%!  pfcWhyCommand(+Command, +Predicate, +Justifications) is nondet.
%
%   Executes a command during Pfc justification exploration. This predicate processes
%   different commands, such as quitting, viewing help, focusing on a specific justification,
%   or navigating through steps.
%
%   @arg Command The command to execute (e.g., `q` for quit, `h` for help).
%   @arg Predicate The predicate being explored.
%   @arg Justifications The justifications being explored.
%
pfcWhyCommand(q, _, _) :- !. % Quit.
pfcWhyCommand(h, _, _) :- !, % Help.
  format("~nJustification Browser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M browse step M of the Nth justification
 u   up a level~n", []).
pfcWhyCommand(N, _P, Js) :- float(N), !,
  pfcSelectJustificationNode(Js, N, Node),
  pfcWhy1(Node).

pfcWhyCommand(u, _, _) :- !. % Up a level.
%!  pfcCommand(+Command, +Predicate, +Justifications) is nondet.
%
%   Handles commands during Pfc justification exploration. This predicate checks if a given
%   command is implemented or recognized and provides appropriate feedback.
%
%   @arg Command The command to execute.
%   @arg Predicate The predicate being explored (not used in these cases).
%   @arg Justifications The justifications being explored (not used in these cases).
%
pfcCommand(N, _, _) :-
    integer(N), !,
    pfcPrintf("~p is a yet unimplemented command.", [N]),
    fail.
pfcCommand(X, _, _) :-
    pfcPrintf("~p is an unrecognized command, enter h. for help.", [X]),
    fail.

%!  pfcShowJustifications(+Predicate, +Justifications) is nondet.
%
%   Pretty prints the justifications for a given predicate. This predicate formats and
%   prints each justification associated with the predicate.
%
%   @arg Predicate The predicate to print justifications for.
%   @arg Justifications The list of justifications to print.
%
pfcShowJustifications(P, Js) :-
  show_current_source_location,
  reset_shown_justs,
  %color_line(yellow,1),
  format("~N~nJustifications for ", []),
  ansi_format([fg(green)], '~@', [pp(P)]),
  format(" :~n", []),
  pfcShowJustification1(Js, 1),!,
  printLine.

%!  pfcShowJustification1(+Justifications, +N) is nondet.
%
%   Pretty prints the Nth justification in a list of justifications. This predicate
%   recursively prints each justification, incrementing the justification number.
%
%   @arg Justifications The list of justifications to print.
%   @arg N The current justification number.
%
pfcShowJustification1([J|Js], N) :- !,
  % show one justification and recurse.
  %reset_shown_justs,
  pfcShowSingleJustStep(N, J),!,
  N2 is N+1,
  pfcShowJustification1(Js, N2).
pfcShowJustification1(J, N) :-
  %reset_shown_justs, % nl,
  pfcShowSingleJustStep(N, J),!.

%!  pfcShowSingleJustStep(+JustNo, +Justification) is nondet.
%
%   Pretty prints a single step in a justification.
%   This predicate handles the formatting and printing of the justification step.
%
%   @arg JustNo The justification number.
%   @arg Justification The justification step to print.
%
pfcShowSingleJustStep(N, J) :- pfcShowSingleJust(N, step(1), J),!.
pfcShowSingleJustStep(N, J) :- pp(pfcShowSingleJustStep(N, J)),!.

%!  incrStep(+StepNo, -Step) is det.
%
%   Increments the step number in a justification.
%   This predicate updates the step number by incrementing it by 1.
%
%   @arg StepNo The current step number.
%   @arg Step The incremented step number.
%
incrStep(StepNo, Step) :- compound(StepNo), arg(1, StepNo, Step), X is Step+1, nb_setarg(1, StepNo, X).

%!  pfcShowSingleJust(+JustNo, +StepNo, +Justification) is nondet.
%
%   Pretty prints a single justification step. The predicate handles various formats of
%   justification steps, including conjunctions, conditionals, and Prolog clauses.
%
%   @arg JustNo The justification number.
%   @arg StepNo The step number within the justification.
%   @arg Justification The justification step to print.
%
pfcShowSingleJust(JustNo, StepNo, C) :- is_ftVar(C), !, incrStep(StepNo, Step),
  ansi_format([fg(cyan)], "~N    ~w.~w ~w ", [JustNo, Step, C]), !, maybe_more_c(C).
pfcShowSingleJust(_JustNo,_StepNo,[]):-!.
pfcShowSingleJust(JustNo, StepNo, (P, T)) :- !,
  pfcShowSingleJust(JustNo, StepNo, P),
  pfcShowSingleJust(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, (P, F, T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P),
  pfcShowSingleJust(JustNo, StepNo, F),
  pfcShowSingleJust1(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, (P *-> T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P), format('      *-> ', []),
  pfcShowSingleJust1(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, (P :- T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P), format(':- ~p.', [T]).
pfcShowSingleJust(JustNo, StepNo, (P : - T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P), format('      :- ', []),
  pfcShowSingleJust(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, (P :- T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, call(T)),
  pfcShowSingleJust1(JustNo, StepNo, P).
pfcShowSingleJust(JustNo, StepNo, [P|T]) :- !,
  pfcShowSingleJust(JustNo, StepNo, P),
  pfcShowSingleJust(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, '$pt$'(P, Body)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, '$pt$'(P)),
  pfcShowSingleJust(JustNo, StepNo, Body).
pfcShowSingleJust(JustNo, StepNo, C) :-
  pfcShowSingleJust1(JustNo, StepNo, C).

%!  fmt_cl(+Clause) is det.
%
%   Formats and writes a clause to the output. It uses various formatting strategies,
%   such as handling variables, pretty printing, and special term portrayals.
%
%   @arg Clause The clause to format and write.
%
fmt_cl(P) :- \+ \+ (numbervars(P, 666, _, [attvars(skip), singletons(true)]), write_src(P)), !.
fmt_cl(P) :- \+ \+ (pretty_numbervars(P, PP), numbervars(PP, 126, _, [attvar(skip), singletons(true)]),
   write_term(PP, [portray(true), portray_goal(fmt_cl)])), write('.').
fmt_cl(S,_):- term_is_ansi(S), !, write_keeping_ansi(S).
fmt_cl(G,_):- is_grid(G),write('"'),user:print_grid(G),write('"'),!.
% fmt_cl(P,_):- catch(arc_portray(P),_,fail),!.
fmt_cl(P,_):- is_list(P),catch(p_p_t_no_nl(P),_,fail),!.
%ptg(PP,Opts):- is_list(PP),select(portray_goal(ptg),Opts,Never),write_term(PP,Never).

%!  unwrap_litr(+Clause, -UnwrappedClause) is det.
%
%   Unwraps a literal clause to its core form. This predicate is used to simplify
%   nested terms such as `call/1`, `'$pt$'/1`, and similar wrappers.
%
%   @arg Clause The clause to unwrap.
%   @arg UnwrappedClause The unwrapped version of the clause.
%
unwrap_litr(C, CCC+VS) :-
    copy_term(C, CC, VS),
    numbervars(CC+VS, 0, _),
    unwrap_litr0(CC, CCC), !.
unwrap_litr0(call(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0('$pt$'(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0(body(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0(head(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0(C, C).


% Declares `shown_why/1` as a thread-local predicate, meaning that each thread can have its
% own instance of `shown_why/1`. This allows different threads to track which justifications
% have been shown independently of one another.
:- thread_local t_l:shown_why/1.

%!  pfcShowSingleJust1(+JustNo, +StepNo, +Clause) is det.
%
%   Pretty prints a single clause in a justification. This predicate processes the clause and,
%   if necessary, unwraps the literal before printing.
%
%   @arg JustNo The justification number.
%   @arg StepNo The step number in the justification.
%   @arg Clause The clause to print.
%
pfcShowSingleJust1(JustNo, _, MFL) :- is_mfl(MFL), JustNo \== 1, !.
pfcShowSingleJust1(JustNo, StepNo, C) :-
    unwrap_litr(C, CC), !,
    pfcShowSingleJust4(JustNo, StepNo, C, CC).

%!  pfcShowSingleJust4(+JustNo, +StepNo, +Clause, +UnwrappedClause) is det.
%
%   Helper predicate for `pfcShowSingleJust1/3`. It handles printing the clause and its unwrapped version.
%
%   @arg JustNo The justification number.
%   @arg StepNo The step number in the justification.
%   @arg Clause The clause to print.
%   @arg UnwrappedClause The unwrapped version of the clause.
%
pfcShowSingleJust4(_, _, _, CC) :- t_l:shown_why(C), C =@= CC, !.
pfcShowSingleJust4(_, _, _, MFL) :- is_mfl(MFL), !.
pfcShowSingleJust4(JustNo, StepNo, C, CC) :- assert(t_l:shown_why(CC)), !,
   incrStep(StepNo, Step),
   ansi_format([fg(cyan)], "~N    ~w.~w ~@ ", [JustNo, Step, user:fmt_cl(C)]),
   %write('<'),
   pfcShowSingleJust_C(C),!,%write('>'),
   format('~N'),
   ignore((maybe_more_c(C))),
   assert(t_l:shown_why(C)),
   format('~N'), !.

%!  is_mfl(+Term) is nondet.
%
%   Checks if a term is an mfl (module/file/line) reference. An mfl reference is represented
%   as `mfl4/4`.
%
%   @arg Term The term to check.
%
is_mfl(MFL) :- compound(MFL), MFL = mfl4(_, _, _, _).

%!  maybe_more_c(+Term) is det.
%
%   Triggers exploration of more clauses if needed. This predicate checks if more clauses
%   related to a term should be explored based on certain conditions.
%
%   @arg Term The term to check for further clause exploration.
%
maybe_more_c(MFL) :- is_mfl(MFL), !.
maybe_more_c(_) :- t_l:shown_why(no_recurse).
maybe_more_c(C) :- t_l:shown_why(more(C)), !.
maybe_more_c(C) :- t_l:shown_why((C)), !.
maybe_more_c(C) :-
    assert(t_l:shown_why(more(C))),
    assert(t_l:shown_why((C))),
    locally(t_l:shown_why(no_recurse),
        locally(t_l:shown_why((C)),
            locally(t_l:shown_why(more(C)),
                ignore(catch(pfcWhy2(C, 1.1), E, fbugio(E)))))),
    !.

%!  pfcShowSingleJust_C(+Clause) is det.
%
%   Helper predicate for `pfcShowSingleJust1/3` that displays a single clause justification.
%
%   @arg Clause The clause to display.
%
pfcShowSingleJust_C(C) :- is_file_ref(C), !.
pfcShowSingleJust_C(C) :- find_mfl(C, MFL), assert(t_l:shown_why(MFL)), !, pfcShowSingleJust_MFL(MFL).
pfcShowSingleJust_C(_) :- ansi_format([hfg(black)], " % [no_mfl] ", []), !.

%!  short_filename(+File, -ShortFilename) is det.
%
%   Extracts a short filename from a full file path. This predicate simplifies the file path by
%   removing unnecessary components.
%
%   @arg File The full file path.
%   @arg ShortFilename The extracted short filename.
%
short_filename(F, FN) :- symbolic_list_concat([_, FN], '/pack/', F), !.
short_filename(F, FN) :- symbolic_list_concat([_, FN], swipl, F), !.
short_filename(F, FN) :- F = FN, !.

%!  pfcShowSingleJust_MFL(+MFL) is det.
%
%   Helper predicate for `pfcShowSingleJust_C/1` that displays an mfl (module/file/line) reference.
%
%   @arg MFL The mfl (module/file/line) reference to display.
%
pfcShowSingleJust_MFL(MFL) :-
    MFL = mfl4(VarNameZ, _M, F, L), atom(F), short_filename(F, FN), !,
    varnames_load_context(VarNameZ),
    ansi_format([hfg(black)], " % [~w:~w] ", [FN, L]).
pfcShowSingleJust_MFL(MFL) :-
    MFL = mfl4(V, M, F, L), my_maplist(var, [V, M, F, L]), !.
pfcShowSingleJust_MFL(MFL) :-
    ansi_format([hfg(black)], " % [~w] ", [MFL]), !.

%!  pfcAsk(+Message, -Answer) is det.
%
%   Asks the user for input during Pfc justification exploration.
%
%   @arg Message The message to display to the user.
%   @arg Answer The user input.
%
pfcAsk(Msg, Ans) :-
    format("~n~w", [Msg]),  % Display the message to the user.
    read(Ans).              % Read the user input.

%!  pfcSelectJustificationNode(+Justifications, +Index, -Node) is det.
%
%   Selects a specific node in a list of justifications based on an index. The node
%   corresponds to a specific step in the justification process.
%
%   @arg Justifications The list of justifications.
%   @arg Index The index used to select the node.
%   @arg Node The selected node.
%
pfcSelectJustificationNode(Js, Index, Step) :-
    JustNo is integer(Index),         % Convert index to an integer for selecting the justification.
    nth1(JustNo, Js, Justification),  % Get the Justification at position JustNo.
    StepNo is 1 + integer(Index*10 - JustNo*10),  % Calculate the step number.
    nth1(StepNo, Justification, Step).  % Get the Step at position StepNo within the justification.


