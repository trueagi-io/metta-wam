%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Actions with ChatGPT Integration
%
% Code actions are automated suggestions or fixes provided by a language server in
% response to issues detected in the code or user-initiated requests. These actions
% can help refactor, correct, optimize, execute code, and run tests based on
% diagnostics or contextual understanding.
%
% Common types of code actions:
% - Diagnostics-Triggered Actions: Suggested fixes based on errors or warnings
%   detected in the code (e.g., adding missing imports, fixing syntax errors).
% - Refactoring: Suggestions to improve code structure, such as renaming variables,
%   extracting methods, or simplifying complex code.
% - Quick Fixes: Fast fixes for small issues like formatting, adding required
%   annotations, or removing unused variables.
% - User-Initiated: Actions triggered by the user, such as formatting the document,
%   running tests, executing code, or applying all linting fixes.
%
% How they work in the LSP context:
% 1. The IDE (client) sends a request to the language server to generate code actions
%    for a specific range in the code, often based on diagnostics (errors/warnings).
% 2. The language server returns a list of possible actions to the IDE, each with a
%    title and a description of the changes it will make or the tests it will run.
% 3. When the user selects a code action, the IDE applies the suggested changes,
%    executes code, or runs the tests automatically, improving developer productivity
%    and code quality.
%
% In our system, we extend traditional code actions by integrating them with
% ChatGPT to provide more intelligent, context-aware assistance.
%
% ChatGPT Integration:
% - Contextual Understanding: ChatGPT analyzes the code and provides suggestions
%   based on the logic and intent of the code, going beyond syntax to understand
%   the deeper context.
% - Customized Code Suggestions: ChatGPT generates more meaningful refactoring
%   actions, optimizations, or style improvements tailored to the current project
%   or coding patterns.
% - Enhanced Error Explanations: When diagnostics occur, ChatGPT explains the
%   errors in human-readable terms, provides potential fixes, and helps developers
%   understand the root cause of issues.
% - User-Driven Interaction: Developers can ask ChatGPT for help by triggering
%   code actions with specific questions like "How can I improve this function?"
%   or "What's the best way to handle this error?" ChatGPT then analyzes and
%   provides customized responses.
% - Automated Refactoring: ChatGPT can suggest and apply more complex refactorings,
%   such as converting synchronous code to asynchronous code, improving performance,
%   or restructuring large blocks of code.
% - **Code Execution and Testing**: ChatGPT can trigger actions that execute the code
%   or run associated tests. Developers can invoke ChatGPT to ensure their code
%   executes correctly, verify output, or identify failing tests. It can also analyze
%   test results and suggest fixes based on the outcomes.
%
% Benefits of ChatGPT Integration:
% - Dynamic Code Suggestions: ChatGPT tailors responses to the specific problem
%   or request at hand, providing intelligent solutions that adapt to the code context.
% - Improved Developer Learning: ChatGPT helps developers understand best practices
%   by offering detailed explanations and examples alongside the code fixes.
% - Flexibility and Customization: ChatGPT adapts to the user's style preferences
%   and provides suggestions aligned with project-specific needs.
% - Natural Language Interaction: Developers can communicate naturally with ChatGPT
%   to seek assistance, making it easier to request help without interrupting workflow.
% - **Code and Test Execution**: Code actions can directly execute code or run tests
%   through ChatGPT, simplifying the process of verifying code correctness and ensuring
%   functionality without needing to leave the IDE.
%
% Author: Douglas Miles
% Date: 10-21-2024
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile lsp_hooks:handle_msg_hook/3.
:- dynamic lsp_hooks:handle_msg_hook/3.
:- discontiguous lsp_hooks:handle_msg_hook/3.

:- discontiguous code_action_hook/3.
:- discontiguous lsp_hooks:handle_msg_hook/3.

% can comment this entire subsystem by commenting out the next hook
lsp_hooks:handle_msg_hook(Method, Msg, Result) :-
   code_action_hook(Method, Msg, Result), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learning Experiment: `codeAction` and `Refactor`
% Handles code actions for renaming, GPT-based rewriting, commenting,
% evaluating metta code, and running all tests, with symbols in titles.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Importing required libraries for HTTP and JSON
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(readutil)).

must_succeed(G):- call(G)*->true;(wdmsg(failed_succeed(G)),fail).

must_succeed1((A,B)):- !, must_succeed1(A),must_succeed1(B),!.
must_succeed1(G):- call(G)->true;(wdmsg(failed_succeed(G)),throw(failed_succeed(G)),fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the textDocument/codeAction Request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_action_hook("textDocument/codeAction", Msg, _{id: Id, result: Actions}) :-
    _{id: Id, params: Params} :< Msg,
   _{textDocument: _{uri: Uri}, range: Range, context: _Context} :< Params,
    compute_code_actions(Uri, Range, Actions).

% Compute Code Actions for Specified Kinds, with symbols in titles
compute_code_actions(Uri, Range, Actions) :- % Extract the symbol from the code at the range
    get_code_at_range(term, Uri, Range, Symbol), %symbol(Symbol),
    compute_symbol_code_actions(Uri, Range, Symbol, Actions),!.
compute_code_actions(_Uri,_Range, []).

% nb_append_code_actions(+HT, +ElementToAppend)
% Destructively append ElementToAppend to the end of HT by finding the last cons cell.
nb_append_code_actions(HT, ElementToAppend) :-
    arg(2, HT, Tail),  % Get the second argument (the tail) of the cons cell
    ( Tail == []  % If the tail is the empty list, we've reached the end
    -> nb_setarg(2, HT, [ElementToAppend])  % Destructively append the new element
    ; nb_append_code_actions(Tail, ElementToAppend)  % Otherwise, keep traversing the list
    ).

% nb_append_code_actions(+HT, +ElementToAppend)
% Destructively append ElementToAppend to the end of HT by finding the last cons cell.
nb_append_code_actions(HT, ElementToAppend) :-
    arg(2, HT, Tail),  % Get the second argument (the tail) of the cons cell
    ( Tail == []  % If the tail is the empty list, we've reached the end
    -> nb_setarg(2, HT, [ElementToAppend])  % Destructively append the new element
    ; nb_append_code_actions(Tail, ElementToAppend)  % Otherwise, keep traversing the list
    ).

% get_filepart(+Uri, -FilePart)
% Extracts the part of the URI after the last '/'.
get_filepart(Uri, FilePart) :-
    atomic_list_concat(Segments, '/', Uri),  % Split the URI by '/'
    last(Segments, FilePart).  % Get the last segment, which is the file part

compute_symbol_code_actions(Uri, Range, Symbol, Actions) :-
    get_src_code_at_range(toplevel_form, Uri, Range, Expression),
    get_src_code_at_range(expression, Uri, Range, SubExpression),
    get_src_code_at_range(block, Uri, Range, Code),
    get_filepart(Uri, FilePart),
    trim_to_length(Code, 300, Block),

    % Initialize Actions as an open list with one element (Run All Tests action)
    Actions = [FirstAction],

    % Run All Tests Action
    into_line_char_range(Range,LCR),
    sformat(AllTestsTitle, "Run All Tests in File: '~w'", [FilePart]),
    FirstAction = _{
        title: AllTestsTitle,
        kind: "refactor.runAllTests",
        command: _{
            title: AllTestsTitle,
            command: "run_all_tests",
            arguments: [Uri]
        }
    },

    % Append Refactor: Rename Symbol, only if Symbol is not empty
    ( ( debugging(lsp(position)), Symbol \== '' ) ->
       (sformat(RefactorTitle, "Refactor: Rename Symbol '~w' (TODO) ~w", [Symbol, LCR]),
        RefactorAction = _{
           title: RefactorTitle,
           kind: "refactor.rename",
           command: _{
               title: "Rename Symbol",
               command: "refactor_rename",
               arguments: [Uri, Range]
           }
        },
        nb_append_code_actions(Actions, RefactorAction))
    ; true),

    % Conditional placeholder for GPT Rewrite Block action
    ( debugging(lsp(todo)) /* Condition for GPT Rewrite */ ->
       (sformat(RewriteTitle, "Refactor: Have GPT Rewrite Block '~w' (TODO)", [Block]),
        RewriteAction = _{
           title: RewriteTitle,
           kind: "refactor.rewrite",
           command: _{
               title: "Rewrite Code",
               command: "refactor_gpt_rewrite",
               arguments: [Uri, Range]
           }
        },
        nb_append_code_actions(Actions, RewriteAction))
    ; true),

    % Conditional placeholder for GPT Comment Block action
    ( debugging(lsp(todo)) /* Condition for GPT Comment */ ->
       (sformat(CommentTitle, "Source: Comment this file: '~w...' (TODO)", [FilePart]),
        CommentCodeAction = _{
            title: CommentTitle,
            kind: "refactor.comment",
            command: _{
                title: CommentTitle,
                command: "source_gpt_comment",
                arguments: [Uri, Range]
            }
        },
        nb_append_code_actions(Actions, CommentCodeAction))
    ; true),

    % Conditional placeholder for Eval Metta action
    ( true ->
       (sformat(EvalMettaTitle, "Eval Metta: '~w'", [Expression]),
        EvalMettaAction = _{
            title: EvalMettaTitle,
            kind: "quickfix.eval",
            command: _{
                title: "Eval Metta",
                command: "eval_metta",
                arguments: [Uri, Range, Expression]
            }
        },
        nb_append_code_actions(Actions, EvalMettaAction))
    ; true),

    % Conditional placeholder for Eval Metta action
    ( (SubExpression \=@= Expression) ->
       (sformat(EvalMettaSubTitle, "Eval Sub-expression: '~w'", [SubExpression]),
        EvalMettaSubAction = _{
            title: EvalMettaSubTitle,
            kind: "quickfix.eval",
            command: _{
                title: "Eval SubExpression",
                command: "eval_metta",
                arguments: [Uri, Range, SubExpression]
            }
        },
        nb_append_code_actions(Actions, EvalMettaSubAction))
    ; true).



% trim_to_length(+InputString, +MaxLength, -TrimmedString)
% Trims InputString to MaxLength characters.
% If InputString is shorter than MaxLength, returns InputString as TrimmedString.
% Otherwise, it truncates the string to MaxLength characters.
trim_to_length(InputString, MaxLength, TrimmedString) :-
    string_length(InputString, Length),
    (
      Length =< MaxLength ->
      TrimmedString = InputString  % If the string is shorter than or equal to MaxLength, return it as is.
    ;
      sub_string(InputString, 0, MaxLength, _, TrimmedString)  % Otherwise, trim it to MaxLength.
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the workspace/executeCommand Request
code_action_hook("workspace/executeCommand", Msg, _{id: Id, result: Result}) :-
    _{id: Id, params: Params} :< Msg,
    _{command: Command, arguments: Arguments} :< Params,
    execute_command(Command, Arguments, ExecutionResult),
    Result = _{message: ExecutionResult}.

% Execute Command Implementation

execute_command("refactor_rename", [Uri, Range, NewName], ExecutionResult) :-
    % Check if a new name was provided
    (   NewName == '' ->
        % If no new name is provided, return a message indicating cancellation
        ExecutionResult = "Rename cancelled: No new name provided."
    ;   % Otherwise, proceed with the rename
        get_code_at_range(symbol, Uri, Range, OldSymbol),
        sformat(ExecutionResult, "Renaming symbol '~w' to '~w' in range ~w at URI: ~w", [OldSymbol, NewName, Range, Uri]),
        apply_rename(Uri, Range, OldSymbol, NewName)
    ).


% GPT-based refactor rewrite command
execute_command("refactor_gpt_rewrite", [Uri, Range], ExecutionResult) :-
    get_code_at_range(block, Uri, Range, Code),
    gpt_rewrite_code(Code, ExecutionResult).

% GPT-based comment suggestion command
execute_command("source_gpt_comment", [Uri], ExecutionResult) :-
    source_file_text(Uri, Text),
    gpt_comment_code(Text, ExecutionResult).

% Evaluate Metta code
execute_command("eval_metta", [_Uri, _Range, Code], ExecutionResult) :-
    %get_code_at_range(expression, Uri, Range, Code),
    eval_metta(Code, ExecutionResult).

% Run all tests and report results
execute_command("run_all_tests", [Uri], ExecutionResult) :-
    run_all_tests(Uri, ResultList),
    format_tests_summary(ResultList, ExecutionResult).

% Fallback for unrecognized commands
execute_command(_, _, "Command not recognized.").
% Retrieves the entire code content from a file given its URI.
get_code_from_file(Uri, Code) :-
    path_doc(Path, Uri),  % Extract the file path from the URI.
    read_file_to_string(Path, Code, []).  % Read the entire file content into Code.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GPT-Based Code Refactor and Commenting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GPT Rewrite Code
gpt_rewrite_code(Code, RewrittenCode) :-
    call_openai_for_gpt_task(Code, "Refactor this code.", RewrittenCode).

% GPT Comment Code
gpt_comment_code(Code, CommentedCode) :-
    call_openai_for_gpt_task(Code, "Add comments to this code.", CommentedCode).

% Generic GPT Task Execution (rewrite, comment, etc.)
call_openai_for_gpt_task(Code, Task, Result) :-
    getenv('OPENAI_API_KEY', ApiKey),
    OpenAIURL = 'https://api.openai.com/v1/chat/completions',
    sformat(Prompt, "Task: ~w\n\nCode:\n~w", [Task, Code]),
    RequestPayload = _{
        model: "gpt-3.5-turbo",
        messages: [
            _{role: "system", content: "You are a helpful assistant."},
            _{role: "user", content: Prompt}
        ],
        max_tokens: 100,
        stop: "\n"
    },
    % Send the request to OpenAI
    http_post(
        OpenAIURL,
        json(RequestPayload),
        ResponseDict,
        [authorization(bearer(ApiKey)), json_object(dict)]
    ),
    % Extract the rewritten or commented code from the response
    (   _{choices: Choices} :< ResponseDict,
        member(Choice, Choices),
        get_dict(message, Choice, Message),
        get_dict(content, Message, Result)
    ;   Result = "Error: No result returned from GPT"
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluate Metta Code (Dummy Implementation)
eval_metta(Code, Result) :-
    sformat(Result, "Evaluating Metta code: '~w'", [Code]).
% Execute the Code
execute_code(Code, Result) :-
    % For safety, catch any errors during execution
    catch_with_backtrace((
        % Replace with actual code execution logic
        % For demonstration, we'll just unify Result with Code
        % In practice, you might use metta:eval_string/2 or similar
        eval(Code,CodeResult),
        sformat(Result,"~w ; ~q",[Code,CodeResult])
    ), Error, (
        sformat(Result,"~w ; Error: ~q",[Code,Error])
    )).


% Run all tests (Dummy Test Results)
run_all_tests([pass("Test 1"), fail("Test 2"), error("Test 3", "Some error")]).

% Format the test summary
format_tests_summary(ResultList, Summary) :-
    length(ResultList, Total),
    include(pass_test, ResultList, Passes),
    include(fail_test, ResultList, Fails),
    include(error_test, ResultList, Errors),
    length(Passes, PassCount),
    length(Fails, FailCount),
    length(Errors, ErrorCount),
    format(string(Summary),
        "Total: ~d\nPassed: ~d\nFailed: ~d\nErrors: ~d",
        [Total, PassCount, FailCount, ErrorCount]).

% Helpers for test results
pass_test(pass(_)).
fail_test(fail(_)).
error_test(error(_, _)).


% Execute Command Implementation


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactor rename command: Show what will be renamed and accept the new name
%
% Workflow:
% - User selects "Rename Symbol" in the IDE.
% - IDE opens a rename prompt for the user to input the new name.
% - If the user confirms (presses Enter), the IDE sends a
%   `workspace/executeCommand` request with the old symbol, the new name, and range.
% - If the user cancels (presses Esc), no request is sent to the LSP server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Helper to apply the rename across the specified range
apply_rename(Uri, _Range, OldSymbol, NewName) :-
    get_code_from_file(Uri, Code),
    % Replace occurrences of OldSymbol with NewName in the code at the specified range
    replace_symbol_in_code(OldSymbol, NewName, Code, UpdatedCode),
    save_updated_code(Uri, UpdatedCode).

% Helper to replace occurrences of a symbol in the code
replace_symbol_in_code(OldSymbol, NewName, Code, UpdatedCode) :-
    % Simple string replacement in this case
    atomic_list_concat(Parts, OldSymbol, Code),
    atomic_list_concat(Parts, NewName, UpdatedCode).

% Helper to save the updated code to the file
save_updated_code(Uri, UpdatedCode) :-
    path_doc(Path, Uri),
    open(Path, write, Stream),
    write(Stream, UpdatedCode),
    close(Stream).


