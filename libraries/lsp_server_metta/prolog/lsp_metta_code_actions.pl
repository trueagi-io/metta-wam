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

:- include(lsp_metta_include).

% Can comment this entire subsystem by commenting out the next hook
lsp_hooks:handle_msg_hook(Method, Msg, Result) :-
    clause(handle_code_action_msg(Method, Msg, Result), Body), !,
    must_det_ll(Body).

:- discontiguous(handle_code_action_msg/3).
:- discontiguous(lsp_hooks:compute_code_action/3).
:- discontiguous(lsp_hooks:exec_code_action/3).
:- discontiguous(lsp_hooks:compute_code_lens/3).


% Importing required libraries for HTTP and JSON
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(readutil)).
% General Helper Predicates
% These helpers are used across different handlers.
safe_clause_call(Head):- clause(Head,Body), catch(Body,_,true).

must_succeed(G):- call(G)*->true;(wdmsg(failed_succeed(G)),fail).

must_succeed1((A,B)):- !, must_succeed1(A),must_succeed1(B),!.
must_succeed1(G):- call(G)->true;(wdmsg(failed_succeed(G)),throw(failed_succeed(G)),fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the textDocument/codeAction Request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_code_action_msg("textDocument/codeAction", Msg, _{id: Id, result: Actions}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}, range: Range, context: _Context} :< Params,
    findall(Action, safe_clause_call(lsp_hooks:compute_code_action(Uri, Range, Action)), Actions), !.

% Handle the 'textDocument/codeLens' Request
handle_code_action_msg("textDocument/codeLens", Msg, _{id: Id, result: CodeLenses}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}} :< Params,
    compute_code_lenses(Uri, CodeLenses).

% Generate a list of code lenses for a document
compute_code_lenses(Uri, CodeLenses) :-
    doc_path(Uri, Path),
    findall(CodeLens, safe_clause_call(lsp_hooks:compute_code_lens(Uri, Path, CodeLens)), CodeLenses).

% Handle the workspace/executeCommand Request
handle_code_action_msg("workspace/executeCommand", Msg, _{id: Id, result: null}) :- _{id: Id} :< Msg,
    ignore(handle_execute_command(Msg, _)).


% General Helper Predicates
% Helper to extract the file part from a URI
get_filepart(Uri, FilePart) :-
    atomic_list_concat(Segments, '/', Uri),
    last(Segments, FilePart).

% Helper to trim a string to a maximum length
trim_to_length(InputString, MaxLength, TrimmedString) :-
    string_length(InputString, Length),
    (
        Length =< MaxLength ->
        TrimmedString = InputString
    ;
        sub_string(InputString, 0, MaxLength, _, TrimmedString)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Action: GPT Rewrite Block
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_hooks:compute_code_action/3 for GPT Rewrite Block
lsp_hooks:compute_code_action(Uri, Range, RewriteAction) :-
    debugging(lsp(todo)),  % Condition for GPT Rewrite
    get_src_code_at_range(block, Code, Range, Uri),
    trim_to_length(Code, 300, Block),
    sformat(RewriteTitle, "Refactor: Have GPT Rewrite Block '~w' (TODO)", [Block]),
    RewriteAction = _{
        title: RewriteTitle,
        kind: "refactor.rewrite",
        command: _{
            title: "Rewrite Code",
            command: "refactor_gpt_rewrite",
            arguments: [Uri, Range]
        }
    }.

% lsp_hooks:exec_code_action/3 for GPT Rewrite Block
lsp_hooks:exec_code_action("refactor_gpt_rewrite", [Uri, Range], ExecutionResult) :-
    get_src_code_at_range(block, Code, Range, Uri),
    gpt_rewrite_code(Code, ExecutionResult).

% GPT Rewrite Code
gpt_rewrite_code(Code, RewrittenCode) :-
    call_openai_for_gpt_task(Code, "Refactor this code.", RewrittenCode).

% Generic GPT Task Execution
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
    % Extract the rewritten code from the response
    (   _{choices: Choices} :< ResponseDict,
        member(Choice, Choices),
        get_dict(message, Choice, Message),
        get_dict(content, Message, Result)
    ;   Result = "Error: No result returned from GPT"
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Action: GPT Comment Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_hooks:compute_code_action/3 for GPT Comment Code
lsp_hooks:compute_code_action(Uri, _Range, CommentCodeAction) :-
    get_filepart(Uri, FilePart),
    debugging(lsp(todo)),  % Condition for GPT Comment
    sformat(CommentTitle, "Source: Comment this file: '~w...' (TODO)", [FilePart]),
    CommentCodeAction = _{
        title: CommentTitle,
        kind: "refactor.comment",
        command: _{
            title: CommentTitle,
            command: "source_gpt_comment",
            arguments: [Uri]
        }
    }.

% lsp_hooks:exec_code_action/3 for GPT Comment Code
lsp_hooks:exec_code_action("source_gpt_comment", [Uri], ExecutionResult) :-
    % Retrieves the entire code content from a file given its URI.
    source_file_text(Uri, Text),
    gpt_comment_code(Text, ExecutionResult).

% GPT Comment Code
gpt_comment_code(Code, CommentedCode) :-
    call_openai_for_gpt_task(Code, "Add comments to this code.", CommentedCode).

% The call_openai_for_gpt_task/3 predicate is defined earlier.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Action: Load Metta Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_hooks:compute_code_action/3 for Load Metta Code
lsp_hooks:compute_code_action(Uri, Range, MettaFileAction) :-
    get_src_code_at_range(toplevel_form, Uri, Range, Expression),
    RunLoadTitle = "Run/Load Metta",
    sformat(RunLoadTitleExpression, "~w: ~w", [RunLoadTitle, Expression]),
    MettaFileAction = _{
        title: RunLoadTitleExpression,
        kind: "quickfix.file",
        command: _{
            title: RunLoadTitle,
            command: "load_metta",
            arguments: [Expression, Range, Uri]
        }
    }.

% lsp_hooks:exec_code_action/3 for Load Metta Code
lsp_hooks:exec_code_action("load_metta", [Code, Range, Uri], ExecutionResult) :-
    load_metta_code(+, Uri, Code, ExecutionResult),
    report_diagnostics(Uri, Range, ExecutionResult).

% Load/Evaluate Metta Code
load_metta_code(For, Uri, Code, Result) :-
    once(maybe_parse_sexpr_metta1(Code, Parsed)),
    Code \=@= Parsed,
    !,
    load_metta_code(For, Uri, Parsed, Result).
load_metta_code(For, Uri, Code, Result) :-
    maybe_doc_path(Uri, Path),
    !,
    load_metta_code(For, Path, Code, Result).
% Load/Execute the form and generate the output using do_metta/5.
load_metta_code(For, Path, Code, Result) :-
    catch_with_backtrace((
        ignore(current_self(Self)),
        wots(Out, ignore(do_metta(file(lsp(Path)), For, Self, Code, _LastAnswer))),
        sformat(Result, "; ~w", [Out])
    ), Error, (
        sformat(Result, "~w ; Error: ~q", [Code, Error])
    )), !.
% Evaluate Metta Code (Fallback)
load_metta_code(For, Uri, Code, Result) :-
    sformat(Result, "Failed ~q", [load_metta_code(For, Uri, Code, Result)]).

% Helper Predicate: maybe_parse_sexpr_metta1/2
maybe_parse_sexpr_metta1(Code, Parsed) :-
    string(Code),
    catch(parse_sexpr_metta1(Code, Parsed), _, fail), !.
maybe_parse_sexpr_metta1(PreParsed, PreParsed).

% The report_diagnostics/3 predicate would be defined elsewhere.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Action: Evaluate Metta Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_hooks:compute_code_action/3 for Evaluate Metta Expression
lsp_hooks:compute_code_action(Uri, Range, EvalMettaAction) :-
    once(get_src_code_at_range(toplevel_form, Uri, Range, TopExpression)),
    get_src_code_at_range(expression, Uri, Range, Expression),
    TopExpression \=@= Expression,
    EvalTitle = "Eval Expression",
    sformat(EvalTitleExpression, "~w: ~w", [EvalTitle, Expression]),
    EvalMettaAction = _{
        title: EvalTitleExpression,
        kind: "quickfix.eval",
        command: _{
            title: EvalTitle,
            command: "eval_metta",
            arguments: [Expression, Range, Uri]
        }
    }.

% lsp_hooks:exec_code_action/3 for Evaluate Metta Expression
lsp_hooks:exec_code_action("eval_metta", [Code, Range, Uri], ExecutionResult) :-
    load_metta_code(exec, Uri, Code, ExecutionResult),
    report_diagnostics(Uri, Range, ExecutionResult).

% The load_metta_code/4 and report_diagnostics/3 predicates are defined earlier.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Action: Run All Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_hooks:compute_code_action/3 for Run All Tests
lsp_hooks:compute_code_action(Uri, _Range, FirstAction) :-
    get_filepart(Uri, FilePart),
    sformat(AllTestsTitle, "Run All Tests in File: '~w'", [FilePart]),
    FirstAction = _{
        title: AllTestsTitle,
        kind: "refactor.runAllTests",
        command: _{
            title: AllTestsTitle,
            command: "run_all_tests",
            arguments: [Uri]
        }
    }.

% lsp_hooks:exec_code_action/3 for Run All Tests
lsp_hooks:exec_code_action("run_all_tests", [Uri], Summary) :- !, run_all_tests(Uri, Summary).

% Run All Tests Implementation
run_all_tests(Uri, Summary) :-
    doc_path(Uri, Path),
    Lvl = 0,
    !,
    findall(Diagnostic,
        (   metta_file_buffer(Lvl, Ord, Kind, What, VL, Path, BRange),
            test_metta_file_buffer(Uri, Lvl, Ord, Kind, What, VL, Path, BRange, Diagnostic)
        ),
        Diagnostics
    ),
    publish_diagnostics(Uri, Diagnostics, _Version, _Options),
    maplist(=, [Total, PassCount, FailCount, ErrorCount], [6, 3, 2, 1]),  % Placeholder counts
    % Format the test summary
    sformat(Summary, "Total: ~d\nPassed: ~d\nFailed: ~d\nErrors: ~d",
        [Total, PassCount, FailCount, ErrorCount]).

% Test Metta File Buffer
test_metta_file_buffer(_Uri, _Lvl, _Ord, _Kind, What, VL, Path, BRange, Diagnostic) :-
    into_json_range(BRange, Range),
    maybe_name_vars(VL),
    current_self(Self),
    once(with_answer_output(do_metta(file(Path), +, Self, What, _Out), ExecutionResult)),
    wots(Src, write_src(What)),
    Diagnostic = _{
        range: Range,
        severity: 3,  % Information severity
        code: Src,
        source: "MeTTa LSP",
        message: ExecutionResult
    }.

% The publish_diagnostics/4 predicate would be defined elsewhere.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Lens Implementations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_hooks:compute_code_lens/3 Definitions

% Code Lens for Running Unit Tests in Metta Files
lsp_hooks:compute_code_lens(Uri, _, CodeLens) :-
    atom_concat(_, '.metta', Uri),
    CodeLens = _{
        range: _{start: _{line: 0, character: 0}, end: _{line: 0, character: 1}},
        command: _{title: "Run Unit Tests", command: "run_all_tests", arguments: [Uri]}
    }.

% Compute Code Lens for Buffer
lsp_hooks:compute_code_lens(Uri, Path, CodeLens) :-
    compute_code_lens_for_buffer(Uri, Path, CodeLens).

compute_code_lens_for_buffer(Uri, Path, CodeLens) :-
    Lvl = 0,
    metta_file_buffer(Lvl, Ord, Kind, What, VL, Path, BRange),
    compute_each_buffer_lens(Uri, Lvl, Ord, Kind, What, VL, Path, BRange, CodeLens).

compute_each_buffer_lens(Uri, _Lvl, _Ord, Kind, What, VL, _Path, BRange, CodeLens) :-
    \+ is_list(What),
    What = exec(_),
    !,
    into_json_range(BRange, Range),
    maybe_name_vars(VL),
    wots(Title, (write("Run "), nop(write_src(Kind)))),
    wots(Src, write_src(What)),
    CodeLens = _{
        range: Range,
        command: _{title: Title, command: "load_metta", arguments: [Src, Range, Uri]}
    }.

compute_each_buffer_lens(Uri, Lvl, _Ord, Kind, What, VL, _Path, BRange, CodeLens) :- fail,
    Lvl = 0,
    into_json_range(BRange, Range),
    maybe_name_vars(VL),
    wots(Title, (write("Load "), nop(write_src(Kind)))),
    wots(Src, write_src(What)),
    CodeLens = _{
        range: Range,
        command: _{title: Title, command: "load_metta", arguments: [Src, Range, Uri]}
    }, !.

% unused currently since Lvl = 0 always
compute_each_buffer_lens(_Uri, Lvl, _Ord, _Kind, Symbol, _VL, _Path, BRange, CodeLens):-  Lvl >= 1,
    symbol(Symbol), \+ upcase_symbol(Symbol, Symbol),
    into_json_range(BRange, Range),
    CodeLens = _{
        range: Range,
        data: Symbol  % Include symbol name for resolve
    }.


% Handle "codeLens/resolve" Request
handle_code_action_msg("codeLens/resolve", Msg, _{id: Id, result: ResolvedCodeLens}) :-
    _{params: CodeLensParams, id: Id} :< Msg,
    resolve_code_lens(CodeLensParams, ResolvedCodeLens), !.
handle_code_action_msg("codeLens/resolve", Msg, _{id: Msg.id, result: null}) :- !.  % Fallback if resolution fails


% Resolve Code Lens
resolve_code_lens(CodeLens, ResolvedCodeLens) :-
    get_dict(data, CodeLens, Symbol),
    symbol_reference_locations(Symbol, Locations),
    % Build the command with the resolved locations
    Command = _{
        title: "Show References",
        command: "editor.action.showReferences",
        arguments: [
            CodeLens.range.start,  % Position of the symbol
            Locations              % List of locations where the symbol is referenced
        ]
    },
    put_dict(command, CodeLens, Command, ResolvedCodeLens).

% Resolve additional information for a code lens
resolve_code_lens(CodeLens, ResolvedCodeLens) :-
    % Here, we could add more specific details or dynamically update the Code Lens if needed
    ResolvedCodeLens = CodeLens.



% Helper predicate to find all locations where the symbol is referenced.
symbol_reference_locations(Symbol, Locations) :-
    findall(Location, symbol_reference_uri(Symbol, Location), Locations).

% The symbol_reference_uri/2 predicate is defined earlier.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle Execute Command
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement handle_execute_command/2
handle_execute_command(Msg, Response) :-
    % Extract parameters from the message
    _{id: Id, params: Params} :< Msg,
    _{command: Command, arguments: Arguments} :< Params,
    % Look up the clause for lsp_hooks:exec_code_action(Command, ExpectedArguments, ExecutionResult)
    HeadPattern = lsp_hooks:exec_code_action(Command, ExpectedArguments, ExecutionResult),
    (   clause(HeadPattern, Body)
    ->  (
            % We found a matching command
            (   arguments_match(ExpectedArguments, Arguments)
            ->  (   % Proceed to execute the command
                    catch(
                        call(Body),
                        Error,
                        % Handle errors that occur during execution
                        (   format(string(ErrorMessage), "Error ~w ~q:~q", [Error, Command, Arguments]),
                            send_feedback_message(ErrorMessage, warning),
                            Response = _{id: Id, error: _{code: -32603, message: ErrorMessage}}
                        )
                    )
                ->  % Execution succeeded without throwing an error
                    (   send_feedback_message(ExecutionResult, info),
                        Response = _{id: Id, result: ExecutionResult}
                    )
                ;   (   % Execution failed without an exception, but Body failed
                        format(string(ErrorMessage), "Failed ~q:~q", [Command, Arguments]),
                        send_feedback_message(ErrorMessage, warning),
                        Response = _{id: Id, error: _{code: -32603, message: ErrorMessage}}
                    )
                )
            ;   (   % Argument mismatch
                    format(string(ErrorMessage),
                        "Argument mismatch ~q:~q Expected ~w",
                        [Command, Arguments, ExpectedArguments]),
                    send_feedback_message(ErrorMessage, warning),
                    Response = _{id: Id, error: _{code: -32602, message: ErrorMessage}}
                )
            )
        )
    ;   (   % Command not recognized
            format(string(ErrorMessage), "Command not recognized: ~q: ~q", [Command, Arguments]),
            send_feedback_message(ErrorMessage, warning),
            Response = _{id: Id, error: _{code: -32601, message: ErrorMessage}}
        )
    ).

% Helper predicate to check if provided arguments match the expected pattern
arguments_match(ExpectedArguments, ProvidedArguments) :-
    % Attempt to unify ExpectedArguments with ProvidedArguments
    % This checks if the structure and number of arguments are compatible
    ExpectedArguments = ProvidedArguments.


% Additional Helper Predicates

already_message_result(ExecutionResult) :- is_dict(ExecutionResult), !.
already_message_result(ExecutionResult) :- ExecutionResult == false, !.
already_message_result(ExecutionResult) :- ExecutionResult == [], !.
already_message_result([H|ExecutionResult]):- is_list(ExecutionResult),!,already_message_result(H).

% into_message_result/2 converts the execution result into a message result
%into_message_result(_ExecutionResult, null):- !.
into_message_result(ExecutionResult, Result) :-
    already_message_result(ExecutionResult), !,
    Result = ExecutionResult.
into_message_result(ExecutionResult, Result) :-
    sformat(Str, '~w', [ExecutionResult]),
    Result = _{message: Str}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Action: Refactor Rename Symbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Workflow:
% - User selects "Rename Symbol" in the IDE.
% - IDE opens a rename prompt for the user to input the new name.
% - If the user confirms (presses Enter), the IDE sends a
%   `workspace/executeCommand` request with the old symbol, the new name, and range.
% - If the user cancels (presses Esc), no request is sent to the LSP server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% lsp_hooks:compute_code_action/3 for Refactor Rename Symbol
lsp_hooks:compute_code_action(Uri, Range, RefactorAction) :-
    debugging(lsp(todo)),
    get_code_at_range(term, Uri, Range, Symbol),
    symbol(Symbol),
    Symbol \== '', % Rename Symbol, only if Symbol is not empty
    fail,  % Placeholder for future implementation
    debugging(lsp(position)),
    sformat(RefactorTitle, "Refactor: Rename Symbol '~w' (TODO)", [Symbol]),
    RefactorAction = _{
        title: RefactorTitle,
        kind: "refactor.rename",
        command: _{
            title: "Rename Symbol",
            command: "refactor_rename",
            arguments: [Uri, Range, Symbol]
        }
    }.

% lsp_hooks:exec_code_action/3 for Refactor Rename Symbol
lsp_hooks:exec_code_action("refactor_rename", [Uri, Range, NewName], ExecutionResult) :-
    % Check if a new name was provided
    (   NewName == '' ->
        % If no new name is provided, return a message indicating cancellation
        ExecutionResult = "Rename cancelled: No new name provided."
    ;   % Otherwise, proceed with the rename
        get_code_at_range(symbol, Uri, Range, OldSymbol),
        sformat(ExecutionResult, "Renaming symbol '~w' to '~w' in range ~w at URI: ~w", [OldSymbol, NewName, Range, Uri]),
        apply_rename(Uri, Range, OldSymbol, NewName)
    ).

% Helper to apply the rename across the specified range
apply_rename(Uri, _Range, OldSymbol, NewName) :-
    % Retrieves the entire code content from a file given its URI.
    source_file_text(Uri, Code),
    % Replace occurrences of OldSymbol with NewName in the code
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

% Additional code related to refactor rename


% Execute specific Code Lens commands
lsp_hooks:exec_code_action("find_references", [Uri, Symbol], ExecutionResult) :-
    find_symbol_references(Uri, Symbol, References),
    format_reference_result(References, ExecutionResult).

% Helper to find all references of a given symbol in a specified URI
find_symbol_references(Uri, Symbol, References) :-
    % Retrieve all locations of the symbol in the specified file
    findall(Location, symbol_reference_uri(Symbol, Location), References),
    ( References == [] ->
        References = [_{uri: Uri, message: "No references found for symbol."}]
    ; true
    ).

% Format the references result to send back to the client
format_reference_result(References, Result) :-
    % Convert the list of references into a string format, if needed
    maplist(format_reference_entry, References, Entries),
    atomic_list_concat(Entries, "\n", Result).

% Helper to format each reference entry
format_reference_entry(Location, Formatted) :-
    get_dict(uri, Location, Uri),
    get_dict(range, Location, Range),
    format(string(Formatted), "Reference found in ~w at ~w", [Uri, Range]).

% Helper to find symbol reference locations
symbol_reference_uri(Symbol, Location) :-
    % Retrieve symbol's information including file path and range
    metta_file_buffer(_Lvl, _Ord, _Kind, Symbol, _VL, Path, Range),
    path_doc(Path, Uri),               % Convert file path to URI
    into_json_range(Range, JRange),     % Convert range to JSON-compatible range format
    Location = _{
        uri: Uri,                      % Document URI for the symbol reference
        range: JRange                  % Range within the document
    }.
