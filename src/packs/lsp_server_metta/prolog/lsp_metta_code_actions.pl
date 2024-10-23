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
must_succeed1(G):- call(G)->true;(wdmsg(failed_succeed(G)),fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the textDocument/codeAction Request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_action_hook("textDocument/codeAction", Msg, _{id: Id, result: Actions}) :-
    _{id: Id, params: Params} :< Msg,
   _{textDocument: _{uri: Uri}, range: Range, context: _Context} :< Params,
    compute_code_actions(Uri, Range, Actions).

% Compute Code Actions for Specified Kinds, with symbols in titles
compute_code_actions(Uri, Range, Actions) :- % Extract the symbol from the code at the range
    get_code_at_range(symbol, Uri, Range, Symbol), symbol(Symbol),
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
    get_code_at_range(expression, Uri, Range, Expression),
    get_code_at_range(block, Uri, Range, Code),
    get_filepart(Uri, FilePart),
    trim_to_length(Code, 300, Block),
    
    % Initialize Actions as an open list with one element (Run All Tests action)
    Actions = [FirstAction],

    % Run All Tests Action
    into_line_char_range(Range,LCR),
    sformat(AllTestsTitle, "Run All Tests in File: '~w' (~w)", [FilePart, LCR]),
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
    ( Symbol \== '' ->
       (sformat(RefactorTitle, "Refactor: Rename Symbol '~w'", [Symbol]),
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
    ( true /* Condition for GPT Rewrite */ ->
       (sformat(RewriteTitle, "Refactor: Have GPT Rewrite Block '~w'", [Block]),
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
    ( true /* Condition for GPT Comment */ ->
       (sformat(CommentTitle, "Source: Comment this file: '~w...'", [FilePart]),
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
                arguments: [Uri, Range]
            }
        },
        nb_append_code_actions(Actions, EvalMettaAction))
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
execute_command("eval_metta", [Uri, Range], ExecutionResult) :-
    get_code_at_range(expression, Uri, Range, Code),
    eval_metta(Code, ExecutionResult).

% Run all tests and report results
execute_command("run_all_tests", [Uri], ExecutionResult) :-
    run_all_tests(Uri, ResultList),
    format_tests_summary(ResultList, ExecutionResult).

% Fallback for unrecognized commands
execute_command(_, _, "Command not recognized.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks if the first line_char comes before the second one.
before_line_char(line_char(Line1,_Char1),line_char(Line2,_Char2)):- 
    Line1 > Line2, !, fail.  % If the first line is greater than the second, it's not before.
before_line_char(line_char(Line1,Char1),line_char(Line2,Char2)):- 
    Line1 =:= Line2, Char1 > Char2, !, fail.  % If on the same line, compare characters.
before_line_char(_, _).  % Otherwise, the first one is before the second.

% Checks if the first range ends before the second range starts.
completely_before_range(range(_,LineChar1),range(LineChar2,_)):- 
    before_line_char(LineChar1, LineChar2), !.
% Checks if the first range starts before the second range starts.
before_range(range(LineChar1,_),range(LineChar2,_)):- 
    before_line_char(LineChar1, LineChar2), !.


% Checks if the first line_char comes after the second one.
after_line_char(line_char(Line1,_Char1),line_char(Line2,_Char2)):- 
    Line1 < Line2, !, fail.  % If the first line is greater than the second, it's not after.
after_line_char(line_char(Line1,Char1),line_char(Line2,Char2)):- 
    Line1 =:= Line2, Char1 < Char2, !, fail.  % If on the same line, compare characters.
after_line_char(_, _).  % Otherwise, the first one is after the second.


% Checks if the first range starts after the second range ends.
completely_after_range(range(LineChar1,_),range(_,LineChar2)):- 
    after_line_char(LineChar1, LineChar2), !.
% Checks if the first range starts after the second range starts.
after_range(range(LineChar1,_),range(LineChar2,_)):- 
    after_line_char(LineChar1, LineChar2), !.

% Extract code at the specified range for different types of targets (symbol, expression, block, exact).
% For `symbol`, it looks for a clause/term in the file at the specified position.
get_code_at_range(TargetType, Uri, Range, Target):- \+ (is_dict(Range), _{start: RStart, end: _REnd} :< Range),
   into_json_range(Range, LspRange), !,
   get_code_at_range(TargetType, Uri, LspRange, Target).

get_code_at_range(symbol, Uri, Range, Target):- !, 
    must_succeed1((
        _{start: RStart, end: _REnd} :< Range,  % Extract the start and end from the LSP range.
        _{line: StartLine0, character: StartChar} :< RStart,  % Get line and character from the start position.
        path_doc(Path, Uri),  % Extract the file path from the URI.
        Start = line_char(StartLine0, StartChar),  % Convert to a line_char pair.
        lsp_metta_utils:clause_with_arity_in_file_at_position(Target, _Arity, Path, Start)  % Get the clause at the specified position.
    )).

% For `expression`, it first resolves the symbol and then looks for the code within the buffer at the range.
get_code_at_range(expression, Uri, Range, SrcCode) :-
    get_code_at_range(symbol, Uri, Range, Target),  % First, get the symbol at the range.
    Target \== '',
    path_doc(Path, Uri),  % Extract the file path from the URI.
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    metta_file_buffer(_, Code, Vs, Path, BRange),  % Get the buffer contents for the file.    
    sub_var(Target, Code),  % Check that the symbol (Target) appears within the buffer (Code).
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    \+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    maybe_name_vars(Vs), !,
    with_output_to(string(SrcCode),write_src_wi(Code)).
    % brange_to_dict(BRange,CodeRange), get_code_at_range(exact, Uri, BRange, Code).  % Refine the code extraction with exact range.

get_code_at_range(expression, Uri, Range, Target):- get_code_at_range(symbol, Uri, Range, Target), Target \== '' , !.
get_code_at_range(expression, Uri, Range, Code):- get_code_at_range(exact, Uri, Range, Code).
% For `block`, it acts similarly to expression but with larger code blocks.
get_code_at_range(block, Uri, Range, Code):- 
    get_code_at_range(expression, Uri, Range, Code),!.
    

% Extracts the exact range of code specified by the Range (LSP-style start and end).
get_code_at_range(exact, Uri, Range, Code) :-
    path_doc(Path, Uri),  % Extract the file path from the URI.
    source_file_text(Path, FullText),  % Retrieve the full file text.
    split_string(FullText, "\n", "", Lines),  % Split the file into lines.
    _{start: Start, end: End} :< Range,  % Extract start and end positions from the range.
    _{line: StartLine0, character: StartChar} :< Start,  % Get line and character of the start position.
    _{line: EndLine0, character: EndChar} :< End,  % Get line and character of the end position.
    StartLine is StartLine0 + 0,  % Set the start line (Prolog indexing adjustment if needed).
    EndLine is EndLine0 + 1,  % Set the end line (since Prolog indexes from 1).
    extract_code(Lines, StartLine, StartChar, EndLine, EndChar, Code).  % Extract the exact code range.

% Helper to extract code from lines based on the start and end line/character positions.
extract_code(Lines, StartLine, StartChar, EndLine, EndChar, Code) :-
    findall(LineText, (
        between(StartLine, EndLine, LineNum),  % Iterate over the line numbers in the range.
        nth1(LineNum, Lines, Line),  % Get the line at the current number.
        (
            (LineNum =:= StartLine, LineNum =:= EndLine) ->  % Case where the start and end are on the same line.
            (EndCharStartChar is EndChar - StartChar,  % Get the substring within this line.
             sub_atom(Line, StartChar, EndCharStartChar, _, LineText))
        ;
            LineNum =:= StartLine ->  % Case where the current line is the start line.
            sub_atom(Line, StartChar, _, 0, LineText)  % Get the substring starting from StartChar.
        ;
            LineNum =:= EndLine ->  % Case where the current line is the end line.
            sub_atom(Line, 0, EndChar, _, LineText)  % Get the substring ending at EndChar.
        ;
            LineText = Line  % In-between lines are added fully.
        )
    ), CodeLines),
    atomic_list_concat(CodeLines, '\n', Code).  % Combine the extracted lines into a single code string.

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
    Prompt = format("Task: ~w\n\nCode:\n~w", [Task, Code]),
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


