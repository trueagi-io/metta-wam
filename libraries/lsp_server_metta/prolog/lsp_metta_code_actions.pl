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
%   or "What’s the best way to handle this error?" ChatGPT then analyzes and
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
% - Flexibility and Customization: ChatGPT adapts to the user’s style preferences
%   and provides suggestions aligned with project-specific needs.
% - Natural Language Interaction: Developers can communicate naturally with ChatGPT
%   to seek assistance, making it easier to request help without interrupting workflow.
% - **Code and Test Execution**: Code actions can directly execute code or run tests
%   through ChatGPT, simplifying the process of verifying code correctness and ensuring
%   functionality without needing to leave the IDE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile(lsp_hooks:handle_msg_hook/3).
:- dynamic(lsp_hooks:handle_msg_hook/3).
:- discontiguous(lsp_hooks:handle_msg_hook/3).

% can comment this entire subsystem by commenting out the next hook
lsp_hooks:handle_msg_hook(Method, Msg, Result) :-
   code_action_hook(Method, Msg, Result), !.

% Handle the textDocument/codeAction Request
code_action_hook("textDocument/codeAction", Msg, _{id: Id, result: Actions}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri},
      range: Range,
      context: _Context} :< Params,
    compute_code_actions(Uri, Range, Actions).

% Compute Code Actions
compute_code_actions(Uri, Range, [Action]) :-
    Action = _{
        title: "Execute Code",
        kind: "quickfix",
        command: _{
            title: "Execute Code",
            command: "execute_code",
            arguments: [Uri, Range]
        }
    }.

% Handle the workspace/executeCommand Request
code_action_hook("workspace/executeCommand", Msg, _{id: Id, result: Result}) :-
    _{id: Id, params: Params} :< Msg,
    _{command: Command, arguments: Arguments} :< Params,
    execute_command(Command, Arguments, ExecutionResult),
    Result = _{message: ExecutionResult}.

% Execute Command Implementation
execute_command("execute_code", [Uri, Range], ExecutionResult) :-
    get_code_at_range(Uri, Range, Code),
    execute_code(Code, ExecutionResult).
execute_command(_, _, "Command not recognized.").

% Get Code at the Specified Range
get_code_at_range(Uri, Range, Code) :-
    atom_concat('file://', Path, Uri),
    lsp_metta_changes:doc_text(Path, SplitText),
    coalesce_text(SplitText, Text),
    split_string(Text, "\n", "", Lines),
    _{start: Start, end: End} :< Range,
    _{line: StartLine0, character: StartChar} :< Start,
    _{line: EndLine0, character: EndChar} :< End,
    StartLine is StartLine0 + 1,
    EndLine is EndLine0 + 1,
    extract_code(Lines, StartLine, StartChar, EndLine, EndChar, Code).

% Extract Code from Lines
extract_code(Lines, StartLine, StartChar, EndLine, EndChar, Code) :-
    findall(LineText, (
        between(StartLine, EndLine, LineNum),
        nth1(LineNum, Lines, Line),
        (
            LineNum =:= StartLine, LineNum =:= EndLine ->
            sub_atom(Line, StartChar, EndChar - StartChar, _, LineText)
        ;
            LineNum =:= StartLine ->
            sub_atom(Line, StartChar, _, 0, LineText)
        ;
            LineNum =:= EndLine ->
            sub_atom(Line, 0, EndChar, _, LineText)
        ;
            LineText = Line
        )
    ), CodeLines),
    atomic_list_concat(CodeLines, '\n', Code).

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


