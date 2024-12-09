% :- module(lsp_metta_completion, [completions_at/3]).
% /** <module> LSP Completion
%
% This module implements code completion, based on defined predicates in
% the file & imports.
%
% Uses =lsp_metta_changes= in order to see the state of the buffer being edited.
%
% @see lsp_metta_changes:doc_text_fallback/2
%
% @author James Cash
% */
%

% Douglas added
:- use_module(library(http/http_open)).  % For making HTTP requests
:- use_module(library(http/json)).       % For handling JSON
:- use_module(library(http/http_header)).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [numlist/3]).
:- use_module(library(yall)).
%:- use_module(lsp_metta_utils, [linechar_offset/3]).
:- use_module(lsp_metta_changes, [doc_text_fallback_d4/2]).

% James added
:- use_module(library(prolog_xref), [xref_defined/3, xref_source/2]).

:- include(lsp_metta_include).



:- discontiguous(handle_completions/3).

:- discontiguous(handle_codelens/3).

%
part_of_prefix(Code) :- code_type(Code, prolog_var_start).
part_of_prefix(Code) :- code_type(Code, prolog_atom_start).
part_of_prefix(Code) :- code_type(Code, prolog_identifier_continue).
%
get_prefix_codes(Stream, Offset, Codes) :-
    get_prefix_codes(Stream, Offset, [], Codes).
%
get_prefix_codes(Stream, Offset0, Codes0, Codes) :-
    peek_code(Stream, Code),
    part_of_prefix(Code), !,
    succ(Offset1, Offset0),
    seek(Stream, Offset1, bof, Offset),
    get_prefix_codes(Stream, Offset, [Code|Codes0], Codes).
get_prefix_codes(_, _, Codes, Codes).
%
prefix_at(File, Position, Prefix) :-
    source_file_text(File, DocCodes),
    setup_call_cleanup(
        open_string(DocCodes, Stream),
        ( linechar_offset(Stream, Position, _),
          seek(Stream, -1, current, Offset),
          get_prefix_codes(Stream, Offset, PrefixCodes),
          string_codes(Prefix, PrefixCodes) ),
        close(Stream)
    ).
%
completions_at(File, Position, Completions) :-
    prefix_at(File, Position, Prefix),
    xref_source(File, [silent(true)]),
    findall(
        Result,
        ( xref_defined(File, Goal, _),
          functor(Goal, Name, Arity),
          atom_concat(Prefix, _, Name),
          args_str(Arity, Args),
          format(string(Func), "~w(~w)$0", [Name, Args]),
          format(string(Label), "~w/~w", [Name, Arity]),
          Result = _{label: Label,
                     insertText: Func,
                     insertTextFormat: 2}),
        Completions
    ).
%
args_str(Arity, Str) :-
    numlist(1, Arity, Args),
    maplist([A, S]>>format(string(S), "${~w:_}", [A]),
           Args, ArgStrs),
    atomic_list_concat(ArgStrs, ', ', Str).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Completion Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_msg_hook(Method,Msg,Response):- handle_completions(Method,Msg,Response).

handle_completions("textDocument/completion", Msg, _{id: Id, result: Completions}) :-
     _{id: Id, params: Params} :< Msg,
     _{textDocument: _{uri: Uri},
       position: _{line: Line0, character: Char0}} :< Params,
     path_doc(Path, Uri),
     succl(Line0, Line1),
     completions_at(Path, line_char(Line1, Char0), Completions).


% Handle the 'textDocument/completion' Request
handle_completions("textDocument/completion", Msg, _{id: Id, result: CompletionList}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}, position: Position} :< Params,
    compute_completions_combined(Uri, Position, CompletionList), !.
handle_completions("textDocument/completion", Msg, _{id: Msg.id, result: []}) :- !.

% Handle the 'completionItem/resolve' Request
handle_completions("completionItem/resolve", Msg, _{id: Id, result: ResolvedItem}) :-
    _{id: Id, params: CompletionItem} :< Msg,
    resolve_completion_item(CompletionItem, ResolvedItem).


% Compute completions by merging local and OpenAI completions
compute_completions_combined(Uri, Position, CompletionList) :-
    get_word_at_position(Uri, Position, Word),
    % Step 1: Get local completions from predefined symbols
    find_local_completions(Word, LocalCompletions),
    % Step 2: Call OpenAI to get completions if API key is set
    (   Word \= '', getenv('OPENAI_API_KEY', ApiKey) ->
        call_openai_for_completions(Word, OpenAICompletions, ApiKey)
    ;   OpenAICompletions = []),
    % Step 3: Merge both local and OpenAI completions
    append(LocalCompletions, OpenAICompletions, AllCompletions),
    process_completions(AllCompletions, CompletionList).

% Resolve Completion Item to add documentation
resolve_completion_item(CompletionItem, ResolvedItem) :-
    get_dict(data, CompletionItem, Symbol),
    symbol_documentation(Symbol, Documentation),
    % Add documentation to the completion item
    put_dict(_{documentation: Documentation}, CompletionItem, ResolvedItem).

% Find completions from predefined symbols
find_local_completions(Word, CompletionItems) :-
    findall(CompletionItem,
        (
            symbol_possibility(Symbol),
            (Word == '' ; sub_atom(Symbol, 0, _, _, Word)),  % Match symbols starting with Word
            CompletionItem = _{
                label: Symbol,
                kind: 1,  % 1 indicates a "Text" kind in LSP
                data: Symbol  % Include symbol name in data for resolution
            }
        ),
        CompletionItems).


% Process and convert completions into LSP CompletionItems
process_completions(Completions, CompletionList) :-
    findall(CompletionItem,
        (
            member(Completion, Completions),
            CompletionItem = _{
                label: Completion,
                kind: 1,  % 1 indicates a "Text" kind in LSP
                data: Completion  % Include the completion text as data for resolution
            }
        ),
        Items),
    CompletionList = _{isIncomplete: false, items: Items}.

% Get the current word at the cursor position
get_word_at_position(Uri, Position, Word) :-
    get_document_lines(Uri, Lines),
    _{line: Line0, character: Character} :< Position,
    LineNum is Line0 + 1,
    nth1(LineNum, Lines, Line),
    sub_atom(Line, 0, Character, _, SubLine),
    atom_codes(SubLine, Codes),
    reverse(Codes, RevCodes),
    get_word_codes(RevCodes, WordCodesRev),
    reverse(WordCodesRev, WordCodes),
    atom_codes(Word, WordCodes).

% Collect word character codes in reverse order
get_word_codes([Code|Rest], [Code|WordCodes]) :-
    code_type(Code, csym),  % Accepts alphanumerics and underscores
    get_word_codes(Rest, WordCodes).
get_word_codes(_, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Get document lines from URI
get_document_lines(Uri, Lines) :-
    path_doc(Path, Uri),
    source_file_text(Path, DocFullText),
    split_string(DocFullText, "\n", "", Lines).

% Check if a line contains a symbol and get the symbol positions
contains_symbol(Line, Symbol, StartChar, EndChar) :-
    symbol_possibility(Symbol),
    sub_string(Line, StartChar, Length, _, Symbol),
    EndChar is StartChar + Length,
    is_whole_word(Line, StartChar, EndChar).

% Check if the symbol is a whole word in the line
is_whole_word(Line, StartChar, EndChar) :-
    ( StartChar =:= 0
    ; StartBefore is StartChar - 1,
      sub_atom(Line, StartBefore, 1, _, BeforeChar),
      \+ code_type(BeforeChar, csym)
    ),
    ( sub_atom(Line, EndChar, 1, _, AfterChar)
    -> \+ code_type(AfterChar, csym)
    ; true  % End of line
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Symbol Definitions and Documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example symbol possibilities
symbol_possibility(joe_blow).
symbol_possibility(john_doe).
symbol_possibility(jane_smith).

% Example symbol documentation
symbol_documentation(joe_blow, "Joe Blow is a sample function that does XYZ.").
symbol_documentation(john_doe, "John Doe is a placeholder name used in examples.").
symbol_documentation(jane_smith, "Jane Smith represents a generic person in examples.").
symbol_documentation(_, "No documentation available.").


% run with:
%                  clear ; cat lsp_server_metta_llm.pl ; swipl -s lsp_server_metta_llm.pl -g test_for_completions_with_context,test_for_completions_with_context,run_tests,halt

:- use_module(library(http/http_client)).  % Ensure HTTP client is loaded
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).    % Correct JSON library for SWI-Prolog
:- use_module(library(readutil)).          % For reading file content

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Non-context version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Call the OpenAI API for completions based only on the given Word and ApiKey (no additional context)
call_openai_for_completions(Word, Completions, ApiKey) :-
    % Set up the OpenAI API URL and Headers
    OpenAIURL = 'https://api.openai.com/v1/chat/completions',

    % Construct the request payload for gpt-3.5-turbo model
    RequestPayload = _{
        model: "gpt-3.5-turbo",
        messages: [
            _{role: "system", content: "You are a helpful assistant."},
            _{role: "user", content: Word}
        ],
        max_tokens: 10,
        n: 5,
        stop: "\n"
    },

    % Print the request payload for diagnostics
    writeln('Sending Request Payload:'),
    writeln(RequestPayload),
    flush_output,  % Ensure the output is printed immediately

    % Prepare the HTTP headers including the API Key for authentication
    (   getenv('OPENAI_API_KEY', ApiKey)
    ->  (
            % Send the HTTP request and print the response
            writeln('Sending request to OpenAI...'),
            flush_output,  % Ensure the output is printed immediately
            http_post(
                OpenAIURL,
                json(RequestPayload),
                ResponseDict,
                [authorization(bearer(ApiKey)), json_object(dict)]
            ),
            writeln('Received Response:'),
            writeln(ResponseDict),
            flush_output,  % Ensure the output is printed immediately

            % Extract the completions from the response
            (   _{choices: Choices} :< ResponseDict ->
                extract_choices(Choices, Completions),
                writeln('Extracted Completions:'),
                writeln(Completions),
                flush_output  % Ensure the output is printed immediately
            ;   Completions = [],
                writeln('No completions returned.'),
                flush_output  % Ensure the output is printed immediately
            )
        )
    ;   writeln('Error: API key not set'),
        flush_output,  % Ensure the output is printed immediately
        Completions = []
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Context version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read the content of the file to provide context
get_context_from_file(FilePath, Context) :-
    read_file_to_string(FilePath, Context, []).

% Call the OpenAI API for completions based on the given Word, additional Context, and ApiKey
call_openai_for_completions_with_context(Word, Context, Completions, ApiKey) :-
    % Prepare a combined prompt using the context and word
    atom_concat(Context, '\n\nUser: ', PrePrompt),
    atom_concat(PrePrompt, Word, Prompt),

    % Set up the OpenAI API URL and Headers
    OpenAIURL = 'https://api.openai.com/v1/chat/completions',

    % Construct the request payload for gpt-3.5-turbo model
    RequestPayload = _{
        model: "gpt-3.5-turbo",
        messages: [
            _{role: "system", content: "You are a helpful assistant."},
            _{role: "user", content: Prompt}
        ],
        max_tokens: 10,
        n: 5,
        stop: "\n"
    },

    % Print the request payload for diagnostics
    writeln('Sending Request Payload:'),
    writeln(RequestPayload),
    flush_output,  % Ensure the output is printed immediately

    % Prepare the HTTP headers including the API Key for authentication
    (   getenv('OPENAI_API_KEY', ApiKey)
    ->  (
            % Send the HTTP request and print the response
            writeln('Sending request to OpenAI...'),
            flush_output,  % Ensure the output is printed immediately
            http_post(
                OpenAIURL,
                json(RequestPayload),
                ResponseDict,
                [authorization(bearer(ApiKey)), json_object(dict)]
            ),
            writeln('Received Response:'),
            writeln(ResponseDict),
            flush_output,  % Ensure the output is printed immediately

            % Extract the completions from the response
            (   _{choices: Choices} :< ResponseDict ->
                extract_choices(Choices, Completions),
                writeln('Extracted Completions:'),
                writeln(Completions),
                flush_output  % Ensure the output is printed immediately
            ;   Completions = [],
                writeln('No completions returned.'),
                flush_output  % Ensure the output is printed immediately
            )
        )
    ;   writeln('Error: API key not set'),
        flush_output,  % Ensure the output is printed immediately
        Completions = []
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%============================
% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Extract the choices from the OpenAI response (for the chat API)
extract_choices(Choices, Completions) :-
    writeln('Choices received:'),
    writeln(Choices),
    findall(CompletionText,
        (
            member(Choice, Choices),
            get_dict(message, Choice, Message),
            get_dict(content, Message, CompletionText)  % Extract the content from message
        ),
        Completions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(openai_api).

% Test API key retrieval (mocked or real environment)
test(api_key_retrieval) :-
    (   getenv('OPENAI_API_KEY', ApiKey)
    ->  assertion(ApiKey \= '')
    ;   assertion(false)  % Fails if API key is not set
    ).

% Test successful call to OpenAI API without context
test(call_openai_for_completions) :-
    % Replace with a test or mock word and a real API key for live testing
    Word = "test",
    getenv('OPENAI_API_KEY', ApiKey),
    call_openai_for_completions(Word, Completions, ApiKey),
    assertion(is_list(Completions)),
    assertion(length(Completions, _)).

% Test successful call to OpenAI API with context
test(call_openai_for_completions_with_context) :-
    % Replace with a test or mock word, context, and a real API key for live testing
    Word = "test",
    Context = "Some context from the file...",
    getenv('OPENAI_API_KEY', ApiKey),
    call_openai_for_completions_with_context(Word, Context, Completions, ApiKey),
    assertion(is_list(Completions)),
    assertion(length(Completions, _)).

% Test response parsing from OpenAI (mocked response)
test(response_parsing) :-
    % Mock OpenAI chat response for testing
    MockResponse = _{
        choices: [
            _{message: _{content: "completion_1"}},
            _{message: _{content: "completion_2"}},
            _{message: _{content: "completion_3"}}
        ]
    },
    extract_choices(MockResponse.choices, Completions),
    assertion(Completions == ["completion_1", "completion_2", "completion_3"]).

:- end_tests(openai_api).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manual testing functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For manual testing without context
test_for_completions :-
    getenv('OPENAI_API_KEY', ApiKey),
    call_openai_for_completions("test", Completions, ApiKey), nl, nl,
    writeq(Completions), nl.

% For manual testing with context from a file
test_for_completions_with_context(FilePath) :-
    getenv('OPENAI_API_KEY', ApiKey),
    get_context_from_file(FilePath, Context),  % Read context from the given file
    call_openai_for_completions_with_context("test", Context, Completions, ApiKey), nl, nl,
    writeq(Completions), nl.

test_for_completions_with_context:-
  test_for_completions_with_context('context_file.txt').



