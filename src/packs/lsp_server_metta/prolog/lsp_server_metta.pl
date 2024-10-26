:- module(lsp_server_metta, [main/0 ]).
/** <module> LSP Server

The main entry point for the Language Server implementation with dynamic handling based on max threads.

Handles workspace folder changes, file indexing, and incremental updates.

Supports LSP methods like hover, document symbol, definition, references, and more.

@author James Cash
*/

:- use_module(library(apply), [maplist/2]).
:- use_module(library(debug), [debug/3, debug/1]).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(thread)).
:- use_module(library(thread_pool)).
%:- use_module(library(prolog_xref)).
%:- use_module(library(prolog_source), [directory_source_files/3]).
:- use_module(library(utf8), [utf8_codes//1]).
:- use_module(library(yall)).




:- discontiguous(lsp_hooks:handle_msg_hook/3).

:- multifile(lsp_hooks:handle_msg_hook/3).
:-   dynamic(lsp_hooks:handle_msg_hook/3).
:- multifile(user:predicate_help_hook/5).
:-   dynamic(user:predicate_help_hook/5).
:- multifile(lsp_hooks:hover_hook/5).
:-   dynamic(lsp_hooks:hover_hook/5).


:- user:ensure_loaded(lsp_metta_utils).
:- use_module(lsp_metta_checking, [metta_check_errors/2]).
:- use_module(lsp_json_parser, [lsp_metta_request//1]).
:- use_module(lsp_metta_changes, [handle_doc_changes/2]).
:- ensure_loaded(lsp_metta_completion).
:- use_module(lsp_metta_colours, [
%                            file_colours/2,
%                            file_range_colours/4,
                            token_types/1,
                            token_modifiers/1]).
:- use_module(lsp_metta_xref).
:- use_module(lsp_metta_split, [
        split_text_document/2,
        coalesce_text/2
]).


% will change to module in a few days (easier to test externally from `user`)
:- user:ensure_loaded(lsp_metta_code_actions).
%:- user:ensure_loaded(lsp_metta_save_actions).
:- user:ensure_loaded(lsp_metta_hover).
:- user:ensure_loaded(lsp_metta_workspace).
:- user:ensure_loaded(lsp_metta_references).
:- user:ensure_loaded(lsp_metta_outline). %( [xref_metta_source/1, xref_document_symbol/5, xref_document_symbols/2]).
:- dynamic(user:full_text/2).
%:- user:ensure_loaded(lsp_prolog_checking).
%:- user:ensure_loaded(lsp_prolog_colours).
%:- user:ensure_loaded(lsp_prolog_utils).

:- dynamic lsp_metta_changes:doc_text/2.

:- discontiguous lsp_server_metta:handle_msg/3.

:- multifile(lsp_hooks:handle_msg_hook/3).
:- dynamic(lsp_hooks:handle_msg_hook/3).
:- discontiguous(lsp_hooks:handle_msg_hook/3).


% If the max worked thread count is 0, it processes requests synchronously;
% otherwise, it uses a thread pool for parallel processing.
% 2 is a good default as it needs to be able to implement interruptions anyway
% (this is separate from the file indexer threads)
:- dynamic(lsp_worker_threads/1).
lsp_worker_threads(1). % no threads thus "$/cancelRequest" cant be implemented
% lsp_worker_threads(1). % 1 thread thus "$/cancelRequest" is working
% lsp_worker_threads(10). % 10 threads might be OK but overkill

% Main entry point
main :-
    set_prolog_flag(debug_on_error, false),
    set_prolog_flag(report_error, true),
    set_prolog_flag(toplevel_prompt, ''),
    current_prolog_flag(argv, Args),
    nodebug(lsp(_)), % Everything
    %debug(lsp(low)),
    debug(lsp(main)),
    debug(lsp(errors)),
    debug(lsp(threads)),
    debug(lsp(high)),
    debug(lsp(todo)),
    debug(lsp(position)),
    load_mettalog_xref,
    ignore(handle_threads(Args)),  % Handle threading based on max threads.
    start(Args).

% Handle thread pool or synchronous mode based on max threads.
handle_threads([MaxThreadsArg | _]) :-
    ignore((atom_number(MaxThreadsArg, MaxThreadsN),
    retractall(lsp_worker_threads(_)),
    assertz(lsp_worker_threads(MaxThreadsN)))).

start_lsp_worker_threads:-
    lsp_worker_threads(MaxThreads),
    ( MaxThreads > 0
    -> create_workers('$lsp_worker_pool', MaxThreads) % Create thread pool if max threads > 0
    ; debug(lsp(threads), "Running synchronously since max threads = 0", [])  % Sync mode
    ).

% Worker pool implementation
% Create a pool with Id and number of workers.
% After the pool is created, post_job/1 can be used to send jobs to the pool.

create_workers(Id, N) :-
    message_queue_create(Id),
    forall(between(1, N, _),
           thread_create(do_work(Id), _, [])).

% Dynamic predicates for cancellation mechanism
:- dynamic thread_request/2.
:- dynamic id_was_canceled/1.

:- if( \+ prolog_load_context(reloading,true)).
% Create a mutex for synchronization
:- mutex_create('$lsp_request_mutex').
:- endif.


do_work(QueueId) :-
    repeat,
      thread_get_message(QueueId, Task),
      ( Task = lsp_task(Out, Req) ->
    thread_self(ThreadId),
    ( get_dict(id, Req.body, RequestId) ->
        true
    ; RequestId = none ),
    % Register this thread handling RequestId
    ( id_was_canceled(RequestId) ->
        debug(lsp(threads), "Request ~w was canceled before it got started!", [RequestId])
    ; ( with_mutex('$lsp_request_mutex', assertz(thread_request(RequestId, ThreadId))),
        debug(lsp(threads), "Worker ~w processing task with ID ~w", [ThreadId, RequestId]),
        catch(
            handle_request(Out, Req),
            canceled,
            ( debug(lsp(threads), "Request ~w was canceled", [RequestId]),
              send_cancellation_response(Out, RequestId)
            )
        ),
        % Clean up after handling
        with_mutex('$lsp_request_mutex',
            retract(thread_request(RequestId, ThreadId))
        )
      )
          )
      ; % Handle other types of tasks if needed
        true
    ),
    fail.

% Post a job to be executed by one of the pool's workers.

post_job(Id, Task) :-
    thread_send_message(Id, Task).

% Send a cancellation response if necessary
send_cancellation_response(_OutStream, _RequestId) :-
    % According to LSP, the server should not send a response to a canceled request,
    % but some clients may expect a response indicating cancellation.
    % Uncomment the following lines if you want to send such a response.
    % Response = _{jsonrpc: "2.0", id: RequestId, error: _{code: -32800, message: "Request canceled"}},
    % send_message(OutStream, Response),
    true.

% Start the server based on input arguments
start([stdio]) :- !,
    debug(lsp(threads), "Starting stdio client", []),
    stdio_server.
start(Args) :-
    debug(lsp(threads), "Unknown args ~w", [Args]).

% stdio server initialization
stdio_server :-
    current_input(In),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    start_lsp_worker_threads,
    % Handling UTF decoding in JSON parsing, but doing the auto-translation
    % causes Content-Length to be incorrect
    set_stream(In, encoding(octet)),
    current_output(Out),
    set_stream(Out, encoding(utf8)),
    %stdio_handler_io(In, Out). %(might use this one later)
    stdio_handler(A-A, In, Out).



/*
% Handling requests from input/ouput stream (might use this one later)
stdio_handler_io(In, Out) :-
    lsp_worker_threads(MaxThreads),
    read_message(In, Codes),
    ( Codes == end_of_file ->
        true
    ; phrase(lsp_metta_request(Req), Codes, RemainingCodes),
      handle_parsed_request(Req, Out, MaxThreads),
      ( RemainingCodes == [] ->
          stdio_handler(In, Out)
      ; % There might be multiple requests in the buffer
        process_remaining_requests(RemainingCodes, Out, MaxThreads),
        stdio_handler(In, Out)
      )
    ).

% Process remaining requests in the buffer
process_remaining_requests(Codes, Out, MaxThreads) :-
    ( phrase(lsp_metta_request(Req), Codes, RemainingCodes) ->
        handle_parsed_request(Req, Out, MaxThreads),
        ( RemainingCodes == [] ->
            true
        ; process_remaining_requests(RemainingCodes, Out, MaxThreads)
        )
    ; % Could not parse a complete request, ignore or handle error
      true
    ).


% Read a complete message from the input stream
read_message(In, Codes) :-
    read_header(In, ContentLength),
    ( ContentLength = end_of_file ->
        Codes = end_of_file
    ; read_codes(In, ContentLength, Codes)
    ).

% Read the header to get Content-Length
read_header(In, ContentLength) :-
    read_line_to_codes(In, HeaderCodes),
    ( HeaderCodes == end_of_file ->
        ContentLength = end_of_file
    ; atom_codes(HeaderLine, HeaderCodes),
      ( HeaderLine = '' ->
          % Empty line, headers end
          read_header(In, ContentLength)
      ; split_string(HeaderLine, ": ", "", ["Content-Length", LengthStr]) ->
          number_string(ContentLength, LengthStr),
          % Read the blank line after headers
          read_line_to_codes(In, _)
      ; % Other headers, ignore
        read_header(In, ContentLength)
      )
    ).

% Read the message body based on Content-Length
read_codes(In, ContentLength, Codes) :-
    read_n_codes(In, ContentLength, Codes).

% Helper to read N codes from input
read_n_codes(In, N, Codes) :-
    ( N > 0 ->
        get_code(In, C),
        N1 is N - 1,
        read_n_codes(In, N1, RestCodes),
        Codes = [C|RestCodes]
    ; Codes = []
    ).
*/
% Handling requests from input stream



% [TODO] add multithreading? Guess that will also need a message queue
% to write to stdout
stdio_handler(Extra-ExtraTail, In, Out) :-
    wait_for_input([In], _, infinite),
    fill_buffer(In),
    read_pending_codes(In, ReadCodes, Tail),
    ( Tail == []
    -> true
    ; ( ExtraTail = ReadCodes,
        handle_requests(Out, Extra, Remainder),
        stdio_handler(Remainder-Tail, In, Out) )
    ).

handle_requests(Out, InCodes, Tail) :-
    phrase(lsp_metta_request(Req), InCodes, Rest), !,
    handle_parsed_request(Out, Req), !,
    ( var(Rest)
    -> Tail = Rest
    ; handle_requests(Out, Rest, Tail) ).
handle_requests(_, T, T).


immediate_method(Request):- is_dict(Request), !, immediate_request(Request).
immediate_method("$/cancelRequest"). % Handle cancel immediately
immediate_method(TM):- \+ cancelable_method(TM).
immediate_request(Req):- Method = Req.body.method, !, immediate_method(Method).
immediate_request(Req):- Body = Req.body, !, immediate_request(Body).
immediate_request(Body):- Method = Body.method, !, immediate_method(Method).

cancelable_method('textDocument/documentSymbol').
%cancelable_method('textDocument/hover').

% Handle parsed requests
handle_parsed_request(Out, Req) :-
    ( ( lsp_worker_threads(0) ; immediate_request(Req) )
    -> handle_request(Out, Req)
    ; post_job('$lsp_worker_pool', lsp_task(Out, Req))
    ).

% Backtrace error handler
catch_with_backtrace(Goal):-
     catch_with_backtrace(Goal,Err,
        ( Err == canceled ->
            throw(canceled)
        ; ( debug(lsp(errors), "Error in:\n\n?- catch_with_backtrace(~q).\n\nHandling message:\n\n~@~n\n", [Goal, print_message(error, Err)]),
            throw(Err)
          )
        )
     ).

% Send LSP message to client
send_message(Stream, Msg) :-
  catch_with_backtrace((
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes), width(0)]),
    phrase(utf8_codes(JsonCodes), UTF8Codes),
    length(UTF8Codes, ContentLength),
    format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]),
    flush_output(Stream))).

debug_lsp(Topic,Format,Args):-
 \+ \+ ((maplist(hide_gvars,Args),
         debug(lsp(Topic),Format,Args))).
hide_gvars(Arg):- numbervars(Arg,696,_,[attvar(skip),singletons(true)]).

% Handle individual requests
handle_request(OutStream, Req) :-
    debug_lsp(high, "Request ~q", [Req.body]),
    catch(
        ( catch_with_backtrace(handle_msg(Req.body.method, Req.body, Resp)),
          ignore((user:nodebug_lsp_response(Req.body.method) ->
                  debug(lsp(high), "response id: ~q", [Resp.id])
                ; debug_lsp(high, "response ~q", [Resp])
                )),
          ( is_dict(Resp) -> send_message(OutStream, Resp) ; true ) ),
        Err,
        ( Err == canceled ->
            throw(canceled)
        ; ( debug(lsp(error), "error handling msg ~q", [Err]),
          get_dict(id, Req.body, Id),
          send_message(OutStream, _{id: Id,
                           error: _{code: -32001,
                                             message: "server error"}})
          )
        )).

% Hide responses for certain methods
% user:nodebug_lsp_response("textDocument/hover").
user:nodebug_lsp_response("textDocument/documentSymbol").
user:nodebug_lsp_response("textDocument/codeAction").


% Server capabilities declaration

server_capabilities(
    _{textDocumentSync: _{openClose: true,
                          change: 2, %incremental
                          save: _{includeText: false},
                          willSave: false,
                          willSaveWaitUntil: true %???
                          },
      hoverProvider: true,
      completionProvider: _{},


      documentSymbolProvider: true,
      workspaceSymbolProvider: true,  % Workspace symbol provider

      definitionProvider: true,
      declarationProvider: false, % we are using menu for "definition" for declarations for now
      implementationProvider: true,
      typeDefinitionProvider: true,
      referencesProvider: true,

      documentHighlightProvider: false,
      codeActionProvider: true,  % Changed from false to true


      codeLensProvider: _{resolveProvider: true},  % Enabled resolveProvider
      documentFormattingProvider: false, % [TODO] almost finished
      %% documentOnTypeFormattingProvider: false,
      renameProvider: false,
      % documentLinkProvider: false,
      % colorProvider: true,
      foldingRangeProvider: false,
      executeCommandProvider: _{commands: ["eval_metta", "query_metta", "assert_metta", "source_gpt_comment", "refactor_gpt_rewrite", "run_all_tests"]},
      semanticTokensProvider: _{legend: _{tokenTypes: TokenTypes,
                                          tokenModifiers: TokenModifiers},
                                range: true,
                                % [TODO] implement deltas
                                full: _{delta: false}},

      workspace: _{workspaceFolders: _{supported: true,
                                       changeNotifications: true}}
     }
) :-
    token_types(TokenTypes),
    token_modifiers(TokenModifiers).

:- dynamic(user:client_capabilities/1).

:- dynamic in_editor/1.

:- discontiguous(handle_msg/3).

handle_msg(Method, Msg, Response):-
   lsp_hooks:handle_msg_hook(Method, Msg, Response),!.

% messages (with a response)
handle_msg("initialize", Msg,
           _{id: Id, result: _{capabilities: ServerCapabilities} }) :-
    _{id: Id, params: Params} :< Msg, !,
    ( Params.rootUri \== null
    -> xref_metta_source(Params.rootUri)
    ; true ),
    assert(user:client_capabilities(Params)),
    server_capabilities(ServerCapabilities).

handle_msg("shutdown", Msg, _{id: Id, result: null}) :-
    _{id: Id} :< Msg,
    debug(lsp(main), "received shutdown message", []).

% CALL: textDocument/hover
% IN: params:{position:{character:11,line:56},textDocument:{uri:file://<FILEPATH>}}}
% OUT: {id:21,result:{contents:{kind:plaintext,value:<ALL_THE_STUFF>}}}
handle_msg("textDocument/hover", Msg, _{id: Id, result: Response}) :- % fail,
    _{params: _{position: _{character: Char0, line: Line0},
                textDocument: _{uri: Doc}}, id: Id} :< Msg,
    hover_at_position(Doc, Line0, Char0, Response), !.
handle_msg("textDocument/hover", Msg, _{id: Msg.id, result: null}) :- !. % Fallback


% CALL: textDocument/documentSymbol
% IN: params:{textDocument:{uri:file://<FILEPATH>}}
% OUT: {id:1,result:[
%    {kind:12,location:{range:{end:{character:0,line:37},start:{character:1,line:35}},uri:file://<FILEPATH>},name:called_at/4},
%    {kind:12,location:{range:{end:{character:0,line:66},start:{character:1,line:64}},uri:file://<FILEPATH>},},name:defined_at/3} ... ]}

%handle_msg("textDocument/documentSymbol", Msg, _{id: Id, result: Symbols}) :-
%     _{id: Id, params: _{textDocument: _{uri: Doc}}} :< Msg, xref_document_symbols(Doc, Symbols),
%     assertion(is_list(Symbols)), !.

%convert_docsymbol_json(x(L,C0,C1,K,Name),Json) :-
%    Json=_{name:Name,kind:K,location:_{range:_{end:_{character:C1,line:L},start:_{character:C0,line:L}}}}.

%handle_msg("textDocument/documentSymbol", Msg, _{id: Id, result: DocJson}) :-
%    _{id: Id, params: _{textDocument: _{uri: Doc}}} :< Msg,
%     doc_path(Doc, Path), !,
%     get_document_symbols(Path,DocKinds),
%     maplist(convert_docsymbol_json,DocKinds,DocJson).

handle_msg("textDocument/documentSymbol", Msg, _{id: Id, result: Symbols}) :-
     _{id: Id, params: _{textDocument: _{uri: Doc}}} :< Msg, xref_document_symbols(Doc, Symbols),
     assertion(is_list(Symbols)), !.
%handle_msg("textDocument/documentSymbol", Msg, _{id: Msg.id, error: _{ code: -32602, message: "No symbol changes" }}):-!.
handle_msg("textDocument/documentSymbol", Msg, _{id: Msg.id, result: null}) :- !. % No symbol changes
%handle_msg("textDocument/documentSymbol", Msg, _{id: Msg.id, result: []}) :- !. % No symbol changes

message_id_target(Msg, Id, Doc, HintPath, Loc, Name/Arity):-
       _{id: Id, params: Params} :< Msg,
       _{textDocument: _{uri: Doc},
         position: _{line: Line0, character: Char0}} :< Params,
    path_doc(HintPath, Doc),
    Loc = line_char(Line0, Char0),
    lsp_metta_utils:clause_with_arity_in_file_at_position(Name, Arity, HintPath, Loc),
        debug(lsp(position),"~n~q~n",[message_id_target(Msg, Id, Doc, HintPath, Loc, Name/Arity)]).

% CALL: method:textDocument/definition
% IN: params:{position:{character:55,line:174},textDocument:{uri:file://<FILEPATH>}}
% OUT: {id:42,result:null}
% OUT: {id:37,result:{range:{end:{character:0,line:62},start:{character:1,line:60}},uri:file://<FILEPATH>}}
% textDocument/definition: returns the specific location in the document or file where the symbol is defined or documented. It points to the exact spot where the symbol is introduced in the code.
handle_msg("textDocument/definition", Msg, _{id: Id, result: Location}) :-
     message_id_target(Msg, Id, _, HintPath, _, Target),
     defined_at(definition,HintPath, Target, Location),!.
handle_msg("textDocument/definition", Msg, _{id: Msg.id, result: null}) :- !.


% CALL: method:textDocument/references
% IN: params:{context:{includeDeclaration:false},position:{character:9,line:81},textDocument:{uri:file://<FILEPATH>}}
% OUT: {id:42,result:null}
% OUT: {id:54,result:[{range:{end:{character:0,line:96},start:{character:29,line:95}},uri:file://<FILEPATH>},{range:{end:{character:0,line:100},start:{character:10,line:99}},uri:file://<FILEPATH>}]}
% textDocument/references: returns a list of specific locations where the symbol is referenced or called from. Moreover, it includes the results from textDocument/implementation (which itself includes textDocument/definition and textDocument/declaration), providing a comprehensive overview of the symbol's usage across the codebase.
handle_msg("textDocument/references", Msg, _{id: Id, result: Locations}) :-
     message_id_target(Msg, Id, _, HintPath, _, Target),
     findall(Location,defined_at(references, HintPath, Target, Location),Locations), !.
handle_msg("textDocument/references", Msg, _{id: Msg.id, result: []}) :- !.

% CALL: method:textDocument/implementation
% IN: params:{position:{character:11,line:22},textDocument:{uri:file://<FILEPATH>}}
% Protocal allows List as well as single (We give the list version)
% textDocument/implementation: returns a list of specific locations where the symbol is implemented. Additionally, it includes the locations returned by both textDocument/definition and textDocument/declaration, showing the full picture of where the symbol is implemented and its type associations.
handle_msg("textDocument/implementation", Msg, _{id: Id, result: Locations}) :-
     message_id_target(Msg, Id, _, HintPath, _, Target),
   findall(Location,defined_at(implementation, HintPath, Target, Location),Locations), !.
handle_msg("textDocument/implementation", Msg, _{id: Msg.id, result: []}) :- !.

% CALL: method:textDocument/declaration
% IN: params:{position:{character:11,line:22},textDocument:{uri:file://<FILEPATH>}}
% textDocument/declaration: returns the specific location of the symbol's type declaration, which can include its function definition, symbol definition, etc. Since only one location can be returned, the system chooses the most relevant type declaration for the symbol.
handle_msg("textDocument/declaration", Msg, _{id: Id, result: Location}) :-
     message_id_target(Msg, Id, _, HintPath, _, Target),
     defined_at(declaration, HintPath, Target, Location),!.
handle_msg("textDocument/declaration", Msg, _{id: Msg.id, result: null}) :- !.


% textDocument/typeDefinition: returns the specific location of the symbol's type declaration, which can include its function definition, symbol definition, etc. Since only one location can be returned, the system chooses the most relevant type declaration for the symbol.
handle_msg("textDocument/typeDefinition", Msg, _{id: Id, result: Location}) :-
     message_id_target(Msg, Id, _, HintPath, _, Target),
     defined_at(typeDefinition, HintPath, Target, Location),!.
handle_msg("textDocument/typeDefinition", Msg, _{id: Msg.id, result: null}) :- !.

% CALL: method:textDocument/completion
% IN: params:{context:{triggerKind:1},position:{character:1,line:88},textDocument:{uri:file://<FILEPATH>}}
% OUT: {id:120,result:[
%    {insertText:handle_requests(${1:_}, ${2:_}, ${3:_})$0,insertTextFormat:2,label:handle_requests/3},
%    {insertText:handle_request(${1:_}, ${2:_}, ${3:_})$0,insertTextFormat:2,label:handle_request/3},
%    {insertText:handle_msg(${1:_}, ${2:_}, ${3:_})$0,insertTextFormat:2,label:handle_msg/3},
%    {insertText:hover_at_position(${1:_}, ${2:_}, ${3:_}, ${4:_})$0,insertTextFormat:2,label:hover_at_position/4},
%    {insertText:handle_doc_changes(${1:_}, ${2:_})$0,insertTextFormat:2,label:handle_doc_changes/2}]}
% OUT: {id:123,result:[]}
handle_msg("textDocument/completion", Msg, _{id: Id, result: Completions}) :-
     _{id: Id, params: Params} :< Msg,
     _{textDocument: _{uri: Uri},
       position: _{line: Line0, character: Char0}} :< Params,
     doc_path(Uri, Path),
     succ(Line0, Line1),
     completions_at(Path, line_char(Line1, Char0), Completions), !.
handle_msg("textDocument/completion", Msg, _{id: Msg.id, result: []}) :- !. % FIXME

handle_msg("textDocument/semanticTokens", Msg, Response) :-
    handle_msg("textDocument/semanticTokens/full", Msg, Response).

% CALL: textDocument/semanticTokens/full
% IN: params:{textDocument:{uri:file://<FILEPATH>}}
% No Example from Prolog yet FIXME
handle_msg("textDocument/semanticTokens/full", Msg,
            _{id: Id, result: _{data: Highlights}}) :- fail,
     _{id: Id, params: Params} :< Msg,
     _{textDocument: _{uri: Uri}} :< Params,
     doc_path(Uri, Path),
     xref_metta_source(Path),
     metta_colours(Path, Highlights) ,!.
handle_msg("textDocument/semanticTokens/full", Msg, _{id: Msg.id, result: []}) :- !.

% CALL: textDocument/semanticTokens/range
% IN: {range:{end:{character:0,line:40},{character:0,line:0}},textDocument:{uri:file://<FILE_PATH>}}
% No Example from Prolog yet FIXME
handle_msg("textDocument/semanticTokens/range", Msg,
            _{id: Id, result: _{data: Highlights}}) :- fail,
     _{id: Id, params: Params} :< Msg,
     _{textDocument: _{uri: Uri}, range: Range} :< Params,
     _{start: _{line: StartLine0, character: StartChar},
       end: _{line: EndLine0, character: EndChar}} :< Range,
     doc_path(Uri, Path), !,
     succ(StartLine0, StartLine), succ(EndLine0, EndLine),
     xref_metta_source(Path),
     file_range_colours(Path,
                        line_char(StartLine, StartChar),
                        line_char(EndLine, EndChar),
                        Highlights).
handle_msg("textDocument/semanticTokens/range", Msg, _{id: Msg.id, result: []}) :- !.

% notifications (no response)

% CALL: textDocument/didOpen
% IN: params:{textDocument:{languageId:prolog,text:<FILE_CONTENTS>,uri:file://<FILEPATH>,version:1}}
% OUT: {method:textDocument/publishDiagnostics,params:{diagnostics:[
%    {message:Singleton variable Offset,range:{end:{character:21,line:319},start:{character:15,line:319}},severity:2,source:prolog_xref},
%    {message:Singleton variable SubPos,range:{end:{character:29,line:319},start:{character:23,line:319}},severity:2,source:prolog_xref} ... ]
%   ,uri:file://<FILEPATH>}}
handle_msg("textDocument/didOpen", Msg, Resp) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    _{text: FullText} :< TextDoc,
    %debug(lsp(low),"~w",[FullText]),
    split_text_document(FullText,SplitText),
    debug(lsp(low),"~w",[SplitText]),
    doc_path(FileUri, Path),
    retractall(lsp_metta_changes:doc_text(Path, _)),
    assertz(lsp_metta_changes:doc_text(Path, SplitText)),
    ( in_editor(Path) -> true ; assertz(in_editor(Path)) ),
    source_file_text(Path, DocFullText), % Derive from lsp_metta_changes:doc_text/2
    xref_maybe(Path, DocFullText), % Check if changed and enqueue the reindexing
    check_errors_resp(FileUri, Resp).

% Handle document change notifications
%  returning false in handle_msg/3 is because no response is expected from the lsp server
handle_msg("textDocument/didChange", Msg, false) :-
    _{params: _{textDocument: TextDoc,
                contentChanges: Changes}} :< Msg,
    _{uri: Uri} :< TextDoc,
    doc_path(Uri, Path),
    handle_doc_changes(Path, Changes),
    source_file_text(Path, DocFullText), % Derive from lsp_metta_changes:doc_text/2
    xref_maybe(Path, DocFullText). % Check if changed and enqueue the reindexing

% Handle document save notifications
handle_msg("textDocument/didSave", Msg, Resp) :-
    _{params: Params} :< Msg,
    % xref_source_expired(Params.textDocument.uri),
    check_errors_resp(Params.textDocument.uri, Resp).

% Handle document close notifications
handle_msg("textDocument/didClose", Msg, false) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    doc_path(FileUri, Path),
    retractall(in_editor(Path)).

handle_msg("initialized", Msg, false) :- !,
    debug(lsp(main), "initialized ~w", [Msg]).

handle_msg("$/setTrace", _Msg, false).

% Handle the $/cancelRequest Notification
handle_msg("$/cancelRequest", Msg, false) :-
    _{params: _{id: CancelId}} :< Msg,
    debug(lsp(threads), "Cancel request received for ID ~w", [CancelId]),
    with_mutex('$lsp_request_mutex',
        (   thread_request(CancelId, ThreadId)
        ->  % Attempt to interrupt the thread
            debug(lsp(threads), "Attempting to cancel thread ~w", [ThreadId]),
            catch(thread_signal(ThreadId, throw(canceled)), _, true),  % In case thread is already gone
            ignore(retract(thread_request(CancelId, ThreadId)))        % In case thread retracted it
        ;   ( debug(lsp(threads), "No running thread found for request ID ~w", [CancelId]),
              assertz(id_was_canceled(CancelId))
            )
        )).

% Handle the 'exit' notification
handle_msg("exit", _Msg, false) :-
    debug(lsp(main), "Received exit, shutting down", []),
    halt.


% Handle the 'workspace/symbol' Request
handle_msg("workspace/symbol", Msg, _{id: Id, result: Symbols}) :-
    _{id: Id, params: Params} :< Msg,
    _{query: Query} :< Params,
    collect_workspace_symbols(Query, Symbols).

% Collect Workspace Symbols
collect_workspace_symbols(Query, Symbols) :-
    findall(Symbol,
        (
            in_editor(Path),
            % Convert file path to URI
            path_doc(Path, DocUri),
            xref_document_symbols(DocUri, DocSymbols),
            member(Symbol, DocSymbols),
            symbol_matches_query(Symbol, Query)
        ),
        Symbols).

% Predicate to check if a symbol matches the query
symbol_matches_query(Symbol, Query) :-
    ( Query == "" -> true  % If query is empty, include all symbols
    ; get_symbol_name(Symbol, Name),
      sub_atom_icasechk(Name, _, Query)  % Case-insensitive match
    ).

% Helper predicate to extract the symbol's name
get_symbol_name(Symbol, Name) :-
    % Symbol may be in hierarchical or non-hierarchical format
    ( get_dict(name, Symbol, Name)
    ; get_dict(label, Symbol, Name)
    ).

% wildcard
handle_msg(_, Msg, _{id: Id, error: _{code: -32603, message: "Unimplemented"}}) :-
    _{id: Id} :< Msg, !,
    debug(lsp(errors), "unknown message ~w", [Msg]).
handle_msg(_, Msg, false) :-
    debug(lsp(errors), "unknown notification ~w", [Msg]).

% [TODO]Check errors and respond with diagnostics
check_errors_resp(FileUri, _{method: "textDocument/publishDiagnostics",
                             params: _{uri: FileUri, diagnostics: Errors}}) :-
    doc_path(FileUri, Path),
    metta_check_errors(Path, Errors).  % only notices unbalanced parens (and not smart about ones found in quotes)

% [TODO]Check errors and respond with diagnostics
check_errors_resp(FileUri, _{method: "textDocument/publishDiagnostics",
                             params: _{uri: FileUri, diagnostics: Errors}}) :- fail,
    doc_path(FileUri, Path),
    prolog_check_errors(Path, Errors).
check_errors_resp(_, false) :-
    debug(lsp(errors), "Failed checking errors", []).

