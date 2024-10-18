:- module(lsp_server_metta, [main/0]).
/** <module> LSP Server

The main entry point for the Language Server implementation.

@author James Cash
*/

:- use_module(library(apply), [maplist/2]).
:- use_module(library(debug), [debug/3, debug/1]).
:- use_module(library(http/json), [atom_json_dict/3]).
%:- use_module(library(prolog_xref)).
%:- use_module(library(prolog_source), [directory_source_files/3]).
:- use_module(library(utf8), [utf8_codes//1]).
:- use_module(library(yall)).

:- use_module(lsp_metta_utils).
:- use_module(lsp_metta_checking, [check_errors/2]).
:- use_module(lsp_json_parser, [lsp_metta_request//1]).
:- use_module(lsp_metta_changes, [handle_doc_changes/2]).
:- use_module(lsp_metta_completion, [completions_at/3]).
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
:- user:ensure_loaded(lsp_metta_outline). %( [xref_source/1, xref_document_symbol/5, xref_document_symbols/2]).
:- dynamic(user:full_text/2).

:- dynamic lsp_metta_changes:doc_text/2.

:- discontiguous lsp_server_metta:handle_msg/3.

main :-
    set_prolog_flag(debug_on_error, false),
    set_prolog_flag(report_error, true),
    set_prolog_flag(toplevel_prompt, ''),
    current_prolog_flag(argv, Args),
    debug(server),
    debug(server(high)),
    load_mettalog_xref,
    start(Args).

start([stdio]) :- !,
    debug(server, "Starting stdio client", []),
    stdio_server.
start(Args) :-
    debug(server, "Unknown args ~w", [Args]).

% stdio server

stdio_server :-
    current_input(In),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    % handling UTF decoding in JSON parsing, but doing the auto-translation
    % causes Content-Length to be incorrect
    set_stream(In, encoding(octet)),
    current_output(Out),
    set_stream(Out, encoding(utf8)),
    stdio_handler(A-A, In).

% [TODO] add multithreading? Guess that will also need a message queue
% to write to stdout
stdio_handler(Extra-ExtraTail, In) :-
    wait_for_input([In], _, infinite),
    fill_buffer(In),
    read_pending_codes(In, ReadCodes, Tail),
    ( Tail == []
    -> true
    ; ( current_output(Out),
        ExtraTail = ReadCodes,
        handle_requests(Out, Extra, Remainder),
        stdio_handler(Remainder-Tail, In) )
    ).

handle_requests(Out, In, Tail) :-
    handle_request(Out, In, Rest), !,
    ( var(Rest)
    -> Tail = Rest
    ; handle_requests(Out, Rest, Tail) ).
handle_requests(_, T, T).

% general handling stuff
catch_with_backtrace(Goal):-
     catch_with_backtrace(Goal,Err,
             (debug(server(high), "error in ~n~n?- catch_with_backtrace(~q).~n~n handling msg:~n~n~@~n~n", [Goal, print_message(error, Err)]),throw(Err))).


send_message(Stream, Msg) :-
  catch_with_backtrace((
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes), width(0)]),
    phrase(utf8_codes(JsonCodes), UTF8Codes),
    length(UTF8Codes, ContentLength),
    format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]),
    flush_output(Stream))).

handle_request(OutStream, Input, Rest) :-
    phrase(lsp_metta_request(Req), Input, Rest),
    debug(server(high), "Request ~q", [Req.body]),
    catch(
        ( catch_with_backtrace(handle_msg(Req.body.method, Req.body, Resp)),
          ignore((user:nodebug_lsp_response(Req.body.method) -> debug(server(high), "response id: ~q", [Resp.id]) ; debug(server(high), "response ~q", [Resp]))),
           %debug(server(high), "response ~q", [Resp]),
          ( is_dict(Resp) -> send_message(OutStream, Resp) ; true ) ),
        Err,
        ( debug(server, "error handling msg ~q", [Err]),
          get_dict(id, Req.body, Id),
          send_message(OutStream, _{id: Id,
                           error: _{code: -32001,
                                             message: "server error"}})
        )).

%hide some response messages
%user:nodebug_lsp_response("textDocument/hover").
user:nodebug_lsp_response("textDocument/documentSymbol").


% Handling messages

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
      workspaceSymbolProvider: true,
      definitionProvider: true,
      declarationProvider: true,
      implementationProvider: true,
      typeDefinitionProvider: true,
      referencesProvider: true,
      %% documentHighlightProvider: false,
      codeActionProvider: false,
      %% codeLensProvider: false,
      documentFormattingProvider:false,
      %% documentOnTypeFormattingProvider: false,
      renameProvider: false,
      % documentLinkProvider: false,
      % colorProvider: true,
      foldingRangeProvider: false,
      executeCommandProvider: _{commands: ["eval_metta", "query_metta", "assert_metta"]},
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



:- dynamic loaded_source/1.

% is not already an object?
into_result_object(Help,Response):- \+ is_dict(Help),
   Response = _{contents: _{kind: plaintext, value: Help}}.
into_result_object(Help,Response):-  Help=Response,!.

:- discontiguous(handle_msg/3).

% messages (with a response)
handle_msg("initialize", Msg,
           _{id: Id, result: _{capabilities: ServerCapabilities} }) :-
    _{id: Id, params: Params} :< Msg, !,
    ( Params.rootUri \== null
    -> ( atom_concat('file://', RootPath, Params.rootUri),
         directory_source_files(RootPath, Files, [recursive(true)]),
         maplist([F]>>assert(loaded_source(F)), Files) )
    ; true ),
    server_capabilities(ServerCapabilities).
handle_msg("shutdown", Msg, _{id: Id, result: null}) :-
    _{id: Id} :< Msg,
    debug(server, "recieved shutdown message", []).

% CALL: textDocument/hover
% IN: params:{position:{character:11,line:56},textDocument:{uri:file://<FILEPATH>}}}
% OUT: {id:21,result:{contents:{kind:plaintext,value:<ALL_THE_STUFF>}}}
handle_msg("textDocument/hover", Msg, _{id: Id, result: Response}) :- % fail,
    _{params: _{position: _{character: Char0, line: Line},
                textDocument: _{uri: Doc}}, id: Id} :< Msg,
    atom_concat('file://', Path, Doc),
    (  help_at_position(Path, Line, Char0, Help)
    -> into_result_object(Help, Response)
    ;  Response = null), !.
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
%     atom_concat('file://', Path, Doc), !,
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
    atom_concat('file://', HintPath, Doc),   Loc = line_char(Line0, Char0),
    Loc = line_char(Line0, Char0),
    lsp_metta_utils:clause_with_arity_in_file_at_position(Name, Arity, HintPath, Loc),
        debug(server(high),"~n~q~n",[message_id_target(_Msg, Id, Doc, HintPath, Loc, Name/Arity)]).

% CALL: method:textDocument/definition
% IN: params:{position:{character:55,line:174},textDocument:{uri:file://<FILEPATH>}}
% OUT: {id:42,result:null}
% OUT: {id:37,result:{range:{end:{character:0,line:62},start:{character:1,line:60}},uri:file://<FILEPATH>}}
% textDocument/definition: returns the specific location in the document or file where the symbol is defined or documented. It points to the exact spot where the symbol is introduced in the code.
handle_msg("textDocument/definition", Msg, _{id: Id, result: Location}) :-
     message_id_target(Msg, Id, _, PathHint, _, Target),
     debug(server(high),"~q",[defined_at(definition, PathHint, Target, Location)]),
     defined_at(definition,PathHint, Target, Location),!.
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
%    {insertText:help_at_position(${1:_}, ${2:_}, ${3:_}, ${4:_})$0,insertTextFormat:2,label:help_at_position/4},
%    {insertText:handle_doc_changes(${1:_}, ${2:_})$0,insertTextFormat:2,label:handle_doc_changes/2}]}
% OUT: {id:123,result:[]}
handle_msg("textDocument/completion", Msg, _{id: Msg.id, result: []}) :- !. % FIXME
% handle_msg("textDocument/completion", Msg, _{id: Id, result: Completions}) :-
%     _{id: Id, params: Params} :< Msg,
%     _{textDocument: _{uri: Uri},
%       position: _{line: Line0, character: Char0}} :< Params,
%     atom_concat('file://', Path, Uri),
%     succ(Line0, Line1),
%     completions_at(Path, line_char(Line1, Char0), Completions).

handle_msg("textDocument/semanticTokens", Msg, Response) :-
    handle_msg("textDocument/semanticTokens/full", Msg, Response).

% CALL: textDocument/semanticTokens/full
% IN: params:{textDocument:{uri:file://<FILEPATH>}}
% No Example from Prolog yet FIXME
handle_msg("textDocument/semanticTokens/full", Msg, _{id: Msg.id, result: []}) :- !. % FIXME
% handle_msg("textDocument/semanticTokens/full", Msg,
%            _{id: Id, result: _{data: Highlights}}) :-
%     _{id: Id, params: Params} :< Msg,
%     _{textDocument: _{uri: Uri}} :< Params,
%     atom_concat('file://', Path, Uri), !,
%     xref_source(Path),
%     file_colours(Path, Highlights).

% CALL: textDocument/semanticTokens/range
% IN: {range:{end:{character:0,line:40},{character:0,line:0}},textDocument:{uri:file://<FILE_PATH>}}
% No Example from Prolog yet FIXME
handle_msg("textDocument/semanticTokens/range", Msg, _{id: Msg.id, result: []}) :- !. % FIXME
% handle_msg("textDocument/semanticTokens/range", Msg,
%            _{id: Id, result: _{data: Highlights}}) :-
%     _{id: Id, params: Params} :< Msg,
%     _{textDocument: _{uri: Uri}, range: Range} :< Params,
%     _{start: _{line: StartLine0, character: StartChar},
%       end: _{line: EndLine0, character: EndChar}} :< Range,
%     atom_concat('file://', Path, Uri), !,
%     succ(StartLine0, StartLine), succ(EndLine0, EndLine),
%     xref_source(Path),
%     file_range_colours(Path,
%                        line_char(StartLine, StartChar),
%                        line_char(EndLine, EndChar),
%                        Highlights).

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
    %debug(server,"~w",[FullText]),
    split_text_document(FullText,SplitText),
    debug(server,"~w",[SplitText]),
    atom_concat('file://', Path, FileUri),
    xref_maybe(Path, FullText), % Check if changed and enqueue the reindexing
    retractall(lsp_metta_changes:doc_text(Path, _)),
    assertz(lsp_metta_changes:doc_text(Path, SplitText)),
    ( loaded_source(Path) ; assertz(loaded_source(Path)) ),
    check_errors_resp(FileUri, Resp).

handle_msg("textDocument/didChange", Msg, false) :-
    _{params: _{textDocument: TextDoc,
                contentChanges: Changes}} :< Msg,
    _{uri: Uri} :< TextDoc,
    atom_concat('file://', Path, Uri),
    handle_doc_changes(Path, Changes),
    lsp_metta_changes:doc_text(Path,FullText),
    xref_maybe(Path, FullText). % Check if changed and enqueue the reindexing

handle_msg("textDocument/didSave", Msg, Resp) :-
    _{params: Params} :< Msg,
    % xref_source_expired(Params.textDocument.uri),
    check_errors_resp(Params.textDocument.uri, Resp).

handle_msg("textDocument/didClose", Msg, false) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    atom_concat('file://', Path, FileUri),
    retractall(loaded_source(Path)).

handle_msg("initialized", Msg, false) :-
    debug(server, "initialized ~w", [Msg]).

handle_msg("$/setTrace", _Msg, false).

handle_msg("$/cancelRequest", Msg, false) :-
    debug(server, "Cancel request Msg ~w", [Msg]).

handle_msg("exit", _Msg, false) :-
    debug(server, "recieved exit, shutting down", []),
    halt.

% wildcard
handle_msg(_, Msg, _{id: Id, error: _{code: -32603, message: "Unimplemented"}}) :-
    _{id: Id} :< Msg, !,
    debug(server, "unknown message ~w", [Msg]).
handle_msg(_, Msg, false) :-
    debug(server, "unknown notification ~w", [Msg]).

check_errors_resp(FileUri, _{method: "textDocument/publishDiagnostics",
                             params: _{uri: FileUri, diagnostics: Errors}}) :-
    atom_concat('file://', Path, FileUri),
    check_errors(Path, Errors).
check_errors_resp(_, false) :-
    debug(server, "Failed checking errors", []).

