:- module(lsp_server_metta, [main/0, send_client_message/1, debug_lsp/3, debug_lsp/2, first_dict_key/3 ]).
/** <module> LSP Server

The main entry point for the Language Server implementation with dynamic handling based on max threads.

Handles workspace folder changes, file indexing, and incremental updates.

Supports LSP methods like hover, document symbol, definition, references, and more.

@author James Cash
*/

:- debug(lsp(main)).
:- debug(lsp(errors)),
   debug(lsp(todo)),
   debug(lsp(threads)),
   debug(lsp(high)),
   debug(lsp(xref)),
   debug(lsp(todo)),
   %debug(lsp(position)),
   !.

:- use_module(library(apply), [maplist/2]).
:- use_module(library(debug), [debug/3, debug/1]).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(thread)).
:- use_module(library(thread_pool)).
%:- use_module(library(prolog_xref)).
%:- use_module(library(prolog_source), [directory_source_files/3]).
:- use_module(library(utf8), [utf8_codes//1]).
:- use_module(library(yall)).

:- include(lsp_server_hooks).
:- include(lsp_metta_include).

:- set_prolog_flag(gc,false).

:- user:ensure_loaded(lsp_metta_utils).
:- use_module(lsp_metta_checking, [metta_check_errors/2]).
:- use_module(lsp_json_parser, [lsp_metta_request//1]).
%:- use_module(lsp_metta_changes, [handle_doc_changes_d4/2]).

:- ensure_loaded(lsp_metta_completion).

:- use_module(lsp_prolog_colours, [
                            file_colours/2,
                            file_range_colours/4,
                            token_types/1,
                            token_modifiers/1]).
:- use_module(lsp_metta_xref).
/*
:- use_module(lsp_metta_split, [
        split_text_document_d4/2,
        coalesce_text_d4/2
]).
*/

% will change to module in a few days (easier to test externally from `user`)
:- user:ensure_loaded(lsp_metta_code_actions).
:- user:ensure_loaded(lsp_metta_save_actions).
:- user:ensure_loaded(lsp_metta_hover).
:- user:ensure_loaded(lsp_metta_workspace).
:- user:ensure_loaded(lsp_metta_references).
:- user:ensure_loaded(lsp_metta_outline). %( [xref_metta_source/1, xref_document_symbol/5, xref_document_symbols/2]).
:- dynamic(user:full_text/2).
:- user:ensure_loaded(lsp_prolog_changes).
:- user:ensure_loaded(lsp_prolog_checking).
:- user:ensure_loaded(lsp_prolog_colours).
:- user:ensure_loaded(lsp_prolog_utils).

:- dynamic lsp_metta_changes:doc_text_d4/2.

:- discontiguous lsp_server_metta:handle_msg/3.

:- multifile(lsp_hooks:handle_msg_hook/3).
:- dynamic(lsp_hooks:handle_msg_hook/3).
:- discontiguous(lsp_hooks:handle_msg_hook/3).


% Main entry point
main :-
    set_prolog_flag(debug_on_error, false),
    set_prolog_flag(report_error, true),
    set_prolog_flag(toplevel_prompt, ''),
    current_prolog_flag(argv, Args),
   nodebug(lsp(_)), % Everything
    %prolog_ide(debug_monitor),
    %debug(lsp(low)),
    debug(lsp(main)),
    debug(lsp(errors)),
    debug(lsp(threads)),
    debug(lsp(high)),
    debug(lsp(todo)),
    %debug(lsp(position)),
    debug(lsp(xref)),
    load_mettalog_xref,
    start(Args).


% Start the server based on input arguments
start([stdio]) :- !,
    debug_lsp(main, "Starting stdio client", []),
    stdio_server.
start(Args) :-
    debug_lsp(main, "Unknown args ~w", [Args]).

:- dynamic(lsp_hooks:is_lsp_output_stream/1).
:- volatile(lsp_hooks:is_lsp_output_stream/1).

% stdio server initialization
stdio_server :-
    current_input(In),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    % Handling UTF decoding in JSON parsing, but doing the auto-translation
    % causes Content-Length to be incorrect
    set_stream(In, encoding(octet)),
    current_output(Out),
    set_stream(Out, encoding(utf8)),
    %stdio_handler_io(In, Out). %(might use this one later)
    asserta(lsp_hooks:is_lsp_output_stream(Out)),
    stream_property(StdErr,file_no(2)),
    set_system_IO(In,Out,StdErr), % ensure we are talking over stdin/stdout
    set_prolog_IO(In,StdErr,StdErr), % redirect **accidental** writes to stdout to stderr instead
    stdio_handler(In, Out).

stdio_handler(In, Out):-
  repeat,
   catch(stdio_handler(A-A, In, Out),_,fail),
   fail.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BEGIN Threading/Queueing System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle requests that need immediate or cancellable responses
immediate_method(Request) :- is_dict(Request), !, immediate_request(Request).
immediate_method("$/cancelRequest").
immediate_method(TM) :- cancelable_method(TM), !, fail.
% immediate_method(_). % This line would act as a fallback for "immediate" methods, allowing any request method not specified as "cancellable" to be treated as immediate if uncommented.

cancelable_method('textDocument/documentSymbol').
cancelable_method('textDocument/hover').
% cancelable_method('textDocument/didOpen'). % This line is an option to make 'textDocument/didOpen' cancelable if needed

% Extract everything after the last '/' in the FileUri
after_slash(FileUri, FileUriAS) :-
    % Split the URI on '/' and get the last segment
    atomic_list_concat(Segments, '/', FileUri),
    last(Segments, FileUriAS), !.
after_slash(FileUri, FileUriAS) :-
    % Fallback to formatted string if no '/' is found
    sformat(FileUriAS, '~q', [FileUri]), !.

% Handle parsed requests
handle_parsed_request(Out, Req) :-
    % Extract the method name, request ID, and URI for logging
    first_dict_key(method, Req, Method),
    first_dict_key(command;data;uri, Req, FileUri),
    request_id(Req, RequestId),
    after_slash(Method,MethodAS),
    after_slash(FileUri,FileUriAS),
    atomic_list_concat([MethodAS,FileUriAS,''],'_',Stem),
    (number(RequestId) -> JobId = RequestId ; gensym(Stem, JobId)),
    get_time(Time), asserta(post_time(JobId, Time)),
    (number(RequestId)-> ( assert(id_info(RequestId,JobInfo))) ; true),
    sformat(JobInfo, "JID: ~w ~q=~q", [JobId, Method, FileUri]),
    % Handle the request based on threading mode or immediacy
    ((lsp_worker_threads(0) ; immediate_method(Method)) ->
        (handle_request(JobId, JobInfo, Out, Req))
    ;( post_job('$lsp_worker_pool', lsp_task(Out, JobId, JobInfo, Req)),
       debug_lsp(threads, "Posted job for ~w", [JobInfo]))),!.

% Post a job by storing it in the database and posting the job ID to the queue
post_job(QueueId, Task) :-
    % Determine the JobId to use: use the RequestId if available, otherwise generate a unique ID
    (  (Task = lsp_task(_, JobId, _JobInfo, _))
    ->  true
    ;   gensym(job_id_, JobId)  % Generate a unique ID if RequestId is absent
    ),

    % Store the job in the database with the chosen JobId
    assertz(job_data(JobId, Task)),

    % Post only the JobId to the message queue
    with_mutex('$lsp_request_mutex', (
        start_lsp_worker_threads,
        thread_send_message(QueueId, JobId),
        nop(debug_lsp(threads, "Posted job with ID ~w", [JobId]))
    )),!.

% New dynamic predicate to store job data in the database
:- dynamic job_data/2.
:- dynamic id_info/2.

% Worker loop
do_work(QueueId) :-
    repeat,
    catch(do_work_stuff(QueueId), _, true),
    fail.

do_work_stuff(QueueId) :-
    thread_self(ThreadId),
    stream_property(StdIn,file_no(0)), % locate the REAL stdin
    stream_property(StdErr,file_no(2)), % locate the REAL stderr
    set_prolog_IO(StdIn,StdErr,StdErr), % redirect accidental writes to stdout to stderr instead
    repeat,
    once(do_work_stuff_tid(QueueId, ThreadId)),
    fail.

do_work_stuff_tid(QueueId, ThreadId) :-
    repeat,
    % Retrieve the job ID from the queue
    thread_get_message(QueueId, JobId),

    % Fetch the actual job data from the database
    (   retract(job_data(JobId, Task))
    -> (Task = lsp_task(Out, _, JobInfo, Req),
        request_id(Req, RequestId),
        (JobId==RequestId -> JR = JobId  ; JR = (JobId/RequestId)),
        % Register this thread handling RequestId
        with_mutex('$lsp_request_mutex', (
            (id_was_canceled(RequestId) ->
               (debug_lsp(threads, "Request ~w was canceled before it got started! ~w", [JR, JobInfo]),
                ignore(retract(job_data(RequestId,_))),
                ignore(id_was_canceled(RequestId))),
                ignore(retract(id_info(RequestId,_))),
                debug_lsp(threads, "Request ~w was canceled: ~w", [JR, JobInfo]),
                send_cancellation_response(Out, RequestId),
                throw(canceled),
                true)
            ; assertz(task_thread(RequestId, ThreadId))
        ))),

        debug_lsp(threads, "Worker ~w processing task with JobId ~w: ~w", [ThreadId, JR, JobInfo]),

        % Process the request and handle cancellation
        catch(
            handle_request(JobId, JobInfo, Out, Req),
            canceled,
            ( debug_lsp(threads, "Request ~w was canceled: ~w", [JR, JobInfo]),
              send_cancellation_response(Out, RequestId)
            )
        ),

        % Clean up task_thread
        with_mutex('$lsp_request_mutex', (
            ignore(retract(task_thread(RequestId, ThreadId))),
            true))
    ;   debug_lsp(threads, "Job ID ~w not found in the database", [JobId])
    ).



% If the max worker thread count is 0, it processes requests synchronously;
% otherwise, it uses a thread pool for parallel processing.
:- dynamic(lsp_worker_threads_max/1).

% Handle threading based on max threads.
lsp_worker_threads(N) :- lsp_worker_threads_max(N), !.
lsp_worker_threads(MaxThreads) :-
    current_prolog_flag(argv, Args),
    append(_, ['--workers', MaxThreadsArg | _], Args),
    atom_number(MaxThreadsArg, MaxThreads),
    assert(lsp_worker_threads_max(MaxThreads)),
    !.
% The following commented-out lines are configuration options for the worker threads:
%lsp_worker_threads(0). % no threads, thus "$/cancelRequest" cannot be implemented as requests are processed synchronously
% lsp_worker_threads(1). % 1 thread, enabling "$/cancelRequest" since requests are handled one at a time asynchronously
% lsp_worker_threads(3). % 3 threads, allows some parallel processing and handling of "$/cancelRequest"
lsp_worker_threads(10). % 10 threads is the current setting, allowing high concurrency but could be overkill for some cases

:- dynamic(started_lsp_worker_threads/0).

% Start worker threads or run synchronously based on max threads.
start_lsp_worker_threads :-
    started_lsp_worker_threads, !.
start_lsp_worker_threads :-
    assert(started_lsp_worker_threads),
    lsp_worker_threads(MaxThreads),
    (MaxThreads > 0 ->
        create_workers('$lsp_worker_pool', MaxThreads)
    ; debug_lsp(threads, "Running synchronously since max threads = 0", [])
    ).

% Worker pool implementation
create_workers(QueueId, N) :-
    message_queue_create(QueueId),
    forall(between(1, N, _),
           thread_create(do_work(QueueId), _, [])),
    debug_lsp(threads, "~q", [create_workers(QueueId, N)]).

% Dynamic predicates for task management and cancellation
:- dynamic task_thread/2.
:- dynamic id_was_canceled/1.

% Create a mutex for synchronization
:- if(\+ prolog_load_context(reloading, true)).
    :- mutex_create('$lsp_request_mutex').
    :- mutex_create('$lsp_response_mutex').
:- endif.

% Cancel a specific task by ID
cancel_taskid(CancelId) :-
    debug_lsp(threads, "Cancel request received for ID ~w", [CancelId]),
    with_mutex('$lsp_request_mutex', (
        ignore(retract(job_data(CancelId, _))),
        (task_thread(CancelId, ThreadId) ->
           (debug_lsp(threads, "Attempting to cancel thread ~w", [ThreadId]),
            catch(thread_signal(ThreadId, throw(canceled)), _, true),  % in case the thread is gone
            ignore(retract(task_thread(CancelId, ThreadId))),
            ignore(retract(id_info(CancelId, _))),
            ignore(retract(task_thread(CancelId, ThreadId))))  % in case it didnt clean up after itself
        ; (debug_lsp(threads, "No running thread found for request ID ~w", [CancelId]),
           assertz(id_was_canceled(CancelId)))
        )
    )).

% Send a cancellation response if required
send_cancellation_response(OutStream, RequestId) :-
    % According to LSP, the server should not send a response to a canceled request,
    % but some clients may expect a response indicating cancellation.
    % Uncomment the following lines if you want to send such a response.
    nop((Response = _{jsonrpc: "2.0", id: RequestId, error: _{code: -32800, message: "Request canceled"}},
    send_message(OutStream, Response))),
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END Threading/Queueing System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Backtrace error handler
catch_with_backtrace(Goal):-
     catch_with_backtrace(Goal,Err,
        ( Err == canceled ->
            throw(canceled)
        ; ( debug_lsp(errors, "Error in:\n\n?- catch_with_backtrace(~q).\n\nHandling message:\n\n~@~n\n", [Goal, print_message(error, Err)]),
            throw(Err)
          )
        )
     ).

lsp_output_stream(OutStream):- nb_current('$lsp_output_stream', OutStream),!.
lsp_output_stream(OutStream):- lsp_hooks:is_lsp_output_stream(OutStream).

send_client_message(Msg) :-
  debug_lsp(requests,"~q",[send_client_message(Msg)]),
  ignore((lsp_output_stream(OutStream),
    send_message(OutStream,Msg))),!.

% Send LSP message to client
send_message(Stream, Msg):-
  (var(Stream)->lsp_output_stream(Stream);true),
   catch(send_message_unsafe(Stream, Msg),_,true),!.

send_message_unsafe(Stream, Msg) :-
  catch_with_backtrace((
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes), width(0)]),
    phrase(utf8_codes(JsonCodes), UTF8Codes),
    length(UTF8Codes, ContentLength),
    with_mutex('$lsp_response_mutex',
         (%text_to_string(JsonCodes,JsonString),sformat(S, "\n\nContent-Length: ~w\r\n\r\n-->~q<--\n\n", [ContentLength, JsonString]),
          %flush_output(user_error), format(user_error, '~N~w',[S]), nl(user_error),nl(user_error),flush_output(user_error),
          format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]),
          flush_output(Stream))))).

    trim_to_slength(InputString, MaxLength, S) :-
        string_length(InputString, Length),
        (
          Length =< MaxLength ->
          S = InputString  % If the string is shorter than or equal to MaxLength, return it as is.
        ;
         ( sub_string(InputString, 0, MaxLength, _, TrimmedString),  % Otherwise, trim it to MaxLength.
          sformat(S,'~w...~w...',[TrimmedString,Length]))
        ),! .


debug_lsp(Topic,Arg):- debug_lsp(Topic,'~q',[Arg]).
debug_lsp(Topic,Format,Args):-
 ignore((% debugging(Topic),
   \+ \+ ((hide_gvars(Args),
           flush_output(user_error), format(user_error, '~N~w: ',[Topic]),
           sformat(S, Format, Args), trim_to_slength(S,300,SS),
           format(user_error, '~w~n',[SS]),
           nl(user_error),nl(user_error),flush_output(user_error),
           nop((debug(lsp(Topic),Format,Args))))))), !.

hide_gvars(Arg):- numbervars(Arg,696,_,[attvar(skip),singletons(true)]).

request_id(Req, RequestId):- \+ is_dict(Req), !, RequestId = not_dict(Req).
request_id(Req, RequestId):- get_dict(id, Req, RequestId), !.
request_id(Req, RequestId):- get_dict(body, Req, Body), !, request_id(Body, RequestId).
request_id(  _, none).

first_dict_key(_Key, Req, RequestId):- \+ is_dict(Req), !, RequestId = not_dict(Req).
first_dict_key( Key, Req, RequestId):- each_key(Key,Try), get_dict(Try, Req, RequestId), !.
first_dict_key( Key, Req, RequestId):- get_dict(_, Req, Body), first_dict_key(Key, Body, RequestId), RequestId\==none, RequestId\=not_dict(_), !.
first_dict_key(_Key,   _, none).

each_key(Key,Try):- \+ compound(Key),!,atom(Key),Try=Key.
each_key(Key,Try):- arg(_,Key,Maybe),each_key(Maybe,Try).

% Handle individual requests
% Predicate to store the posting time of each job by JobId
:- dynamic post_time/2.

% Helper to calculate and format time difference with appropriate units
% If Description is provided, it prepends it to the formatted time. If not applicable, returns an empty string.
time_diff_string(Start, End, Description, DurationString) :-
    Duration is End - Start,
    (   Duration < 0.01
    ->  _Microseconds is Duration * 1_000_000,
        format(atom(DurationString), "~w: usecs", [Description])
    ;   Duration < 1
    ->  Milliseconds is Duration * 1_000,
        format(atom(DurationString), "~w: ~2f ms", [Description, Milliseconds])
    ;   format(atom(DurationString), "~w: ~2f secs!!!!", [Description, Duration])
    ), !.

time_diff_string(Start, End, Description, DurationString) :-
    Duration is End - Start,
    (   Duration < 0.001
    ->  Microseconds is Duration * 1_000_000,
        format(atom(DurationString), "~w: ~2f usecs", [Description, Microseconds])
    ;   Duration < 1
    ->  Milliseconds is Duration * 1_000,
        format(atom(DurationString), "~w: ~2f ms", [Description, Milliseconds])
    ;   format(atom(DurationString), "~w: ~2f secs", [Description, Duration])
    ).
time_diff_string(_, _, "", "").  % If no Description, return empty string

% Handle the job and log all time durations
handle_request(JobId, JobInfo, OutStream, Req) :-
    nb_setval('$lsp_output_stream', OutStream),
    % Retrieve post time
    (retract(post_time(JobId, PostTime)) -> true ; get_time(PostTime)),
    first_dict_key(body, Req, Body),
    first_dict_key(method, Body, Method),
    request_id(Req, Id),
    catch(
        ( get_time(StartTime),
          time_diff_string(PostTime, StartTime, "Waited", DurationPostToStart),
          debug_lsp(high, "Request ~w started after ~w", [JobInfo, DurationPostToStart]),
          debug_lsp(high, "..~q.", [Req]),

          % Process the request
          catch_with_backtrace(handle_msg(Method, Req.body, Resp)),

          get_time(EndTime),
          time_diff_string(StartTime, EndTime, "Processed", DurationStartToEnd),
          time_diff_string(PostTime, EndTime, "Total", DurationPostToEnd),
          request_id(Resp, RespIdNone),
          (number(RespIdNone) -> RespId = RespIdNone ; RespId = Id),

          % Calculate time for sending response if needed
          ( is_dict(Resp) ->
              get_time(SendStartTime),
              send_message(OutStream, Resp),
              get_time(SendEndTime),
              time_diff_string(SendStartTime, SendEndTime, "Response", SendTime)
          ;  SendTime = ""  % No response to send, so no send time
          ),

          % Log all durations including SendTime
          ignore((
               (user:nodebug_lsp_response(Method), Resp \== false)
               -> debug_lsp(high, "Response id: ~q (~w) <.. hidden debug ...> -- ~w, ~w, ~w, ~w",
                            [RespId, JobInfo, DurationPostToEnd, DurationPostToStart, DurationStartToEnd, SendTime])
               ; debug_lsp(high, "Response id: ~q (~w) ~q -- ~w, ~w, ~w, ~w",
                            [RespId, JobInfo, Resp, DurationPostToEnd, DurationPostToStart, DurationStartToEnd, SendTime])
               ))
        ),
        Err,
        ( Err == canceled ->
            ( get_time(CancelTime),
              time_diff_string(StartTime, CancelTime, "Processed", DurationStartToEnd),
              time_diff_string(PostTime, CancelTime, "Total", DurationPostToEnd),
              debug_lsp(high, "Request id ~w canceled after -- ~q, ~q, ~q",
                        [JobInfo, DurationPostToEnd, DurationPostToStart, DurationStartToEnd]),
              throw(canceled)
            )
        ; ( get_time(ErrorTime),
            time_diff_string(StartTime, ErrorTime, "Processed", DurationStartToEnd),
            time_diff_string(PostTime, ErrorTime, "Total", DurationPostToEnd),
            debug_lsp(errors, "Error handling msg ~q in ~w after -- ~q, ~q, ~q",
                      [Err, JobInfo, DurationPostToEnd, DurationPostToStart, DurationStartToEnd]),
            ignore((
                get_dict(id, Req.body, Id),
                send_message(OutStream, _{id: Id,
                           error: _{code: -32001, message: "server error"}}))
            )
        ))
    ).

% Hide responses for certain methods
user:nodebug_lsp_response("textDocument/hover").
user:nodebug_lsp_response("textDocument/documentSymbol").
user:nodebug_lsp_response("textDocument/codeAction").


% Server capabilities declaration
server_capabilities(
    _{
        % Configuration change notifications
        workspace: _{
            workspaceFolders: _{
                supported: true,
                changeNotifications: true  % Notify server of added/removed workspace folders
            },
            didChangeConfiguration: true,  % Notify server when configuration changes
            didChangeWatchedFiles: _{
                watchers: [_{globPattern: "**/*"}]  % Watch all files in the workspace
            }
        },

        textDocumentSync: _{
            openClose: false,
            change: 2,  % incremental
            save: _{includeText: false},
            willSave: true,
            willSaveWaitUntil: true
        },

        /*
        Explanation of textDocumentSync settings:

        1. openClose: false
           - The server does not receive notifications when documents are opened (`didOpen`) or closed (`didClose`).
           - Although `openClose` is disabled, we can still obtain information about open documents through `documentSymbol` requests, as they can only be requested for documents currently open in the editor.

        2. change: 2
           - Specifies that the server wants to receive only incremental changes when a document is modified (`didChange`).
           - This setting reduces the amount of data sent, as only differences (not the full document) are sent with each change.

        3. save: _{includeText: false}
           - On save (`didSave`), the server will receive a notification but not the full document content.
           - If `includeText` were true, the entire document text would be included in each save notification.

        4. willSave: true
           - Enables `willSave` notifications, letting the server know when a document is about to be saved.
           - This allows the server to prepare for the save, such as performing any necessary validation.

        5. willSaveWaitUntil: true
           - Enables `willSaveWaitUntil` requests, which allow the server to make edits on the document before it is saved.
           - This can be useful for pre-save formatting or other modifications to ensure the document is in a desired state before it’s saved.

        Summary:
        - This configuration reduces data by using incremental changes (`change: 2`) and disabling full text on save (`includeText: false`).
        - The server can respond to `willSaveWaitUntil` requests to modify documents right before they are saved.
        */

        % Popups
        hoverProvider: true,
        codeLensProvider: _{resolveProvider: true},  % Code lens resolve provider enabled to support resolving additional data on code lenses
        codeActionProvider: true,  % Enabled to support code actions
        % Dynamically enumerate commands for Popups
        executeCommandProvider: _{
            commands: CommandsList  % List available Code Lens commands
        },

        % Outline Panels
        documentSymbolProvider: true,

        % Completion
        workspaceSymbolProvider: true,  % Enables workspace symbol provider
        completionProvider: _{},  % Completion provider configuration

        % Menu/Links
        definitionProvider: true,
        declarationProvider: false,  % Currently using a menu for "definition" for declarations
        implementationProvider: true,
        typeDefinitionProvider: true,
        referencesProvider: true,
        documentLinkProvider: false,  % Commented out; may implement later if document linking is required

        % Refactoring
        renameProvider: false,  % Rename support disabled for now

        % Colors
        documentHighlightProvider: false,  % Highlights are currently disabled
        colorProvider: false,  % No color provider support

        /* semanticTokensProvider: _{
            legend: _{
                tokenTypes: TokenTypes,
                tokenModifiers: TokenModifiers
            },
            range: true,
            % [TODO] Implement deltas
            full: _{delta: false}
        }, */
        % semanticTokensProvider: false,  % Semantic tokens provider disabled as not fully implemented

        % Formatting
        documentFormattingProvider: false,  % [TODO] Formatting provider is almost finished
        documentOnTypeFormattingProvider: false,  % Disabled as it is not yet implemented
        foldingRangeProvider: false  % Folding support is disabled as it is not required currently
    }
) :-
  findall(Command, (clause(lsp_hooks:exec_code_action(Command, _, _), _),string(Command)), CommandsList).  % Collect all commands using clause/2
    % token_types(TokenTypes),  % Token types configuration placeholder for future semantic token support
    % token_modifiers(TokenModifiers).  % Token modifiers configuration placeholder for future semantic token support


:- dynamic(user:client_capabilities/1).

:- dynamic in_editor/1.

:- discontiguous(handle_msg/3).


% recompile/update code for the lsp server
handle_msg( _, _, _) :- notrace(catch(make,_,true)),fail.

% Save the last Msg.body Object for each method  (must fail to allow further processing)
:- dynamic(user:last_request/2).
handle_msg( Method, MsgBody, _) :-
    once((
      (string(Method)-> retractall(user:last_request(Method,_)) ; true),
      asserta(user:last_request(Method, MsgBody)))),
      fail.

%  Saves last infos that might be realivant to context (must fail to allow further processing)
:- dynamic(user:last_range/2).
handle_msg(Method, Msg, _) :-
   %Method \== "textDocument/hover",
   Method \== "textDocument/semanticTokens/range",
   % Method =="textDocument/codeAction" % is the most authoratative
    once((  _{params: Params} :< Msg,
      _{ range: Range } :< Params,
      retractall(user:last_range(Method,_)),
      asserta(user:last_range(Method,Range)))),
      fail.


handle_msg(Method, Msg, Response):-
   lsp_hooks:handle_msg_hook(Method, Msg, Response),!.

% Our request listener hooks monitor these on their own
handle_msg(Method, Msg, false) :- \+ string(Method), _{id: Id} :< Msg, \+ number(Id),!.


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
    debug_lsp(main, "received shutdown message", []).

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
        nop(debug_lsp((position),"~q",[message_id_target(Msg, Id, Doc, HintPath, Loc, Name/Arity)])).

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
%    {insertText:handle_doc_changes_d4(${1:_}, ${2:_})$0,insertTextFormat:2,label:handle_doc_changes_d4/2}]}
% OUT: {id:123,result:[]}
handle_msg("textDocument/completion", Msg, _{id: Id, result: Completions}) :- fail,
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
    doc_path(FileUri, Path),
    %debug_lsp((low),"~w",[FullText]),
    /*split_text_document_d4(FullText,SplitText),
    debug_lsp(low,"~w",[SplitText]),
    retractall(lsp_metta_changes:doc_text_d4(Path, _)),
    assertz(lsp_metta_changes:doc_text_d4(Path, SplitText)),*/
    ( in_editor(Path) -> true ; assertz(in_editor(Path)) ),
    %source_file_text(Path, DocFullText), % Derive from lsp_metta_changes:doc_text_d4/2
    xref_maybe(Path, FullText), % Check if changed and enqueue the reindexing
    check_errors_resp(FileUri, Resp), !.

% Handle document change notifications
%  returning false in handle_msg/3 is because no response is expected from the lsp server
handle_msg("textDocument/didChange", Msg, false) :-
    _{params: _{textDocument: TextDoc,
                contentChanges: Changes}} :< Msg,
    _{uri: Uri} :< TextDoc,
    doc_path(Uri, Path),
    handle_doc_changes(Path, Changes),
    source_file_text(Path, DocFullText), % Derive from lsp_metta_changes:doc_text_d4/2
    xref_maybe(Path, DocFullText). % Check if changed and enqueue the reindexing

% Handle document save notifications
handle_msg("textDocument/didSave", Msg, Resp) :-
    _{params: Params} :< Msg,
    xref_source_expired(Params.textDocument.uri),
    check_errors_resp(Params.textDocument.uri, Resp).

% Handle document close notifications
handle_msg("textDocument/didClose", Msg, false) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    doc_path(FileUri, Path),
    retractall(in_editor(Path)).

handle_msg("initialized", Msg, false) :- !,
    debug_lsp(main, "initialized ~w", [Msg]).

handle_msg("$/setTrace", _Msg, false):-
   fetch_workspace_configuration.


% Handle the $/cancelRequest Notification
handle_msg("$/cancelRequest", Msg, false) :-
    _{params: _{id: CancelId}} :< Msg,
    ignore(cancel_taskid(CancelId)).

% Handle the 'exit' notification
handle_msg("exit", _Msg, false) :-
    debug_lsp(main, "Received exit, shutting down", []),
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


/*
    workspace_symbol(Query,Symbol) :-
      try_profile_symbol(URI, Query, Range, Name, _Detail, Kind),
      Symbol = symbol{
        name: Name,
        kind: Kind,
        location: _{
          uri: URI,
          range: Range
          }
        }.

    document_symbols(URI, SymbolInfos) :-
      findall(SymbolInfo, document_symbol(URI, SymbolInfo), SymbolInfos).

    document_symbol(URI, Symbol) :-
      try_profile_symbol(URI, '', Range, Name, Detail, Kind, Detail),
      Symbol = symbol{
        name: Name,
        detail: Detail,
        kind: Kind,
        range: Range,
        selectionRange: Range
        }.
*/

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
handle_msg(_, Msg, _{id: Id, error: _{code: -32603, message: "Unimplemented handle_msg"}}) :-
    _{id: Id} :< Msg, !,
    debug_lsp(todo, "unknown message ~w", [Msg]).
handle_msg(_, Msg, false) :-
    debug_lsp(todo, "unknown notification ~w", [Msg]).

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
    debug_lsp(errors, "Failed checking errors", []).

