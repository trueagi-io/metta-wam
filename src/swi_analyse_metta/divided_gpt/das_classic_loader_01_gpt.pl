/*
    PLDoc header for the predicate include_atomspace_1_0/1
    This predicate loads an atomspace from a file given its relative filename.

    @param RelFilename Relative file path to load the atomspace.
    @example include_atomspace_1_0('&flybase').
*/
include_atomspace_1_0(RelFilename):-
  % Convert the relative file name into an absolute file path.
  absolute_file_name(RelFilename, Filename),

  % Load the atomspace from the file using track_load_into_file/2.
  track_load_into_file(Filename,

  % Ensure that the relative filename is an atom and exists.
  must_det_ll((
    atom(RelFilename),                      % Confirm RelFilename is an atom.
    current_self(Self),                     % Get the current context (Self).
    exists_file(RelFilename),!,             % Ensure the file exists.
    
    % Setup file reading and processing with cleanup after completion.
    must_det_ll((setup_call_cleanup(open(Filename, read, In, [encoding(utf8)]),
      (
        directory_file_path(Directory, _, Filename),  % Get the file's directory.
        assert(metta_file(Self, Filename, Directory)), % Store file metadata in the atomspace.
        
        % Change the working directory and load the atomspace stream.
        with_cwd(Directory,
          must_det_ll(load_atomspace_1_0_file_stream(Filename, Self, In)))
      ),
      close(In)   % Ensure the file is closed after processing.
    ))))).

/*
    PLDoc header for load_atomspace_1_0_file_stream/3
    This predicate reads and processes an atomspace file stream.

    @param Filename The name of the file being processed.
    @param Self The current execution context.
    @param In Input stream from the file to be read.
*/
load_atomspace_1_0_file_stream(Filename, Self, In):-
  % Determine the appropriate predicate to read based on file size.
  once((
    is_file_stream_and_size(In, Size),  % Get file size.
    Size > 102400 -> P2 = read_sform2;  % Use read_sform2 for large files.
    P2 = read_metta  % Use read_metta for smaller files.
  )), !,
  
  % Set options for the loading file.
  with_option(loading_file, Filename,
    (
      % Set the execution number and other settings.
      must_det_ll((
        set_exec_num(Filename, 1),      % Initialize execution number for tracking.
        % load_answer_file(Filename),   % previously: loading answers from the file was skipped.
        set_exec_num(Filename, 0)       % Reset execution number after processing.
      )),
      
      % Read and process the atomspace expressions from the file.
      once((repeat, (
        current_read_mode(Mode),         % Get the current read mode.
        once(call(P2, In, Expr)),        % Read an expression using P2.
        
        % Process the expression in the atomspace.
        must_det_ll((
          ((do_atomspace_1_0(file(Filename), Mode, Self, Expr, _O)) -> true;
           pp_m(unknown_do_atomspace_1_0(file(Filename), Mode, Self, Expr)))),
          flush_output                    % Flush output after processing.
        )),
        at_end_of_stream(In)             % Stop once the end of the stream is reached.
      )))
    )
  ).

/*
    PLDoc header for do_atomspace_1_0/5
    Processes a single expression from an atomspace file.

    @param _W File descriptor (unused here).
    @param _M Current read mode.
    @param _S Current execution context (Self).
    @param E Expression to be processed.
    @param _O Additional options (unused here).
*/
do_atomspace_1_0(_W, _M, _S, end_of_file, _O):- !.  % Stop when reaching the end of the file.
do_atomspace_1_0(W, M, S, E, _O):-
  % Rewrite the expression into version 2.0 format.
  rewrite_as10_to_as20(E, E2, Extras), !,
  
  % Process each expression in the list (main expression + extras).
  maplist(do_atomspace_2_0(W, M, S), [E2 | Extras]).

/*
    PLDoc header for do_atomspace_2_0/4
    Asserts the expression into the atomspace.

    @param _W File descriptor (unused here).
    @param _M Current read mode.
    @param _S Current execution context (Self).
    @param E Expression to be asserted into the atomspace.
*/
do_atomspace_2_0(_W, _M, _S, E):-
  assert_OBO(E),  % Assert the expression into the atomspace using assert_OBO/1.
  !.

/*
    PLDoc header for rewrite_as10_to_as20/3
    This predicate rewrites an Atomspace expression from version 1.0 to version 2.0.

    @param A The input expression in version 1.0 format.
    @param Res The rewritten expression in version 2.0 format.
    @param Extras Additional information extracted during the rewrite.
*/
rewrite_as10_to_as20(A, A, []):- \+ is_list(A).  % If A is not a list, leave it unchanged.

rewrite_as10_to_as20([CN, Arg], Arg, []):- CN = 'ConceptNode', !.  % Rewrite ConceptNode expressions.

rewrite_as10_to_as20([ConceptNode, Arg1], Arg, [is_a(Arg, ConceptNode) | R]):- 
  atom(ConceptNode), atom_concat(_Concept, 'Node', ConceptNode), !,
  rewrite_as10_to_as20(Arg1, Arg, R).  % Handle node concatenations.

rewrite_as10_to_as20(['EvaluationLink', ['PredicateNode', Pred], ['ListLink' | Args]], Res, [arity(Pred, Len), is_a(Pred, 'Predicate') | ExtrasF]):-
  % Rewrite EvaluationLink expressions into Prolog format.
  length(Args, Len),  % Get the arity of the predicate.
  maplist(rewrite_as10_to_as20, Args, ArgsL, Extras),  % Rewrite arguments.
  flatten(Extras, ExtrasF),  % Flatten extra information.
  Res =.. [Pred | ArgsL].  % Construct the Prolog term from the predicate and arguments.

rewrite_as10_to_as20([InheritanceLink | Args], [Inheritance | ArgsL], ExtrasF):-
  atom(InheritanceLink), atom_concat(Inheritance, 'Link', InheritanceLink),  % Handle InheritanceLink expressions.
  maplist(rewrite_as10_to_as20, Args, ArgsL, Extras), flatten(Extras, ExtrasF), !.

rewrite_as10_to_as20(A, A, []).  % If none of the above rules apply, leave the expression unchanged.