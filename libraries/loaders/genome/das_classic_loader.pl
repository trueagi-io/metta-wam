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

%********************************************************************************************* 
% PROGRAM FUNCTION: loads and processes files into the Atomspace structure,
% including predicates to load, interpret, and transform data related to entities and relationships.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   (EvaluationLink
    (PredicateNode "has_name")
    (ListLink
        (DiseaseOntologyNode "DOID:0001816")
        (ConceptNode "angiosarcoma")))


                  load_metta('&flybase',File)).
*/

%!  include_atomspace_1_0(+RelFilename) is det.
%
%   Loads an Atomspace file specified by `RelFilename` and processes its contents
%   into the current knowledge base. This predicate ensures the file is loaded
%   with UTF-8 encoding, tracks its directory path, and uses a dedicated file
%   stream for reading.
%
%   @arg RelFilename A relative path to the Atomspace file, which is converted 
%                    to an absolute file path before processing.
include_atomspace_1_0(RelFilename):-
    % Convert RelFilename to absolute path
    absolute_file_name(RelFilename, Filename),
    track_load_into_file(Filename,
        must_det_ll((
            % Check if RelFilename is an atom
            atom(RelFilename),
            % Get the current self identifier
            current_self(Self),
            % Ensure file exists, or cut if not
            exists_file(RelFilename), !,
            must_det_ll((
                setup_call_cleanup(
                    % Open file with UTF-8 encoding
                    open(Filename, read, In, [encoding(utf8)]),
                    (( % Get the directory path
                      directory_file_path(Directory, _, Filename),
                      % Record file details for tracking
                      assert(metta_file(Self, Filename, Directory)),
                      % Process file contents
                      with_cwd(Directory,
                          must_det_ll(load_atomspace_1_0_file_stream(Filename, Self, In))))),
                    % Ensure stream closes after reading
                    close(In)
                )))))).

%!  load_atomspace_1_0_file_stream(+Filename, +Self, +In) is det.
%
%   Reads and processes an Atomspace file stream (`In`) from the specified 
%   `Filename`, interpreting each entry and invoking operations based on the 
%   content. Chooses a parsing strategy based on file size, applies options for 
%   loading, and iteratively reads expressions to load into Atomspace.
%
%   @arg Filename The absolute path to the file being read.
%   @arg Self The identifier of the current instance.
%   @arg In The file stream associated with `Filename` opened for reading.
load_atomspace_1_0_file_stream(Filename,Self,In):-
    % Choose parser based on file size: use read_sform2 if size > 102400, else read_metta
    once((is_file_stream_and_size(In, Size), Size > 102400) -> P2 = read_sform2 ; P2 = read_metta),!,
    with_option(loading_file, Filename,
          %current_exec_file(Filename),
        (( % Set execution number to 1 for initial loading state
          must_det_ll((
              set_exec_num(Filename,1),
              % The following line is commented out but may be used to load answers from a file
              % load_answer_file(Filename),
              % Reset execution number to 0 after setup
              set_exec_num(Filename,0))),
          % Begin reading expressions from the stream, applying the parser P2
          once((repeat, ((
              % Set the current reading mode
              current_read_mode(Mode),
              % Attempt to parse an expression from the file stream
              once(call(P2, In, Expr)),
              % The following line is commented out but may be used for debug logging of expressions
              % write_src(read_atomspace_1_0=Expr), nl,
              % Process the parsed expression in Atomspace, or log if unrecognized
              must_det_ll((((do_atomspace_1_0(file(Filename),Mode,Self,Expr,_O))) -> true;
                  pp_m(unknown_do_atomspace_1_0(file(Filename),Mode,Self,Expr)))),
              flush_output)),
             % Terminate on reaching the end of the stream
             at_end_of_stream(In)))))),!.

%  ['InheritanceLink',['DiseaseOntologyNode','DOID:0112326'],['DiseaseOntologyNode','DOID:0050737']]

%!  do_atomspace_1_0(+W, +M, +S, +E, +O) is det.
%
%   Processes an expression (`E`) within an Atomspace context, rewriting it
%   according to specific transformation rules and asserting any additional
%   information derived from the expression.
%
%   @arg W Context identifier, potentially representing a workspace.
%   @arg M Current mode or context mode.
%   @arg S Self identifier or context entity within which `E` is processed.
%   @arg E Expression to be processed or transformed.
%   @arg O Output parameter or flag (not used in this implementation).
%   
%   @example
%     ?- do_atomspace_1_0(_, _, _, ['ConceptNode', 'ExampleConcept'], _).
%     % Asserts `ExampleConcept` as a concept node in the Atomspace.
do_atomspace_1_0(_W,_M,_S,end_of_file,_O):- !.  % End-of-file case, succeeds without action.
do_atomspace_1_0(W,M,S,E,_O):-
    % Rewrite expression E into E2 with additional extracted statements (Extras).
    rewrite_as10_to_as20(E,E2,Extras),!,
    % Process the main transformed expression (E2) and any additional expressions (Extras).
    maplist(do_atomspace_2_0(W,M,S),[E2|Extras]).

%!  do_atomspace_2_0(+W, +M, +S, +E) is det.
%
%   Asserts the transformed expression `E` into the Atomspace as an ontology-based object (OBO).
%
%   @arg W Context identifier, potentially representing a workspace.
%   @arg M Current mode or context mode.
%   @arg S Self identifier or context entity within which `E` is processed.
%   @arg E Expression to be asserted.
%
%   @example
%     ?- do_atomspace_2_0(_, _, _, [has_property, 'ExampleConcept', 'ExampleProperty']).
%     % Asserts the relationship 'has_property' for 'ExampleConcept' and 'ExampleProperty'.
do_atomspace_2_0(_W,_M,_S,E):-
    assert_OBO(E),
    !.  % The following commented line would output `E` for debugging purposes.
    % writeq(E),!,nl.

%!  rewrite_as10_to_as20(+A, -Transformed, -Extras) is det.
%
%   Transforms expressions from an older Atomspace format (AS1.0) to a newer
%   format (AS2.0), extracting additional properties as needed. Handles various
%   cases such as concept nodes, evaluation links, and inheritance links.
%
%   @arg A Input expression in the AS1.0 format.
%   @arg Transformed Resulting expression in AS2.0 format.
%   @arg Extras List of additional statements derived during the transformation.
%
%   @example
%     ?- rewrite_as10_to_as20(['ConceptNode', 'ExampleConcept'], Transformed, Extras).
%     Transformed = 'ExampleConcept',
%     Extras = [].
%
%   @example
%     ?- rewrite_as10_to_as20(['EvaluationLink', ['PredicateNode', 'related_to'], 
%                              ['ListLink', 'ConceptA', 'ConceptB']], Transformed, Extras).
%     Transformed = related_to('ConceptA', 'ConceptB'),
%     Extras = [arity(related_to, 2), is_a(related_to, 'Predicate')].
rewrite_as10_to_as20(A,A,[]):- \+ is_list(A).  % Base case: if `A` is not a list, no transformation.
rewrite_as10_to_as20([CN,Arg],Arg,[]):- CN='ConceptNode',!.  % ConceptNode case: directly map `Arg`.
rewrite_as10_to_as20([ConceptNode,Arg1],Arg,[is_a(Arg,ConceptNode)|R]):-
    % Handle ConceptNode terms with 'is_a' assertions.
    atom(ConceptNode),atom_concat(_Concept,'Node',ConceptNode),!,
    rewrite_as10_to_as20(Arg1,Arg,R),!.
rewrite_as10_to_as20(['EvaluationLink',['PredicateNode',Pred],['ListLink'|Args]], Res,
    [arity(Pred,Len),is_a(Pred,'Predicate')|ExtrasF]):-
    % Transform EvaluationLink with PredicateNode, including arity information.
    length(Args,Len),
    maplist(rewrite_as10_to_as20,Args,ArgsL,Extras),
    flatten(Extras,ExtrasF),
    Res =.. [Pred|ArgsL].
rewrite_as10_to_as20([InheritanceLink|Args],[Inheritance|ArgsL],ExtrasF):-
    % Transform InheritanceLink with 'is_a' assertion.
    atom(InheritanceLink),atom_concat(Inheritance,'Link',InheritanceLink),
    maplist(rewrite_as10_to_as20,Args,ArgsL,Extras),
    flatten(Extras,ExtrasF),!.
rewrite_as10_to_as20(A,A,[]).  % Fallback case: if no other rules apply, return `A` as-is with no extras.