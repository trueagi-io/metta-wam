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
% PROGRAM FUNCTION:  parse and process GraphML files to extract graph structures, particularly 
% nodes and edges, with their associated attributes like labels, descriptions, and relationships.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/** <module> read_graphml - Read graph information from a yEd graphml file
 *
 * Extracts nodes with label and description, edges with label,
 * from a yEd graphml file,
 * using SWI-Prolog.
 *
 * @author Carlos Lang-Sanou
 */

 :- module(read_graphml, [
    read_graphml/2,      % Reads a GraphML file and extracts graph terms
    rgml/0,              % Example loader for specific GraphML files
    rgml2/0,             % Example loader for another GraphML pattern
    load_graphml/2,      % General loader for GraphML files
    load_fb_graphml/2    % Loader for GraphML files with specific FB processing
]).

%! load_fb_graphml(+KB, +Filename) is det.
%
%   Loads a GraphML file and processes its content to assert graph data into the Prolog database.
%   This predicate links the graph data with a specific knowledge base identifier.
%
%   @arg KB       A knowledge base identifier to associate with the graph data.
%   @arg Filename The full path to the GraphML file to be processed.
%
%   @example
%     % Load the GraphML file 'example.graphml' with knowledge base ID 'graph1'.
%     ?- load_fb_graphml(graph1, 'data/example.graphml').
%
load_fb_graphml(Fn, Filename) :-
    % Track file loading and ensure logical consistency using must_det_ll.
    track_load_into_file(Filename,
        must_det_ll((
            % Log the loading operation for debugging.
            fbug(load_fb_graphml(Fn, Filename)),
            % Decompose the file path into directory, base name, and file ID.
            directory_file_path(Directory, BaseName, Filename),
            file_name_extension(Id, _, BaseName),
            % Define the file type as 'SequenceFile' for graph data.
            Type = 'SequenceFile',
            % Assert metadata about the file into the Prolog database.
            assert_OBO(id_type(Id, Type)),
            assert_OBO(pathname(Id, Filename)), !,
            assert_OBO(basename(Id, BaseName)), !,
            assert_OBO(directory(Id, Directory)), !,
            % Parse the GraphML file to extract its content as terms.
            read_graphml(Filename, In),
            % Process the parsed GraphML terms and assert them into the database.
            load_fb_graphml_read(Id, In)
        ))).

%! load_fb_graphml_read(+Id, +Input) is det.
%
%   Processes the GraphML input and asserts it into the Prolog database.
%   This predicate recursively processes lists of GraphML terms or individual terms.
%
%   @arg Id    Identifier used as a prefix for asserting terms.
%   @arg Input Input GraphML term or a list of terms to be processed.
%
%   @example
%     ?- load_fb_graphml_read(my_id, [element1, element2]).
%
%     % This will process and assert terms prefixed with 'my_id'.
%
load_fb_graphml_read(Id, In) :-
    is_list(In), !,
    % Recursively process each element in the list.
    maplist(load_fb_graphml_read(Id), In).
load_fb_graphml_read(Id, In) :-
    % Convert input into a term with Id as the first argument.
    In =.. [P | InL],
    Save =.. [P, Id | InL],
    % Assert the term into the OBO database.
    assert_OBO(Save),
    % Print the asserted term for debugging purposes.
    writeln(Save).

%! s_list_assert(+Prefix, +List, -Assert) is det.
%
%   Converts a list of name-value pairs into a Prolog term with a prefixed predicate.
%   This predicate is useful for restructuring and asserting GraphML data.
%
%   @arg Prefix   The prefix used as the functor name for the resulting term.
%   @arg List     A list of name-value pairs.
%   @arg Assert   The resulting Prolog term to be asserted.
%
%   @example
%     ?- s_list_assert(node, [id=1, label='Node A'], Term).
%     Term = node_id_label(1, 'Node A').
%
s_list_assert(S, List, Assert) :-
    must_det_ll((
        % Convert list into names and values.
        into_name_values(List, Ns, Vs),
        % Concatenate prefix and names to form the predicate name.
        atomic_list_concat([S | Ns], '_', Pred),
        % Create a term with the predicate name and values.
        Assert =.. [Pred | Vs]
    )).

%! fix_value(+Input, -Output) is det.
%
%   Normalizes values from the GraphML input into usable Prolog terms.
%   Handles atoms, lists, and numeric conversions.
%
%   @arg Input   The input value to be normalized.
%   @arg Output  The resulting normalized value.
%
%   @example
%     ?- fix_value('123', X).
%     X = 123.
%
%     ?- fix_value([abc], Y).
%     Y = abc.
%
fix_value(X, Y) :-
    \+ callable(X), X = Y.
fix_value([X], Y) :- !,
    % If the input is a list with a single element, normalize the element.
    fix_value(X, Y).
fix_value(X, Y) :-
    % If the input is a list, normalize each element.
    is_list(X), !,maplist(fix_value, X, Y).
fix_value(X, Y) :-
    % If the input is not an atom, keep it as is.
    \+ atom(X), !,X = Y.
fix_value(X, Y) :-
    % Convert atoms that represent numbers into numbers.
    atom_number(X, Y), !.
fix_value(X, X).

%! elements_are_kv(+Data, +Element, -KeyValue) is det.
%
%   Extracts key-value pairs from GraphML `data` elements.
%   Handles both `id` and `key` attributes as identifiers for the key-value pairs.
%
%   @arg Data     The tag or data type to be matched.
%   @arg Element  A Prolog term representing an XML/SGML element.
%   @arg KeyValue The resulting key-value pair in the form `Key=Value`.
%
%   @example
%     ?- elements_are_kv(data, element(data, [id=my_id], ['Some Value']), KeyValue).
%     KeyValue = my_id='Some Value'.
%
%     ?- elements_are_kv(data, element(data, [key=key_123], ['123']), KeyValue).
%     KeyValue = key_123=123.
%
elements_are_kv(Data, element(Data, [id=Key], LValue), Key=Value) :-
    % Normalize the value extracted from the data element.
    fix_value(LValue, Value).
elements_are_kv(Data, element(Data, [key=Key], LValue), Key=Value) :-
    % Normalize the value when 'key' is used as an attribute.
    fix_value(LValue, Value).
% elements_are_kv(S2, Content, List2) :-
%     % Uncomment this line if recursive processing of content elements is needed.
%     % Map each sub-element and extract key-value pairs recursively.
%     maplist(elements_are_kv(S2), Content, List2).

%! restructure_graphml(+Input, -Output) is det.
%
%   Restructures GraphML terms into a more manageable format.
%   Processes nodes and edges from a GraphML file, extracting relevant attributes
%   and creating a simplified representation.
%
%   @arg Input  The input GraphML term or list of terms to be restructured.
%   @arg Output The resulting list of simplified GraphML terms.
%
%   @example
%     % Example input structure for nodes and edges:
%     ?- restructure_graphml([element(node, [id=node1], []),
%                             element(edge, [source=node1, target=node2], [element(data, [key=label], ['A'])])], Output).
%     Output = [node_prop(node1, label, 'A')].
%
restructure_graphml(Term_list, Terms) :-
    % If the input is a list, recursively process each element in the list.
    is_list(Term_list), !,
    maplist(restructure_graphml, Term_list, Terms).
restructure_graphml(element(S, List, []), Assert) :-
    % Process an empty element with a list of attributes.
    List \== [], % Ensure the list is not empty.
    s_list_assert(S, List, Assert). % Convert the attributes into a Prolog term.
restructure_graphml(element(graphml, _, Term_list), Terms) :-
    % Entry point for a GraphML document. Process the inner terms.
    !, restructure_graphml(Term_list, Terms).
restructure_graphml(element(graph, [id='G' | _], Term_list), Terms) :-
    % Process the main 'graph' element (with id='G').
    !, restructure_graphml(Term_list, Terms).
restructure_graphml(element(S, [source=B, target=E], Term_list), Assert) :-
    % Process 'edge' elements.
    S == edge, % Ensure this is an 'edge' element.
    %atomic_list_concat([B,E],'_',Id),
    maplist(elements_are_kv(data), Term_list, NVList),
    % Extract key-value pairs, filtering out irrelevant values.
    findall(edge_prop(B, E, N, V),
            (member(N=V, NVList), \+ member(V, ['false', 'None'])),
            Assert).
restructure_graphml(element(S, [id=Id], Term_list), Assert) :-
    % Process 'node' elements.
    S == node, % Ensure this is a 'node' element.
    maplist(elements_are_kv(data), Term_list, NVList),
    % Extract key-value pairs, filtering out irrelevant values.
    findall(node_prop(Id, N, V),
            (member(N=V, NVList), \+ member(V, ['false', 'None'])),
            Assert).
restructure_graphml(IO, IO). % Default case: return the input unchanged.

%! read_graphml(+File_basename:atom, -Term_list:list) is det
%
%   Reads a GraphML file and produces a corresponding list of terms.
%   This predicate processes GraphML data using SWI-Prolog's XML parser.
%   It handles different ways of reading the input, including direct structure loading
%   and custom SGML parsing.
%
%   @arg File_basename The base name of the input file, without the '.graphml' extension.
%   @arg Term_list     The resulting list of terms extracted from the GraphML file.
%
%   @example
%     % Read a GraphML file and output the parsed terms:
%     ?- read_graphml('example', Terms).
%     Terms = [node_prop(...), edge_prop(...), ...].
%
read_graphml(Graphfile, Term_list) :-
    % If the file has a '.graphml' extension, strip it and reprocess.
    file_name_extension(Base_name, '.graphml', Graphfile), !,
    read_graphml(Base_name, Term_list).
read_graphml(Id, Terms) :-
    % Load the structure of the '.graphml' file directly.
    file_name_extension(Id, '.graphml', Graphfile), !,
    load_structure(Graphfile, Term_list, [
        dialect(xml),                   % Use XML dialect.
        space(remove),                  % Remove spaces from parsed content.
        case_preserving_attributes(false) % Normalize attribute names to lowercase.
    ]),
    % Restructure the parsed terms into a usable format.
    restructure_graphml(Term_list, Terms).
read_graphml(Id, Term_list) :-
    file_name_extension(Id, '.graphml', Graphfile), !,
   %load_html(Graphfile, [Graphml], []), !, graphml_term_list(Graphml, Term_list).
%read_graphml(File,Out) :-
    Graphfile = File,
   file_name_extension(Id, _, File),
            open(File, read, In),
            new_sgml_parser(Parser, []),
            set_sgml_parser(Parser, file(File)),
            set_sgml_parser(Parser, dialect(xml)),
            set_sgml_parser(Parser, space(remove)),

            sgml_parse(Parser,
                       [ source(In),
                         call(begin, on_begin),
                         call(end, on_end)
                       ]),
            close(In),
      findall(feature_data(A,B,C), feature_data(A,B,C),Term_list).


:- dynamic(feature_data/3).

%! on_end(+Tag, +Parser) is det.
%
%   Handles the end of parsing specific tags in the GraphML file.
%   This predicate performs final processing when the 'graphml' tag is encountered.
%   For other tags, it does nothing.
%
%   @arg Tag    The current XML/SGML tag being closed.
%   @arg Parser The SGML parser in use (not explicitly needed here).
%
on_end('graphml', _) :-  
    % When parsing ends for the 'graphml' tag, finalize feature data.
    !,finish_feature_data,!,
    % Print all the asserted feature data for debugging.
    listing(feature_data(_, _, _)),
    % Clean up by retracting all feature_data terms.
    retractall(feature_data(_, _, _)),
    % Introduce a small delay to ensure synchronization or debugging visibility.
    sleep(0.1), 
    !.

%on_end(Tag, _Parser):- 
%    current_tag(Is), Is = Tag, !, pop_tag(Tag), finish_tag(Tag).

on_end(_, _).
% Default case: Do nothing for other tags.

%! on_begin(+Tag, +Attributes, +Parser) is det.
%
%   Handles the beginning of parsing specific tags in the GraphML file.
%   This predicate processes tags like 'graphml', 'chado', and 'feature' specifically,
%   while attempting to handle other tags with additional logic.
%
%   @arg Tag        The current XML/SGML tag being processed.
%   @arg Attributes List of attributes associated with the tag.
%   @arg Parser     The SGML parser being used.
%
on_begin('chado', _, _) :- !. 
% Do nothing for the 'chado' tag.
on_begin('graphml', _, _) :- !. 
% Do nothing for the 'graphml' tag itself; handled in on_end.
on_begin('feature', _, _Parser) :- !. 
% Do nothing when encountering a 'feature' tag.
%on_begin(Tag, _Attr, _Parser):- 
%    push_tag(Tag), fail.
on_begin(Tag, Attr, Parser) :-
    % Read the content of the current element and store the reset position.
    read_element(Parser, Content, Reset), 
    !,
    % Attempt to store the feature data. If it fails, reset the parser position.
    (store_feature(Tag, Attr, Content) 
     -> true 
     ;  (set_sgml_parser(Parser, position(Reset)), fail)).
on_begin(Tag, Attr, Parser) :-
    % Read the element's content and reset position if needed.
    read_element(Parser, Content, Reset), 
    !,
    % Attempt to process the beginning of the tag.
    (try_begin(Tag, Attr, Content) 
     -> true 
     ;  (set_sgml_parser(Parser, position(Reset)), fail)).
on_begin(Any, _, Parser) :-
    % Debugging: Print unexpected tag content.
    read_element(Parser, Content, _),
    nl, print(Any = Content), nl.
on_begin(Tag, Attr, Parser) :-
    % Parse the content of the current element and assert it as feature data.
    sgml_parse(Parser, [
        document(Content),
        parse(content)
    ]),
    % Construct the feature_data term and assert it into the database.
    FD = feature_data(Tag, Attr, Content),
    print(FD), nl,
    assertz(FD).


%! current_tag(-Tag) is det.
%
%   Retrieves the current tag at the top of the tag stack.
%   If no tags are on the stack, it defaults to 'none'.
%
%   @arg Tag The tag currently at the top of the stack or `none` if empty.
current_tag(Tag) :-
    once(clause(current_tag_stack(Was), true, _Ref) ; Was = []),
    append(_New, [Tag], Was), !.
current_tag(none).

%! parent_tag(-Tag) is det.
%
%   Retrieves the parent tag, which is the second-to-last tag on the stack.
%   If no parent tags exist, it defaults to 'none'.
%
%   @arg Tag The parent tag on the stack or `none` if empty.
parent_tag(Tag) :-
    once(clause(current_tag_stack(Was), true, _Ref) ; Was = []),
    append(_New, [Tag, _], Was), !.
parent_tag(none).

%! pop_tag(+Tag) is det.
%
%   Removes the given tag from the top of the tag stack.
%   If the stack exists, it erases the current stack and reasserts a new one without the tag.
%
%   @arg Tag The tag to be removed from the stack.
pop_tag(Tag) :-
    once(clause(current_tag_stack(Was), true, Ref) ; Was = []),
    append(New, [Tag], Was),
    it_t(nonvar(Ref), erase(Ref)), % Remove the old clause.
    assert(current_tag_stack(New)), !.

%! push_tag(+Tag) is det.
%
%   Adds a new tag to the top of the tag stack.
%
%   @arg Tag The tag to be pushed onto the stack.
push_tag(Tag) :-
    once(retract(current_tag_stack(Was)) ; Was = []),
    append(Was, [Tag], New),
    assert(current_tag_stack(New)).

%! finish_tag(+Tag) is det.
%
%   Placeholder predicate for finalizing operations on a tag.
%   Currently does nothing but can be extended as needed.
%
%   @arg Tag The tag to finalize.
finish_tag(_Tag).

%! peek_element(+Parser, -Content) is det.
%
%   Reads an element from the parser stream without consuming it.
%   This is achieved by resetting the parser position after reading.
%
%   @arg Parser  The SGML parser being used.
%   @arg Content The content read from the parser.
peek_element(Parser, Content) :-
    call_cleanup(
        read_element(Parser, Content, Pos),          % Read content and store the current position.
        set_sgml_parser(Parser, position(Pos))       % Reset the parser position after reading.
    ).

%! read_element(+Parser, -Content, -Pos) is det.
%
%   Reads content from the current SGML parser stream and captures its position.
%
%   @arg Parser  The SGML parser being used.
%   @arg Content The parsed content.
%   @arg Pos     The current position in the stream.
read_element(Parser, Content, Pos) :-
    % Retrieve the stream source from the parser.
    get_sgml_parser(Parser, source(S)),
    % Capture the current position in the stream.
    stream_property(S, position(Pos)),
    % Parse the content as an SGML/XML document.
    sgml_parse(Parser, [
        document(Content),
        parse(content)
    ]), !.

%! try_begin(+Tag, +Attributes, +Content) is det.
%
%   Processes the beginning of an SGML/XML element during parsing.
%   Handles nested structures, lists, and individual values to extract or process feature data.
%
%   @arg Tag        The current tag being processed.
%   @arg Attributes The list of attributes associated with the tag.
%   @arg Content    The content within the tag, which can be a list, an element, or a single value.
%
%   @example
%     ?- try_begin('node', [id=1], element(data, [key=label], ['Node A'])).
%     % Processes the nested element and extracts feature data.
%
try_begin(Tag, Attr, element(T, A, L)) :-
    % If the content is an element, combine the current attributes with the new attributes (A).
    !,
    append(Attr, A, AttrA),
    % Recursively call try_begin with the updated attributes and nested content (L).
    try_begin(Tag = T, AttrA, L).
try_begin(Tag, Attr, List) :-
    % If the content is a list, process it to absorb type IDs or nested content.
    is_list(List),
    absorb_type_ids(Tag, Attr, List), !.
try_begin(Tag, Attr, V) :-
    % If the content is a single value, process it as feature data.
    process_feature_data(Tag, Attr, V).
%try_begin(Tag,Attr, element(T,A,L)):-
 %   % Placeholder for processing nested elements with absorb_type_ids.
 %   % absorb_type_ids(Tag,Attr, element(T,A,L)),
 %   % maplist(try_begin(T,A),L).

%! absorb_type_ids(+Tag, +Attributes, +Elements) is det.
%
%   Extracts and processes type IDs and associated content from a list of elements.
%   This predicate handles nested `type_id` structures to retrieve type names and values,
%   and calls `process_feature_data/3` to process the extracted information.
%
%   @arg Tag        The parent tag being processed.
%   @arg Attributes The list of attributes associated with the tag.
%   @arg Elements   A list of elements containing a `type_id` to be processed.
%
%   @example
%     ?- absorb_type_ids(tag, [id=1], [element(type_id, [], [element(cvterm, [], ['Name'])]), Other]).
%
absorb_type_ids(Tag, Attr, Elements) :-
    % Select an element of type 'type_id' from the list.
    select(element(type_id, [], C), Elements, Rest),
    % Extract the type name using the 'cv' and 'name' hierarchy.
    get_content([cv, name], C, TypeName), !,
    must_det_ll((
        % Extract the name using the 'cvterm' and 'name' hierarchy.
        get_content([cvterm, name], C, Name),
        % Process the remaining elements to extract their values.
        maplist(get_element_value_each, Rest, Values),
        % Process each value as feature data with the Tag, TypeName, and Name.
        maplist(process_feature_data(ntv(Tag, TypeName, Name), Attr), Values)
    )), !.

%! absorb_type_ids(+Tag, +Attributes, +Elements) is det.
%
%   Processes `type_id` elements where only `cvterm` names are available.
%   This version does not require `cv` content and processes values directly.
%
%   @arg Tag        The parent tag being ignored (set as `_`).
%   @arg Attributes The list of attributes associated with the tag.
%   @arg Elements   A list of elements containing `type_id` to be processed.
%
absorb_type_ids(_Tag, Attr, Elements) :-
    % Select an element of type 'type_id'.
    select(element(type_id, [], C), Elements, Rest),
    must_det_ll((
        % Extract the name using the 'cvterm' and 'name' hierarchy.
        get_content([cvterm, name], C, Name),
        % Process the remaining elements to extract their values.
        maplist(get_element_value_each, Rest, Values),
        % Process each value as feature data with the Tag and Name.
        maplist(process_feature_data(nv(Name), Attr), Values)
    )), !.

%! store_feature(+Tag, +Attributes, +Content) is det.
%
%   Converts and stores a feature represented by a tag, attributes, and content.
%   This predicate converts an XML/SGML element into a Prolog term and asserts it.
%
%   @arg Tag      The tag representing the feature.
%   @arg Attributes The list of attributes associated with the feature.
%   @arg Content  The content to be stored.
%
store_feature(Tag, Attr, Content) :-
    % Convert the input element into a Prolog term.
    cvt_element(element(Tag, Attr, Content), Val),
    % Assert the feature data into the database.
    assert(feature_data(Tag, Attr, Val)).

%! skip_over(+ElementType) is semidet.
%
%   Succeeds if the given element type is one of the elements to be skipped.
%   These elements are ignored during processing.
%
%   @arg ElementType The element type to be checked.
%
skip_over(cvterm).
skip_over(cv).
skip_over(pub).

%! skip_over_s(+ElementType) is semidet.
%
%   Succeeds if the given element type matches an extended list of elements to be skipped.
%   This includes specific types such as `dbxref_id`, `library_id`, and related entries.
%
%   @arg ElementType The element type to be checked.
%
skip_over_s(featureprop).
skip_over_s(featureprop_pub).
skip_over_s(E) :-
    % Check if the element belongs to a predefined list of skip types.
    member(E, [dbxref_id, dbxref,
               db_id, library_id, library, library_feature]).
skip_over_s(X) :-
    % Use `skip_over/1` for additional checks.
    skip_over(X).

%! cvt_element(+Input, -Output) is det.
%
%   Converts an XML/SGML element into a simplified Prolog representation.
%   This predicate processes nested elements, skips certain types of content, 
%   and extracts atomic values when possible.
%
%   @arg Input   The input to be converted, which can be an element, list, or atomic value.
%   @arg Output  The resulting simplified Prolog representation of the input.
%
%   @example
%     % Convert an element containing a nested structure.
%     ?- cvt_element(element(tag, [], [element(subtag, [], ['Value'])]), Result).
%     Result = tag=subtag='Value'.
%
%     % Convert a list of elements.
%     ?- cvt_element([element(tag, [], ['A']), element(tag, [], ['B'])], Result).
%     Result = [tag='A', tag='B'].
%
cvt_element(List, Val) :-
    % If the input is a list, process each element recursively.
    is_list(List), !,maplist(cvt_element, List, Val).
cvt_element(element(Tag, [], [element(CVTerm, [], L)]), TagVal) :-
    % Skip over specific terms (e.g., featureprop, dbxref) and process nested content.
    skip_over_s(CVTerm), !,cvt_element(element(Tag, [], L), TagVal).
cvt_element(element(CVTerm, [], [Atomic]), Val) :-
    % Skip over certain terms and extract an atomic value.
    skip_over_s(CVTerm), !,cvt_element(Atomic, Val).
cvt_element(element(CVTerm, [], Atomic), Val) :-
    % Skip over specific tags and directly process the atomic content.
    skip_over(CVTerm), !,cvt_element(Atomic, Val).
cvt_element(element(Tag, [], [element(T, A, L)]), Tag = Val) :-
    % Process a nested element with its tag, attributes, and list of values.
    !,cvt_element(element(T, A, L), Val).
cvt_element(element(Tag, [], [Atomic]), Tag = Atomic) :-
    % Process a single atomic value within an element.
    !.
cvt_element(element(Tag, [], List), Tag = Val) :-
    % Process a list of nested elements and extract their values.
    !,cvt_element(List, Val).
cvt_element(Val, Val).
% Default case: return the input value as is.

%! get_content(+Tags, +Input, -Result) is det.
%
%   Recursively extracts content from nested elements based on a sequence of tags.
%   The predicate searches through lists and elements to match the desired tags.
%
%   @arg Tags    A list of tag names to follow in sequence.
%   @arg Input   The current list or element being inspected.
%   @arg Result  The extracted content matching the tag sequence.
%
%   @example
%     ?- get_content([cv, name], element(cv, [], [element(name, [], ['Value'])]), Result).
%     Result = 'Value'.
%
get_content([], R, R) :- !. % Base case: no tags left, unify with the current result.
get_content([S | Tags], L, R) :-
    % If the input is a list, check each element recursively.
    is_list(L),
    member(E, L),
    get_content([S | Tags], E, R), !.
get_content([S | Tags], element(S, _, L), R) :-
    % If the current element matches the tag, process its content.
    get_content(Tags, L, R), !.
get_content(STags, element(_, _, L), R) :-
    % Fallback case: check the content of other elements for matching tags.
    member(C, L),
    get_content(STags, C, R), !.

%! get_element_value_each(+Element, -Output) is det.
%
%   Processes an element to extract its value. If the element contains sub-elements,
%   it triggers `try_begin/3` for additional processing.
%
%   @arg Element The XML/SGML element to process.
%   @arg Output  The extracted value or term.
%
%   @example
%     ?- get_element_value_each(element(node, [], [element(name, [], ['A'])]), Output).
%     Output = node=name='A'.
%
get_element_value_each(element(R, [], List), Out) :-
    % If the list contains nested elements, trigger try_begin and extract value.
    \+ \+ member(element(_, _, _), List), % Double negation ensures safe checking.
    try_begin(R, [], List),
    get_element_value(element(R, [], List), Out).
get_element_value_each(R, Out) :-
    % Directly process the input if it doesn't contain nested elements.
    get_element_value(R, Out), !.

%! get_element_value(+Input, -Output) is det.
%
%   Extracts values from an element or list of elements.
%   Handles both atomic values and nested elements recursively.
%
%   @arg Input   The input element, list, or atomic value.
%   @arg Output  The extracted or normalized value.
%
%   @example
%     ?- get_element_value(element(tag, [], ['A']), Output).
%     Output = tag='A'.
%
%     ?- get_element_value([element(tag, [], ['A']), element(tag, [], ['B'])], Output).
%     Output = [tag='A', tag='B'].
%
get_element_value([L], R) :-
    % If input is a list with one element, process it recursively.
    !, get_element_value(L, R).
get_element_value(element(T, [], [L]), T = R) :-
    % If the element contains a single value, bind it to the tag.
    get_element_value(L, R), !.
get_element_value(element(T, [], L), T = R) :-
    % If the element contains a list, process each nested element.
    is_list(L), !,
    maplist(get_element_value, L, R).
get_element_value(L, V) :-
    % If input is a list, process each element.
    is_list(L), !,
    maplist(get_element_value, L, V).
get_element_value(L, v(L)). % Default case: wrap the value into `v/1`.

%! finish_feature_data is det.
%
%   Finalizes feature data processing by iterating over all stored `feature_data/3` terms.
%   Calls `process_feature_data/3` on each term and clears the feature data store.
%
%   @example
%     % Assuming feature_data(node, [id=1], 'A') is asserted:
%     ?- finish_feature_data.
%     ====================================
%
finish_feature_data :-
    forall(
        feature_data(Tag, Attr, Content),
        once(process_feature_data(Tag, Attr, Content))
    ),
    writeln('====================================').

%! sub_prop(?Name) is semidet.
%
%   Succeeds if the input is a predefined sub-property name.
%   Used to identify valid sub-properties when processing elements.
%
%   @arg Name A valid property name (e.g., `name`, `value`).
%
sub_prop(name).
sub_prop(value).

%! process_feature_data(+Tag, +Attributes, +Content) is det.
%
%   Processes feature data by analyzing its tag, attributes, and content.
%   This predicate handles various cases such as atomic values, lists, and nested elements.
%   It dynamically constructs and asserts Prolog terms based on the input.
%
%   @arg Tag        The tag or prefix for the feature data.
%   @arg Attributes A list of name-value pairs representing attributes.
%   @arg Content    The content associated with the feature, which can be a list,
%                   an atomic value, or a nested element.
%
%   @example
%     ?- process_feature_data(node, [id=1], [value='NodeA']).
%     node_id_value(1, 'NodeA').
%
%     ?- process_feature_data(node, [name='example'], []).
%     node_name('example').
%
%   @note Commented-out clauses are preserved for potential future logic changes.
%

%process_feature_data(_,_,_).

%process_feature_data(featureprop, Attr, element(T,A,B)):-!,
%  process_feature_data(T, A, B).

%process_feature_data(Tag, Attr, Content):- is_list(Content),!,
%  maplist(process_feature_data(_, Attr), Content).

%process_feature_data(Tag, Attr, element(T,A,B)):- !,
%   process_feature_data(T, Attr, B).

%process_feature_data(Tag, Attr, element(cvterm,A,B)):- !, % sub_prop(T),
%  append(Attr,A,AttrA),
%   process_feature_data(Tag, AttrA, B).
process_feature_data(S, List, [Value]) :-
    % Process a non-compound value by appending it to the attribute list.
    \+ compound(Value), !,
    append(List, [value=Value], VList),
    process_feature_data(S, VList, []).
process_feature_data(S, List, Nil) :-
    % Process an empty content list by dynamically constructing a term.
    Nil == [], List \== [], 
    must_det_ll((
        % Convert attributes into names and values.
        into_name_values(List, Ns, Vs),
        % Concatenate tag and names into a predicate name.
        atomic_list_concat([S | Ns], '_', Pred),
        % Construct the term dynamically and assert it.
        Assert =.. [Pred | Vs], !,
        afd(Assert)
    )), !.
process_feature_data(S1, List1, Content) :-
    % Fail-safe clause for list-based content with nested key-value pairs.
    fail, Content \== [], is_list(Content),
    maplist(elements_are_kv(S2), Content, List2),
    atomic_list_concat([S1, S2], '_', SS),
    append(List1, List2, List), !,
    process_feature_data(SS, List, []).
process_feature_data(S, List, Ele) :-
    % Process an element where the attributes match the list.
    Ele = element(S2, List2, Nil),
    List == List2, S2 == S, !,
    process_feature_data(S, List, Nil).
process_feature_data(S, [N=V], Content) :-
    % Process nested content that contains additional elements.
    is_list(Content), member(element(_, _, _), Content), !,
    process_feature_data(S, [N=V], []),
    process_feature_data(V, [], Content).
process_feature_data(S1, List1, Ele) :-
    % Fail-safe clause for mismatched tags in nested elements.
    fail,
    Ele = element(S2, List2, Nil),
    S2 \== S1, !,
    atomic_list_concat([S1, S2], '_', SS),
    append(List1, List2, List),
    %process_feature_data(S1, List1, []),
    process_feature_data(SS, List, Nil).
process_feature_data(Tag, Attr, Content) :-
    % Process a list of elements containing further nested content.
    is_list(Content), member(element(_, _, _), Content),
    maplist(process_feature_data(Tag, Attr), Content).
process_feature_data(T, A, B) :-
    % Debugging clause: Print the current tag, attributes, and content with a slight delay.
    print(tab(T, A, B)), nl, sleep(0.1).


%! afd(+Assert) is det.
%
%   Writes a debug message for the given term.
%
%   @arg Assert The term to be printed for debugging.
%
afd(Assert) :-
    wdmsg(Assert).

%! into_name_values(+Pairs, -Names, -Values) is det.
%
%   Splits a list of name-value pairs into separate lists of normalized names and values.
%
%   @arg Pairs   A list of name-value pairs (e.g., [name=val, attr.key=123]).
%   @arg Names   The resulting list of normalized names.
%   @arg Values  The resulting list of corresponding values.
%
%   @example
%     ?- into_name_values([name=val, attr.key=123], Ns, Vs).
%     Ns = [name, key], Vs = [val, 123].
%
into_name_values([], [], []) :- !. % Base case: empty input results in empty outputs.
into_name_values([N=V | List], [FN | Ns], [FV | Vs]) :-
    % Normalize the name and value, then recurse on the remaining pairs.
    fix_name(N, FN),
    fix_value(V, FV),
    into_name_values(List, Ns, Vs).

%! fix_name(+Name, -FixedName) is det.
%
%   Normalizes a name by removing certain prefixes (e.g., `attr.`, `_`).
%
%   @arg Name       The original name (atom or non-atom).
%   @arg FixedName  The normalized name.
%
%   @example
%     ?- fix_name('attr.name', FN).
%     FN = name.
%     ?- fix_name('_key', FN).
%     FN = key.
%
fix_name(N, FN) :-
    \+ atom(N), !, FN = N. % If the name is not an atom, return it unchanged.
fix_name(N, FN) :-
    atom_concat('attr.', FFN, N), !, fix_name(FFN, FN). % Remove 'attr.' prefix.
%fix_name(N, FN):- atom_concat('v_', FFN, N),!, fix_name(FFN, FN).
%fix_name(N, FN):- atom_concat('n_', FFN, N),!, fix_name(FFN, FN).
%fix_name(N, FN):- atom_concat('e_', FFN, N),!, fix_name(FFN, FN).
fix_name(N, FN) :-
    atom_concat('_', FFN, N), !, fix_name(FFN, FN). % Remove leading '_'.
fix_name(N, N). % Default case: return the name unchanged.

%! rgml is det.
%
%   Loads a specific GraphML file for testing and performance analysis.
%
%   This predicate uses a predefined path to a knowledge graph stored as GraphML.
%
%   @example
%     ?- rgml.
%
rgml :-
    load_graphml('CKG_N', 'tests/performance/knowledge_graphs/graphml_csv/cml/ckg_neighbors_cml_graph_n15612_e21425.graphml').

%! rgml2 is det.
%
%   Loads multiple GraphML files for testing purposes.
%
%   This predicate uses wildcard paths to load multiple GraphML files.
%
%   @example
%     ?- rgml2.
%
rgml2 :- load_graphml('&self', 'library/graphml/tests/*.graphml').

%! load_graphml(+KB, +Paths) is det.
%
%   Loads GraphML files from the specified paths and processes them into the database.
%   Supports single files, wildcard patterns, and lists of file paths.
%
%   @arg KB     A knowledge base or identifier for processing.
%   @arg Paths  The file path, wildcard, or list of file paths to load.
%
%   @example
%     ?- load_graphml(kb_name, 'path/to/file.graphml').
%     ?- load_graphml(kb_name, '*.graphml').
%
load_graphml(KB, Paths) :-
    % If Paths is an atom with wildcards, expand it into a list of file names.
    atom(Paths),expand_file_name(Paths, List),List \== [Paths], !,maplist(load_graphml(KB), List).
load_graphml(KB, Paths) :-
    % If Paths is already a list, process each file in the list.
    is_list(Paths), !,maplist(load_graphml(KB), Paths).
load_graphml(KB, Paths) :-
    % Otherwise, read the GraphML file and process its content.
    read_graphml(Paths, To), !,load_fb_graphml_read(KB, To).

%! dump_graph(+File_basename:atom) is det
%
%   Read the file File_basename.graphml and write the parsed structure into File_basename.pl
%
%   Reads a GraphML file, parses its structure, and writes the parsed representation
%   into a corresponding Prolog file. The output file has the same base name with 
%   the `.pl` extension.
%
%   @arg File_basename The base name of the GraphML file (without the `.graphml` extension).
%
%   @example
%     % Given a file 'example.graphml', this writes 'example.pl' with the parsed content:
%     ?- dump_graph('example').
%
%   @note
%     The parsed GraphML structure is printed in Prolog's canonical form into the `.pl` file.
%
dump_graph(Base_name) :-
    % Generate the full path to the GraphML file by appending the extension.
    atomic_list_concat([Base_name, '.graphml'], Graphfile),
    % Generate the full path to the output Prolog file.
    atomic_list_concat([Base_name, '.pl'], PLfile),
    % Load the GraphML file into memory as an HTML/XML structure.
    load_html(Graphfile, Graphml, []),
    % Open the Prolog output file for writing.
    open(PLfile, write, Out),
    % Write the GraphML structure to the file in a readable Prolog term format.
    print_term(Graphml, [output(Out)]),
    % Ensure the output is terminated properly with a period.
    writeln(Out, '.'),
    % Flush the output buffer to ensure all data is written to disk.
    flush_output(Out),
    % Close the output file to finalize writing.
    close(Out).

%! run(+File_basename:atom) is det
%
%   Reads a GraphML file, parses its structure, and prints the corresponding list of terms.
%   This is useful for debugging or examining the parsed representation of the GraphML file.
%
%   @arg File_basename The base name of the GraphML file (without the `.graphml` extension).
%
%   @example
%     % Read and print the content of 'example.graphml':
%     ?- run('example').
%     [node_prop(node1, label, 'Node A'), edge_prop(node1, node2, label, 'Edge A')].
%
run(Base_name) :-
    % Parse the GraphML file into a list of terms.
    read_graphml(Base_name, Term_list),
    % Print the parsed terms in a readable format.
    print_term(Term_list, []),
    !.

%! new_node(-Node_dict) is det.
%
%   Defines the structure of a node dictionary.
%   Nodes are represented as `node{id, label, description}` where:
%     - `id` is the node identifier.
%     - `label` is the human-readable label for the node.
%     - `description` provides additional descriptive text.
%
%   @arg Node_dict A dictionary structure representing a node.
%
%   @example
%     ?- new_node(Node).
%     Node = node{id:_, label:_, description:_}.
%
new_node(node{id:_, label:_, description:_}).

%! new_edge(-Edge_dict) is det.
%
%   Defines the structure of an edge dictionary.
%   Edges are represented as `edge{id, source_id, target_id, label}` where:
%     - `id` is the edge identifier.
%     - `source_id` is the ID of the source node.
%     - `target_id` is the ID of the target node.
%     - `label` is the label describing the edge.
%
%   @arg Edge_dict A dictionary structure representing an edge.
%
%   @example
%     ?- new_edge(Edge).
%     Edge = edge{id:_, source_id:_, target_id:_, label:_}.
%
new_edge(edge{id:_, source_id:_, target_id:_, label:_}).

%! graphml_term_list(++Graph_element:term, -Term_list:list) is det
%
%   Term_list is the list of terms for Graph_element.
%
%   Converts a GraphML element into a list of Prolog terms.
%   This predicate extracts nodes and edges from a GraphML structure, processes them,
%   and generates a corresponding list of simplified terms.
%
%   @arg Graph_element A term representing the entire GraphML document.
%                      It has the structure `element(graphml, _, Element_list)`, where:
%                      - `Element_list` contains the graph and its components.
%   @arg Term_list     A list of terms representing the nodes and edges in the graph.
%
%   @example
%     % Example input structure:
%     ?- graphml_term_list(element(graphml, [], [
%                                     element(graph, [], [
%                                       element(node, [id=node1], []),
%                                       element(edge, [source=node1, target=node2], [])
%                                     ])
%                                   ]), Terms).
%     Terms = [node(...), edge(...)]. 
%
graphml_term_list(element(graphml, _Graphml_prop_list, Element_list), Term_list) :-
    % Extract the keys (attribute mappings) from the GraphML elements.
    keys(Element_list, Key_list),
    % Find the 'graph' element containing the nodes and edges.
    memberchk(element(graph, _Graph_prop_list, Graph_element_list), Element_list),
    % Process the graph's element list to extract terms for nodes and edges.
    element_list_term_list(Graph_element_list, Key_list, Term_list).

%! element_list_term_list(++Element_list:list, ++Attr_key_list:list, -Term_list:list) is det
%
%   Term_list is the list of terms that corresponds to Element_list given the list of attribute keys Attr_key_list.
%
%   Converts a list of GraphML elements into a list of Prolog terms.
%   This predicate processes each element in `Element_list`, using the provided
%   attribute keys in `Attr_key_list` to extract relevant node or edge terms.
%
%   @arg Element_list   A list of graph elements (e.g., nodes and edges).
%   @arg Attr_key_list  A list of key mappings in the form `key(From, Attr, Key)`.
%                       These keys map attributes like labels and descriptions to GraphML elements.
%   @arg Term_list      The resulting list of Prolog terms representing nodes and edges.
%
%   @example
%     % Example input:
%     ?- element_list_term_list(
%            [element(node, [id=node1], []), element(edge, [source=node1, target=node2], [])],
%            [key(node, label, key_label), key(edge, label, key_edge_label)],
%            Terms).
%     Terms = [node(id=node1), edge(source=node1, target=node2)].
%
element_list_term_list(Element_list, Attr_key_list, Term_list) :-
    % Use findall/3 to collect terms for all elements in Element_list.
    findall(
        Term,
        (
            % For each element in Element_list, process it into a term.
            member(Element, Element_list),
            graph_element_term(Element, Attr_key_list, Term)
        ),
        Term_list
    ).

%! graph_element_term( ++Element:term, ++Attr_key_list:list, -Term ) is det
%
%   Term is the term that corresponds to Element.
%
%   Converts a GraphML element into a corresponding Prolog term.
%   This predicate processes both `node` and `edge` elements. For nodes,
%   it extracts the `id`, `label`, and `description` attributes.
%
%   @arg Element        A GraphML element, such as `node` or `edge`, represented as a Prolog term.
%   @arg Attr_key_list  A list of keys used to map attributes within the GraphML structure.
%                       Keys are of the form `key(From, Attr, Key)`, where:
%                         - `From` specifies the type (e.g., `node` or `edge`).
%                         - `Attr` is the attribute name.
%                         - `Key` is the unique identifier for the attribute.
%   @arg Term           The resulting Prolog term representing the element.
%
%   @example
%     % Example input and output for a node element:
%     ?- graph_element_term(
%            element(node, [id=node1], []),
%            [key(node, description, key_desc), key(node, label, key_label)],
%            Term
%        ).
%     Term = node{id=node1, label='', description=''}.
%
graph_element_term(
    element(node, Node_props, Node_elements), % Match a 'node' element.
    Attr_key_list,                            % List of keys for mapping attributes.
    Node                                      % Output term.
) :- 
    !, % Cut to ensure this clause handles only 'node' elements.
    % Extract the node ID from the properties.
    memberchk(id=Node_id, Node_props),
    % Extract the node description using helper predicate.
    node_description(Node_elements, Node_description, Attr_key_list),
    % Extract the node label using helper predicate.
    node_label(Node_elements, Node_label, Attr_key_list),
    % Initialize a new node dictionary structure.
    new_node(Node),
    % Assign extracted values to the node dictionary.
    Node.id = Node_id, 
    Node.label = Node_label, 
    Node.description = Node_description.
% edge(Edge_id:atom, Source_id:atom, Target_id:atom, Edge_label:string)
graph_element_term(
    element(edge, Edge_props, Edge_elements), % Match an 'edge' element.
    Attr_key_list,                            % List of attribute keys for mapping labels.
    Edge                                      % Output term.
) :-
    !, % Cut to ensure this clause handles only 'edge' elements.
    % Extract the edge ID from the properties.
    memberchk(id=Edge_id, Edge_props),
    % Extract the source node ID from the properties.
    memberchk(source=Source_id, Edge_props),
    % Extract the target node ID from the properties.
    memberchk(target=Target_id, Edge_props),
    % Extract the edge label using a helper predicate.
    edge_label(Edge_elements, Edge_label, Attr_key_list),
    % Initialize a new edge dictionary structure.
    new_edge(Edge),
    % Assign extracted values to the edge dictionary.
    Edge.id = Edge_id, 
    Edge.source_id = Source_id, 
    Edge.target_id = Target_id, 
    Edge.label = Edge_label.

%! node_description(++Node_element_list:list, -Node_description:string, ++Attr_key_list) is det
%
%   Extracts the description of a node from a list of node elements.
%   The description is identified using the attribute key provided in `Attr_key_list`.
%   If no description is found, it defaults to an empty string.
%
%   @arg Node_element_list A list of elements describing the node (parsed GraphML content).
%   @arg Node_description  The resulting description for the node as a string.
%   @arg Attr_key_list     A list of keys mapping attributes in the form `key(From, Attr, Key)`.
%
%   @example
%     % Example input with a description key in the attribute list:
%     ?- node_description(
%            [element(data, [key=key_desc], ['Node description'])],
%            Desc,
%            [key(node, description, key_desc)]
%        ).
%     Desc = 'Node description'.
%
%     % Example input where no description is present:
%     ?- node_description([], Desc, [key(node, description, key_desc)]).
%     Desc = "".
%
node_description(Node_element_list, Node_description, Attr_key_list) :-
    % Find the description key for nodes in the attribute key list.
    memberchk(key(node, description, Key_node_description), Attr_key_list),
    % Search for the data element corresponding to the description key and extract its value.
    data(Key_node_description, Node_element_list, [Node_description]), 
    !. % Succeed once the description is found.
node_description(_, "", _). % Default case: if no description is found, return an empty string.

%! node_label(++Node_element_list:list, -Node_label:string, ++Attr_key_list:list) is det
%
%   Node_label is the node label found in Node_element_list.
%
%   Extracts the label of a node from a list of node elements.
%   The label is identified using the attribute key provided in `Attr_key_list`.
%   If no label is found, it defaults to an empty string.
%
%   @arg Node_element_list A list of elements describing the node (parsed GraphML content).
%   @arg Node_label        The resulting label for the node as a string.
%   @arg Attr_key_list     A list of keys mapping attributes in the form `key(From, Attr, Key)`.
%
%   @example
%     % Example input with a nodegraphics key and NodeLabel element:
%     ?- node_label(
%            [element(data, [key=key_nodegraphics], [
%                element('y:ImageNode', [], [
%                    element('y:NodeLabel', [], ['Node Label'])
%                ])
%            ])],
%            Label,
%            [key(node, nodegraphics, key_nodegraphics)]
%        ).
%     Label = 'Node Label'.
%
%     % Example input where no label is present:
%     ?- node_label([], Label, [key(node, nodegraphics, key_nodegraphics)]).
%     Label = "".
%
node_label(Node_element_list, Node_label, Attr_key_list) :-
    % Find the 'nodegraphics' key for nodes in the attribute key list.
    memberchk(key(node, nodegraphics, Key_nodegraphics), Attr_key_list),
    (
        % Retrieve the 'nodegraphics' data element for the key.
        data(Key_nodegraphics, Node_element_list, Nodegraphics_elements),
        % Search for the 'y:ImageNode' element containing graphics definitions.
        member(element('y:ImageNode', _Image_props, Image_elements), Nodegraphics_elements),
        % Extract the 'y:NodeLabel' content as the node label.
        member(element('y:NodeLabel', _Label_props, [Node_label]), Image_elements)
    ;
        % Default case: if no label is found, set Node_label to an empty string.
        Node_label = ""
    ),
    !.

%! edge_label(+Edge_element_list, -Edge_label, +Attr_key_list) is det.
%
%   Edge_label is the edge label found in Edge_element_list.
%
%   Extracts the label of an edge from a list of edge elements.
%   The label is identified using the attribute key provided in `Attr_key_list`.
%   If no label is found, it defaults to an empty string.
%
%   @arg Edge_element_list A list of elements describing the edge (parsed GraphML content).
%   @arg Edge_label        The resulting label for the edge as a string.
%   @arg Attr_key_list     A list of keys mapping attributes in the form `key(From, Attr, Key)`.
%
%   @example
%     % Example input with an edgegraphics key and EdgeLabel element:
%     ?- edge_label(
%            [element(data, [key=key_edgegraphics], [
%                element('y:PolyLineEdge', [], [
%                    element('y:EdgeLabel', [], ['Edge Label'])
%                ])
%            ])],
%            Label,
%            [key(edge, edgegraphics, key_edgegraphics)]
%        ).
%     Label = 'Edge Label'.
%
%     % Example input where no label is present:
%     ?- edge_label([], Label, [key(edge, edgegraphics, key_edgegraphics)]).
%     Label = ''.
%
edge_label(Edge_element_list, Edge_label, Attr_key_list) :-
    % Find the 'edgegraphics' key for edges in the attribute key list.
    memberchk(key(edge, edgegraphics, Key_edgegraphics), Attr_key_list),
    (
        % Retrieve the 'edgegraphics' data element for the key.
        data(Key_edgegraphics, Edge_element_list, Edgegraphics_elements),
        % Locate a graphic element type containing edge graphics.
        member(element(_Graphic_type, _Graphic_type_props, Graphic_type_elements), Edgegraphics_elements),
        % Extract the 'y:EdgeLabel' content from the edge graphics.
        member(element('y:EdgeLabel', _Label_props, Label_elements), Graphic_type_elements),
        % Extract the label content and normalize spacing.
        member(Edge_label_1, Label_elements),
        normalize_space(atom(Edge_label), Edge_label_1)
    ;
        % Default case: if no label is found, set Edge_label to an empty string.
        Edge_label = ''
    ),
    !.

%! data(+Key_id:atom, ++Element_list:list, -Sub_element_list:list) is nondet
%
%   Sub_element_list is the list of sub_elements.
%
%   Finds the content of a `data` element with a specific key from a list of elements.
%   This predicate searches through `Element_list` for elements of type `data`
%   that have a `key` attribute matching `Key_id`, and unifies their content with `Sub_element_list`.
%
%   @arg Key_id           The key identifier (atom) used to locate the desired `data` element.
%   @arg Element_list     A list of elements (typically parsed GraphML or XML terms).
%   @arg Sub_element_list The content of the matching `data` element, represented as a list of sub-elements.
%
%   @example
%     % Example input where a data element has a key 'key1' and associated content:
%     ?- data(key1, [element(data, [key=key1], ['Some Content'])], Content).
%     Content = ['Some Content'].
%
%     % Example where no matching data element exists:
%     ?- data(key2, [element(data, [key=key1], ['Some Content'])], Content).
%     false.
%
data(Key_id, Element_list, Sub_element_list) :-
    % Search for an element of type 'data' in the list of elements.
    member(element(data, Props, Sub_element_list), Element_list),
    % Ensure that the 'data' element has a 'key' attribute matching Key_id.
    member(key=Key_id, Props).

%! element_attribute(?Element_type, ?Attr_name) is nondet.
%
%   Attr_name is the name of an attribute of interest for elements of type Element_type.
%
%   Defines attributes of interest for specific types of GraphML elements.
%   This predicate associates `Element_type` (e.g., `node` or `edge`) with
%   relevant attributes (`Attr_name`) such as graphics or descriptions.
%
%   @arg Element_type The type of the element, e.g., `node` or `edge`.
%   @arg Attr_name    The name of the attribute associated with the element type.
%
%   @example
%     % Find all attributes for a node element:
%     ?- element_attribute(node, Attr).
%     Attr = nodegraphics ;
%     Attr = description.
%
%     % Check if a specific attribute belongs to an edge:
%     ?- element_attribute(edge, edgegraphics).
%     true.
%
%     % Attempt an invalid query:
%     ?- element_attribute(edge, label).
%     false.
%
element_attribute(node, nodegraphics).
element_attribute(node, description).
element_attribute(edge, edgegraphics).
element_attribute(edge, description).

%! keys(++Element_list:list, -Key_list:list) is det
%
%   Key_list is the list of terms of form key(element_type, attribute_name, key_id),
%   corresponding to elements in Element_list which define key_ids.
%
%   Extracts a list of keys defined in a GraphML document.
%   For each relevant element type (e.g., `node` or `edge`) and attribute name
%   (e.g., `nodegraphics` or `description`), it identifies corresponding `key_id`s
%   and constructs terms of the form `key(Element_type, Attribute_name, Key_id)`.
%
%   @arg Element_list A list of GraphML elements to analyze for key definitions.
%   @arg Key_list     The resulting list of key terms in the format:
%                     `key(Element_type, Attribute_name, Key_id)`.
%
%   @example
%     % Example input with GraphML key definitions:
%     ?- keys([
%            element(key, [for=node, 'attr.name'=description, id=k1], []),
%            element(key, [for=edge, 'attr.name'=edgegraphics, id=k2], [])
%        ], Keys).
%     Keys = [key(node, description, k1), key(edge, edgegraphics, k2)].
%
keys(Elements, Keys) :-
    % Use findall/3 to collect all matching key terms.
    findall(
        key(For, Attr, Key_id), % Define the term structure to collect.
        (
            % Ensure the element type (For) and attribute name (Attr) are valid.
            element_attribute(For, Attr),
            % Retrieve the key_id corresponding to the element type and attribute name.
            key(Elements, For, Attr, Key_id)
        ),
        Keys
    ).

%! key(++Element_list:list, +Element_type:atom, +Attr_name:atom, -Key_id:atom) is det
%
%   Key_id is used for the Attr_name of the Element_type.
%
%   Extracts the `Key_id` corresponding to a specific `Element_type` and `Attr_name`
%   from a list of GraphML `key` elements. The `key` elements provide metadata for
%   nodes or edges, such as attributes and their corresponding identifiers.
%
%   @arg Element_list  A list of GraphML elements to search for key definitions.
%   @arg Element_type  The type of element to match (e.g., `node` or `edge`).
%   @arg Attr_name     The name of the attribute to locate (e.g., `description` or `nodegraphics`).
%   @arg Key_id        The key ID (atom) associated with the given `Element_type` and `Attr_name`.
%
%   @example
%     % Example input where keys for nodes and edges are defined:
%     ?- key([
%            element(key, [for=node, 'attr.name'=description, id=k1], []),
%            element(key, [for=edge, 'yfiles.type'=edgegraphics, id=k2], [])
%        ], node, description, Key_id).
%     Key_id = k1.
%
%     % Example with edge and edgegraphics:
%     ?- key([
%            element(key, [for=edge, 'yfiles.type'=edgegraphics, id=k2], [])
%        ], edge, edgegraphics, Key_id).
%     Key_id = k2.
%
key(Element_list, Element_type, Attr_name, Key_id) :-
    % Find a 'key' element in the list of elements.
    member(element(key, Key_props, _), Element_list),
    % Ensure the 'for' attribute matches the given Element_type.
    member(for=Element_type, Key_props),
    % Check if the attribute matches either 'attr.name' or 'yfiles.type'.
    (   member('attr.name'=Attr_name, Key_props)
    ;   member('yfiles.type'=Attr_name, Key_props)
    ),
    % Extract the ID of the key.
    member(id=Key_id, Key_props),
    !. % Cut to ensure only the first matching key is returned.

% :- rgml.
