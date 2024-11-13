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
% PROGRAM FUNCTION: parse and process Chado XML files (which contain biological/genomic data), 
% extracting features and their attributes from the XML structure while handling different types of elements and content.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Loads library(sqml) for XML and SGML. This predicate is equivalent to ensure_loaded/1, except that it raises an error if Files are not module files.
:- use_module(library(sgml)).
%:- use_module(library(logicmoo/xml_reader)).

%!  extract_dtd_file(+XMLFile, -DTDFileName) is det.
%
%   Loads an XML file and extracts the name of the associated DTD file.
%
%   This predicate opens an XML file, reads its structure, and extracts the
%   DTD file name (if specified). It uses the `doctype` option in `load_structure/3`
%   to capture the DTD details, then closes the file stream after reading.
%
%   @arg XMLFile The path to the XML file to be processed.
%   @arg DTDFileName The variable to unify with the extracted DTD file name, if available.
%
%   @example Extract the DTD file name from an XML file:
%     ?- extract_dtd_file('sample.xml', DTDFileName).
%     DTDFileName = 'sample.dtd'.
%
extract_dtd_file(XMLFile, DTDFileName) :-
    % Open the XML file in read mode.
    open(XMLFile, read, Stream),
    % Load the XML structure, capturing the DTD declaration.
    load_structure(Stream, _XML, [dialect(xml), doctype(Doctype)]),
    % Close the stream after loading the XML structure.
    close(Stream),
    % Extract the DTD file name from the Doctype information.
    dtd_file_from_doctype(Doctype, DTDFileName).

%!  dtd_file_from_doctype(+Doctype, -FileName) is nondet.
%
%   Extracts the DTD file name from a DOCTYPE declaration structure.
%
%   This helper predicate processes the `Doctype` structure to retrieve
%   the external DTD file name, if it exists. It expects a `doctype/2`
%   structure and calls `extract_system_id/2` for the SYSTEM identifier.
%
%   @arg Doctype The DOCTYPE structure from which to extract the DTD file name.
%   @arg FileName The variable to unify with the extracted DTD file name.
%
dtd_file_from_doctype(Doctype, FileName) :-
    % Ensure Doctype is non-variable to prevent unintended unification.
    nonvar(Doctype),
    % Match the Doctype structure, retrieving the external ID.
    Doctype = doctype(_Name, ExternalID),
    % Extract the SYSTEM identifier to obtain the DTD file name.
    extract_system_id(ExternalID, FileName).

%!  extract_system_id(+ExternalID, -FileName) is nondet.
%
%   Retrieves the DTD file name from a SYSTEM identifier.
%
%   Given the `ExternalID` term, which typically contains a `system/1`
%   structure, this predicate extracts and unifies the DTD file name
%   with `FileName`.
%
%   @arg ExternalID The external identifier structure, expected to be `system(FileName)`.
%   @arg FileName The variable to unify with the DTD file name extracted from the SYSTEM identifier.
%
extract_system_id(ExternalID, FileName) :-
    % Ensure ExternalID is non-variable and matches the system structure.
    nonvar(ExternalID),
    % Unify FileName with the identifier inside the system structure.
    ExternalID = system(FileName).

/*
root@gitlab:/wam/data/FB_current# du . -h
14M     ./precomputed_files/insertions
26M     ./precomputed_files/stocks
231M    ./precomputed_files/references
3.9M    ./precomputed_files/human_disease
43M     ./precomputed_files/synonyms
293M    ./precomputed_files/ontologies
34M     ./precomputed_files/clones
22M     ./precomputed_files/orthologs
4.9G    ./precomputed_files/genes
1.4M    ./precomputed_files/species
102M    ./precomputed_files/alleles
148K    ./precomputed_files/map_conversion
35M     ./precomputed_files/go
125M    ./precomputed_files/collaborators
988K    ./precomputed_files/transposons
1.9G    ./precomputed_files/metadata
7.6G    ./precomputed_files
11G     ./dmel_r6.56/fasta
6.3G    ./dmel_r6.56/gff
75M     ./dmel_r6.56/gtf
47G     ./dmel_r6.56/chado-xml
139M    ./dmel_r6.56/dna
64G     ./dmel_r6.56
329G    ./chado-xml
400G    .


*/
/*
load_dtd(DTDFile, DTD) :-
    catch(
        (
            open(DTDFile, read, Stream),
            load_structure(Stream, DTD, [dtd(DTD), dialect(xml)]),
            close(Stream)
        ),
        Error,
        (   print_message(error, Error),
            fail
        )
    ).
*/

%!  lfb0 is det.
%
%   Parses an XML file and outputs its content.
%
%   This predicate reads the XML file at the specified path using
%   `fileToLineInfoElements/3`, parsing it into a structured format. 
%   The parsed XML structure is then printed to the standard output. 
%   The cut (`!`) ensures no backtracking after the XML content is output.
%
%   @example Parse and print XML content:
%     ?- lfb0.
%     % Outputs the parsed XML content of 'chado_FBim.xml'.
%
lfb0 :-
    % Parse the XML file and store the result in the variable XML.
    fileToLineInfoElements(_Ctx, '/wam/data/FB_current/chado-xml/chado_FBim.xml', XML),
    % Output the XML structure to the console.
    writeln(XML), !.

%!  lfb1 is det.
%
%   Loads the Chado XML file for FBgn data.
%
%   This predicate calls `load_chado_xml/1` with the specified FBgn XML file path.
%   It initiates the loading process for the XML file.
%
%   @example Load the FBgn XML data file:
%     ?- lfb1.
%
lfb1 :-
    % Initiate the loading of the XML file located at the specified path.
    load_chado_xml('/wam/data/FB_current/chado-xml/chado_FBgn.xml').

%!  lfb2 is det.
%
%   Loads the Chado XML file for Dmel gene model data.
%
%   This predicate calls `load_chado_xml/1` to load the Dmel gene model XML file
%   located at the specified path. It starts the loading process for this file.
%
%   @example Load the Dmel gene model XML data file:
%     ?- lfb2.
%
lfb2 :-
    % Load the specified XML file for Dmel gene model data.
    load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_gene_models.xml').

%!  lfb3 is det.
%
%   Loads the Chado XML file for Dmel predicted data.
%
%   This predicate calls `load_chado_xml/1` with the Dmel predicted XML file path,
%   initiating the loading process for this file.
%
%   @example Load the Dmel predicted XML data file:
%     ?- lfb3.
%
lfb3 :-
    % Load the specified XML file for Dmel predicted data.
    load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_predicted.xml').

%!  load_chado_xml(+File) is det.
%
%   Parses an XML file and applies specific handlers at the start and end of elements.
%
%   This predicate opens the specified XML file, initializes an SGML parser,
%   and configures the parser settings to treat the file as XML and remove extra
%   whitespace. It then parses the XML content, triggering `on_begin/2` and `on_end/2`
%   calls at the start and end of each element. Finally, it closes the file stream.
%
%   @arg File The path to the XML file to be parsed.
%
%   @example Load and parse an XML file, applying element start and end handlers:
%     ?- load_chado_xml('/path/to/file.xml').
%
load_chado_xml(File) :-
    % Open the XML file in read mode and associate it with `In`.
    open(File, read, In),
    % Create a new SGML parser instance.
    new_sgml_parser(Parser, []),
    % Set the parser to associate parsing actions with the specified file.
    set_sgml_parser(Parser, file(File)),
    % Configure the parser to use XML dialect rules.
    set_sgml_parser(Parser, dialect(xml)),
    % Remove unnecessary whitespace from XML content during parsing.
    set_sgml_parser(Parser, space(remove)),

    % Parse the XML file and apply handlers for element start and end events.
    sgml_parse(Parser,
               [ source(In),
                 call(begin, on_begin),
                 call(end, on_end)
               ]),
    % Close the input file after parsing completes.
    close(In).

% 'dynamic' allows modifications at runtime.
:- dynamic(feature_data/3).

%!  on_end(+Tag, +Parser) is det.
%
%   Handles the end of an XML element, completing feature data processing if the
%   element is `feature`.
%
%   When the end of a `feature` element is encountered, this predicate finalizes
%   the feature data processing with `finish_feature_data/0`, lists all `feature_data/3`
%   facts, and clears them from memory using `retractall/1`. A brief pause (`sleep/1`)
%   is included after clearing the data. For any other tag, the predicate does nothing.
%
%   @arg Tag The name of the XML element ending.
%   @arg Parser The SGML parser instance, not used in this implementation.
%   @example Completing feature data processing:
%     ?- on_end('feature', _).
%     % Completes feature data processing, lists and clears all `feature_data/3` facts.
%
on_end('feature', _) :-  !,
    % Finalize processing for the feature data.
    finish_feature_data, !,
    % List all current feature_data facts to standard output.
    listing(feature_data(_,_,_)),
    % Remove all feature_data facts from the knowledge base.
    retractall(feature_data(_,_,_)),
    % Briefly pause execution (0.1 seconds).
    sleep(0.1), !.
% on_end(Tag, _Parser) :- current_tag(Is), Is = Tag, !, pop_tag(Tag), finish_tag(Tag).
% Default clause for handling other tags, doing nothing.
on_end(_, _).

%!  on_begin(+Tag, +Attributes, +Parser) is det.
%
%   Handles the start of XML elements, processing `chado` and `feature` tags
%   directly and attempting to parse and store data for other tags.
%
%   - For `chado` and `feature` tags, no action is taken other than recognizing
%     the start of the element.
%   - For other tags, it reads the element content and resets the parser position
%     if parsing fails.
%   - Finally, unhandled tags and their content are printed and asserted as
%     `feature_data/3` facts in the knowledge base.
%
%   @arg Tag The name of the XML element starting.
%   @arg Attributes A list of attributes associated with the XML element.
%   @arg Parser The SGML parser instance.
%   @example Processing an XML element:
%     ?- on_begin('feature', [], _Parser).
%     % Begins processing for the feature element.
%
on_begin('chado', _, _) :- !.
on_begin('feature', _, _Parser) :- !.
% on_begin(Tag, _Attr, _Parser) :- push_tag(Tag), fail.
% Attempt to parse and store feature data or reset parser position on failure.
on_begin(Tag, Attr, Parser) :- 
    read_element(Parser, Content, Reset), !,
    % Store the feature data or reset the parser position if unsuccessful.
    (store_feature(Tag, Attr, Content) -> true ; (set_sgml_parser(Parser, position(Reset)), fail)).
% If the previous clause fails, try another parse attempt and reset if unsuccessful.
on_begin(Tag, Attr, Parser) :- 
    read_element(Parser, Content, Reset), !,
    (try_begin(Tag, Attr, Content) -> true ; (set_sgml_parser(Parser, position(Reset)), fail)).
% Default clause to print any other tag and its content.
on_begin(Any, _, Parser) :- 
    read_element(Parser, Content, _), nl,
    % Print the tag name and content to the standard output.
    print(Any = Content), nl.
% Final clause to parse the document content and assert it as a `feature_data/3` fact.
on_begin(Tag, Attr, Parser) :-
    sgml_parse(Parser,
               [ document(Content),
                 parse(content)
               ]),
    FD = feature_data(Tag, Attr, Content),
    % Print and assert the feature data.
    print(FD), nl,
    assertz(FD).

%!  current_tag(-Tag) is det.
%
%   Retrieves the current tag from the `current_tag_stack/1` if it exists.
%
%   This predicate uses `current_tag_stack/1` to find the most recent tag added
%   to the stack. If no tag is available, `Tag` defaults to an empty list.
%
%   @arg Tag The most recent tag in the stack or `none` if the stack is empty.
%   @example Retrieving the current tag:
%     ?- current_tag(Tag).
%     Tag = some_tag.
%
current_tag(Tag) :-
    once(clause(current_tag_stack(Was), true, _Ref) ; Was = []),
    append(_New, [Tag], Was), !.
current_tag(none).

%!  parent_tag(-Tag) is det.
%
%   Retrieves the parent tag from the `current_tag_stack/1`, if it exists.
%
%   This predicate returns the second-to-last tag in the stack as the parent tag.
%   If no parent tag exists, `Tag` defaults to an empty list.
%
%   @arg Tag The parent tag in the stack or `none` if no parent is found.
%   @example Retrieving the parent tag:
%     ?- parent_tag(Tag).
%     Tag = parent_tag.
%
parent_tag(Tag) :-
    once(clause(current_tag_stack(Was), true, _Ref) ; Was = []),
    append(_New, [Tag, _], Was), !.
parent_tag(none).

%!  pop_tag(+Tag) is det.
%
%   Removes the most recent tag from the `current_tag_stack/1`.
%
%   This predicate matches and removes `Tag` from the end of the stack. If
%   successful, it updates the stack using `assert/1`. If a reference exists,
%   `erase/1` is used to clear it.
%
%   @arg Tag The tag to remove from the end of the stack.
%   @example Popping a tag:
%     ?- pop_tag('some_tag').
%
pop_tag(Tag) :-
    once(clause(current_tag_stack(Was), true, Ref) ; Was = []),
    append(New, [Tag], Was),
    it_t(nonvar(Ref), erase(Ref)),
    assert(current_tag_stack(New)), !.

%!  push_tag(+Tag) is det.
%
%   Adds a tag to the end of the `current_tag_stack/1`.
%
%   This predicate appends `Tag` to the end of the current tag stack,
%   storing the updated stack with `assert/1`.
%
%   @arg Tag The tag to add to the stack.
%   @example Pushing a tag:
%     ?- push_tag('new_tag').
%
push_tag(Tag) :-
    once(retract(current_tag_stack(Was)) ; Was = []),
    append(Was, [Tag], New),
    assert(current_tag_stack(New)).

%!  finish_tag(+Tag) is det.
%
%   Placeholder predicate for finishing operations on a tag.
%
%   This predicate is defined as a placeholder with no operations.
%
%   @arg Tag The tag to be "finished" (no operation is performed).
%   @example Call finish_tag with a tag:
%     ?- finish_tag('some_tag').
%
finish_tag(_Tag).

%!  peek_element(+Parser, -Content) is det.
%
%   Reads the next element from the parser without advancing its position.
%
%   This predicate reads the content of the next XML element from `Parser`
%   and immediately resets the parser position to its original state using
%   `set_sgml_parser/2`.
%
%   @arg Parser The SGML parser instance.
%   @arg Content The content of the next XML element.
%   @example Peek at the next element:
%     ?- peek_element(Parser, Content).
%
peek_element(Parser, Content) :-
    call_cleanup(
        read_element(Parser, Content, Pos),
        set_sgml_parser(Parser, position(Pos))
    ).

%!  read_element(+Parser, -Content, -Pos) is det.
%
%   Reads the content of the next XML element, storing the parser position.
%
%   This predicate reads the document content using `sgml_parse/2` and stores
%   the current position in `Pos` using `stream_property/2`.
%
%   @arg Parser The SGML parser instance.
%   @arg Content The content of the current XML element.
%   @arg Pos The current position in the XML stream.
%   @example Read an element and store position:
%     ?- read_element(Parser, Content, Pos).
%
read_element(Parser, Content, Pos) :-
    get_sgml_parser(Parser, source(S)),
    stream_property(S, position(Pos)),
    sgml_parse(Parser,
               [ document(Content),
                 parse(content)
               ]), !.

%!  try_begin(+Tag, +Attr, +Element) is det.
%
%   Processes the start of an XML element, handling various element structures.
%
%   This predicate processes an XML element based on the structure of `Element`.
%   It either combines attributes, handles lists, or delegates processing to
%   `process_feature_data/3` if `Element` is a value.
%
%   @arg Tag The tag name or identifier for the element.
%   @arg Attr The list of attributes associated with the element.
%   @arg Element The content or structure of the XML element.
%   @example Begin processing an element:
%     ?- try_begin('feature', [id=123], element(tag, [attr=value], [])).
%
try_begin(Tag, Attr, element(T, A, L)) :- !,
    % Combine `Attr` with `A` into `AttrA` and recursively call try_begin.
    append(Attr, A, AttrA), 
    try_begin(Tag = T, AttrA, L).

try_begin(Tag, Attr, List) :- 
    % If `List` is a list, process it with `absorb_type_ids/3`.
    is_list(List),
    absorb_type_ids(Tag, Attr, List), !.

try_begin(Tag, Attr, V) :- 
    % If `Element` is a value, process it directly with `process_feature_data/3`.
    process_feature_data(Tag, Attr, V).
% This version attempts to absorb types and IDs from nested elements.
% try_begin(Tag, Attr, element(T, A, L)) :-
%     % Absorbs type identifiers and processes nested elements.
%     % absorb_type_ids(Tag, Attr, element(T, A, L)),
%     maplist(try_begin(T, A), L).


%!  absorb_type_ids(+Tag, +Attr, +Elements) is det.
%
%   Processes XML elements to extract type and name identifiers and handle element content.
%
%   This predicate searches `Elements` for a `type_id` element and retrieves associated
%   type and name content, then maps the values into structured feature data. If the `type_id`
%   contains both `cv` and `cvterm` identifiers, it retrieves both the `TypeName` and `Name` and
%   constructs structured feature data with a fully qualified tag `ntv(Tag, TypeName, Name)`.
%   If only a `cvterm` identifier is present, it processes the `Name` alone, generating feature
%   data using a simplified tag `nv(Name)`. The remaining elements are processed by calling
%   `process_feature_data/3` on each item.
%
%   @arg Tag The tag name associated with the element being processed.
%   @arg Attr The attributes list associated with the element.
%   @arg Elements The list of elements, expected to include a `type_id` entry.
%   @example Processing type and name identifiers:
%     ?- absorb_type_ids('feature', [attr=value], [element(type_id, [], C) | Rest]).
%
absorb_type_ids(Tag, Attr, Elements) :-
    % Find and remove the `type_id` element from `Elements`, storing the rest in `Rest`.
    select(element(type_id, [], C), Elements, Rest),
    % Extract `TypeName` from the `C` content using `get_content/3`.
    get_content([cv, name], C, TypeName), !,
    must_det_ll((
        % Retrieve `Name` content and process the remaining elements.
        get_content([cvterm, name], C, Name),
        maplist(get_element_value_each, Rest, Values),
        maplist(process_feature_data(ntv(Tag, TypeName, Name), Attr), Values)
    )), !.
absorb_type_ids(_Tag, Attr, Elements) :-
    % Select the `type_id` element and separate the remaining elements.
    select(element(type_id, [], C), Elements, Rest),
    must_det_ll((
        % Extract `Name` content.
        get_content([cvterm, name], C, Name),
        % Process the remaining elements as feature data.
        maplist(get_element_value_each, Rest, Values),
        maplist(process_feature_data(nv(Name), Attr), Values)
    )), !.


%!  store_feature(+Tag, +Attr, +Content) is det.
%
%   Converts an XML element into a feature data structure and stores it.
%
%   This predicate converts an XML element into an internal representation,
%   `Val`, and then asserts it as `feature_data/3` in the knowledge base.
%
%   @arg Tag The tag name associated with the element being stored.
%   @arg Attr The attributes associated with the element.
%   @arg Content The content of the XML element.
%   @example Storing a feature:
%     ?- store_feature('feature', [attr=value], Content).
%
store_feature(Tag, Attr, Content) :-
    % Convert the element into a structured value.
    cvt_element(element(Tag, Attr, Content), Val),
    % Assert the structured value as feature_data.
    assert(feature_data(Tag, Attr, Val)).

%!  skip_over(+Element) is nondet.
%
%   Succeeds if `Element` matches a specific tag to be skipped.
%
%   This predicate succeeds if `Element` matches one of several predefined tags,
%   allowing selective skipping of certain XML elements during processing.
%
%   @arg Element The tag to check for skipping.
skip_over(cvterm).
skip_over(cv).
skip_over(pub).

%!  skip_over_s(+Element) is nondet.
%
%   Succeeds if `Element` matches a tag in a predefined list to be skipped.
%
%   This predicate succeeds if `Element` matches one of several tags specified
%   individually or as members of a list, allowing for selective skipping of XML elements.
%
%   @arg Element The tag to check for skipping.
skip_over_s(featureprop).
skip_over_s(featureprop_pub).
skip_over_s(E) :-
    member(E, [dbxref_id, dbxref, db_id, library_id, library, library_feature]).
skip_over_s(X) :- skip_over(X).

%!  cvt_element(+Input, -Output) is det.
%
%   Converts XML elements and lists into structured values.
%
%   This predicate recursively converts elements and lists of elements into
%   a structured format, handling specific cases for tags and atomic values.
%   It uses the `skip_over/1` and `skip_over_s/1` predicates to determine
%   elements that should be ignored during conversion.
%
%   @arg Input The original XML element or list to convert.
%   @arg Output The resulting structured value.
%   @example Convert a list of XML elements:
%     ?- cvt_element([element(tag, [], [content])], Val).
%
cvt_element(List, Val) :- 
    % If Input is a list, apply `cvt_element` to each element in the list.
    is_list(List), !, maplist(cvt_element, List, Val).
cvt_element(element(Tag, [], [element(CVTerm, [], L)]), TagVal) :- 
    % Skip over certain tags in nested elements and continue conversion.
    skip_over_s(CVTerm), !,
    cvt_element(element(Tag, [], L), TagVal).
cvt_element(element(CVTerm, [], [Atomic]), Val) :- 
    % Skip over specific tags for single atomic elements.
    skip_over_s(CVTerm), !, cvt_element(Atomic, Val).
cvt_element(element(CVTerm, [], Atomic), Val) :- 
    % Skip over specific tags for atomic content directly.
    skip_over(CVTerm), !, cvt_element(Atomic, Val).
cvt_element(element(Tag, [], [element(T, A, L)]), Tag = Val) :- 
    % Recursively convert nested elements, returning a structured tag-value pair.
    !, cvt_element(element(T, A, L), Val).
cvt_element(element(Tag, [], [Atomic]), Tag = Atomic) :- !.
cvt_element(element(Tag, [], List), Tag = Val) :- 
    % If there is a list of elements under a tag, convert each to a structured value.
    !, cvt_element(List, Val).
cvt_element(Val, Val).

%!  get_content(+Tags, +Element, -Result) is nondet.
%
%   Retrieves content from nested XML elements matching a sequence of tags.
%
%   This predicate navigates through nested XML elements based on a list of tags,
%   `Tags`, and extracts the content found within the specified structure.
%
%   @arg Tags A list of tag names defining the path to the target content.
%   @arg Element The XML element structure to search within.
%   @arg Result The extracted content.
%   @example Retrieve content with a tag path:
%     ?- get_content([cv, name], element(cv, [], [name=Value]), Value).
%
get_content([], R, R) :- 
    % Base case: return the content when there are no tags left to match.
    !.
get_content([S | Tags], L, R) :- 
    % Traverse through lists of elements to match the path defined by `Tags`.
    is_list(L),
    member(E, L),
    get_content([S | Tags], E, R), !.
get_content([S | Tags], element(S, _, L), R) :- 
    % When the current tag matches, continue with the next tags in the list.
    get_content(Tags, L, R), !.
get_content(STags, element(_, _, L), R) :- 
    % Continue searching within sub-elements if the current tag does not match.
    member(C, L),
    get_content(STags, C, R), !.

%!  get_element_value_each(+Element, -Output) is det.
%
%   Processes individual elements to retrieve or compute their values.
%
%   This predicate attempts to extract the value from an XML element. If the
%   element has sub-elements, it applies `try_begin/3` before retrieving the
%   final value.
%
%   @arg Element The XML element to process.
%   @arg Output The resulting value of the element.
%   @example Process and retrieve element value:
%     ?- get_element_value_each(element(tag, [], [value]), Out).
%
get_element_value_each(element(R, [], List), Out) :-
    % Check if the element has sub-elements and apply `try_begin/3`.
    \+ \+ member(element(_, _, _), List),
    try_begin(R, [], List),
    % Retrieve the value after any additional processing.
    get_element_value(element(R, [], List), Out).
get_element_value_each(R, Out) :- 
    % For atomic elements, directly retrieve the value.
    get_element_value(R, Out), !.

%!  get_element_value(+Element, -Value) is det.
%
%   Extracts the value from an XML element or list of elements.
%
%   This predicate retrieves the value of an element by converting its
%   content into a structured format, whether the input is a single element
%   or a list of elements.
%
%   @arg Element The XML element or list to extract the value from.
%   @arg Value The resulting value.
%   @example Retrieve value from an element:
%     ?- get_element_value(element(tag, [], [value]), Val).
%
get_element_value([L], R) :- 
    % If there is a single item list, process it as a single element.
    !, get_element_value(L, R).
get_element_value(element(T, [], [L]), T = R) :-  
    % Process single-element lists nested within a tag structure.
    get_element_value(L, R), !.
get_element_value(element(T, [], L), T = R) :- 
    % Process lists under a tag, converting each to a structured value.
    is_list(L), !, maplist(get_element_value, L, R).
get_element_value(L, V) :- 
    % If `L` is a list, apply `get_element_value` to each item.
    is_list(L), !, maplist(get_element_value, L, V).
get_element_value(L, v(L)).

%!  finish_feature_data is det.
%
%   Completes processing for all feature data entries.
%
%   This predicate iterates over all `feature_data/3` entries, applying
%   `process_feature_data/3` to each. It outputs a separator line after
%   completing all processing.
%
%   @example Complete feature data processing:
%     ?- finish_feature_data.
%
finish_feature_data :-
    % Process each `feature_data/3` entry in the knowledge base.
    forall(feature_data(Tag, Attr, Content),
           once(process_feature_data(Tag, Attr, Content))),
    % Print a separator to mark the end of processing.
    writeln('====================================').

%!  sub_prop(+Property) is nondet.
%
%   Defines specific sub-properties for use in XML processing.
%
%   This predicate specifies which properties are treated as sub-properties,
%   allowing special handling in element processing.
%
%   @arg Property The property name to check.
%   @example Define a sub-property:
%     ?- sub_prop(name).
%
sub_prop(name).
sub_prop(value).

%!  process_feature_data(+Tag, +Attributes, +Content) is det.
%
%   Processes and outputs feature data, printing the tag, attributes, and content.
%
%   This predicate prints the provided tag, attributes, and content using `print/1`
%   and inserts a brief pause (`sleep/1`) after each output, simulating processing
%   time for each data entry.
%
%   @arg Tag The tag name associated with the feature data.
%   @arg Attributes The attributes list associated with the feature.
%   @arg Content The content associated with the feature data.
%   @example Process feature data for a tag:
%     ?- process_feature_data('gene', [id=123], 'example_content').
%

/*
process_feature_data(featureprop, Attr, element(T,A,B)):-!,
  process_feature_data(T, A, B).
%process_feature_data(Tag, Attr, Content):- is_list(Content),!,
%  maplist(process_feature_data(_, Attr), Content).
process_feature_data(Tag, Attr, element(T,A,B)):- !,
   process_feature_data(T, Attr, B).
*//*
process_feature_data(Tag, Attr, element(cvterm,A,B)):- !, % sub_prop(T),
   append(Attr,A,AttrA),
   process_feature_data(Tag, AttrA, B).
process_feature_data(Tag, Attr, Content):- is_list(Content),
  member(element(_,_,_),Content),
  maplist(process_feature_data(Tag, Attr), Content).*/
process_feature_data(T, A, B):- print(tab(T,A,B)),nl,sleep(0.1).