/**
 * @module XML_DTD_Extractor
 * @author  
 * @summary
 * Module to extract the DTD file from an XML document using the sgml library.
 * It demonstrates the usage of SGML/XML parsing and manipulation in Prolog.
 *
 * @example 
 * % To extract a DTD file from an XML:
 * ?- extract_dtd_file('example.xml', DTDFileName).
 * 
 * @see
 * SGML library used for parsing: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/sgml.html%27)
 */

:- use_module(library(sgml)). % Import the SGML library to parse XML and handle DTDs
%:- use_module(library(logicmoo/xml_reader)). % Previously: Disabled custom XML reader library, no longer in use

/**
 * extract_dtd_file(+XMLFile:atom, -DTDFileName:atom) is det.
 *
 * Loads the XML file and extracts the associated DTD file name.
 * 
 * @param XMLFile The path to the XML file
 * @param DTDFileName The DTD file name extracted from the DOCTYPE declaration
 *
 * @example
 * ?- extract_dtd_file('file.xml', DTDFileName).
 * 
 * Reads the XML file, parses it, and retrieves the DTD information.
 */
 
extract_dtd_file(XMLFile, DTDFileName) :-
    open(XMLFile, read, Stream),  % Open the XML file for reading
    load_structure(Stream, _XML, [dialect(xml), doctype(Doctype)]),  % Load XML structure, capturing the DOCTYPE
    close(Stream),  % Close the file stream
    dtd_file_from_doctype(Doctype, DTDFileName).  % Extract DTD file name from the DOCTYPE declaration

/**
 * dtd_file_from_doctype(+Doctype:compound, -FileName:atom) is det.
 *
 * Extracts the DTD file name from the DOCTYPE declaration.
 * 
 * @param Doctype The DOCTYPE term from XML
 * @param FileName The DTD file name extracted from the DOCTYPE
 *
 * This helper predicate handles the parsing of the DOCTYPE declaration to find the external DTD reference.
 */
dtd_file_from_doctype(Doctype, FileName) :-
    nonvar(Doctype),  % Ensure the Doctype is not a variable (fully instantiated)
    Doctype = doctype(_Name, ExternalID),  % Decompose DOCTYPE term to get the external identifier
    extract_system_id(ExternalID, FileName).  % Extract the system identifier (which typically contains the DTD file name)

/**
 * extract_system_id(+ExternalID:compound, -FileName:atom) is det.
 *
 * Extracts the SYSTEM identifier from the external ID, which usually contains the DTD file name.
 * 
 * @param ExternalID External identifier from the DOCTYPE declaration
 * @param FileName Extracted file name from the SYSTEM identifier
 *
 * The SYSTEM identifier is part of the external ID, which holds the path or name of the DTD file.
 */
extract_system_id(ExternalID, FileName) :-
    nonvar(ExternalID),  % Ensure ExternalID is fully instantiated
    ExternalID = system(FileName).  % Extract the SYSTEM ID, which holds the file name

/*
load_dtd/2 predicate: Disabled due to redundancy and current focus on XML-based structures.
Previously: Loaded the DTD file directly and parsed it. 
This code has been commented out but preserved for future use if needed for direct DTD parsing.

Previously:
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

/**
 * @section Test Load Functions
 * Various functions to load large Chado XML datasets. These were written for processing specific XML files used in bioinformatics and genome data systems.
 *
 * lfb0, lfb1, lfb2, lfb3 are test cases that invoke loading and processing of different Chado XML files.
 */

/**
 * lfb0/0 is det.
 * 
 * Loads and prints a large XML file from a specific path.
 */
lfb0 :-
    fileToLineInfoElements(_Ctx, '/wam/data/FB_current/chado-xml/chado_FBim.xml', XML),  % Load specific XML file and extract line info elements
    writeln(XML),  % Write out the result to the console
    !.

/**
 * lfb1/0 is det.
 *
 * Test case to load a specific Chado XML file.
 */
lfb1 :-
    load_chado_xml('/wam/data/FB_current/chado-xml/chado_FBgn.xml').  % Load another specific XML file

/**
 * lfb2/0 is det.
 *
 * Test case to load another Chado XML file.
 */
lfb2 :-
    load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_gene_models.xml').  % Load gene model data

/**
 * lfb3/0 is det.
 *
 * Test case to load a Chado XML file containing predicted gene models.
 */
lfb3 :-
    load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_predicted.xml').  % Load predicted gene models

/**
 * load_chado_xml(+File:atom) is det.
 *
 * Parses a given Chado XML file and processes it.
 * 
 * @param File The XML file to load
 * 
 * This predicate opens the specified XML file and uses the sgml library to parse it. It also defines hooks for begin and end events during parsing.
 */
load_chado_xml(File) :-
    open(File, read, In),  % Open the file for reading
    new_sgml_parser(Parser, []),  % Create a new SGML parser instance
    set_sgml_parser(Parser, file(File)),  % Set the file to be parsed
    set_sgml_parser(Parser, dialect(xml)),  % Specify XML dialect
    set_sgml_parser(Parser, space(remove)),  % Configure parser to remove extra spaces
    sgml_parse(Parser,  % Parse the XML file with event hooks
               [ source(In),
                 call(begin, on_begin),  % Hook for start tags
                 call(end, on_end)  % Hook for end tags
               ]),
    close(In).  % Close the file after parsing

:- dynamic(feature_data/3).  % Declare dynamic predicate for storing feature data

/**
 * on_end(+Tag:atom, +Attributes:list) is det.
 *
 * Handles end tag event for 'feature'. Processes the feature data when a 'feature' element ends.
 * 
 * @param Tag The XML tag that ended
 * @param Attributes Attributes associated with the tag
 *
 * When a 'feature' element ends, it finishes processing the feature data and clears the data storage.
 */
on_end('feature', _) :-  
    !,  % Cut to prevent backtracking once this condition is satisfied
    finish_feature_data,  % Finalize and process the collected feature data
    !,
    listing(feature_data(_,_,_)),  % List all feature_data facts for inspection
    retractall(feature_data(_,_,_)),  % Clear the stored feature data
    sleep(0.1),  % Pause briefly to simulate processing time
    !.

% Previously: A generic tag handler was commented out because feature-specific handling was introduced.
%on_end(Tag, _Parser):- current_tag(Is), Is = Tag, !, pop_tag(Tag), finish_tag(Tag).
/* File Directive Explanation
   No file directives present, but if encountered, they would typically include module declarations or imports. */

% This predicate is triggered at the end of parsing. It takes two arguments, but doesn't do anything specific here.
% @param _ Ignored parameter
% @param _ Ignored parameter
on_end(_, _).

% Predicate for handling the beginning of a parsing event when the tag is 'chado'.
% It succeeds immediately, using the cut operator (!) to prevent backtracking.
% @param 'chado' A tag to handle
% @param _ Unused attribute
% @param _ Unused parser
on_begin('chado', _, _) :- !.

% Predicate for handling the beginning of a parsing event when the tag is 'feature'.
% Succeeds immediately as well, cutting to avoid further processing.
% @param 'feature' The feature tag
% @param _ Unused attribute
% @param _Parser Unused parser
on_begin('feature', _, _Parser) :- !.

% The following line of code is commented out. It was likely intended to push a tag into a stack and then fail.
% However, this logic has been skipped. Possibly redundant logic or replaced by more refined handling.
% previously: 
% %on_begin(Tag, _Attr, _Parser):- push_tag(Tag),fail.

% Predicate handling the beginning of a parsing event for general tags.
% Reads the element content and tries to store the feature. If it fails, resets the parser's position.
% @param Tag The tag to handle
% @param Attr The attributes associated with the tag
% @param Parser The parser in use
on_begin(Tag, Attr, Parser):- 
    read_element(Parser, Content, Reset), !,  % Read the element content, reset if necessary
    (store_feature(Tag, Attr, Content) -> true  % Try to store the feature data
    ; (set_sgml_parser(Parser, position(Reset)), fail)).  % Reset parser if storage fails

% If storing the feature fails, try alternative logic with `try_begin`. Resets parser if it also fails.
% @param Tag The tag to handle
% @param Attr The attributes associated with the tag
% @param Parser The parser in use
on_begin(Tag, Attr, Parser):- 
    read_element(Parser, Content, Reset), !,  % Read the element content, reset if necessary
    (try_begin(Tag, Attr, Content) -> true  % Try alternative processing
    ; (set_sgml_parser(Parser, position(Reset)), fail)).  % Reset parser if it fails

% General case for handling the start of an element that doesn't match prior conditions.
% Prints the tag and its content.
% @param Any Any tag name
% @param _ Unused attribute
% @param Parser The parser in use
on_begin(Any, _, Parser) :- 
    read_element(Parser, Content, _), nl,  % Read element content
    print(Any=Content), nl.  % Print tag and its content

% Another case of `on_begin` to parse an SGML document.
% Asserts and prints the feature data extracted from the tag, attributes, and content.
% @param Tag The tag name
% @param Attr The attributes associated with the tag
% @param Parser The SGML parser
on_begin(Tag, Attr, Parser) :-
    sgml_parse(Parser, [document(Content), parse(content)]),  % Parse the SGML document
    FD = feature_data(Tag, Attr, Content),  % Create feature data term
    print(FD), nl,  % Print feature data
    assertz(FD).  % Assert feature data

% Returns the current tag at the top of the stack.
% If no stack exists, defaults to an empty list.
% @param Tag The current tag to return
current_tag(Tag):- 
    once(clause(current_tag_stack(Was), true, _Ref); Was=[]),  % Check if tag stack exists
    append(_New, [Tag], Was), !.  % Append the new tag to the stack

% Fallback for when there is no current tag.
current_tag(none).

% Returns the parent tag, the second item from the tag stack.
% Defaults to an empty list if no stack exists.
% @param Tag The parent tag to return
parent_tag(Tag):- 
    once(clause(current_tag_stack(Was), true, _Ref); Was=[]),  % Check if parent tag exists
    append(_New, [Tag, _], Was), !.  % Append the new tag to the stack

% Fallback for when there is no parent tag.
parent_tag(none).

% Pops the top tag from the tag stack.
% If the stack exists, it removes the topmost tag and updates the stack.
% @param Tag The tag to pop from the stack
pop_tag(Tag):- 
    once(clause(current_tag_stack(Was), true, Ref); Was=[]),  % Check if tag stack exists
    append(New, [Tag], Was),  % Append new tag
    it_t(nonvar(Ref), erase(Ref)),  % Erase the reference if valid
    assert(current_tag_stack(New)), !.  % Update the tag stack

% Pushes a new tag onto the tag stack.
% If the stack exists, the new tag is appended to it.
% @param Tag The tag to push onto the stack
push_tag(Tag):- 
    once(retract(current_tag_stack(Was)); Was=[]),  % Retrieve the current tag stack
    append(Was, [Tag], New),  % Append the new tag
    assert(current_tag_stack(New)).  % Update the tag stack

% Placeholder predicate that doesn't perform any actions when finishing a tag.
% @param _Tag The tag being finished
finish_tag(_Tag).

% Reads an element from the parser and allows peeking at it without consuming it.
% @param Parser The parser in use
% @param Content The content of the element
peek_element(Parser, Content):- 
    call_cleanup(read_element(Parser, Content, Pos),  % Read the element content
    set_sgml_parser(Parser, position(Pos))).  % Restore the parser's position

% Reads an element from the parser, capturing the content and position.
% This predicate parses the document content using the SGML parser.
% @param Parser The parser in use
% @param Content The parsed content
% @param Pos The position of the stream
read_element(Parser, Content, Pos):- 
    get_sgml_parser(Parser, source(S)),  % Get the source stream of the parser
    stream_property(S, position(Pos)),  % Get the current position in the stream
    sgml_parse(Parser, [document(Content), parse(content)]), !.  % Parse the content

% Attempts to begin processing an element of type `element/3`.
% Merges attributes and recursively calls `try_begin`.
% @param Tag The tag to process
% @param Attr The attributes associated with the tag
% @param element(T, A, L) The element structure
try_begin(Tag, Attr, element(T, A, L)):- !, 
    append(Attr, A, AttrA),  % Append attributes
    try_begin(Tag=T, AttrA, L).  % Recur with new attributes

% Handles the case where the content is a list and processes type IDs.
% @param Tag The tag to process
% @param Attr The attributes associated with the tag
% @param List The list of elements
try_begin(Tag, Attr, List):- 
    is_list(List),  % Check if content is a list
    absorb_type_ids(Tag, Attr, List), !.  % Process type IDs if present

% Process feature data if no special cases apply.
% @param Tag The tag to process
% @param Attr The attributes associated with the tag
% @param V The value associated with the tag
try_begin(Tag, Attr, V):- 
    process_feature_data(Tag, Attr, V).

% Handles absorbing type IDs from elements.
% This version extracts the type and name and processes each value.
% @param Tag The tag to process
% @param Attr The attributes associated with the tag
% @param Elements The elements to process
absorb_type_ids(Tag, Attr, Elements):- 
    select(element(type_id, [], C), Elements, Rest),  % Extract the type ID element
    get_content([cv, name], C, TypeName), !,  % Get the content of the type ID
    must_det_ll((
        get_content([cvterm, name], C, Name),  % Get the cvterm name
        maplist(get_element_value_each, Rest, Values),  % Extract values
        maplist(process_feature_data(ntv(Tag, TypeName, Name), Attr), Values))), !.  % Process the data

% Skips over type IDs and processes name-value pairs.
% @param _Tag The tag to process
% @param Attr The attributes associated with the tag
% @param Elements The elements to process
absorb_type_ids(_Tag, Attr, Elements):- 
    select(element(type_id, [], C), Elements, Rest),  % Extract the type ID element
    must_det_ll((
        get_content([cvterm, name], C, Name),  % Get the cvterm name
        maplist(get_element_value_each, Rest, Values),  % Extract values
        maplist(process_feature_data(nv(Name), Attr), Values))), !.  % Process the data

% Stores a feature by converting an element into a term and asserting it as feature data.
% @param Tag The tag name
% @param Attr The attributes associated with the tag
% @param Content The content of the element
store_feature(Tag, Attr, Content):- 
    cvt_element(element(Tag, Attr, Content), Val),  % Convert the element to a term
    assert(feature_data(Tag, Attr, Val)).  % Assert the feature data

% Skips over certain tags. This predicate is likely used to ignore tags such as 'cvterm', 'cv', and 'pub'.
% @param cvterm The tag to skip
skip_over(cvterm).
skip_over(cv).
skip_over(pub).


% File directive: This section might define directives or file-related instructions (none present in this snippet)

% Predicate to skip over certain elements based on their type
% @param featureprop The element that will be skipped
skip_over_s(featureprop).

% Another predicate to skip over elements, in this case, featureprop_pub
% @param featureprop_pub Another element that will be skipped
skip_over_s(featureprop_pub).

% Predicate to check if the current element E is part of a predefined list of items to be skipped
% @param E Element to check
% @example skip_over_s(library_id). would succeed because library_id is in the list
skip_over_s(E):-
    member(E,[dbxref_id, dbxref, db_id, library_id, library, library_feature]).

% A general predicate that checks if an element needs to be skipped by consulting the skip_over/1 predicate
% @param X Element to check
skip_over_s(X):- 
    skip_over(X).

% Predicate to convert elements in a list to a specific value
% @param List A list of elements
% @param Val The value to which the elements in the list will be converted
cvt_element(List,Val):- 
    is_list(List), !, 
    maplist(cvt_element, List, Val).

% Convert an element while skipping certain terms and recursively converting inner structures
% @param element(Tag,[],[element(CVTerm,[],L)]) XML-like structure
% @param TagVal Result after conversion
cvt_element(element(Tag,[],[element(CVTerm,[],L)]), TagVal):- 
    skip_over_s(CVTerm), !, 
    cvt_element(element(Tag,[],L), TagVal).

% Predicate to handle an atomic value nested within an element tag
% @param element(CVTerm,[],[Atomic]) Element to process
% @param Val Result after conversion
cvt_element(element(CVTerm,[],[Atomic]),Val):- 
    skip_over_s(CVTerm), !, 
    cvt_element(Atomic, Val).

% Another rule for converting element structures that skips over terms
% @param element(CVTerm,[],Atomic) XML structure with atomic value
% @param Val Result after conversion
cvt_element(element(CVTerm,[],Atomic),Val):- 
    skip_over(CVTerm), !, 
    cvt_element(Atomic, Val).

% Convert an element with a substructure (T,A,L)
% @param element(Tag,[],[element(T,A,L)]) Structure to process
% @param Tag=Val Result after conversion
cvt_element(element(Tag,[],[element(T,A,L)]),Tag=Val):- 
    !, cvt_element(element(T,A,L),Val).

% Base case: atomic element conversion
% @param element(Tag,[],[Atomic]) Structure to process
% @param Tag=Atomic Result after conversion
cvt_element(element(Tag,[],[Atomic]),Tag=Atomic):- !.

% Recursive case for lists within elements
% @param element(Tag,[],List) Element to process
% @param Tag=Val Result after processing the list
cvt_element(element(Tag,[],List),Tag=Val):- 
    !, cvt_element(List,Val).

% Fallback: No conversion needed, Val remains the same
% @param Val Atomic value
cvt_element(Val,Val).

% Predicate to accumulate content from a list
% @param [] Empty list case
% @param R Result
get_content([],R,R):- !.

% Predicate to get content from tags and lists
% @param [S|Tags] Tag list
% @param L List of elements to process
% @param R Result
get_content([S|Tags],L,R):- 
    is_list(L), 
    member(E,L), 
    get_content([S|Tags],E,R), !.

% Processing XML elements, getting content recursively
% @param [S|Tags] List of tags to process
% @param element(S,_,L) XML structure to process
% @param R Result after recursive processing
get_content([S|Tags],element(S,_,L),R):- 
    get_content(Tags,L,R), !.

% Another rule for processing elements where a match is found in the member list
% @param STags Tags to process
% @param element(_,_,L) Element to process
% @param R Result after processing
get_content(STags,element(_,_,L),R):- 
    member(C,L), 
    get_content(STags,C,R), !.

% Predicate to extract value from an element with nested structures
% @param element(R,[],List) Element to process
% @param Out Result after processing
get_element_value_each(element(R,[],List),Out):- 
    \+ \+ member(element(_,_,_),List),
    try_begin(R,[],List),
    get_element_value(element(R,[],List),Out).

% Base case for getting element value
% @param R Element to process
% @param Out Result after processing
get_element_value_each(R,Out):- 
    get_element_value(R,Out), !.

% Recursive case for getting element value
% @param [L] List of elements
% @param R Result
get_element_value([L],R):- 
    !, get_element_value(L,R).

% Recursive case for converting an XML structure to a key-value pair
% @param element(T,[],[L]) XML structure
% @param T=R Resulting key-value pair
get_element_value(element(T,[],[L]),T=R):-  
    get_element_value(L,R), !.

% Recursively process a list of elements
% @param element(T,[],L) XML structure
% @param T=R Resulting key-value pairs
get_element_value(element(T,[],L),T=R):- 
    is_list(L), !, 
    maplist(get_element_value,L,R).

% Process a list of elements and extract values
% @param L List of elements
% @param V Resulting values
get_element_value(L,V):- 
    is_list(L), !, 
    maplist(get_element_value,L,V).

% Fallback case: wrap the value inside a v/1 term
% @param L Value
% @param v(L) Wrapped value
get_element_value(L,v(L)).

% Predicate to finish processing all feature data by calling process_feature_data/3 on each
% @example finish_feature_data.
finish_feature_data:-
    forall(feature_data(Tag, Attr, Content),
        once(process_feature_data(Tag, Attr, Content))),
    writeln('====================================').

% Predicate that marks certain properties for processing
% @param name A property to process
sub_prop(name).

% Another property to process
% @param value A property to process
sub_prop(value).


%process_feature_data(_,_,_).
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




process_feature_data(T, A, B):- 
    print(tab(T,A,B)), nl, sleep(0.1).
