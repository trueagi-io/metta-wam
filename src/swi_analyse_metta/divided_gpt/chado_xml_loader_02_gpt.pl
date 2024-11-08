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

