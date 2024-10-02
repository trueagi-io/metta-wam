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