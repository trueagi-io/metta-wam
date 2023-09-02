
get_key_and_rest(Line, Key, RestOfLine) :-
    atomic_list_concat([KeyPart|RestParts], ":", Line),
    normalize_space(atom(Key), KeyPart),
    atomic_list_concat(RestParts, ":", RestString),
    normalize_space(atom(RestOfLine), RestString).



get_some_items([H|T]) -->  get_one_item(H), get_some_items(T).
get_some_items([]).

get_one_item(bracketed(Items)) -->
    "[", whitespaces, items(Items), whitespaces, "]".

get_one_item(quoted(Item)) -->
    "\"", string_without_quotes(Chars), "\"",
    { string_chars(Item, Chars) }.

items([Item]) -->
    item(Item), !.

items([Item|Rest]) -->
    item(Item), whitespaces, ",", whitespaces, items(Rest).

item(Item) -->
    string_without_comma_or_bracket(Chars),
    { string_chars(Item, Chars) }.

string_without_comma_or_bracket([]) --> [].
string_without_comma_or_bracket([H|T]) -->
    [H],
    { \+ member(H, [',', '[', ']', '\"']) },
    string_without_comma_or_bracket(T).

string_without_quotes([]) --> [].
string_without_quotes([H|T]) -->
    [H],
    { H \= '\"' },
    string_without_quotes(T).

whitespaces --> [].
whitespaces --> [' '], whitespaces.


whitespaces --> [' '],!,whitespaces.
whitespaces --> [].


end_of_file.


:- dynamic term/2.
:- dynamic relationship/3.
:- dynamic obo_subset/2.
:- dynamic namespace/2.
:- dynamic comment/2.
:- dynamic alt_id/2.
:- dynamic is_obsolete/1.
:- dynamic intersection_of/3.
:- dynamic union_of/3.
:- dynamic consider/2.
:- dynamic replaced_by/2.
:- dynamic property_value/3.
:- dynamic dbxref/2.
:- dynamic ontology/1.
:- dynamic typedef/2.
:- dynamic instance/3.
:- dynamic complex_synonym/4.

load_obo(File) :-
    open(File, read, Stream),
    read_terms(Stream),
    close(Stream).


% Base case: If the string is empty, the trimmed version is also empty.
trim("", "").
% If the string starts with a whitespace character, remove it and recurse.
trim(String, Trimmed) :-
    atom_string(Atom, String),
    atom_concat(' ', Rest, Atom),
    atom_string(Rest, RestString),
    trim(RestString, Trimmed), !.
trim(String, Trimmed) :-
    atom_string(Atom, String),
    atom_concat('\t', Rest, Atom),
    atom_string(Rest, RestString),
    trim(RestString, Trimmed), !.

trim(String, Trimmed) :-
    atom_string(Atom, String),
    atom_concat('\n', Rest, Atom),
    atom_string(Rest, RestString),
    trim(RestString, Trimmed), !.

% If the string ends with a whitespace character, remove it and recurse.
trim(String, Trimmed) :-
    atom_string(Atom, String),
    atom_concat(Rest, ' ', Atom),
    atom_string(Rest, RestString),
    trim(RestString, Trimmed), !.

trim(String, Trimmed) :-
    atom_string(Atom, String),
    atom_concat(Rest, '\t', Atom),
    atom_string(Rest, RestString),
    trim(RestString, Trimmed), !.

trim(String, Trimmed) :-
    atom_string(Atom, String),
    atom_concat(Rest, '\n', Atom),
    atom_string(Rest, RestString),
    trim(RestString, Trimmed), !.
% If the string doesn't start or end with whitespace, it's already trimmed.
trim(String, String).


load_obo(File) :-
    open(File, read, Stream),
    read_terms(Stream),
    close(Stream).

read_terms(Stream) :-
    read_line_to_string(Stream, Line),
    ( Line = "[Term]" -> read_term(Stream); read_terms(Stream) ).

get_key_and_rest(Line, Key, Parts) :-
    atomic_list_concat([KeyPart|RestParts], ":", Line),
    normalize_space(atom(Key), KeyPart),
    atomic_list_concat(RestParts, ":", RestString),
    normalize_space(string(RestOfLine), RestString),
    read_parts(RestOfLine,Parts).

read_parts(RestOfLine,Parts):-
   r

read_term(Stream) :-
    read_term_details(Stream, _).

begin_read_header_details(Stream,Type):-
  read_line("id", IdString, Line),
  atom_string(Id, IdString),
  assert_isa(Id, Type),
  continue_id_details(Stream,Id).


continue_id_details(Stream,Id) :-
    read_line_to_string(Stream, Line),
    ( Line = "" -> read_term_details(Stream, Id)
    ; atomic_list_concat(['[',Term,']'],Line),
        begin_read_header_details(Stream,Type)
    ; split_line('id', IdString, Line) ->
        atom_string(Id, IdString),
        assert_relationship(Id, id_type, Term)
        continue_id_details(Stream,Id),
    ; split_line(Key, ValueString, Line) ->
        add_key_details(Id,Key,ValueString),
        continue_id_details(Stream,Id)).

split_line(Key, Value, Line) :-
    atomic_list_concat([Key, ':'' ValueC ], Line),

    sub_string(Line, Before, _, After, KeyWithColon),
    sub_string(Line, _, After, 0, ValueString),
    string_strip(ValueString, Value).

load_obo(File) :-
    open(File, read, Stream),
    read_terms(Stream),
    close(Stream).

read_terms(Stream) :-
    read_line_to_string(Stream, Line),
    ( Line = "[Term]" -> read_term(Stream); read_terms(Stream) ).

read_term(Stream) :-
    read_term_details(Stream, _, _).

read_term_details(Stream, Id, Name) :-
    read_line_to_string(Stream, Line),
   ( Line = "" -> read_term_details(Stream, Id, Name)
    ;Line = "[Term]" ->
        read_term(Stream) % start of new term
    ; split_line("id", IdString, Line) ->
        atom_string(IdNew, IdString),
        read_term_details(Stream, New, Name)
    ; split_line("name", NameString, Line) ->
        atom_string(Name, NameString),
        assert(term_id_name(Id, Name)),
        read_term_details(Stream, Id, Name)
    ; split_line("relationship", RelationshipString, Line) ->
        split_string(RelationshipString, " ", "", [Type, Target]),
        assert(relationship(Id, Type, Target)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "subset: ") ->
        atom_concat("subset: ", SubsetString, Line),
        assert(obo_subset(Id, SubsetString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "namespace: ") ->
        atom_concat("namespace: ", NamespaceString, Line),
        assert(namespace(Id, NamespaceString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "comment: ") ->
        atom_concat("comment: ", CommentString, Line),
        assert(comment(Id, CommentString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "alt_id: ") ->
        atom_concat("alt_id: ", AltIdString, Line),
        assert(alt_id(Id, AltIdString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "is_obsolete: ") ->
        atom_concat("is_obsolete: ", IsObsoleteString, Line),
        (IsObsoleteString = "true" -> assert(is_obsolete(Id)); true),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "intersection_of: ") ->
        atom_concat("intersection_of: ", IntersectionString, Line),
        split_string(IntersectionString, " ", "", [Relation, Target]),
        assert(intersection_of(Id, Relation, Target)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "union_of: ") ->
        atom_concat("union_of: ", UnionString, Line),
        split_string(UnionString, " ", "", [Relation, Target]),
        assert(union_of(Id, Relation, Target)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "consider: ") ->
        atom_concat("consider: ", ConsiderString, Line),
        assert(consider(Id, ConsiderString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "replaced_by: ") ->
        atom_concat("replaced_by: ", ReplacedByString, Line),
        assert(replaced_by(Id, ReplacedByString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "property_value: ") ->
        atom_concat("property_value: ", PropertyValueString, Line),
        split_string(PropertyValueString, " ", "", [Property, Value]),
        assert(property_value(Id, Property, Value)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "dbxref: ") ->
        atom_concat("dbxref: ", DbxrefString, Line),
        assert(dbxref(Id, DbxrefString)),
        read_term_details(Stream, Id, Name)
    ; Line = '[Ontology]' ->
        read_line_to_string(Stream, NextLine),
        assert(ontology(NextLine)),
        read_term_details(Stream, Id, Name)
    ; Line = '[Typedef]' ->
        read_typedef(Stream)
    ; Line = '[Instance]' ->
        read_instance(Stream)
    ; obo_prefix(Line, "synonym: ") ->
        parse_complex_synonym(Line, Synonym, Scope, Type, Dbxrefs),
        assert(complex_synonym(Id, Synonym, Scope, Dbxrefs)),
        read_term_details(Stream, Id, Name)
    ; Line = '[Term]' ->
        true
    ; read_term_details2(Stream, Id, Name)
    ).


read_term_details2(Stream, Id, Name) :-
    read_line_to_string(Stream, Line),
    ( obo_prefix(Line, "id: ") ->
        atom_concat("id: ", IdString, Line),
        atom_string(Id, IdString),
        read_term_details(Stream, _, Name)
    ; obo_prefix(Line, "name: ") ->
        atom_concat("name: ", NameString, Line),
        atom_string(Name, NameString),
        assert(term(Id, Name)),
        read_term_details(Stream, Id, _)
    ; obo_prefix(Line, "relationship: ") ->
        atom_concat("relationship: ", RelationshipString, Line),
        split_string(RelationshipString, " ", "", [Type, Target]),
        assert(relationship(Id, Type, Target)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "subset: ") ->
        atom_concat("subset: ", SubsetString, Line),
        assert(obo_subset(Id, SubsetString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "namespace: ") ->
        atom_concat("namespace: ", NamespaceString, Line),
        assert(namespace(Id, NamespaceString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "comment: ") ->
        atom_concat("comment: ", CommentString, Line),
        assert(comment(Id, CommentString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "alt_id: ") ->
        atom_concat("alt_id: ", AltIdString, Line),
        assert(alt_id(Id, AltIdString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "is_obsolete: ") ->
        atom_concat("is_obsolete: ", IsObsoleteString, Line),
        (IsObsoleteString = "true" -> assert(is_obsolete(Id)); true),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "intersection_of: ") ->
        atom_concat("intersection_of: ", IntersectionString, Line),
        split_string(IntersectionString, " ", "", [Relation, Target]),
        assert(intersection_of(Id, Relation, Target)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "union_of: ") ->
        atom_concat("union_of: ", UnionString, Line),
        split_string(UnionString, " ", "", [Relation, Target]),
        assert(union_of(Id, Relation, Target)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "consider: ") ->
        atom_concat("consider: ", ConsiderString, Line),
        assert(consider(Id, ConsiderString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "replaced_by: ") ->
        atom_concat("replaced_by: ", ReplacedByString, Line),
        assert(replaced_by(Id, ReplacedByString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "property_value: ") ->
        atom_concat("property_value: ", PropertyValueString, Line),
        split_string(PropertyValueString, " ", "", [Property, Value]),
        assert(property_value(Id, Property, Value)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "dbxref: ") ->
        atom_concat("dbxref: ", DbxrefString, Line),
        assert(dbxref(Id, DbxrefString)),
        read_term_details(Stream, Id, Name)
    ; obo_prefix(Line, "synonym: ") ->
        parse_complex_synonym(Line, Synonym, Scope, Type, Dbxrefs),
        assert(complex_synonym(Id, Synonym, Scope, Dbxrefs)),
        read_term_details(Stream, Id, Name)
    ; Line = '[Term]' ->
        true
    ; read_term_details(Stream, Id, Name)
    ).


replace_substring(Original, Search, Replace, Result) :-
    atomic_list_concat(Split, Search, Original),
    atomic_list_concat(Split, Replace, Result).

parse_complex_synonym(Line, Synonym, Scope, Type, Dbxrefs) :-
    atomic_list_concat([_, Temp, Rest], '"', Line),
    atomic_list_concat([ScopeAndType | DbxrefsList], "[", Rest),
    atomic_list_concat([ScopeStr, Type], " ", ScopeAndType),
    atomic_list_concat(FinalDbxrefs, "]", DbxrefsList),
    atom_string(Synonym, Temp),
    string_lower(ScopeStr, LowerScope),
    atom_string(Scope, LowerScope),
    Dbxrefs = FinalDbxrefs.

read_typedef(Stream) :-
    read_line_to_string(Stream, Line),
    ( obo_prefix(Line, "id: ") ->
        atom_concat("id: ", IdString, Line),
        atom_string(Id, IdString),
        read_typedef(Stream)
    ; obo_prefix(Line, "name: ") ->
        atom_concat("name: ", NameString, Line),
        atom_string(Name, NameString),
        assert(typedef(Id, Name)),
        read_typedef(Stream)
    ; Line = '[Typedef]' ->
        true
    ; read_typedef(Stream)
    ).

read_instance(Stream) :-
    read_line_to_string(Stream, Line),
    ( obo_prefix(Line, "id: ") ->
        atom_concat("id: ", IdString, Line),
        atom_string(Id, IdString),
        read_term_details(Stream, Id, _)
    ; obo_prefix(Line, "name: ") ->
        atom_concat("name: ", NameString, Line),
        atom_string(Name, NameString),
        assert(instance(Id, Name, _)),
        read_term_details(Stream, Id, Name)
    ; Line = '[Instance]' ->
        true
    ; read_term_details(Stream, Id, Name)
    ).

obo_prefix(Line, Prefix) :-
    sub_atom(Line, 0, _, _, Prefix).


% Additional predicates and helper functions

% This utility is used to check the next line without consuming it.
peek_line(Stream, Line) :-
    at_end_of_stream(Stream), !, fail;
    stream_property(Stream, position(Pos)),
    read_line_to_string(Stream, Line),
    set_stream_position(Stream, Pos).

% This predicate reads through the OBO file until it finds the next term or the end.
skip_until_next_term(Stream) :-
    peek_line(Stream, Line),
    ( Line = "[Term]" -> true;
      at_end_of_stream(Stream) -> true;
      read_line_to_string(Stream, _),
      skip_until_next_term(Stream) ).

% We'll also need a way to handle other stanzas we've not coded for.
% For now, it will just skip through the file until it finds the next [Term].
% This way, we won't crash if we encounter an unexpected stanza.
read_unknown_stanza(Stream) :-
    write('Encountered unknown stanza, skipping...'), nl,
    skip_until_next_term(Stream).

% Predicate to split string based on the delimiter and convert to atoms
split_string_to_atoms(String, Delimiter, Atoms) :-
    split_string(String, Delimiter, "", ListOfStrings),
    maplist(atom_string, Atoms, ListOfStrings).

% Add any additional utility predicates or refinements you think are necessary below this comment.


