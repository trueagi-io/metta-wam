/***********************************************************
		DOMAIN DEFINITIONS
***********************************************************/

DOMAINS
  ATOM            = atom(STRING)

  SEXP            = sexp(ATOM);
		    sexp_list(SEXP_LIST)

  SEXP_LIST       = SEXP*

  SEXP_LIST_EXP   = sexp_list_exp(SEXP_LIST)

  SEXP_STRING     = sexp_string(ATOM)


  TOK		  = name(STRING);
		    lpar();
		    rpar();
		    quote();
		    nill;
                    variabel(STRING)

DOMAINS

CURSORq	= INTEGER
CURSORTOq 	= t(TOK, CURSORq )
MESSAGE	= STRING
RESULT	= REAL
SOURCE	= STRING
TOKL 		= CURSORTOq*


PREDICATES
expect(CURSORTOq, TOKL, TOKL)
syntax_error(MESSAGE, TOKL)


/***********************************************************
		PARSING PREDICATES
***********************************************************/

PREDICATES
  s_sexp_list(TOKL,TOKL,SEXP_LIST)
  s_sexp_list1(TOKL,TOKL,SEXP_LIST)
  s_atom(TOKL,TOKL,ATOM)
  s_sexp(TOKL,TOKL,SEXP)
  s_sexp_list_exp(TOKL,TOKL,SEXP_LIST_EXP)
  s_sexp_string(TOKL,TOKL,SEXP_STRING)

CLAUSES
  s_atom([t(name(STRING),_)|LL],LL,atom(STRING)):-!.
  s_atom(LL,_,_):-syntax_error(atom,LL),fail.

  s_sexp(LL1,LL0,sexp(ATOM)):-
	s_atom(LL1,LL0,ATOM),!.
  s_sexp(LL1,LL0,sexp_list(SEXP_LIST)):-
	s_sexp_list(LL1,LL0,SEXP_LIST),!.

  s_sexp_list_exp([t(lpar,_)|LL1],LL0,sexp_list_exp(SEXP_LIST)):-!,
	s_sexp_list(LL1,LL2,SEXP_LIST),
	expect(t(rpar,_),LL2,LL0).
  s_sexp_list_exp(LL,_,_):-syntax_error(sexp_list_exp,LL),fail.

  s_sexp_string([t(quote,_)|LL1],LL0,sexp_string(ATOM)):-!,
	s_atom(LL1,LL2,ATOM),
	expect(t(quote,_),LL2,LL0).
  s_sexp_string(LL,_,_):-syntax_error(sexp_string,LL),fail.

  s_sexp_list(LL1,LL0,[SEXP|SEXP_LIST]):-
	s_sexp(LL1,LL2,SEXP),
	s_sexp_list1(LL2,LL0,SEXP_LIST).

  s_sexp_list1(LL1,LL0,[SEXP|SEXP_LIST]):-
	s_sexp(LL1,LL2,SEXP),!,
	s_sexp_list1(LL2,LL0,SEXP_LIST).
  s_sexp_list1(LL,LL,[]).


	    
		    


CLAUSES

  expect(TOK, [TOK|L],L).

  syntax_error(_, _).


DOMAINS
  NUMBER_OF_EXTRA_CHARACTERS 	= INTEGER
  NUMBER_OF_SPACES		= INTEGER

PREDICATES
  is_a_space(CHAR)
  scan(CURSORq, SOURCE, TOKL) - ( i,i,o)
  skip_spaces(SOURCE, SOURCE, NUMBER_OF_SPACES, NUMBER_OF_SPACES ) - ( i,o,i,o)
  string_tokenq(STRING, TOK)
  
CLAUSES
  is_a_space(' ').	
  is_a_space('\t').	
  is_a_space('\n').

  scan(STARTING_POSITION, SOURCE, [t(TOKEN, LOCATION_OF_TOKEN)|TAIL]) :-

	skip_spaces(SOURCE, NEW_SOURCE, 0, NUMBER_OF_SPACES),
%	 format( Xd , "-%s-%s-%-%-", SOURCE, NEW_SOURCE, STARTING_POSITION, NUMBER_OF_SPACES ),
	% dlg_Note( "cur sorce ", Xd ), 
  	str_len( NEW_SOURCE , Lex ), Lex > 0,	!,
	LOCATION_OF_TOKEN = STARTING_POSITION + NUMBER_OF_SPACES,
	fronttoken(NEW_SOURCE, FRONTTOKEN, REST),
	!,

	LOWER_CASE_FRONTTOKEN = FRONTTOKEN,
	
	string_tokenq(LOWER_CASE_FRONTTOKEN, TOKEN),
	str_len(FRONTTOKEN, LENGTH_OF_FRONTTOKEN),
	NEW_STARTING_POSITION = LOCATION_OF_TOKEN + LENGTH_OF_FRONTTOKEN,
	scan(NEW_STARTING_POSITION, REST, TAIL).
  scan(_, _, []).



%  skip_spaces(SOURCE, NEW_SOURCE, NUMBER_OF_SPACES) :-
%	frontchar(SOURCE, CHAR, SOURCE1 ),
%	is_a_space( CHAR ), ! ,
%	skip_spaces( SOURCE1, NEW_SOURCE, NUMBER_OF_SPACES_IN_SOURCE1 ),
%	NUMBER_OF_SPACES = NUMBER_OF_SPACES_IN_SOURCE1 + 1.
%  skip_spaces(SOURCE, SOURCE, 0).

  skip_spaces(SOURCE, NEW_SOURCE, NUMBER_OF_SPACES, Res) :-
	trap( frontchar(SOURCE, CHAR, SOURCE1 ), _, fail ),
	is_a_space( CHAR ), ! ,
     	 NUMBER_OF_SPACES_IN_SOURCE1 = NUMBER_OF_SPACES + 1,
	skip_spaces( SOURCE1, NEW_SOURCE, NUMBER_OF_SPACES_IN_SOURCE1, Res ).

  skip_spaces(V, V, Resx, Resx ):-!.


%  string_tokenq("/",	div) :- !.
  string_tokenq("(", 	lpar) :- !.
%  string_tokenq("-", 	minus) :- !.
%  string_tokenq("*", 	mult) :- !.
%  string_tokenq(STRING, 	number(INTEGER)) :- str_int(STRING, INTEGER), !.
%  string_tokenq(STRING, 	number(REAL)) :- str_real(STRING, REAL), !.
%  string_tokenq("+",	plus) :- !.
%  string_tokenq("^", 	power) :- !.

%  string_tokenq("$", 	expo) :- !.
%  string_tokenq("#", 	log) :- !.
%  string_tokenq("@", 	lgn ) :- !.
  string_tokenq(")",	rpar) :- !.
  string_tokenq("\"",	quote ) :- !.
  string_tokenq(STRING, 	variabel(STRING)) :-  !.
  


PREDICATES

parse( TOKL, SEXP )
tokenize( SOURCE, TOKL )


CLAUSES
  
  
%  evaluate_expression3(EXPR,R2) :-
    % write(EXPR),
    %  dlg_Note(EXPR),
    % trap(term_str(SOURCE,TT,EXPR),_,fail),!,
	% dlg_Note(EXPR),
%    trap(evaluate_expression(EXPR,R2),_,fail).
%  evaluate_expression( EXPR, R2 ):-
%  	tokenize( EXPR, TOKENS ),	parse( TOKENS, TERM ),  	!,
%  	calculate2( TERM, R2 ).
%  evaluate_expression( A, A ) :- !.

 
parse(TOKENS, TERM) :-   	s_sexp(TOKENS, UNUSED_TOKENS, TERM),  	UNUSED_TOKENS = [].

tokenize(EXPR, TOKENS) :- scan(0, EXPR, TOKENS).

%include "hlptopic.con"
%include "t8w.con"

