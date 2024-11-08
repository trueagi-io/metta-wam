/***********************************************************
		DOMAIN DEFINITIONS
***********************************************************/

DOMAINS
  EXPR            = assigment3(EXPR,EXPR,EXPR);
		    equality3(EXPR,EXPR,EXPR);
		    assigment2(EXPR,EXPR);
		    equality2(EXPR,EXPR);
		    assigment1(EXPR);
		    equality1(EXPR);
		    parenthetical(EXPR);
		    variabel(STRING)


  TOK		  = colon();
		    equalsign();
		    lpar();
		    rpar();
		    variabel(STRING);
		    nill


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
  s_expr(TOKL,TOKL,EXPR)

CLAUSES
  s_expr([t(colon,_)|LL1],LL0,assigment3(EXPR,EXPR1,EXPR2)):-
	s_expr(LL1,LL2,EXPR),
	s_expr(LL2,LL3,EXPR1),
	s_expr(LL3,LL0,EXPR2),!.
  s_expr([t(equalsign,_)|LL1],LL0,equality3(EXPR,EXPR1,EXPR2)):-
	s_expr(LL1,LL2,EXPR),
	s_expr(LL2,LL3,EXPR1),
	s_expr(LL3,LL0,EXPR2),!.
  s_expr([t(colon,_)|LL1],LL0,assigment2(EXPR,EXPR1)):-
	s_expr(LL1,LL2,EXPR),
	s_expr(LL2,LL0,EXPR1),!.
  s_expr([t(equalsign,_)|LL1],LL0,equality2(EXPR,EXPR1)):-
	s_expr(LL1,LL2,EXPR),
	s_expr(LL2,LL0,EXPR1),!.
  s_expr([t(colon,_)|LL1],LL0,assigment1(EXPR)):-!,
	s_expr(LL1,LL0,EXPR).
  s_expr([t(equalsign,_)|LL1],LL0,equality1(EXPR)):-!,
	s_expr(LL1,LL0,EXPR).
  s_expr([t(lpar,_)|LL1],LL0,parenthetical(EXPR)):-!,
	s_expr(LL1,LL2,EXPR),
	expect(t(rpar,_),LL2,LL0).
  s_expr([t(variabel(STRING),_)|LL],LL,variabel(STRING)):-!.
  s_expr(LL,_,_):-syntax_error(expr,LL),fail.


	    
		    


CLAUSES

  expect(TOK, [TOK|L],L).

  syntax_error(_, _).


DOMAINS
  NUMBER_OF_EXTRA_CHARACTERS 	= INTEGER
  NUMBER_OF_SPACES		= INTEGER

PREDICATES
  is_a_space(CHAR)
  scan(CURSORq, SOURCE, TOKL)
  skip_spaces(SOURCE, SOURCE, NUMBER_OF_SPACES)
  string_tokenq(STRING, TOK)
  get_fronttoken( string, string, string) - ( i,o,o)
  
CLAUSES
  is_a_space(' ').	
  is_a_space('\t').	
  is_a_space('\n').

 get_fronttoken( SOURCE, Bg, Res2 ):-
     fronttoken( SOURCE, FRONTTOKEN, REST),   FRONTTOKEN = "\"", !,
	 searchstring( REST, "\"", Pos ), P2 = Pos - 1, frontstr( P2, REST, Bg, Res ),
	 frontstr( 1, Res, _, Res2 ).
 get_fronttoken( SOURCE, FRONTTOKEN, REST ):- fronttoken( SOURCE, FRONTTOKEN, REST), !.
	 

% arrange for the Quote here  
  scan(STARTING_POSITION, SOURCE, [t(TOKEN, LOCATION_OF_TOKEN)|TAIL]) :-
	skip_spaces(SOURCE, NEW_SOURCE, NUMBER_OF_SPACES),
	LOCATION_OF_TOKEN = STARTING_POSITION + NUMBER_OF_SPACES,
	get_fronttoken(NEW_SOURCE, FRONTTOKEN, REST),	!,

	LOWER_CASE_FRONTTOKEN = FRONTTOKEN,
	
	string_tokenq(LOWER_CASE_FRONTTOKEN, TOKEN),
	str_len(FRONTTOKEN, LENGTH_OF_FRONTTOKEN),
	NEW_STARTING_POSITION = LOCATION_OF_TOKEN + LENGTH_OF_FRONTTOKEN,
	scan(NEW_STARTING_POSITION, REST, TAIL).
  scan(_, _, []).

  skip_spaces(SOURCE, NEW_SOURCE, NUMBER_OF_SPACES) :-
	frontchar(SOURCE, CHAR, SOURCE1),
	is_a_space(CHAR),
	!,
	skip_spaces(SOURCE1, NEW_SOURCE, NUMBER_OF_SPACES_IN_SOURCE1),
	NUMBER_OF_SPACES = NUMBER_OF_SPACES_IN_SOURCE1 + 1.
  skip_spaces(SOURCE, SOURCE, 0).


% Only let uncommented hier the ones that are being used in the
% chosen grammar-file 

  string_tokenq(":", 	colon ) :- !.
  string_tokenq("=", 	equalsign ) :- !.
  string_tokenq("(", 	lpar) :- !.
  string_tokenq(")",	rpar) :- !.
% OPTIONAL
%  string_tokenq(";", 	semicolon ) :- !.
%  string_tokenq("?", 	interrogation ) :- !.
%  string_tokenq("\"",	quote ) :- !.
  string_tokenq(STRING, 	variabel(STRING)) :-  !.


% everything for calculations_simple.grm
%  string_tokenq("(", 	lpar) :- !.
%  string_tokenq(")",	rpar) :- !.
%  string_tokenq("/",	div) :- !.
%  string_tokenq("-", 	minus) :- !.
%  string_tokenq("+",	plus) :- !.
%  string_tokenq("*", 	mult) :- !.
%  string_tokenq(STRING, 	number(INTEGER)) :- str_int(STRING, INTEGER), !.
%  string_tokenq(STRING, 	number(REAL)) :- str_real(STRING, REAL), !.
%  string_tokenq(STRING, 	variabel(STRING)) :-  !.
% OPTIONAL
      %  string_tokenq("^", 	power) :- !.
      %  string_tokenq("$", 	expo) :- !.
      %  string_tokenq("#", 	log) :- !.
      %  string_tokenq("@", 	lgn ) :- !.


% DEZE toevoegen zodra je een grammar gebruikt  waarin deze voorkomen 

  


PREDICATES

parse( TOKL, EXPR )
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

tokenize(EXPR, TOKENS) :- scan(0, EXPR, TOKENS).
 
 
% deze moet ook dynamisch 
% parse(TOKENS, TERM) :-   	s_sexp(TOKENS, UNUSED_TOKENS, TERM),  	UNUSED_TOKENS = [].



%include "hlptopic.con"
%include "t8w.con"


  parse(TOKENS, TERM) :-   	s_expr(TOKENS, UNUSED_TOKENS, TERM),  	UNUSED_TOKENS = []. 
