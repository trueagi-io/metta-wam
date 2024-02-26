:- dynamic ngram/6.
:- multifile ngram/6.
:- module_transparent ngram/6.

ngram(Loc, A, oc(X), B, C, NN) :-
    nonvar(X),
    ngram(Loc, _, _, A, oc(X), _),
    ngram(_ULoc, oc(X), B, C, _, NN).
ngram(A, B, C, oc(D), E, F) :-
    nonvar(D),
    ngram(A, _, B, C, oc(D), _),
    ngram(_, oc(D), E, _, _, F).
%FileTestCase	error	=	Unhandled exception: must_sanity:e(error(existence_error(url,http://localhost:4090/?properties=%7B%22annotators%22%3A%22quote,tokenize,ssplit,pos,depparse,ner,parse,coref,mwt,natlog,udfeats,relation,lemma,docdate,entitylink,openie,truecase,kbp,gender,cleanxml,dcoref%22,%22outputFormat%22%3A%22json%22%7D),context(_950,status(500,Internal Server Error))),parser_stanford:get_post_reply(http://localhost:4090,/?properties=%7B%22annotators%22%3A%22quote,tokenize,ssplit,pos,depparse,ner,parse,coref,mwt,natlog,udfeats,relation,lemma,docdate,entitylink,openie,truecase,kbp,gender,cleanxml,dcoref%22,%22outputFormat%22%3A%22json%22%7D,I think he 's a better director than actor personally.,_974)) 
%FileTestCase	error	=	Unterminated conditional compilation from training_terms.pl:328 

