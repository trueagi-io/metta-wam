/* tests/compiler_specific/transpiler/identity.metta */
/* 2024-07-29T07:56:26+0000 */
:- style_check(-discontiguous). 
:- dynamic((piler_identity_1722239786_638979)/2). 
:- dynamic((piler_identity_1722239786_638979)/3). 
:- dynamic((piler_identity_1722239786_638979)/4). 
:- dynamic((piler_identity_1722239786_638979)/5). 
:- dynamic((piler_identity_1722239786_638979)/6). 
:- dynamic((piler_identity_1722239786_638979)/7). 
:- dynamic((piler_identity_1722239786_638979_iz)/4). 
:- dynamic((piler_identity_1722239786_638979_iz)/5). 
:- dynamic((piler_identity_1722239786_638979_iz)/6). 
:- dynamic((piler_identity_1722239786_638979_iz)/7). 
:- dynamic((piler_identity_1722239786_638979_iz)/8). 
:- dynamic(user:asserted_metta_pred/2).
:- multifile(user:asserted_metta_pred/2).
user:asserted_metta_pred(piler_identity_1722239786_638979,'tests/compiler_specific/transpiler/identity.metta'). 
piler_identity_1722239786_638979(A,[:,B,[C,D]]):-piler_identity_1722239786_638979_iz(A,B,C,D).
piler_identity_1722239786_638979(A,[:,B,[C,D,E]]):-piler_identity_1722239786_638979_iz(A,B,C,D,E).
piler_identity_1722239786_638979(A,[:,B,[C,D,E,F]]):-piler_identity_1722239786_638979_iz(A,B,C,D,E,F).
piler_identity_1722239786_638979(A,[:,B,[C,D,E,F,G]]):-piler_identity_1722239786_638979_iz(A,B,C,D,E,F,G).
piler_identity_1722239786_638979(A,[:,B,[C,D,E,F,G,H]]):-piler_identity_1722239786_638979_iz(A,B,C,D,E,F,G,H).
piler_identity_1722239786_638979(1,=,t(f,_x),t(*,_x,_x)).
:-eval_Line([f,1],piler_identity_1722239786_638979,1).

