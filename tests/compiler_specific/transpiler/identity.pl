/* tests/baseline_compat/metta-morph_tests/identity.metta */
/* 2024-07-27T06:44:56+0000 */
:- style_check(-discontiguous). 
:- dynamic((ests_identity_1722062696_9417486)/2). 
:- dynamic((ests_identity_1722062696_9417486)/3). 
:- dynamic((ests_identity_1722062696_9417486)/4). 
:- dynamic((ests_identity_1722062696_9417486)/5). 
:- dynamic((ests_identity_1722062696_9417486)/6). 
:- dynamic((ests_identity_1722062696_9417486)/7). 
:- dynamic((ests_identity_1722062696_9417486_iz)/4). 
:- dynamic((ests_identity_1722062696_9417486_iz)/5). 
:- dynamic((ests_identity_1722062696_9417486_iz)/6). 
:- dynamic((ests_identity_1722062696_9417486_iz)/7). 
:- dynamic((ests_identity_1722062696_9417486_iz)/8). 
:- dynamic(user:asserted_metta_pred/2).
:- multifile(user:asserted_metta_pred/2).
user:asserted_metta_pred(ests_identity_1722062696_9417486,'tests/baseline_compat/metta-morph_tests/identity.metta'). 
ests_identity_1722062696_9417486(A,[:,B,[C,D]]):-ests_identity_1722062696_9417486_iz(A,B,C,D).
ests_identity_1722062696_9417486(A,[:,B,[C,D,E]]):-ests_identity_1722062696_9417486_iz(A,B,C,D,E).
ests_identity_1722062696_9417486(A,[:,B,[C,D,E,F]]):-ests_identity_1722062696_9417486_iz(A,B,C,D,E,F).
ests_identity_1722062696_9417486(A,[:,B,[C,D,E,F,G]]):-ests_identity_1722062696_9417486_iz(A,B,C,D,E,F,G).
ests_identity_1722062696_9417486(A,[:,B,[C,D,E,F,G,H]]):-ests_identity_1722062696_9417486_iz(A,B,C,D,E,F,G,H).
ests_identity_1722062696_9417486(1,=,t(f,_x),t(*,_x,_x)).
:-eval_Line([f,1],ests_identity_1722062696_9417486,1).
