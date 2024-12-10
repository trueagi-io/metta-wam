/* ./tests/compiler_specific/transpiler/match_feval.metta */
/* 2024-07-27T14:10:50+0000 */
:- style_check(-discontiguous).
:- dynamic((r_match_feval_1722089450_8934128)/2).
:- dynamic((r_match_feval_1722089450_8934128)/3).
:- dynamic((r_match_feval_1722089450_8934128)/4).
:- dynamic((r_match_feval_1722089450_8934128)/5).
:- dynamic((r_match_feval_1722089450_8934128)/6).
:- dynamic((r_match_feval_1722089450_8934128)/7).
:- dynamic((r_match_feval_1722089450_8934128_iz)/4).
:- dynamic((r_match_feval_1722089450_8934128_iz)/5).
:- dynamic((r_match_feval_1722089450_8934128_iz)/6).
:- dynamic((r_match_feval_1722089450_8934128_iz)/7).
:- dynamic((r_match_feval_1722089450_8934128_iz)/8).
:- dynamic(user:asserted_metta_pred/2).
:- multifile(user:asserted_metta_pred/2).
user:asserted_metta_pred(r_match_feval_1722089450_8934128,'./tests/compiler_specific/transpiler/match_feval.metta').
r_match_feval_1722089450_8934128(A,[:,B,[C,D]]):-r_match_feval_1722089450_8934128_iz(A,B,C,D).
r_match_feval_1722089450_8934128(A,[:,B,[C,D,E]]):-r_match_feval_1722089450_8934128_iz(A,B,C,D,E).
r_match_feval_1722089450_8934128(A,[:,B,[C,D,E,F]]):-r_match_feval_1722089450_8934128_iz(A,B,C,D,E,F).
r_match_feval_1722089450_8934128(A,[:,B,[C,D,E,F,G]]):-r_match_feval_1722089450_8934128_iz(A,B,C,D,E,F,G).
r_match_feval_1722089450_8934128(A,[:,B,[C,D,E,F,G,H]]):-r_match_feval_1722089450_8934128_iz(A,B,C,D,E,F,G,H).
r_match_feval_1722089450_8934128(1,=,t(f,_x),t(*,2,_x)).
:-eval_Line([match,'&self',[=,[f,_x],_y],_y],r_match_feval_1722089450_8934128,1).



t(f, _x, A):-
   A #= 2 * _x.

