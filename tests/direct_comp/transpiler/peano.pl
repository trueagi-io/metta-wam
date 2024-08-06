/* ./tests/compiler_specific/transpiler/peano.metta */
/* 2024-07-27T14:09:32+0000 */
:- style_check(-discontiguous). 
:- dynamic((nspiler_peano_1722089372_5160968)/2). 
:- dynamic((nspiler_peano_1722089372_5160968)/3). 
:- dynamic((nspiler_peano_1722089372_5160968)/4). 
:- dynamic((nspiler_peano_1722089372_5160968)/5). 
:- dynamic((nspiler_peano_1722089372_5160968)/6). 
:- dynamic((nspiler_peano_1722089372_5160968)/7). 
:- dynamic((nspiler_peano_1722089372_5160968_iz)/4). 
:- dynamic((nspiler_peano_1722089372_5160968_iz)/5). 
:- dynamic((nspiler_peano_1722089372_5160968_iz)/6). 
:- dynamic((nspiler_peano_1722089372_5160968_iz)/7). 
:- dynamic((nspiler_peano_1722089372_5160968_iz)/8). 
:- dynamic(user:asserted_metta_pred/2).
:- multifile(user:asserted_metta_pred/2).
user:asserted_metta_pred(nspiler_peano_1722089372_5160968,'./tests/compiler_specific/transpiler/peano.metta'). 
nspiler_peano_1722089372_5160968(A,[:,B,[C,D]]):-nspiler_peano_1722089372_5160968_iz(A,B,C,D).
nspiler_peano_1722089372_5160968(A,[:,B,[C,D,E]]):-nspiler_peano_1722089372_5160968_iz(A,B,C,D,E).
nspiler_peano_1722089372_5160968(A,[:,B,[C,D,E,F]]):-nspiler_peano_1722089372_5160968_iz(A,B,C,D,E,F).
nspiler_peano_1722089372_5160968(A,[:,B,[C,D,E,F,G]]):-nspiler_peano_1722089372_5160968_iz(A,B,C,D,E,F,G).
nspiler_peano_1722089372_5160968(A,[:,B,[C,D,E,F,G,H]]):-nspiler_peano_1722089372_5160968_iz(A,B,C,D,E,F,G,H).
nspiler_peano_1722089372_5160968(1,=,t('Add',_x,'Z'),_x).
nspiler_peano_1722089372_5160968(1,=,t('Add',_x,t('S',_y)),t('Add',t('S',_x),_y)).
:-eval_Line(['Add',['S',['S','Z']],['S',['S',['S','Z']]]],nspiler_peano_1722089372_5160968,2).
