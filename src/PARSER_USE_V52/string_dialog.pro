/*****************************************************************************

		Copyright (c) Singularity

 Project:  PARSE_USE
 FileName: STRING_DIALOG.PRO
 Purpose: No description
 Written by: R. van Vessum
 Comments:
******************************************************************************/

include "parse_use.inc"
include "parse_use.con"
include "hlptopic.con"

%BEGIN_DLG strdia
/**************************************************************************
	Creation and event handling for dialog: strdia
**************************************************************************/

constants

%BEGIN strdia, CreateParms, 11:57:15-6.8.2024, Code automatically updated!
  dlg_strdia_ResID = idd_strdia
  dlg_strdia_DlgType = wd_Modeless
  dlg_strdia_Help = idh_contents
%END strdia, CreateParms


predicates

filter_out_comments( string, string ) - ( i,  o )

clauses

filter_out_comments( AA, Result ):- 
     searchstring( AA, ";" , Pos ), P2 = Pos - 1, frontstr( P2, AA, Bg, Res ),
	 searchstring( Res, "\n" , Pos3 ),
	 P32 = Pos3 - 1,
     frontstr( P32, Res, _Bg3, Res3 ), !,
	 concat ( Bg,  Res3, C2 ),
    filter_out_comments( C2, Result ),  !.

filter_out_comments( Resu, Resu ):- !.

predicates

  dlg_strdia_eh : EHANDLER

clauses

  dlg_strdia_Create(Parent):-
	win_CreateResDialog(Parent,dlg_strdia_DlgType,dlg_strdia_ResID,dlg_strdia_eh,0).

%BEGIN strdia, idc_ok _CtlInfo
  dlg_strdia_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END strdia, idc_ok _CtlInfo
%MARK strdia, new events

%BEGIN strdia, idc_parse _CtlInfo
dlg_strdia_eh( _Win , e_Control( idc_parse ,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
     retractall( pred_level( _, _, _ ) ),    

	  retractall( is_transp( _, _, _, _, _, _) ),
    retractall( term_parsed( _, _ ) ), 
    CtrlWin = win_GetCtlHandle( _Win, idc_strdia_1 ), OutString0 = win_GetText( CtrlWin ),
    retractall( lasts( _ ) ),  assert( lasts( OutString0 ) ),
    filter_out_comments( OutString0, OutString ), 
	dlg_Note( "start Parsing0" , OutString ),
    tokenize( OutString , TOKENS ),
    term_str( TOKL , TOKENS , Res ), 
    % write( Res , "\n\n" ),  
    dlg_Note("start Parsing", Res ),
%    parse( TOKENS, TERMX ) ,
    parse_report( TOKENS, TERMX, Rest_tokens ) ,
    
	% term_str( EXPR ,TERMX, Res2 ),
    %term_str( TOKL , Rest_tokens, Res33 ),
%    write( "% ", OutString , "\n" ),    
    
    % write( " ", Res , "\n\n\n" ), 
    assert( term_parsed( 1, Termx ) ), 
	% write( Res2 , "\n" ),
    %format( Sxz , "% \n--Rest--\n %", Res2, Res33 ), 
    parse_rest( Rest_tokens, 2 ),
    % cb_PutString( Res2 ),
    display_results(),

    % dlg_Note("RESULT Parsing", Sxz ),
%    assert( pred_level( 0, 1, TERMX ) ), 
%    pprint( TERMX , 1, Result_count ),
%    format( Qwa, " level reached: % ", Result_count ), write( "\n", Qwa , "\n" ),
%    Rsfile = "results_from_parsing.pro",    append_to_total_resultas( Rsfile, OutString, Res, Res2 ),
%    write( "\nwritten to file : ", Rsfile , "\n" ),   
%    Functs_count0 = 0,
%    interpret_predicate_levels( Functs_count0, Result_0_clause ),
%    term_str( EXPR, Result_0_clause, Sx44 ),   write( "\n 0 intrepreted to : ",  Sx44 , "\n" ),  
%    retractall( pred_level( 0, _, _ ) ),    assert( pred_level( 0, 1, Result_0_clause ) ),   
%    New_level_count = Result_count + 1,
%    pprint( Result_0_clause , New_level_count , Result_count_2 ),
%    format( Qwa2, " level reached: % ", Result_count_2 ), write( "\n", Qwa2 , "\n" ),
%    interpret_predicate_levels( New_level_count, Result_2_clause ),
%    term_str( EXPR, Result_2_clause, Sx55 ),   write( "\n 2 intrepreted to : ",  Sx55 ),  
    !.

%END strdia, idc_parse _CtlInfo

%BEGIN strdia, e_Create
  dlg_strdia_eh(_Win,e_Create(_CreationData),0):-!,

	Font = font_Create(ff_helvetica, [], 14),
	
	CtrlWin = win_GetCtlHandle(_Win, idc_strdia_1),	win_SetFont(CtrlWin, Font),
		CtrlWin2 = win_GetCtlHandle(_Win, idc_strdia_2),	win_SetFont(CtrlWin2, Font),
		
 We = "(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs)  \n(if  \n   (and  ",
 We2 = " \n    (conditional_probability_consistency $As $Bs $ABs)      \n ",
 We3 = " (conditional_probability_consistency $Bs $Cs $BCs))\n     \n(if (< 0.99 $Bs)\n  ", 
 We4 = " \n      $Cs\n  \n      (+ (* $ABs $BCs) (/ (* (- 1 $ABs) (- $Cs (* $Bs $BCs))) (- 1 $Bs))  )   ) ",
 We5 = " \n  \n   0))		",
		format( Zq, "% % % % %", We, We2, We3, We4, We5 ),
				      get_last_string( Zq, Last ),
%		      get_last_string( "( : Human Type )", Last ),
		      
	win_SetText(CtrlWin, Last),
	
	!.
%END strdia, e_Create

  dlg_strdia_eh(_,_,_):-!,fail.

%END_DLG strdia

