
% ```prolog

:- use_module(library(logicmoo_utils)).


% facts for creating predcatres
extracted_predicate(transposon, [primaryId, symbol, sequence, url, taxonId, soTermId]).
extracted_predicate(transposon_publications, [primaryId, publications]).
extracted_predicate(transposon_synonyms, [primaryId, symbolSynonyms]).
extracted_predicate(transposon_cross_references, [primaryId, crossReferenceIds]).

extracted_predicate(gene, [primaryId,
  gene_geneId, gene_symbol, gene_url, gene_locusTag, gene_name]).


extracted_predicate(N,[gene_geneId,N]):-
  member(M,[symbol, url, locusTag, name, synonyms]), atom_concat('gene_',M,N).


extracted_predicate(exon_locations,
 [primaryId,
   genomeLocations_assembly,
   genomeLocations_gca_accession,
   exons_INSDC_accession, exons_chromosome, exons_strand,
   exons_startPosition,
   exons_endPosition]).

extracted_predicate(relatedSequences,[primaryId,relatedSequences_sequenceId,
   relatedSequences_relationship]).

extracted_predicate(gene_synonyms, [gene_geneId, gene_synonyms]).
%extracted_predicate(exon, [primaryId, geneId, 'INSDC_accession', chromosome, strand, startPosition, endPosition]).
extracted_predicate(metadata, [dataProvider, schemaVersion, release, genomicCoordinateSystem, dateProduced]).
%extracted_predicate(exon_locations, [ assembly, chromosome, strand, startPosition, endPosition]).

extracted_predicate(N,[fbid,M]):-
  member(M,[transposons, common_terms, major_stages, major_tissues, name,
           pubs, rex_gene, stocks, expression_desc_text, images]), atom_concat('allele_',M,N).
extracted_predicate(allele_image,[fbid,images,images_imageDescription,
            images_publicationId, images_pubFigure, images_permission]).
extracted_predicate(allele_image2,
  [fbid,images,
            imageDescription, publicationId,
            pubFigure, permission]).
extracted_predicate(allele_image3,
  [fbid,    images_imageDescription,
            images_publicationId,
            images_pubFigure, images_permission]).

wdmsg_json(G):- nop(fbug(G)).
note_doing(P):- wdmsg_json(P),!,call(user:P).
assert_JSON(P):- note_doing(assert_OBO(P)).

:- ensure_loaded(flybase_main).
:- ensure_loaded(flybase_obo).

%:- listing(assert_OBO/1).

:- dynamic json_kv/2.

% Base case: an empty curly structure.
mapcurly_or_list(_, {}):-!.
mapcurly_or_list(_, []):-!.
mapcurly_or_list(Pred, [H|T])  :- !, call(Pred, H),   mapcurly_or_list(Pred, T).
mapcurly_or_list(Pred, {H,T}):- !, call(Pred, H),   mapcurly_or_list(Pred, T).
mapcurly_or_list(Pred, {H})  :- !, call(Pred, H).


simple_first(R,_=AA,_=BB):-!, simple_first(R,AA,BB).
simple_first('<',AA,BB):- BB=json(_),AA\=json(_),!.
simple_first('>',AA,BB):- AA=json(_),BB\=json(_),!.
simple_first(R,AA,BB):-!, compare(R,AA,BB).

prefix_key([O|_],Kee,Key):- atom(O), !,
  prefix_key(O,Kee,Key).
prefix_key(O,Kee,Key) :- atom(O),
  O\==Kee, O\==data,symbolic_list_concat([O,'_',Kee],Key),!.
prefix_key(_,Key,Key).



:- use_module(library(http/json)).

load_flybase_json(_Fn,File):-
  process_json_file(File).

process_json_file(File):- atom_concat(File,'.metta_x',MXFile),process_json_file(File,MXFile).
process_json_file(_File,MXFile):- fail, exists_file(MXFile),!,process_metta_x_file(MXFile).
process_json_file(File, MXFile):- fail, exists_file(File),!,
          setup_call_cleanup(
             open(MXFile,write,Strm,[]),
             setup_call_cleanup(
                    set_stream(Strm,alias(metta_x_output)),
                    with_option(make_metta_x,'True',process_json_file_direct(File)),
                    set_stream(current_output,alias(metta_x_output))),
             close(Strm)),
       remove_duplicates(MXFile),
       process_metta_x_file(MXFile).
process_json_file(File, _):- process_json_file_direct(File),!.
process_json_file(File, MXFile):-
    throw(process_json_file(File, MXFile)).


process_json_file_direct(File):-
    setup_call_cleanup(
               open(File, read, Stream,[encoding(utf8)]),
               json_read(Stream, JSONDict),
               close(Stream)),
    process_json([],JSONDict).

process_json(JsonString):- process_json([],JsonString),!.




process_json(O,JsonString) :- atomic(JsonString), !,
    atom_json_term(JsonString, Json, []), process_json(O,Json).
process_json(O,json(Values)) :- !, process_json(O,Values).
process_json(O,K=json(Values)) :- !, process_json([K|O],Values).
process_json(O,Values) :- is_list(Values),!,maplist(with_json1(O),Values).
process_json(O,Values) :- with_json1(O,Values),!.

with_json1(O,K=Values) :- K==driver,!, with_json1(O,Values).
with_json1(O,K=Values) :-!, with_json1([K|O],Values).
with_json1(O,Values) :- is_list(Values),!,maplist(with_json1(O),Values).

with_json1(O,json([driver=json(Values0)])) :- !,with_json2(O,Values0),!.
with_json1(O,json(Values0)) :- is_list(Values0),!,with_json2(O,Values0),!.
with_json1(O,Val):- fbug(error(O=Val)),!.

with_json2([metaData],_Values0):-!.
with_json2(O,Values):-
  retractall(seen_arg(_,_)),
  with_json3(O,Values),
  ignore((seen_arg(_,_),
  fbug(Values),
  listing(seen_arg/2))).

with_json3(O,Values0):-
   retractall(json_kv(_,_)),
   with_json4(O,Values0),
   retractall(json_kv(_,_)),!.

with_json4(O,[json(Values)]):- !, with_json4(O,Values).
with_json4(O,json(Values)):- !, with_json4(O,Values).
with_json4(O,Values0):-
   predsort(simple_first,Values0,Values),
   wdmsg_json(O==Values),
   ignore(maplist(with_entry(O,assert),Values)).


with_entry(O,AR, Key=Value):-!, with_kv([Key|O],AR,Key,Value).
%with_entry(O,assert,JSON) :- !, process_json(O,JSON).
with_entry(O,AR,JSON):- fbug(error_with_entry(O,AR,JSON)).

uses_id_subprops(images).
key_can_nv(M):-
member(M,[major_stages, major_tissues, name, rex_gene, insertions,transposons,
 %expression_desc_text, images,
           pubs,  stocks]).

is_field(Field):- extracted_predicate(_,List), \+ \+ member(Field,List),!.

with_kv_maybe_more(_O,_AR,_Key,json([])):-!.
with_kv_maybe_more(O,AR,Key,Do):- with_kv(O,AR,Key,Do),!.


assert_id_about(O,Key,ID,NVAboutID):-
  with_json4([Key|O],[Key=ID|NVAboutID]).

  %images= json( [ 'FBal0040476_1.jpg'= json( [ imageDescription='GAL4[Bx-MS1096].jpg',

with_kv(O,AR,Key,json([ID=json(NVAboutID)|More])):- uses_id_subprops(Key),
   %wdmsg_json(cr1(Key)=ID),
   atom(ID),!,
   decl_type(ID,Key),
   with_kv(O,AR,Key,ID),
   assert_id_about(O,Key,ID,NVAboutID),
   with_kv_maybe_more(O,AR,Key,json(More)).

with_kv(O,AR,Key,json([ID=Value|More])):- key_can_nv(Key),
   atom(Value),
  % prefix_key(O,ID,Field), \+ is_field(Field),!,
   %prefix_key(O,Value,VField), \+ is_field(VField),!,
   decl_type(ID,Key),
   with_kv(O,AR,Key,ID),
   %atom_concat(Key,'_name',Pred),
   %Pred = object_name,
   assert_JSON([name,ID,Value]),
   with_kv_maybe_more(O,AR,Key,json(More)).

with_kv(O,AR,OK,Key=Values):- !, with_kv([OK|O],AR,Key,Values).
with_kv(O,AR,Key,json(Values)):- !, with_kv(O,AR,Key,Values).
with_kv(O,AR,Key,Value):- is_list(Value),Value\==[],!,
  maplist(with_kv(O,AR,Key),Value).
with_kv(O,AR,Kee,Value):-
    prefix_key(O,Kee,Key),
    retractall(json_kv(Key,_)),
    KV = json_kv(Key,Value),
    decl_seen(Value,Key),
    Do =.. [AR,KV],
    call(Do),
    ignore((AR==assert,
    %wdmsg_json(cr(Key)=Value),
    check_ready(Key))).

check_ready(Key):-
    forall((extracted_predicate(P,List),memberchk(Key,List)),
      (length(List,Len),
       ignore((findall(Arg,(member(K,List),json_kv(K,Arg)),ArgList),
       length(ArgList,Len),
       Fact = [P|ArgList],
       assert_JSON(Fact),
       maplist(decl_type,ArgList,List))))).
% Rows 937,381,148
:- dynamic(arg_typed/2).
:- dynamic(seen_arg/2).

decl_type(Arg,Type):- retractall(seen_arg(Arg,_)),arg_typed(Arg,Type),wdmsg_json(arg_typed(Arg,Type)),!.
decl_type(Arg,Type):- assert(arg_typed(Arg,Type)),!,assert_JSON([Type,Arg]).
decl_seen(Arg,_):- seen_arg(Arg,_),!.
decl_seen(Arg,_):- arg_typed(Arg,_),!.
decl_seen(Arg,Type):- assert(seen_arg(Arg,Type)),!.




err
:- process_json(json([metaData= json( [ dataProvider='FlyBase',
                    publications=['PMID:35266522'], schemaVersion='0.4.0',release=fb_2023_04,
                    genomicCoordinateSystem='1-start, fully-closed',
                    dateProduced='2023-07-25T03:20:14+00:00']),
  data= [ json( [ primaryId='FBtr0070001',
                  symbol='tRNA:Pro-CGG-1-1-RA',
                  sequence='GGCTCGTTGGTCTAGGGGTATGATTCTCGCTTCGGGTGCGAGAGGTCCCGGGTTCAAATCCCGGACGAGCCC',
                  url='http://flybase.org/reports/FBtr0070001.html',
                  symbolSynonyms=['CR32826-RA','tRNA:CR32826-RA'], taxonId='NCBITaxon:7227',soTermId='SO:0000253',
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(+),
                                                              startPosition=20025099,endPosition=20025170])]])],
                  gene= json( [ geneId='FBgn0052826',
                                symbol='tRNA:Pro-CGG-1-1',
                                url='http://flybase.org/reports/FBgn0052826.html',
                                locusTag='Dmel_CR32826',
                                synonyms=['tRNA:P:CGG:AE002611'],
                                name='transfer RNA:Proline-CGG 1-1']),
                  publications=['PMID:26673694']]),
          json( [ primaryId='FBtr0070292',
                  symbol='snoRNA:M-RA',
                  sequence='AATTCAATGATTTCAACTTATTCTAATACACAC',
                  url='http://flybase.org/reports/FBtr0070292.html', taxonId='NCBITaxon:7227',soTermId='SO:0000275',
                  crossReferenceIds=['REFSEQ:NR_002093.1'],
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(-),
                                                              startPosition=1482492,endPosition=1482590])]])],
                  gene= json( [ geneId='FBgn0044508',
                                symbol='snoRNA:M',
                                url='http://flybase.org/reports/FBgn0044508.html',
                                locusTag='Dmel_CR32807', synonyms=['CR32807'],name='snoRNA:M'])]),
          json( [ primaryId='FBtr0308931',
                  symbol='lncRNA:CR33218-RC',
                  sequence='ACGAAATCAATAAACATTTGTACCTTT',
                  url='http://flybase.org/reports/FBtr0308931.html',
                  symbolSynonyms=['CR33218-RC'], taxonId='NCBITaxon:7227',soTermId='SO:0001877',
                  crossReferenceIds=['REFSEQ:NR_047742.1'],
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(+),
                                                              startPosition=2330159,endPosition=2330355]),
                                                      json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(+),
                                                              startPosition=2330413,endPosition=2330826])]])],
                  gene= json( [ geneId='FBgn0053218',
                                symbol='lncRNA:CR33218',
                                url='http://flybase.org/reports/FBgn0053218.html',
                                locusTag='Dmel_CR33218',
                                synonyms=['CG2854','CG33218','CR33218','CT9762'],
                                name='long non-coding RNA:CR33218'])]),
          json( [ primaryId='FBtr0070634', symbol='lncRNA:roX1-RA',sequence='TTGTAGAACAATTACTATA',
                  url='http://flybase.org/reports/FBtr0070634.html',
                  symbolSynonyms=['roX1-RA','roX1-RA....'], taxonId='NCBITaxon:7227',soTermId='SO:0001877',
                  crossReferenceIds=['REFSEQ:NR_002098.2'],
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(-),
                                                              startPosition=3858940,endPosition=3862697])]])],
                  gene= json( [ geneId='FBgn0019661',
                                symbol='lncRNA:roX1',
                                url='http://flybase.org/reports/FBgn0019661.html',
                                locusTag='Dmel_CR32777',
                                synonyms= [ 'BcDNA:GH10432', 'CR32777','EG:EG0002.2','RoX1',
                                            'chrX:3706836..3706970',rox1],
                                name='long non-coding RNA on the X 1']),
                  publications=['PMID:10445033','PMID:12446910','PMID:9038336','PMID:9038337']])]   ])).




err
:- process_json([],'
{
     "metaData": {
          "dataProvider": "FlyBase",
          "publications": [
               "PMID:35266522"
          ],
          "schemaVersion": "0.4.0",
          "release": "fb_2023_04",
          "genomicCoordinateSystem": "1-start, fully-closed",
          "dateProduced": "2023-07-25T03:20:14+00:00"
     },
     "data": [
          {
               "primaryId": "FBtr0070001",
               "symbol": "tRNA:Pro-CGG-1-1-RA",
               "sequence": "GGCTCGTTGGTCTAGGGGTATGATTCTCGCTTCGGGTGCGAGAGGTCCCGGGTTCAAATCCCGGACGAGCCC",
               "url": "http://flybase.org/reports/FBtr0070001.html",
               "symbolSynonyms": [
                    "CR32826-RA",
                    "tRNA:CR32826-RA"
               ],
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0000253",
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "+",
                                   "startPosition": 20025099,
                                   "endPosition": 20025170
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0052826",
                    "symbol": "tRNA:Pro-CGG-1-1",
                    "url": "http://flybase.org/reports/FBgn0052826.html",
                    "locusTag": "Dmel_CR32826",
                    "synonyms": [

                         "tRNA:P:CGG:AE002611"
                    ],
                    "name": "transfer RNA:Proline-CGG 1-1"
               },
               "publications": [
                    "PMID:26673694"
               ]
          },
          {
               "primaryId": "FBtr0070292",
               "symbol": "snoRNA:M-RA",
               "sequence": "AATTCAATGATTTCAACTTATTCTAATACACAC",
               "url": "http://flybase.org/reports/FBtr0070292.html",
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0000275",
               "crossReferenceIds": [
                    "REFSEQ:NR_002093.1"
               ],
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "-",
                                   "startPosition": 1482492,
                                   "endPosition": 1482590
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0044508",
                    "symbol": "snoRNA:M",
                    "url": "http://flybase.org/reports/FBgn0044508.html",
                    "locusTag": "Dmel_CR32807",
                    "synonyms": [
                         "CR32807"
                    ],
                    "name": "snoRNA:M"
               }
          },
          {
               "primaryId": "FBtr0308931",
               "symbol": "lncRNA:CR33218-RC",
               "sequence": "ACGAAATCAATAAACATTTGTACCTTT",
               "url": "http://flybase.org/reports/FBtr0308931.html",
               "symbolSynonyms": [
                    "CR33218-RC"
               ],
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0001877",
               "crossReferenceIds": [
                    "REFSEQ:NR_047742.1"
               ],
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "+",
                                   "startPosition": 2330159,
                                   "endPosition": 2330355
                              },
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "+",
                                   "startPosition": 2330413,
                                   "endPosition": 2330826
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0053218",
                    "symbol": "lncRNA:CR33218",
                    "url": "http://flybase.org/reports/FBgn0053218.html",
                    "locusTag": "Dmel_CR33218",
                    "synonyms": [
                         "CG2854",
                         "CG33218",
                         "CR33218",
                         "CT9762"
                    ],
                    "name": "long non-coding RNA:CR33218"
               }
          },
          {
               "primaryId": "FBtr0070634",
               "symbol": "lncRNA:roX1-RA",
               "sequence": "TTGTAGAACAATTACTATA",
               "url": "http://flybase.org/reports/FBtr0070634.html",
               "symbolSynonyms": [
                    "roX1-RA"
               ],
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0001877",
               "crossReferenceIds": [
                    "REFSEQ:NR_002098.2"
               ],
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "-",
                                   "startPosition": 3858940,
                                   "endPosition": 3862697
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0019661",
                    "symbol": "lncRNA:roX1",
                    "url": "http://flybase.org/reports/FBgn0019661.html",
                    "locusTag": "Dmel_CR32777",
                    "synonyms": [
                         "BcDNA:GH10432",
                         "CR32777",
                         "EG:EG0002.2",
                         "RoX1",
                         "chrX:3706836..3706970",
                         "rox1"
                    ],
                    "name": "long non-coding RNA on the X 1"
               },
               "publications": [
                    "PMID:10445033",
                    "PMID:12446910",
                    "PMID:9038336",
                    "PMID:9038337"
               ]
          }
     ]
  }
}').

err
:- process_json([],'
{
  "metaData": {
    "dataProvider": "FlyBase",
    "title": "Frequently Used GAL4 Table",
    "dateProduced": "2023-07-24T23:20:12-04:00",
    "databaseRelease": "2023_04",
    "annotationRelease": "R6.53"
  },
  "data": [

   {"driver": {
        "name": "Scer\\GAL4<up>dpp.blk1</up>",
        "fbid": "FBal0040480",
        "images": {
          "FBal0040480_1.png": {
            "imageDescription": "GAL4[dpp.blk1].png",
            "publicationId": "FBrf0218242",
            "pubFigure": "Figure 5A",
            "permission": ""
          }
        },
        "pubs": {
          "FBrf0074522": "Staehling-Hampton et al., 1994, Cell Growth Diffn 5(6): 585--593",
          "FBrf0076140": "Wilder and Perrimon, 1995, Development 121(2): 477--488",
          "FBrf0084454": "Treisman and Rubin, 1995, Development 121(11): 3519--3527",
          "FBrf0086426": "Grimm and Pflugfelder, 1996, Science 271(5255): 1601--1604",
          "FBrf0087557": "Lecuit et al., 1996, Nature 381(6581): 387--393",
          "FBrf0087626": "Nellen et al., 1996, Cell 85(3): 357--368",
          "FBrf0087630": "Ng et al., 1996, Nature 381(6580): 316--318",
          "FBrf0088035": "Burke and Basler, 1996, Development 122(7): 2261--2269",
          "FBrf0088295": "Kim et al., 1996, Nature 382(6587): 133--138",
          "FBrf0089604": "Brook and Cohen, 1996, Science 273(5280): 1373--1377",
          "FBrf0089753": "Morimura et al., 1996, Dev. Biol. 177(1): 136--151",
          "FBrf0091093": "Johnston and Schubiger, 1996, Development 122(11): 3519--3529",
          "FBrf0091111": "Leevers et al., 1996, EMBO J. 15(23): 6584--6594",
          "FBrf0091167": "Shen and Mardon, 1997, Development 124(1): 45--52",
          "FBrf0091298": "Gonzalez-Crespo and Morata, 1996, Development 122(12): 3921--3928",
          "FBrf0091412": "Theisen et al., 1996, Development 122(12): 3939--3948",
          "FBrf0092493": "Chanut and Heberlein, 1997, Development 124(2): 559--567",
          "FBrf0092641": "Neumann and Cohen, 1997, Development 124(4): 871--880",
          "FBrf0093060": "Aplin and Kaufman, 1997, Mech. Dev. 62(1): 51--60",
          "FBrf0251844": "Matsuda et al., 2021, Nat. Commun. 12(1): 6435",
          "FBrf0252066": "Kinsey et al., 2021, G3 (Bethesda) 11(12): jkab350",
          "FBrf0253453": "Lu et al., 2022, Int. J. Mol. Sci. 23(9): 4543",
          "FBrf0253792": "Akiyama et al., 2022, Dev. Biol. 488: 91--103",
          "FBrf0255082": "Inoshita et al., 2022, iScience 25(12): 105476",
          "FBrf0255366": "Bharti et al., 2023, Proc. Natl. Acad. Sci. U.S.A. 120(2): e2211189119",
          "FBrf0256654": "He et al., 2023, Development 150(11): dev201297"
        },
        "rex_gene": {
          "FBgn0000490": "dpp"
        },
        "common_terms": "A/P boundary",
        "major_stages": {
          "FBdv00005336": "larval stage"
        },
        "major_tissues": {
          "FBbt00111520": "anterior-posterior compartment boundary of imaginal disc",
          "FBbt00001769": "eye disc morphogenetic furrow"
        },
        "transposons": {
          "FBtp0000365": "P{GAL4-dpp.blk1}"
        },
        "expression_desc_text": "Drives expression at the anterior/posterior compartment boundary of imaginal discs, and at the morphogenetic furrow of the eye disc.",
        "stocks": {
          "FBst0305049": "106380",
          "FBst0001553": "1553",
          "FBst0067066": "67066",
          "FBst0084296": "84296",
          "FBst0084316": "84316",
          "FBst0084337": "84337",
          "FBst0093385": "93385"
        }
      }
    },
    {
      "driver": {
        "name": "Scer\\GAL4<up>Ir25a.PA</up>",
        "fbid": "FBal0249373",
        "images": null,
        "pubs": {
          "FBrf0212725": "Abuin et al., 2011, Neuron 69(1): 44--60",
          "FBrf0215822": "Silbering et al., 2011, J. Neurosci. 31(38): 13357--13375",
          "FBrf0221182": "Min et al., 2013, Proc. Natl. Acad. Sci. U.S.A. 110(14): E1321--E1329",
          "FBrf0230271": "Chen et al., 2015, Nature 527(7579): 516--520",
          "FBrf0232388": "Enjin et al., 2016, Curr. Biol. 26(10): 1352--1358",
          "FBrf0236232": "Frank et al., 2017, Curr. Biol. 27(15): 2381--2388.e4",
          "FBrf0236716": "Chen and Amrein, 2017, Curr. Biol. 27(18): 2741--2750.e4",
          "FBrf0236934": "Rist and Thum, 2017, J. Comp. Neurol. 525(18): 3865--3889",
          "FBrf0237619": "Lee et al., 2018, Neuron 97(1): 67--74.e4",
          "FBrf0237676": "Ahn et al., 2017, eLife 6: e30115",
          "FBrf0238151": "Steck et al., 2018, eLife 7: e31625",
          "FBrf0240321": "Sánchez-Alcañiz et al., 2018, Nat. Commun. 9(1): 4252",
          "FBrf0240352": "Sun et al., 2018, eLife 7: e39249",
          "FBrf0241429": "Chai et al., 2019, Nat. Commun. 10(1): 643",
          "FBrf0242503": "Lei et al., 2019, Front. Physiol. 10: 556",
          "FBrf0246007": "Alpert et al., 2020, Curr. Biol. 30(12): 2275--2288.e5",
          "FBrf0247159": "Weaver et al., 2020, G3 (Bethesda) 10(11): 4147--4158",
          "FBrf0251811": "Dhakal et al., 2021, Commun. Biol. 4(1): 1281",
          "FBrf0253093": "Trisal et al., 2022, J. Neurosci. 42(14): 2930--2941",
          "FBrf0253272": "Task et al., 2022, eLife 11: e72599",
          "FBrf0255204": "Omelchenko et al., 2022, Front. Mol. Neurosci. 15: 1023492",
          "FBrf0256676": "Shrestha et al., 2023, EMBO Rep. 24(6): e56319",
          "unattributed": null
        },
        "rex_gene": {
          "FBgn0031634": "Ir25a"
        },
        "common_terms": "anterior cold cell, ACc, bitter sensing GRN,",
        "major_stages": {
          "FBdv00005336": "larval stage",
          "FBdv00005369": "adult stage"
        },
        "major_tissues": {
          "FBbt00049613": "bitter-sensing neuron",
          "FBbt00005923": "hygrosensory neuron",
          "FBbt00047485": "calcium-sensing neuron of labellar S-type taste bristle",
          "FBbt00048209": "bitter-sensing neuron of the leg",
          "FBbt00049720": "non-aristal sensory neuron VP3",
          "FBbt00051293": "adult thermosensory neuron",
          "FBbt00110990": "adult hygrosensory neuron Ir40a"
        },
        "transposons": {
          "FBtp0057158": "P{Ir25a-GAL4.A}"
        },
        "expression_desc_text": "Drives expression in a subset of bitter-sensing, hygrosensory, and cold-sensing thermosensory neurons in the labellum, legs, arista, sacculus, antenna, labrum, and larval head sensory organs",
        "stocks": {
          "FBst0041728": "41728"
        }
      }
    }
  ]
}').


json1:- process_json('/opt/logicmoo_opencog/hyperon-wam/data/ftp.flybase.org/releases/FB2023_04/precomputed_files/genes/ncRNA_genes_fb_2023_04.json').
% 51,290,751 inferences, 8.285 CPU in 8.289 seconds (100% CPU, 6190948 Lips)

json2:- process_json('/opt/logicmoo_opencog/hyperon-wam/data/ftp.flybase.org/releases/FB2023_04/precomputed_files/insertions/fu_gal4_table_fb_2023_04.json').
% 27,108,104 inferences, 4.454 CPU in 4.456 seconds (100% CPU, 6085908 Lips)




