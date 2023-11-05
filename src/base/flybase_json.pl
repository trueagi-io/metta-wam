
% ```prolog

json_sample('
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
}').

:- use_module(library(logicmoo_utils)).

% facts for creating predcatres
extracted_predicate(transposon, [primaryId, symbol, sequence, url, taxonId, soTermId]).
extracted_predicate(transposon_publications, [primaryId, publications]).
extracted_predicate(transposon_synonyms, [primaryId, symbolSynonyms]).
extracted_predicate(transposon_cross_references, [primaryId, crossReferenceIds]).

extracted_predicate(gene, [primaryId, gene_geneId, gene_symbol, gene_url, gene_locusTag, gene_name]).

extracted_predicate(exon_locations, [primaryId, genomeLocations_assembly, genomeLocations_gca_accession,
  exons_INSDC_accession, exons_chromosome, exons_strand, exons_startPosition, exons_endPosition]).

extracted_predicate(gene_synonyms, [gene_geneId, gene_synonyms]).
%extracted_predicate(exon, [primaryId, geneId, 'INSDC_accession', chromosome, strand, startPosition, endPosition]).
extracted_predicate(metadata, [dataProvider, schemaVersion, release, genomicCoordinateSystem, dateProduced]).
%extracted_predicate(exon_locations, [ assembly, chromosome, strand, startPosition, endPosition]).

extracted_predicate(N,[fbid,M]):-
  member(M,[transposons, common_terms, major_stages, major_tissues, name,
           pubs, rex_gene, stocks, expression_desc_text, images]), atom_concat('allele_',M,N).
extracted_predicate(allele_image,[fbid,images,images_imageDescription,
            images_publicationId, images_pubFigure, images_permission]).

note_doing(P):- nl,wdmsg(P),!,call(P).

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

prefix_key([O|_],Kee,Key):- atom(O), O\==Kee, O\==data,atomic_list_concat([O,'_',Kee],Key),!.
prefix_key(_,Key,Key).



:- use_module(library(http/json)).

process_json(File):- atom(File),exists_file(File),!,
    setup_call_cleanup(
               open(File, read, Stream),
               json_read(Stream, JSONDict),
               close(Stream)),
    process_json([],JSONDict).

process_json(JsonString):- process_json([],JsonString),!.


assert_json(_).



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
with_json1(O,Val):- print(error(O=Val)),!.

with_json2(O,[json(Values)]):- !, with_json2(O,Values).
with_json2(O,json(Values)):- !, with_json2(O,Values).
with_json2(O,Values0):-
   predsort(simple_first,Values0,Values),%nl,
   wdmsg(O==Values),nl,
   retractall(json_kv(_,_)),
   ignore(maplist(with_entry(O,assert),Values)),
   retractall(json_kv(_,_)).

with_entry(O,AR, Key=Value):-!, with_kv([Key|O],AR,Key,Value).
%with_entry(O,assert,JSON) :- !, process_json(O,JSON).
with_entry(O,AR,JSON):- wdmsg(error_with_entry(O,AR,JSON)).

uses_id_subprops(images).

with_kv_maybe_more(_O,_AR,_Key,json([])):-!.
with_kv_maybe_more(O,AR,Key,Do):- with_kv(O,AR,Key,Do),!.

assert_id_about(O,Key,ID,NVAboutID):-
  with_json2([Key|O],[Key=ID|NVAboutID]).

with_kv(O,AR,Key,json([ID=Value|More])):- atom(Value),!,
   assert_json(add_name(ID,Value)),
   decl_type(ID,Key),
   with_kv(O,AR,Key,ID),
   with_kv_maybe_more(O,AR,Key,json(More)).

with_kv(O,AR,Key,json([ID=json(NVAboutID)|More])):-
   uses_id_subprops(Key),atom(ID),
   with_kv(O,AR,Key,ID),
   assert_id_about(O,Key,ID,NVAboutID),
   with_kv_maybe_more(O,AR,Key,json(More)).

with_kv(O,AR,OK,Key=Values):- !, with_kv([OK|O],AR,Key,Values).
with_kv(O,AR,Key,json(Values)):- !, with_kv(O,AR,Key,Values).
with_kv(O,AR,Key,Value):- is_list(Value),Value\==[],!,maplist(with_kv(O,AR,Key),Value).
with_kv(O,AR,Kee,Value):-
    prefix_key(O,Kee,Key),
    retractall(json_kv(Key,_)),
    KV = json_kv(Key,Value),
    Do =.. [AR,KV],
    call(Do),
    ignore((AR==assert,
    %wdmsg(cr(Key)=Value),
    check_ready(Key))).

check_ready(Key):-
    forall((extracted_predicate(P,List),memberchk(Key,List)),
      (length(List,Len),
       ignore((findall(Arg,(member(K,List),json_kv(K,Arg)),ArgList),
       length(ArgList,Len),
       Fact=..[P|ArgList],
       write('==============================================='),
       note_doing(assert(Fact)),
       maplist(decl_type,ArgList,List),
       write('==============================================='))))).

:- dynamic(used_arg/2).
decl_type(Arg,Type):- used_arg(Arg,Type),!.
decl_type(Arg,Type):- assert(used_arg(Arg,Type)),!.

%:- json_sample(Str),process_json(O,Str).
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

%:- process_json('/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/data/ftp.flybase.org/releases/FB2023_04/precomputed_files/genes/ncRNA_genes_fb_2023_04.json').


:- process_json('/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/data/ftp.flybase.org/releases/FB2023_04/precomputed_files/insertions/fu_gal4_table_fb_2023_04.json').
