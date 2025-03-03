
root@HOSTAGE:~/metta-wam/libraries/bioAS# ./prepare.sh neo4j_out_v3.rdf 100M
./prepare.sh2: line 1: #!/bin/bash: No such file or directory
🚀 Splitting 'neo4j_out_v3.rdf' (~25 GB) into ~100M chunks in 'neo4j_out_v3_split_rdf/'...
🎉 Total files created: 258 in 3m 51s.4j_out_v3_part_000258.rdf 🦅 (elapsed: 3m 51s)                       ✅


root@HOSTAGE:~/metta-wam/libraries/bioAS# swipl -l gene_queries1.pl
\U0001F680 Mounting persistence at datastore_neo4j_out_v3
% Restoring 1 snapshots using 1 concurrent workers
datastore_neo4j_out_v3................................... 1 of 1 graphs
% Loaded 1 graphs (6,154,340 triples) in 7.56 sec. (98% CPU = 7.44 sec.)
?-load_rdf_db_directory
Welcome to SWI-Prolog (threaded, 64 bits, version 9.3.19-42-g7a8a2ecb8-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- load_rdf_db_directory.
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000001.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2548779: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000001.rdf" in 13.29 sec; 1,209,940 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000001.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000002.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2576329: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000002.rdf" in 11.83 sec; 2,353,547 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000002.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000003.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2501798: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000003.rdf" in 15.76 sec; 3,988,378 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000003.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000004.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2453529: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000004.rdf" in 13.69 sec; 5,062,555 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000004.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000005.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1959983: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000005.rdf" in 13.81 sec; 6,154,340 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000005.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000006.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2794979: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000006.rdf" in 16.63 sec; 7,436,905 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000006.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000007.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2952934: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000007.rdf" in 18.14 sec; 8,698,490 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000007.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000008.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953957: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000008.rdf" in 20.15 sec; 9,962,340 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000008.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000009.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2957855: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000009.rdf" in 21.65 sec; 11,233,370 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000009.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000010.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954833: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000010.rdf" in 22.96 sec; 12,499,290 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000010.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000011.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2958597: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000011.rdf" in 23.94 sec; 13,764,996 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000011.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000012.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2957501: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000012.rdf" in 25.32 sec; 15,032,432 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000012.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000013.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2939295: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000013.rdf" in 27.42 sec; 16,294,358 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000013.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000014.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2765182: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000014.rdf" in 31.45 sec; 17,827,494 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000014.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000015.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2575775: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000015.rdf" in 37.54 sec; 19,739,519 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000015.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000016.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2582887: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000016.rdf" in 40.14 sec; 21,659,187 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000016.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000017.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2615587: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000017.rdf" in 45.75 sec; 23,593,456 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000017.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000018.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2588540: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000018.rdf" in 44.77 sec; 25,514,775 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000018.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000019.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2550593: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000019.rdf" in 44.79 sec; 27,372,584 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000019.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000020.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2538726: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000020.rdf" in 46.74 sec; 29,236,058 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000020.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000021.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2570453: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000021.rdf" in 49.37 sec; 31,081,849 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000021.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000022.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2575707: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000022.rdf" in 50.25 sec; 32,966,224 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000022.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000023.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2595071: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000023.rdf" in 54.11 sec; 34,857,390 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000023.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000024.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2567272: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000024.rdf" in 54.92 sec; 36,764,950 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000024.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000025.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2582425: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000025.rdf" in 57.38 sec; 38,659,868 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000025.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000026.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2611280: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000026.rdf" in 58.78 sec; 40,604,704 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000026.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000027.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2549700: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000027.rdf" in 59.19 sec; 42,508,000 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000027.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000028.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2685227: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000028.rdf" in 63.09 sec; 44,534,100 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000028.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000029.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746402: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000029.rdf" in 70.74 sec; 46,659,124 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000029.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000030.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740260: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000030.rdf" in 74.71 sec; 48,839,776 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000030.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000031.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740706: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000031.rdf" in 73.73 sec; 51,017,007 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000031.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000032.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2749937: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000032.rdf" in 75.03 sec; 53,172,575 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000032.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000033.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744658: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000033.rdf" in 75.69 sec; 55,339,763 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000033.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000034.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738230: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000034.rdf" in 78.39 sec; 57,523,317 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000034.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000035.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738711: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000035.rdf" in 80.95 sec; 59,706,017 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000035.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000036.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739529: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000036.rdf" in 81.99 sec; 61,889,799 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000036.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000037.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738978: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000037.rdf" in 83.78 sec; 64,073,825 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000037.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000038.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746879: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000038.rdf" in 84.36 sec; 66,221,400 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000038.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000039.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738258: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000039.rdf" in 90.17 sec; 68,407,214 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000039.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000040.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738191: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000040.rdf" in 90.91 sec; 70,592,454 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000040.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000041.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737495: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000041.rdf" in 94.01 sec; 72,778,204 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000041.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000042.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748563: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000042.rdf" in 92.08 sec; 74,859,342 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000042.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000043.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2751172: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000043.rdf" in 95.22 sec; 76,929,727 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000043.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000044.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748393: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000044.rdf" in 100.04 sec; 79,036,921 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000044.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000045.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2750807: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000045.rdf" in 101.62 sec; 81,155,676 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000045.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000046.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748454: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000046.rdf" in 103.19 sec; 83,313,487 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000046.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000047.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2748432: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000047.rdf" in 110.62 sec; 85,487,410 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000047.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000048.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2747401: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000048.rdf" in 113.66 sec; 87,663,959 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000048.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000049.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2751355: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000049.rdf" in 114.90 sec; 89,784,588 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000049.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000050.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2797650: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000050.rdf" in 84.98 sec; 90,870,918 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000050.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000051.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2562947: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000051.rdf" in 86.75 sec; 92,083,016 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000051.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000052.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2387168: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000052.rdf" in 93.45 sec; 93,664,807 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000052.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000053.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2407436: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000053.rdf" in 96.07 sec; 95,287,935 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000053.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000054.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2397715: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000054.rdf" in 99.96 sec; 96,894,283 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000054.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000055.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2392829: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000055.rdf" in 103.98 sec; 98,526,528 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000055.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000056.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2345859: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000056.rdf" in 102.19 sec; 100,045,101 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000056.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000057.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2355365: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000057.rdf" in 101.87 sec; 101,576,609 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000057.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000058.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2302163: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000058.rdf" in 100.41 sec; 103,087,693 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000058.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000059.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1862041: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000059.rdf" in 88.40 sec; 104,247,194 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000059.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000060.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1816027: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000060.rdf" in 87.89 sec; 105,391,578 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000060.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000061.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1801787: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000061.rdf" in 87.82 sec; 106,544,915 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000061.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000062.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1854249: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000062.rdf" in 91.16 sec; 107,700,380 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000062.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000063.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1832916: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000063.rdf" in 103.55 sec; 109,095,072 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000063.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000064.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2384339: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000064.rdf" in 115.33 sec; 110,786,723 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000064.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000065.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2610611: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000065.rdf" in 106.94 sec; 111,884,992 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000065.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000066.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2721902: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000066.rdf" in 101.11 sec; 112,885,371 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000066.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000067.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2527651: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000067.rdf" in 123.45 sec; 114,695,140 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000067.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000068.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2549013: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000068.rdf" in 128.94 sec; 116,532,304 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000068.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000069.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2498826: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000069.rdf" in 129.85 sec; 118,342,051 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000069.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000070.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2442119: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000070.rdf" in 135.29 sec; 120,123,900 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000070.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000071.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2486273: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000071.rdf" in 134.40 sec; 121,952,684 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000071.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000072.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2503999: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000072.rdf" in 134.39 sec; 123,775,773 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000072.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000073.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2553487: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000073.rdf" in 135.21 sec; 125,632,267 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000073.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000074.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2290352: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000074.rdf" in 127.96 sec; 127,376,517 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000074.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000075.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1466343: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000075.rdf" in 107.02 sec; 128,795,388 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000075.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000076.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1468222: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000076.rdf" in 110.00 sec; 130,212,970 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000076.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000077.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1481816: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000077.rdf" in 107.67 sec; 131,622,477 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000077.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000078.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1482675: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000078.rdf" in 104.33 sec; 133,031,580 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000078.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000079.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1484325: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000079.rdf" in 103.91 sec; 134,450,568 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000079.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000080.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1485197: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000080.rdf" in 103.83 sec; 135,888,209 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000080.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000081.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1487662: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000081.rdf" in 104.60 sec; 137,324,535 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000081.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000082.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1501023: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000082.rdf" in 107.57 sec; 138,752,820 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000082.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000083.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1502162: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000083.rdf" in 105.27 sec; 140,180,315 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000083.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000084.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1489420: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000084.rdf" in 102.33 sec; 141,604,296 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000084.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000085.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1592013: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000085.rdf" in 105.12 sec; 143,032,152 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000085.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000086.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2661824: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000086.rdf" in 123.77 sec; 144,528,590 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000086.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000087.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2674382: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000087.rdf" in 124.40 sec; 146,017,963 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000087.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000088.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2657482: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000088.rdf" in 119.59 sec; 147,512,081 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000088.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000089.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2670784: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000089.rdf" in 119.39 sec; 149,003,799 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000089.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000090.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2674474: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000090.rdf" in 120.70 sec; 150,495,556 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000090.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000091.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2059357: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000091.rdf" in 112.27 sec; 151,949,570 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000091.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000092.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1474032: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000092.rdf" in 102.25 sec; 153,363,620 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000092.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000093.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1483821: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000093.rdf" in 104.40 sec; 154,771,865 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000093.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000094.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1482000: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000094.rdf" in 106.87 sec; 156,181,094 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000094.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000095.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2119884: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000095.rdf" in 124.78 sec; 157,916,126 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000095.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000096.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2231907: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000096.rdf" in 128.13 sec; 159,698,066 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000096.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000097.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2258508: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000097.rdf" in 135.59 sec; 161,526,731 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000097.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000098.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2255090: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000098.rdf" in 137.52 sec; 163,342,852 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000098.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000099.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2147519: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000099.rdf" in 132.25 sec; 164,960,349 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000099.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000100.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2222666: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000100.rdf" in 139.11 sec; 166,721,795 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000100.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000101.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2092516: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000101.rdf" in 123.47 sec; 168,044,889 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000101.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000102.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1470481: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000102.rdf" in 117.51 sec; 169,439,437 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000102.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000103.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1476774: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000103.rdf" in 114.09 sec; 170,789,704 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000103.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000104.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1440867: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000104.rdf" in 114.82 sec; 172,161,463 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000104.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000105.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1420623: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000105.rdf" in 115.08 sec; 173,520,769 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000105.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000106.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1450451: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000106.rdf" in 116.45 sec; 174,851,649 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000106.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000107.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1486151: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000107.rdf" in 111.31 sec; 176,060,699 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000107.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000108.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1618390: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000108.rdf" in 114.85 sec; 177,338,364 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000108.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000109.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1405975: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000109.rdf" in 120.01 sec; 178,731,949 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000109.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000110.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406609: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000110.rdf" in 117.02 sec; 180,125,076 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000110.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000111.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406770: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000111.rdf" in 114.90 sec; 181,518,451 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000111.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000112.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406858: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000112.rdf" in 115.27 sec; 182,912,118 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000112.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000113.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1406532: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000113.rdf" in 114.90 sec; 184,305,930 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000113.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000114.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:1689777: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000114.rdf" in 129.91 sec; 185,800,587 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000114.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000115.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2446457: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000115.rdf" in 153.34 sec; 187,671,482 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000115.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000116.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2439242: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000116.rdf" in 152.75 sec; 189,507,112 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000116.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000117.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2478844: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000117.rdf" in 157.10 sec; 191,410,862 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000117.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000118.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2352648: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000118.rdf" in 154.57 sec; 193,231,958 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000118.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000119.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2447799: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000119.rdf" in 164.83 sec; 195,121,736 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000119.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000120.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2499532: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000120.rdf" in 176.18 sec; 197,062,425 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000120.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000121.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2576716: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000121.rdf" in 189.07 sec; 198,967,345 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000121.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000122.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742332: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000122.rdf" in 193.40 sec; 200,919,072 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000122.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000123.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742050: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000123.rdf" in 202.21 sec; 202,981,469 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000123.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000124.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740993: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000124.rdf" in 195.95 sec; 205,061,523 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000124.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000125.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738718: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000125.rdf" in 192.29 sec; 207,166,110 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000125.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000126.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2741473: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000126.rdf" in 202.52 sec; 209,284,249 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000126.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000127.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738870: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000127.rdf" in 208.04 sec; 211,418,961 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000127.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000128.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739899: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000128.rdf" in 214.57 sec; 213,551,366 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000128.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000129.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740369: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000129.rdf" in 223.66 sec; 215,682,051 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000129.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000130.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738682: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000130.rdf" in 224.34 sec; 217,810,209 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000130.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000131.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739227: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000131.rdf" in 227.88 sec; 219,928,544 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000131.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000132.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739583: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000132.rdf" in 221.41 sec; 222,044,931 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000132.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000133.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740113: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000133.rdf" in 233.70 sec; 224,155,431 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000133.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000134.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738269: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000134.rdf" in 233.69 sec; 226,236,796 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000134.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000135.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740555: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000135.rdf" in 229.89 sec; 228,287,534 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000135.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000136.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2741914: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000136.rdf" in 222.51 sec; 230,323,635 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000136.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000137.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743465: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000137.rdf" in 214.83 sec; 232,250,230 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000137.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000138.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742089: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000138.rdf" in 221.16 sec; 234,283,863 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000138.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000139.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737441: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000139.rdf" in 223.54 sec; 236,425,240 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000139.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000140.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739180: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000140.rdf" in 226.81 sec; 238,567,186 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000140.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000141.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739111: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000141.rdf" in 234.81 sec; 240,708,658 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000141.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000142.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737576: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000142.rdf" in 236.23 sec; 242,850,212 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000142.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000143.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737288: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000143.rdf" in 234.26 sec; 244,991,847 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000143.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000144.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737471: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000144.rdf" in 240.87 sec; 247,133,470 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000144.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000145.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738363: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000145.rdf" in 231.51 sec; 249,275,976 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000145.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000146.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738225: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000146.rdf" in 228.96 sec; 251,418,209 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000146.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000147.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736836: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000147.rdf" in 234.20 sec; 253,559,683 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000147.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000148.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736964: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000148.rdf" in 249.91 sec; 255,701,801 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000148.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000149.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737243: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000149.rdf" in 247.60 sec; 257,843,094 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000149.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000150.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737842: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000150.rdf" in 242.15 sec; 259,985,079 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000150.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000151.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736749: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000151.rdf" in 238.98 sec; 262,127,045 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000151.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000152.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740076: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000152.rdf" in 284.63 sec; 264,269,524 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000152.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000153.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737633: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000153.rdf" in 274.27 sec; 266,411,717 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000153.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000154.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737813: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000154.rdf" in 276.82 sec; 268,553,172 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000154.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000155.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739305: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000155.rdf" in 293.46 sec; 270,694,907 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000155.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000156.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2739271: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000156.rdf" in 293.58 sec; 272,836,200 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000156.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000157.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737710: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000157.rdf" in 288.32 sec; 274,978,089 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000157.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000158.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737816: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000158.rdf" in 300.69 sec; 277,120,144 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000158.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000159.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736805: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000159.rdf" in 297.35 sec; 279,261,950 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000159.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000160.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738055: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000160.rdf" in 303.19 sec; 281,404,298 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000160.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000161.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737288: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000161.rdf" in 306.24 sec; 283,546,167 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000161.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000162.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737589: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000162.rdf" in 308.12 sec; 285,672,833 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000162.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000163.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742389: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000163.rdf" in 308.90 sec; 287,725,183 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000163.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000164.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743605: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000164.rdf" in 312.40 sec; 289,744,261 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000164.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000165.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736433: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000165.rdf" in 322.05 sec; 291,895,409 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000165.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000166.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736826: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000166.rdf" in 346.29 sec; 294,046,365 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000166.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000167.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736583: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000167.rdf" in 378.14 sec; 296,198,416 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000167.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000168.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737247: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000168.rdf" in 388.12 sec; 298,350,144 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000168.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000169.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737809: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000169.rdf" in 376.65 sec; 300,501,972 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000169.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000170.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738187: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000170.rdf" in 399.13 sec; 302,654,286 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000170.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000171.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737880: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000171.rdf" in 395.13 sec; 304,806,554 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000171.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000172.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737273: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000172.rdf" in 366.15 sec; 306,956,907 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000172.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000173.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737006: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000173.rdf" in 346.39 sec; 309,106,663 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000173.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000174.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736281: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000174.rdf" in 349.00 sec; 311,256,660 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000174.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000175.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736810: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000175.rdf" in 365.22 sec; 313,406,727 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000175.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000176.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2738331: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000176.rdf" in 360.47 sec; 315,556,374 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000176.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000177.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2735435: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000177.rdf" in 372.99 sec; 317,707,286 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000177.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000178.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736662: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000178.rdf" in 374.34 sec; 319,858,423 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000178.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000179.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736572: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000179.rdf" in 379.45 sec; 322,010,691 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000179.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000180.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2737314: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000180.rdf" in 376.68 sec; 324,162,885 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000180.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000181.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2736901: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000181.rdf" in 388.63 sec; 326,314,354 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000181.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000182.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2740902: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000182.rdf" in 392.01 sec; 328,394,779 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000182.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000183.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745127: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000183.rdf" in 383.74 sec; 330,272,729 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000183.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000184.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2747217: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000184.rdf" in 383.04 sec; 332,230,178 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000184.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000185.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746129: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000185.rdf" in 381.53 sec; 334,137,378 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000185.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000186.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746417: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000186.rdf" in 386.80 sec; 336,029,758 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000186.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000187.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744793: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000187.rdf" in 410.10 sec; 337,922,371 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000187.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000188.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2742348: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000188.rdf" in 402.69 sec; 339,939,280 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000188.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000189.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745329: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000189.rdf" in 408.08 sec; 341,963,607 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000189.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000190.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743085: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000190.rdf" in 418.26 sec; 343,965,536 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000190.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000191.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745443: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000191.rdf" in 417.68 sec; 345,884,887 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000191.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000192.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746874: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000192.rdf" in 433.20 sec; 347,837,304 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000192.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000193.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744757: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000193.rdf" in 454.16 sec; 349,804,770 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000193.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000194.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743228: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000194.rdf" in 456.01 sec; 351,852,613 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000194.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000195.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746152: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000195.rdf" in 482.76 sec; 353,931,571 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000195.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000196.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2745210: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000196.rdf" in 482.82 sec; 356,026,954 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000196.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000197.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2747490: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000197.rdf" in 482.29 sec; 358,118,035 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000197.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000198.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744570: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000198.rdf" in 472.62 sec; 360,200,211 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000198.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000199.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744426: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000199.rdf" in 417.61 sec; 362,283,071 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000199.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000200.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744127: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000200.rdf" in 402.31 sec; 364,368,020 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000200.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000201.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746536: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000201.rdf" in 406.20 sec; 366,456,944 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000201.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000202.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743734: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000202.rdf" in 465.08 sec; 368,543,216 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000202.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000203.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743449: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000203.rdf" in 474.72 sec; 370,628,684 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000203.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000204.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746964: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000204.rdf" in 470.62 sec; 372,717,361 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000204.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000205.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2744665: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000205.rdf" in 447.33 sec; 374,804,360 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000205.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000206.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2743543: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000206.rdf" in 381.07 sec; 376,875,171 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000206.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000207.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2746997: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000207.rdf" in 412.51 sec; 378,915,902 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000207.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000208.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2749912: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000208.rdf" in 416.87 sec; 380,876,064 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000208.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000209.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2922809: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000209.rdf" in 362.57 sec; 382,319,633 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000209.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000210.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2966871: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000210.rdf" in 286.70 sec; 383,590,250 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000210.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000211.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2947676: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000211.rdf" in 313.07 sec; 384,852,949 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000211.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000212.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2943144: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000212.rdf" in 308.53 sec; 386,114,365 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000212.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000213.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2952389: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000213.rdf" in 292.21 sec; 387,378,054 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000213.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000214.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953884: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000214.rdf" in 288.30 sec; 388,641,657 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000214.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000215.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954160: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000215.rdf" in 354.66 sec; 389,904,906 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000215.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000216.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955356: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000216.rdf" in 428.31 sec; 391,168,109 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000216.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000217.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2950300: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000217.rdf" in 438.42 sec; 392,427,957 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000217.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000218.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2949062: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000218.rdf" in 448.49 sec; 393,686,822 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000218.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000219.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2948617: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000219.rdf" in 412.05 sec; 394,946,157 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000219.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000220.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953640: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000220.rdf" in 412.95 sec; 396,209,357 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000220.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000221.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2950846: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000221.rdf" in 459.73 sec; 397,472,157 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000221.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000222.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2961660: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000222.rdf" in 422.45 sec; 398,740,245 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000222.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000223.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954919: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000223.rdf" in 426.94 sec; 400,005,840 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000223.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000224.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2960061: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000224.rdf" in 366.93 sec; 401,275,503 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000224.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000225.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2953736: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000225.rdf" in 420.21 sec; 402,543,884 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000225.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000226.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955881: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000226.rdf" in 383.26 sec; 403,813,915 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000226.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000227.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2961703: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000227.rdf" in 379.79 sec; 405,090,368 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000227.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000228.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2951217: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000228.rdf" in 398.34 sec; 406,362,203 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000228.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000229.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2969167: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000229.rdf" in 375.33 sec; 407,633,256 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000229.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000230.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954906: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000230.rdf" in 355.30 sec; 408,897,860 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000230.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000231.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2949679: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000231.rdf" in 351.82 sec; 410,160,720 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000231.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000232.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2951679: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000232.rdf" in 427.94 sec; 411,423,942 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000232.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000233.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955544: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000233.rdf" in 525.37 sec; 412,688,716 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000233.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000234.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2956514: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000234.rdf" in 527.80 sec; 413,952,051 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000234.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000235.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2970175: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000235.rdf" in 545.31 sec; 415,221,748 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000235.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000236.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2950279: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000236.rdf" in 558.30 sec; 416,483,186 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000236.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000237.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2959209: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000237.rdf" in 574.79 sec; 417,748,664 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000237.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000238.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2954802: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000238.rdf" in 652.18 sec; 419,013,509 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000238.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000239.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2963042: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000239.rdf" in 653.64 sec; 420,283,024 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000239.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000240.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2944955: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000240.rdf" in 625.12 sec; 421,544,613 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000240.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000241.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2966057: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000241.rdf" in 650.58 sec; 422,816,297 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000241.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000242.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2955693: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000242.rdf" in 656.17 sec; 424,083,767 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000242.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000243.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2945529: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000243.rdf" in 669.13 sec; 425,347,583 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000243.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000244.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2940818: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000244.rdf" in 584.91 sec; 426,610,213 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000244.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000245.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2936908: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000245.rdf" in 555.79 sec; 427,871,531 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000245.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000246.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2942066: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000246.rdf" in 529.30 sec; 429,135,385 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000246.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000247.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2933088: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000247.rdf" in 596.14 sec; 430,395,949 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000247.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000248.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2947419: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000248.rdf" in 550.34 sec; 431,661,274 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000248.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000249.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2934380: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000249.rdf" in 519.41 sec; 432,920,859 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000249.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000250.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2868113: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000250.rdf" in 483.78 sec; 433,952,687 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000250.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000251.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811485: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000251.rdf" in 445.54 sec; 434,665,213 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000251.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000252.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811418: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000252.rdf" in 424.60 sec; 435,377,789 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000252.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000253.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811518: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000253.rdf" in 426.45 sec; 436,090,306 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000253.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000254.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811604: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000254.rdf" in 444.97 sec; 436,802,798 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000254.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000255.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811550: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000255.rdf" in 449.11 sec; 437,515,302 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000255.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000256.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2811241: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000256.rdf" in 435.93 sec; 438,228,025 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000256.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000257.rdf
Warning: SGML2PL(xmlns): neo4j_out_v3:2291258: Inserted omitted end-tag for "rdf:RDF"
% Parsed "neo4j_out_v3_part_000257.rdf" in 473.12 sec; 439,306,461 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000257.rdf
\U0001F4C2 Loading RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000258.rdf
% Parsed "neo4j_out_v3_part_000258.rdf" in 492.32 sec; 440,318,102 triples
\U0001F4E5 Loaded RDF file: neo4j_out_v3_split_rdf/neo4j_out_v3_part_000258.rdf
true.

?- rdf_graph_property(neo4j_out_v3,Y).
Y = hash('5617e3db40e363dd9e916d1aefc81299') ;
Y = modified(false) ;
Y = source('file:///root/metta-wam/libraries/bioAS/neo4j_out_v3_split_rdf/neo4j_out_v3_part_000258.rdf') ;
Y = source_last_modified(1740796112.9243655) ;
Y = triples(440318102) ;
Y = persistent(true).


root@HOSTAGE:~/metta-wam/libraries/bioAS# swipl -l gene_queries1.pl
\U0001F680 Mounting persistence at datastore_neo4j_out_v3
% Restoring 2 snapshots using 2 concurrent workers
datastore_neo4j_out_v3................................... 1 of 2 graphs
neo4j_out_v3 2 of 2 graphs
% Loaded 2 graphs (446,472,442 triples) in 4554.54 sec. (91% CPU = 4140.39 sec.)
?-load_rdf_db_directory
Welcome to SWI-Prolog (threaded, 64 bits, version 9.3.19-42-g7a8a2ecb8-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).


    PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND
 132458 root      20   0   67.4g  61.3g   6540 S   0.0  48.8  69:00.65 swipl

?-  rdf(X,Y,Z).
X = 'neo4j://graph.individuals#13441',
Y = 'neo4j://graph.schema#start',
Z = literal('25120014') ;
X = 'neo4j://graph.individuals#14209',
Y = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
Z = 'neo4j://graph.schema#gene' ;
X = 'neo4j://graph.individuals#6747',
Y = 'neo4j://graph.schema#gene_type',
Z = literal(protein_coding) ;
X = 'neo4j://graph.individuals#22552398',
Y = 'neo4j://graph.schema#end',
Z = literal('152668911') ;
X = 'neo4j://graph.individuals#24572',
Y = 'neo4j://graph.schema#gene_name',
Z = literal('GIMAP7') ;
X = 'neo4j://graph.individuals#21575610',
Y = 'neo4j://graph.schema#phred_score',
Z = literal('6.14') ;
X = 'neo4j://graph.individuals#16634',
Y = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
Z = 'neo4j://graph.schema#gene' .

?-























time python ./metta_python_convert.py neo4j_out_v3/ --tree neo4j_out_v3_mw/
find neo4j_out_v3_mw/ -name "*_mw.pl" -printf "%s %p\n" | sort -n | awk '{print $2}' | while read file; do
    qlf_file="${file%.pl}.qlf";
    if [ ! -f "$qlf_file" ] || [ "$file" -nt "$qlf_file" ]; then
        echo "Compiling: $file";
        swipl -q -g "set_prolog_flag(encoding, utf8), qcompile(\"$file\"), halt.";
    fi
done


time python ./metta_python_convert.py neo4j_out_v4/ --tree neo4j_out_v4_mw/
find neo4j_out_v4_mw/ -name "*_mw.pl" -printf "%s %p\n" | sort -n | awk '{print $2}' | while read file; do
    qlf_file="${file%.pl}.qlf";
    if [ ! -f "$qlf_file" ] || [ "$file" -nt "$qlf_file" ]; then
        echo "Compiling: $file";
        swipl -q -g "set_prolog_flag(encoding, utf8), qcompile(\"$file\"), halt.";
    fi
done
