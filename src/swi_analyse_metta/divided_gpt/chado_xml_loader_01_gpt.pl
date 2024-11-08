/**
 * @module XML_DTD_Extractor
 * @author  
 * @summary
 * Module to extract the DTD file from an XML document using the sgml library.
 * It demonstrates the usage of SGML/XML parsing and manipulation in Prolog.
 *
 * @example 
 * % To extract a DTD file from an XML:
 * ?- extract_dtd_file('example.xml', DTDFileName).
 * 
 * @see
 * SGML library used for parsing: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/sgml.html%27)
 */

:- use_module(library(sgml)). % Import the SGML library to parse XML and handle DTDs
%:- use_module(library(logicmoo/xml_reader)). % Previously: Disabled custom XML reader library, no longer in use

/**
 * extract_dtd_file(+XMLFile:atom, -DTDFileName:atom) is det.
 *
 * Loads the XML file and extracts the associated DTD file name.
 * 
 * @param XMLFile The path to the XML file
 * @param DTDFileName The DTD file name extracted from the DOCTYPE declaration
 *
 * @example
 * ?- extract_dtd_file('file.xml', DTDFileName).
 * 
 * Reads the XML file, parses it, and retrieves the DTD information.
 */
 
extract_dtd_file(XMLFile, DTDFileName) :-
    open(XMLFile, read, Stream),  % Open the XML file for reading
    load_structure(Stream, _XML, [dialect(xml), doctype(Doctype)]),  % Load XML structure, capturing the DOCTYPE
    close(Stream),  % Close the file stream
    dtd_file_from_doctype(Doctype, DTDFileName).  % Extract DTD file name from the DOCTYPE declaration

/**
 * dtd_file_from_doctype(+Doctype:compound, -FileName:atom) is det.
 *
 * Extracts the DTD file name from the DOCTYPE declaration.
 * 
 * @param Doctype The DOCTYPE term from XML
 * @param FileName The DTD file name extracted from the DOCTYPE
 *
 * This helper predicate handles the parsing of the DOCTYPE declaration to find the external DTD reference.
 */
dtd_file_from_doctype(Doctype, FileName) :-
    nonvar(Doctype),  % Ensure the Doctype is not a variable (fully instantiated)
    Doctype = doctype(_Name, ExternalID),  % Decompose DOCTYPE term to get the external identifier
    extract_system_id(ExternalID, FileName).  % Extract the system identifier (which typically contains the DTD file name)

/**
 * extract_system_id(+ExternalID:compound, -FileName:atom) is det.
 *
 * Extracts the SYSTEM identifier from the external ID, which usually contains the DTD file name.
 * 
 * @param ExternalID External identifier from the DOCTYPE declaration
 * @param FileName Extracted file name from the SYSTEM identifier
 *
 * The SYSTEM identifier is part of the external ID, which holds the path or name of the DTD file.
 */
extract_system_id(ExternalID, FileName) :-
    nonvar(ExternalID),  % Ensure ExternalID is fully instantiated
    ExternalID = system(FileName).  % Extract the SYSTEM ID, which holds the file name

/*
load_dtd/2 predicate: Disabled due to redundancy and current focus on XML-based structures.
Previously: Loaded the DTD file directly and parsed it. 
This code has been commented out but preserved for future use if needed for direct DTD parsing.

Previously:
load_dtd(DTDFile, DTD) :-
    catch(
        (
            open(DTDFile, read, Stream),
            load_structure(Stream, DTD, [dtd(DTD), dialect(xml)]),
            close(Stream)
        ),
        Error,
        (   print_message(error, Error),
            fail
        )
    ).
*/

/**
 * @section Test Load Functions
 * Various functions to load large Chado XML datasets. These were written for processing specific XML files used in bioinformatics and genome data systems.
 *
 * lfb0, lfb1, lfb2, lfb3 are test cases that invoke loading and processing of different Chado XML files.
 */

/**
 * lfb0/0 is det.
 * 
 * Loads and prints a large XML file from a specific path.
 */
lfb0 :-
    fileToLineInfoElements(_Ctx, '/wam/data/FB_current/chado-xml/chado_FBim.xml', XML),  % Load specific XML file and extract line info elements
    writeln(XML),  % Write out the result to the console
    !.

/**
 * lfb1/0 is det.
 *
 * Test case to load a specific Chado XML file.
 */
lfb1 :-
    load_chado_xml('/wam/data/FB_current/chado-xml/chado_FBgn.xml').  % Load another specific XML file

/**
 * lfb2/0 is det.
 *
 * Test case to load another Chado XML file.
 */
lfb2 :-
    load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_gene_models.xml').  % Load gene model data

/**
 * lfb3/0 is det.
 *
 * Test case to load a Chado XML file containing predicted gene models.
 */
lfb3 :-
    load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_predicted.xml').  % Load predicted gene models

/**
 * load_chado_xml(+File:atom) is det.
 *
 * Parses a given Chado XML file and processes it.
 * 
 * @param File The XML file to load
 * 
 * This predicate opens the specified XML file and uses the sgml library to parse it. It also defines hooks for begin and end events during parsing.
 */
load_chado_xml(File) :-
    open(File, read, In),  % Open the file for reading
    new_sgml_parser(Parser, []),  % Create a new SGML parser instance
    set_sgml_parser(Parser, file(File)),  % Set the file to be parsed
    set_sgml_parser(Parser, dialect(xml)),  % Specify XML dialect
    set_sgml_parser(Parser, space(remove)),  % Configure parser to remove extra spaces
    sgml_parse(Parser,  % Parse the XML file with event hooks
               [ source(In),
                 call(begin, on_begin),  % Hook for start tags
                 call(end, on_end)  % Hook for end tags
               ]),
    close(In).  % Close the file after parsing

:- dynamic(feature_data/3).  % Declare dynamic predicate for storing feature data

/**
 * on_end(+Tag:atom, +Attributes:list) is det.
 *
 * Handles end tag event for 'feature'. Processes the feature data when a 'feature' element ends.
 * 
 * @param Tag The XML tag that ended
 * @param Attributes Attributes associated with the tag
 *
 * When a 'feature' element ends, it finishes processing the feature data and clears the data storage.
 */
on_end('feature', _) :-  
    !,  % Cut to prevent backtracking once this condition is satisfied
    finish_feature_data,  % Finalize and process the collected feature data
    !,
    listing(feature_data(_,_,_)),  % List all feature_data facts for inspection
    retractall(feature_data(_,_,_)),  % Clear the stored feature data
    sleep(0.1),  % Pause briefly to simulate processing time
    !.

% Previously: A generic tag handler was commented out because feature-specific handling was introduced.
%on_end(Tag, _Parser):- current_tag(Is), Is = Tag, !, pop_tag(Tag), finish_tag(Tag).