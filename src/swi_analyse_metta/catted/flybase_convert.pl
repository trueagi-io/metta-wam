/** <module> Flybase Prolog Entry Point
 *
 * This module sets up the environment and loads essential files for the Flybase system.
 *
 * The following code handles various environment configurations, loading necessary
 * files for the main application, and managing output behavior.
 *
 */

% File encoding directive
% This sets the file encoding to ISO Latin 1, which is used for special character sets.
:- encoding(iso_latin_1).

% Directive to ensure that output is flushed after every write operation. 
% This is useful in cases where output needs to be immediately visible.
:- flush_output.

% Sets the RUST_BACKTRACE environment variable to 'full'.
% This environment variable is specific to Rust programs and enables full backtrace logging, 
% which might be helpful for debugging.
:- setenv('RUST_BACKTRACE', full).

% Ensure the following modules are loaded, as they contain core functionalities
% for working with the Flybase system.
:- ensure_loaded(flybase_main).
:- ensure_loaded(flybase_json).
:- ensure_loaded(flybase_obo).

/* previously: 
   The `swi_support` module was intended to be loaded here but was commented out.
   It's likely this module was skipped because its functionality was no longer 
   needed or was replaced by other modules (e.g., `flybase_json`). This helps 
   maintain backward compatibility while preventing the inclusion of potentially 
   unnecessary code.
*/

% :- ensure_loaded(swi_support).
% Skipped loading of `swi_support`. This module was previously necessary but
% has been commented out, indicating it may no longer be essential for the current 
% application functionality.
