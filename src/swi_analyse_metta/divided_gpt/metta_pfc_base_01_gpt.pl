/*
 * @brief Set Prolog flag to ensure that PFC (Prolog Forward Chaining) 
 *        is shared across all modules, specifically setting it to the 'user' module.
 * @details The set_prolog_flag/2 predicate changes runtime flags. 
 *          Here, the flag 'pfc_shared_module' ensures the PFC features are 
 *          available for the 'user' module, which allows using PFC without 
 *          additional imports.
 * @example
 *      ?- set_prolog_flag(pfc_shared_module, user).
 *      true.
 */
:- set_prolog_flag(pfc_shared_module, user).

/* previously: % :- if( \+ current_predicate(set_fileAssertMt/1)). 
 * This line was skipped because it checks whether 'set_fileAssertMt/1' is already defined.
 * If it were not, the code within the 'if' block would execute. 
 * However, this might not be needed anymore because 'set_fileAssertMt/1' 
 * could have been predefined elsewhere, or it's being dynamically handled.
 */

