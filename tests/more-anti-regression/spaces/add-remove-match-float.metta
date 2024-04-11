;;;;;;;;;;;;;;;;;;;;;;
; Adding Atoms to the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb0 (new-space))
!(add-atom &kb0 1.0)
!(add-atom &kb0 2.0)
;MeTTaLog Only: !(assertEqual (atom-count &kb0) 2)
!(assertEqualToResult (get-atoms &kb0) (1.0 2.0))

;;;;;;;;;;;;;;;;;;;;;;
; Removing Atoms from the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb1.0 (new-space))
!(add-atom &kb1.0 1.0)
!(add-atom &kb1.0 2.0)
!(add-atom &kb1.0 3.0)
;; Remove an atom and test for success
!(assertTrue (remove-atom &kb1.0 2.0)) ; "remove_atom on a present atom should return true"
;; Attempt to remove a non-existent atom and test for failure
!(assertFalse (remove-atom &kb1.0 "bogus")) ; "remove_atom on a missing atom should return false"
;; Verify the current state of the knowledge base
!(assertEqualToResult (get-atoms &kb1.0) (1.0 3.0))

;;;;;;;;;;;;;;;;;;;;;;
; Replacing Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb2.0 (new-space))
(= (replace-atom $space $before $after) (if (remove-atom $space $before) (add-atom $space $after) False))
!(add-atom &kb2.0 1.0)
!(add-atom &kb2.0 2.0)
!(add-atom &kb2.0 3.0)
;; Replace an atom and verify the operation was successful
!(assertTrue (replace-atom &kb2.0 2.0 4.0))
;; Check the new set of atoms in the knowledge base
!(assertEqualToResult (get-atoms &kb2.0) (1.0 4.0 3.0))

;;;;;;;;;;;;;;;;;;;;;;
; Querying Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb3.0 (new-space))
!(add-atom &kb3.0 ( -1.0 -2.0))
!(add-atom &kb3.0 ( -3.0 -4.0))
;; Adding a duplicate pattern for testing multiple matches
!(add-atom &kb3.0 ( -1.0 -5.0))
;; Verify that the query returns the expected matches
!(assertEqualToResult (match &kb3.0 ( -1.0 $XX) $XX) (-2.0 -5.0))

;;;;;;;;;;;;;;;;;;;;;;
; Comprehensive Test with Add, Remove, Query
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb4.0 (new-space))
!(add-atom &kb4.0 6.0)
;; Perform a sequence of operations
!(add-atom &kb4.0 6.0)
!(remove-atom &kb4.0 6.0)
!(add-atom &kb4.0 7.0)
;; Final state should have 6.0 and 7.0 only
!(assertEqualToResult (get-atoms &kb4.0) (6.0 7.0))
;; Query to test the presence of 6.0 and 7.0
!(assertTrue (query &kb4.0 6.0))
!(assertTrue (query &kb4.0 7.0))




