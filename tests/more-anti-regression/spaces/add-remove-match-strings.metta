;;;;;;;;;;;;;;;;;;;;;;
; Adding Atoms to the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb0 (new-space))
!(add-atom &kb0 "a")
!(add-atom &kb0 "b")
;MeTTaLog Only: !(assertEqual (atom-count &kb0) 2)
!(assertEqualToResult (get-atoms &kb0) ("a" "b"))

;;;;;;;;;;;;;;;;;;;;;;
; Removing Atoms from the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb1 (new-space))
!(add-atom &kb1 "a")
!(add-atom &kb1 "b")
!(add-atom &kb1 "c")
;; Remove an atom and test for success
!(assertTrue (remove-atom &kb1 "b")) ; "remove_atom on a present atom should return true"
;; Attempt to remove a non-existent atom and test for failure
!(assertFalse (remove-atom &kb1 "bogus")) ; "remove_atom on a missing atom should return false"
;; Verify the current state of the knowledge base
!(assertEqualToResult (get-atoms &kb1) ("a" "c"))

;;;;;;;;;;;;;;;;;;;;;;
; Replacing Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb2 (new-space))
(= (replace-atom $space $before $after) (if (remove-atom $space $before) (add-atom $space $after) False))
!(add-atom &kb2 "a")
!(add-atom &kb2 "b")
!(add-atom &kb2 "c")
;; Replace an atom and verify the operation was successful
!(assertTrue (replace-atom &kb2 "b" "d"))
;; Check the new set of atoms in the knowledge base
!(assertEqualToResult (get-atoms &kb2) ("a" "d" "c"))

;;;;;;;;;;;;;;;;;;;;;;
; Querying Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb3 (new-space))
!(add-atom &kb3 ( "A" "B"))
!(add-atom &kb3 ( "C" "D"))
;; Adding a duplicate pattern for testing multiple matches
!(add-atom &kb3 ( "A" "E"))
;; Verify that the query returns the expected matches
!(assertEqualToResult (match &kb3 ( "A" $XX) $XX) ("B" "E"))

;;;;;;;;;;;;;;;;;;;;;;
; Comprehensive Test with Add, Remove, Query
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb4 (new-space))
!(add-atom &kb4 "initial-state")
;; Perform a sequence of operations
!(add-atom &kb4 "x")
!(remove-atom &kb4 "initial-state")
!(add-atom &kb4 "y")
;; Final state should have "x" and "y" only
!(assertEqualToResult (get-atoms &kb4) ("x" "y"))
;; Query to test the presence of "x" and "y"
!(assertTrue (query &kb4 "x"))
!(assertTrue (query &kb4 "y"))




