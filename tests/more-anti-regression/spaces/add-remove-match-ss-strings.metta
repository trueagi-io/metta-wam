;;;;;;;;;;;;;;;;;;;;;;
; Adding Atoms to the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb0 (new-space))
!(add-atom &kb0 (("S" "a")))
!(add-atom &kb0 (("S" "b")))
;MeTTaLog Only: !(assertEqual (atom-count &kb0) 2)
!(assertEqualToResult (get-atoms &kb0) ((("S" "a")) (("S" "b"))))

;;;;;;;;;;;;;;;;;;;;;;
; Removing Atoms from the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb1 (new-space))
!(add-atom &kb1 (("S" "a")))
!(add-atom &kb1 (("S" "b")))
!(add-atom &kb1 (("S" "c")))
;; Remove an atom and test for success
!(assertTrue (remove-atom &kb1 (("S" "b")))) ; "remove_atom on a present atom should return true"
;; Attempt to remove a non-existent atom and test for failure
!(assertFalse (remove-atom &kb1 (("S" "bogus")))) ; "remove_atom on a missing atom should return false"
;; Verify the current state of the knowledge base
!(assertEqualToResult (get-atoms &kb1) ((("S" "a")) (("S" "c"))))

;;;;;;;;;;;;;;;;;;;;;;
; Replacing Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb2 (new-space))
(= (replace-atom $space $before $after) (if (remove-atom $space $before) (add-atom $space $after) False))
!(add-atom &kb2 (("S" "a")))
!(add-atom &kb2 (("S" "b")))
!(add-atom &kb2 (("S" "c")))
;; Replace an atom and verify the operation was successful
!(assertTrue (replace-atom &kb2 (("S" "b")) (("S" "d"))))
;; Check the new set of atoms in the knowledge base
!(assertEqualToResult (get-atoms &kb2) ((("S" "a")) (("S" "d")) (("S" "c"))))

;;;;;;;;;;;;;;;;;;;;;;
; Querying Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb3 (new-space))
!(add-atom &kb3 (E (("S" "A")) (("S" "B"))))
!(add-atom &kb3 (E (("S" "C")) (("S" "D"))))
;; Adding a duplicate pattern for testing multiple matches
!(add-atom &kb3 (E (("S" "A")) (("S" "E"))))
;; Verify that the query returns the expected matches
!(assertEqualToResult (match &kb3 (E (("S" "A")) $XX) $XX) ((("S" "B")) (("S" "E"))))

;;;;;;;;;;;;;;;;;;;;;;
; Comprehensive Test with Add, Remove, Query
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb4 (new-space))
!(add-atom &kb4 (("S" "initial-state")))
;; Perform a sequence of operations
!(add-atom &kb4 (("S" "x")))
!(remove-atom &kb4 (("S" "initial-state")))
!(add-atom &kb4 (("S" "y")))
;; Final state should have "x" and "y" only
!(assertEqualToResult (get-atoms &kb4) ((("S" "x")) (("S" "y"))))
;; Query to test the presence of "x" and "y"
!(assertTrue (query &kb4 (("S" "x"))))
!(assertTrue (query &kb4 (("S" "y"))))





