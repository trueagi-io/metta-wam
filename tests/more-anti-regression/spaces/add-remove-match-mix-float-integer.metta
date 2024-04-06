
;; !(let* (( ($a $b) (collapse (get-atoms &self)))) (get-atoms (superpose ($a $b))))
;;;;;;;;;;;;;;;;;;;;;;
; Adding Atoms to the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb0 (new-space))
!(add-atom &kb0 1)
!(add-atom &kb0 2)
;MeTTaLog Only: !(assertEqual (atom-count &kb0) 2)
!(assertEqualToResult (get-atoms &kb0) (1 2))

;;;;;;;;;;;;;;;;;;;;;;
; Removing Atoms from the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb1 (new-space))
!(add-atom &kb1 1)
!(add-atom &kb1 2)
!(add-atom &kb1 3)
;; Remove an atom and test for success
!(remove-atom &kb1 2) ; "remove_atom on a present atom should return true"
;; Attempt to remove a non-existent atom and test for failure
!(remove-atom &kb1 "bogus") ; "remove_atom on a missing atom should return false"
;; Verify the current state of the knowledge base
!(assertEqualToResult (get-atoms &kb1) (1 3))

;;;;;;;;;;;;;;;;;;;;;;
; Replacing Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb2 (new-space))
(: replace-atom (-> hyperon::space::DynSpace Atom Atom %Undefined%))
(= (replace-atom $space $before $after) 
    (let () (remove-atom $space $before) 
	  (add-atom $space $after)))
!(add-atom &kb2 1)
!(add-atom &kb2 2)
!(add-atom &kb2 3)
;; Replace an atom and verify the operation was successful
!(replace-atom &kb2 2 4)
;; Check the new set of atoms in the knowledge base
!(assertEqualToResult (get-atoms &kb2) (1 4 3))

;;;;;;;;;;;;;;;;;;;;;;
; Querying Atoms in the Knowledge Base
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb3 (new-space))
!(add-atom &kb3 ( -1 -2))
!(add-atom &kb3 ( -3 -4))
;; Adding a duplicate pattern for testing multiple matches
!(add-atom &kb3 ( -1 -5))
;; Verify that the query returns the expected matches
!(assertEqualToResult (match &kb3 ( -1 $XX) $XX) (-2 -5))

;;;;;;;;;;;;;;;;;;;;;;
; Comprehensive Test with Add, Remove, Query
;;;;;;;;;;;;;;;;;;;;;;
!(bind! &kb4 (new-space))
!(add-atom &kb4 6)
!(add-atom &kb4 6.0)
!(add-atom &kb4 6)
!(add-atom &kb4 (6))
!(add-atom &kb4 (6.0))
!(add-atom &kb4 7)
!(assertEqual (collapse (get-atoms &kb4))  (6 6 6 (6) (6.0) 7))
!(remove-atom &kb4 (6.0))
!(assertEqual (collapse (get-atoms &kb4))  (6 6 6 7))
;; Query to test the presence of 6 and 7
!(assertEqual (match &kb4 7 True) True)
!(assertEqual (match &kb4 6.0 True) (superpose (True True True)))
!(assertEqual (match &kb4 6 True) (superpose (True True True)))

