; Test File: Partitioning Atomspaces and Performance in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/774

; Example: Partitioning Atomspaces by User and Managing Knowledge Bases
; ---------------------------------------------------------------------

; Create a new space for testing
!(bind! &test-space (new-space))

; Define functions for managing user-specific knowledge bases
(= (add-user $name)
   (let $space (new-space)
     (add-atom &test-space (User $name $space))))

(= (write-user $name $data)
   (let $space (match &test-space (User $name $s) $s)
     (add-atom $space $data)))

(= (read-user $name $query $template)
   (let $space (match &test-space (User $name $s) $s)
     (match $space $query $template)))

; Test 1: Add user-specific knowledge bases
!(add-user Alice)
!(add-user Bob)

; Test: Verify user knowledge bases exist
!(assertEqualToResult
   (match &test-space (User $name $space) $name)
   (Alice Bob))

; Test 2: Write data to user-specific knowledge bases
!(write-user Alice (Age 20))
!(write-user Bob (Age 22))

; Test: Verify data written to Alice's knowledge base
!(assertEqualToResult
   (read-user Alice (Age $age) (Age Alice $age))
   (Age Alice 20))

; Test: Verify data written to Bob's knowledge base
!(assertEqualToResult
   (read-user Bob (Age $age) (Age Bob $age))
   (Age Bob 22))

; Example: Manage Atomspaces for Thematic Partitioning
; ----------------------------------------------------

; Define functions to add, retrieve, and manage partitioned knowledge
(= (add-partition $name)
   (let $space (new-space)
     (add-atom &test-space (Partition $name $space))))

(= (write-partition $name $data)
   (let $space (match &test-space (Partition $name $s) $s)
     (add-atom $space $data)))

(= (read-partition $name $query $template)
   (let $space (match &test-space (Partition $name $s) $s)
     (match $space $query $template)))

; Test 3: Add partitioned knowledge bases
!(add-partition People)
!(add-partition Games)

; Test: Verify partitions exist
!(assertEqualToResult
   (match &test-space (Partition $name $space) $name)
   (People Games))

; Test 4: Write and read data from a partitioned knowledge base
!(write-partition People (Friend Bob Alice))
!(write-partition Games (GameRole Predator))

; Test: Verify data in People partition
!(assertEqualToResult
   (read-partition People (Friend $x $y) (Friend $x $y))
   (Friend Bob Alice))

; Test: Verify data in Games partition
!(assertEqualToResult
   (read-partition Games (GameRole $role) (GameRole $role))
   (GameRole Predator))
