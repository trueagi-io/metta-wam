
!(extend-py! src/mettalog)
!(print "; =====================================")
!(print "; Starting 6 - Flybase Full")
!(print "; =====================================")
!(load_vspace)
!(use-mettalog)
;!(pragma! trace-on-load False)



; Includes the file that houses queries that have been deduced from available data.
; Useful for generating more complex queries.
!(import! &self examples/flybase/flybase-deduced-queries.metta)

; Includes the file where all deduced facts and data are stored.
; Provides a higher layer of abstraction and complexity.
!(import! &self examples/flybase/flybase-deduced.metta)

; Includes the file that manages the connections between various deduced facts.
; Useful for tracing relationships and making further deductions.
!(import! &self examples/flybase/flybase-deduced-connections.metta)

; Includes the file that holds information about the types of deduced data.
; Helpful for type-checking and ensuring data integrity.
!(import! &self examples/flybase/flybase-deduced-types.metta)

; Includes the file that manages the versioned space (vspace) within Flybase.
; Keeps track of the different versions of the database or logic layer.
!(import! &self examples/flybase/flybase-vspace.metta)

; Includes the file used to estimate the size that the loader will require.
; Useful for resource allocation and optimization.
!(import! &self examples/flybase/flybase-loader-size-estimates.metta)

; Includes the file responsible for loading data into Flybase.
; This includes both mined and deduced data.
;!(import! &self examples/flybase/flybase-loader.metta)

; The following files are commented out:

; Would include the file containing flattened mined data.
; Simplifies the complexity and allows for easier querying and data manipulation.
;!(import! &self examples/flybase/flybase-mined-flat.metta)

; Would include the file containing mined data.
; Forms the basis for further deductions and queries.
;!(import! &self examples/flybase/flybase-mined.metta)

;;;
(vspace test 1)
;!(extend-py! mettalog)
!(load-flybase-full)

!(print "; =====================================")
!(print "; Starting Shell !!!!!!!!!!!!!!!!!!!!!!")
!(print ";=====================================")

(: my-match (-> Atom Atom Atom Atom))
(= (my-match $a $b $c) (match $a $b $c))
!(match &self $s $s)
!(my-match &self $s $s)
!(mettalog::vspace-main)

