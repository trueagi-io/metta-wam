!(import! &self motto)

; This is another PoC example of LLM-MeTTa interoperability

; Here, we have a set of statements in natural language
(Fact "Sam is a frog")
(Fact "Any frog is not a helicopter")
(Fact "Any alien likes helicopter")
(Fact "Arilou is an alien")
(Fact "Sim is a helicopter")

; And we process them with the use of ChatGPT-based MeTTa-agent
; to put parsed statements into the space of this script
; Check the output to see if all the facts are processed correctly
!(match &self (Fact $fact)
   (let* (($expr ((metta-agent "06b_reason.msa") (user $fact)))
          (() (println! (Adding: $expr)))) ; print what is added
         (add-atom &self $expr)))

; Statements are processed into a functional form for the sake of simplicity -
; no full-fledged reasoning engine is used, so the example is limited.

; The function to retrieve results
(= (whom $what $A)
   (if (and ($what $A $C) ($C $B))
       $B (empty)))

; The following works from time to time, but ChatGPT is not stable,
; so multiple runs might be needed

; We can evaluate MeTTa expressions in the context of the formed KB
!(whom likes Arilou)

; Or we can transform natural language questions into MeTTa expressions
; and evaluate them
!((metta-agent "06b_reason.msa") (system "Question") (user "Is Sam a helicopter?"))
!((metta-agent "06b_reason.msa") (user "Is Sam a frog?"))
