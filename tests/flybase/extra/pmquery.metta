(tad "chr16_23000000_23800000_GRCh38")
(has-property (tad "chr16_23000000_23800000_GRCh38") chr "chr16")
(has-property (tad "chr16_23000000_23800000_GRCh38") start 23000000)
(has-property (tad "chr16_23000000_23800000_GRCh38") end 23800000)
(has-property (tad "chr16_23000000_23800000_GRCh38") genes ("ENSG00000227868" "ENSG00000004487" "ENSG00000169641" "ENSG00000179546" "ENSG00000125944" "ENSG00000125945" "ENSG00000204219" "ENSG00000088280" "ENSG00000007968" "ENSG00000117318" "ENSG00000142676" "ENSG00000011007" "ENSG00000057757" "ENSG00000011009" "ENSG00000117308"))

(has-property (sequence_variant "chr16_10000_T_A_GRCh38") position 23100000)
(has-property (sequence_variant "chr16_10000_T_A_GRCh38") chr "chr16")


;; Give me the tad's on chr C where V is at.
;; Given a variant V, find the genelist of tad T near V, 
(= (query $v)
(match &self (, (has-property (sequence_variant $v) position $z)
              (has-property (sequence_variant $v) chr $chr)  
              (has-property (tad $t) chr $chr)
              (has-property (tad $t) start $x) 
              (has-property (tad $t) end $y))
            (if (or (< $x $z) (> $y $z))
                (match &self (has-property (tad $t) genes $genelist) $genelist)
                (empty))))

!(query "chr16_10000_T_A_GRCh38")