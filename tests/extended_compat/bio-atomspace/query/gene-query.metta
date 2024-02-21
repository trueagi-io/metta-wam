; ;nodes 
 (tad chr16_1000_2000_GRCh38)
 (gene w)
 (gene x)
 (gene y)
 (gene z)
 (sequence_variant rs999)
 (sequence_variant rs1000)

; ;relationships
 (in-tad (tad chr16_1000_2000_GRCh38) (gene w))
 (chr (sequence_variant rs1000) chr16)
 (pos (sequence_variant rs1000) 1200)
 (rsid (sequence_variant rs1000) rs1000)
 (ref (sequence_variant rs1000) A)
 (alt (sequence_variant rs1000) C)

!(match &self ($type $id) (: (W ($type $id)) ($type $id)))


!(match &self ($pred $s $t) (: (W $pred $s $t) ($pred ($pred $s $t))))
