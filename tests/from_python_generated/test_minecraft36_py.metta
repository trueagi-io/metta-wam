
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 36../../../hyperon-experimental/python/tests/test_minecraft.py :77
; <pre>
;        metta.run('''
 

 (= (wood) (spruce-wood))
      (= (spruce-wood) (mine spruce-tree hand))

      (= (four-planks) (craft four-planks inventory (wood)))
      (= (pack $n planks) (if (> $n 0) (allof (four-planks) (pack (- $n 4) planks)) nop))
      (= (crafting-table) (craft crafting-table inventory  (pack 4 planks)))

      (= (stick) (craft stick inventory (pack 2 planks)))
      (= (pack $n sticks) (if (> $n 0) (allof (stick) (pack (- $n 1) sticks)) nop))

      (= (wooden-pickaxe) (craft wooden-pickaxe
                           (crafting-table) (allof (pack 3 planks) (pack 2 sticks))))

      (= (cobblestone) (mine cobble-ore (wooden-pickaxe)))
      (= (pack $n cobblestones) (if (> $n 0) (allof (cobblestone) (pack (- $n 1) cobblestones)) nop))
      (= (stone-pickaxe) (craft stone-pickaxe (crafting-table)
                           (allof (pack 3 cobblestones) (pack 2 sticks))))
;!(wooden-pickaxe)

            (= (can-be-mined diamond) True)
            (= (can-be-made diamond) False)
            (= (diamond mined-using iron-pickaxe) True)
            (= (diamond mined-from diamond-ore) True)

            (= (can-be-made iron-pickaxe) True)
            (= (can-be-mined iron-pickaxe) False)
            (= (iron-pickaxe made-from
                (, stick stick iron-ingot iron-ingot iron-ingot)) True)
            (= (iron-pickaxe made-at crafting-table) True)

            (= (can-be-made crafting-table) True)
            (= (can-be-mined crafting-table) False)
            (= (crafting-table made-from (pack 4 plank)) True)
            (= (crafting-table made-at inventory) True)

            (= (can-be-made inventory) False)
            (= (can-be-mined inventory) False)

            (= (make $x) (if (and ($x made-from $comp) ($x made-at $tool))
                             (, (get $tool) (get $comp) (do-make $x $tool $comp)) (empty)))

            (= (mine $x) (if (and ($x mined-using $tool) ($x mined-from $source))
                 (, (get $tool) (find $source) (do-mine $x $source $tool)) (empty)))

!(get diamond)

;  self.assertEqual(repr(output),
 ;           '[(do-mine ((: stone type) (: stone variant)))]')

;!(do-mine ((: stone type) (: stone variant)))

;</pre>
;  !(assertEqualToResult ( )  (  )  )
;</pre>
;  !(assertEqualToResult ( )  (  )  )

;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br>  lastline 107 <br> 
