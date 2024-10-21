
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 34../../../hyperon-experimental/python/tests/test_minecraft.py :42
; <pre>

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
        
;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br>  lastline 65 <br> 
