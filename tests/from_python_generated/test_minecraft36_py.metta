
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 36../../../hyperon-experimental/python/tests/test_minecraft.py :77
; <pre>
        metta.run('''
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

;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br>  lastline 107 <br> 
