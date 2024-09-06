!(import! &hald116378 hald_spo_116378.metta)
;  !(shell! "grep Doxorubicin hald_spo_116378.metta | grep \\(Doxorubicin | wc -l")
!(assertEqualToResult (length-of (match &hald116378 (Doxorubicin $p $o) (Doxorubicin $p $o))) (96))
