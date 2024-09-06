!(import! &hald100 hald_spo_100.metta)
!(assertEqualToResult 
    (match &hald100 (Doxorubicin $p $o) (Doxorubicin $p $o))
    ((Doxorubicin stratify Liposarcoma)))
