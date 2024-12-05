!(pragma! time debug)
!(import! &hald5000 hald_spo_5000)
!(assertEqualToResult 
   (match &hald5000 (Doxorubicin $p $o) (Doxorubicin $p $o))
   ((Doxorubicin elevated Trastuzumab) (Doxorubicin associated MIR375) (Doxorubicin stratify Liposarcoma)))
