(: t-expression (-> Expression Atom))
(= (t-expression $x) (quote $x))
!(t-expression  (parse "x_string_tag"))
;[(quote (parse "x_string_tag"))]

(: t-atom (-> Atom Atom))
(= (t-atom $x) (quote $x))
!(t-atom  (parse "x_string_tag"))
;[(quote (parse "x_string_tag"))]

(: t-any (-> $_ Atom))
(= (t-any $x) (quote $x))
!(t-any (parse "x_string_tag"))
;[(quote x_string_tag)]

(: t-undef (-> %Undefined% Atom))
(= (t-undef $x) (quote $x))
!(t-undef  (parse "x_string_tag"))
;[(quote x_string_tag)]

(: t-a (-> $t $t))
(= (t-a $x) (quote $x))
!(t-a  (parse "x_string_tag"))
;[(quote x_string_tag)]

(: t-symbol (-> Symbol Atom))
(= (t-symbol $x) (quote $x))
!(t-symbol  (parse "x_string_tag"))
;[(quote x_string_tag)]

!(bind! x1_bind y1_value)
;[()]
!(t-symbol  (parse "x1_bind"))
;[(quote y1_value)]

!(bind! y1_value z1_value_value)
;[()]
!(t-symbol  (parse "x1_bind"))
;[(quote y1_value)]

!(t-symbol  (parse "y1_value"))
;[(quote z1_value_value)]

