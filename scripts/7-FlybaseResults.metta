


;; !(extend-py! mettalog)

!(ensure-loaded! "./whole_flybase.qlf")

(=  (smatch $Query) (match &self  $Query $Query))
(=  (ematch $Query) (eval &self $Query))

(=  (mmatch $Query) (, (is-literal $Q) (smatch
         (= $_ (eval &self  $Query)))



