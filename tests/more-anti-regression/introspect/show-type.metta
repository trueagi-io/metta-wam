;!(pragma! interpreter bare-minimal) 

(: println! (-> Atom Atom))

(: show-type (-> Atom Atom))

(= (show-type $obj)
    (let $type 
	    (get-type $obj) (: $obj $type)))

!(show-type get-atoms)
!(show-type println!)
!(show-type pragma!)
!(show-type bind!)
!(show-type ==)
!(show-type if)
!(show-type import!)
!(show-type match)
!(show-type let*)
!(show-type let)
!(show-type quote)
!(show-type eval)
!(show-type function)
!(show-type return)


(: : (-> Atom Atom))
(: : (-> Atom Atom Atom))
(: : (-> Atom Atom Atom Atom))
(: : (-> Atom Atom Atom Atom Atom))
(: : (-> Atom))
!(show-type :)


(: = (-> Atom Atom))
(: = (-> Atom Atom Atom))
(: = (-> Atom Atom Atom Atom))
(: = (-> Atom Atom Atom Atom Atom))
(: = (-> Atom))

!(show-type =)


(: -> (-> Atom Atom))
(: -> (-> Atom Atom Atom))
(: -> (-> Atom Atom Atom Atom))
(: -> (-> Atom Atom Atom Atom Atom))
(: -> (-> Atom))
!(show-type ->)

