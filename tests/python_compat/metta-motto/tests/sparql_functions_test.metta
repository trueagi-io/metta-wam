!(import! &self motto)

;test filters
!(assertEqual
    (filter (not (bound $url)))
    "filter (!(bound($url)))")

!(assertEqual
    (filter (> $population  15000000))
    "filter ($population > 15000000)")

!(assertEqual
    (filter (or (contains (str $occupation) 'Actor')  (contains (str $occupation) 'Actress')))
    "filter (contains(str($occupation), 'Actor') || contains(str($occupation), 'Actress'))")

!(assertEqual
    (filter (and (> $date "'1968-10-1'^^xsd:date")  (< $date "'1968-10-30'^^xsd:date")))
    "filter ($date > '1968-10-1'^^xsd:date && $date < '1968-10-30'^^xsd:date)")

;test fields
!(assertEqual
  (fields ($country_name $population))
  "$country_name $population")

!(assertEqual
    (fields ($manufacturer (as (count $product) $count)))
    "$manufacturer (count($product) as $count)")

!(assertEqual
    (fields ($name (as (group_concat $birth (= separator '|')) $bth)))
    "$name (group_concat($birth; separator = '|') as $bth)")

;test having
!(assertEqual
    (having (> (count $subject)  1) )
    "having (count($subject) > 1)")

;test where
!(assertEqual
    (where (($person dbo:birthPlace dbr:London) ($person foaf:name $name)
    ($person dbo:birthDate $birth)
    (filter (> $birth  "'1930-01-01'^^xsd:date"))))
   "WHERE{
$person dbo:birthPlace dbr:London .
$person foaf:name $name .
$person dbo:birthDate $birth .
filter ($birth > '1930-01-01'^^xsd:date)}")


;test conditions
!(assertEqual
    (conditions (($person dbo:birthPlace dbr:London) ($person foaf:name $name)
    ($person dbo:birthDate $birth)
    (filter (> $birth  "'1930-01-01'^^xsd:date"))))
   "{
$person dbo:birthPlace dbr:London .
$person foaf:name $name .
$person dbo:birthDate $birth .
filter ($birth > '1930-01-01'^^xsd:date)}")

;test optional
!(assertEqual
    (optional (($X foaf:name $name) (filter (isLiteral $name))))
    "optional {$X foaf:name $name .
filter (isLiteral($name))}"
 )
;test sparql_union
!(assertEqual
     (sparql_union (( $process obo:part_of go:GO_0007165) ($process rdfs:subClassOf go:GO_0007165)))
     "{{$process obo:part_of go:GO_0007165} UNION {$process rdfs:subClassOf go:GO_0007165}}")
;test order by
!(assertEqual
    (order_by ($protons $weight))
    "order by $protons $weight")

;test order by
!(assertEqual
    (order_by_desc $protons)
    "order by desc($protons)")

;test group by
!(assertEqual
    (group_by ($protons $weight))
    "group by $protons $weight")

;test limit
!(assertEqual
    (limit 3)
    "limit 3")

;test offset
!(assertEqual
    (offset 3)
    "offset 3")

;test filter not exists
!(assertEqual 
    (filter_not_exists (($person bio:event $ev) ($ev rdf:type bio:Birth) ($ev bio:date $birthdate)))
"filter not exists {$person bio:event $ev .
$ev rdf:type bio:Birth .
$ev bio:date $birthdate}")

;test filter exists
!(assertEqual
    (filter_exists (($person bio:event $ev) ($ev rdf:type bio:Birth) ($ev bio:date $birthdate)))
"filter exists {$person bio:event $ev .
$ev rdf:type bio:Birth .
$ev bio:date $birthdate}")


(ask ((conditions ((<http://dbpedia.org/resource/Amazon_River> dbp:length $amazon) (<http://dbpedia.org/resource/Nile> dbp:length $nile )
    (filter (> $amazon $nile))))))

;test ASK
; Is the Amazon river longer than the Nile River?
!(assertEqualToResult
    (ask ((conditions ((<http://dbpedia.org/resource/Amazon_River> dbp:length $amazon) (<http://dbpedia.org/resource/Nile> dbp:length $nile )
    (filter (> $amazon $nile))))))
    ())





