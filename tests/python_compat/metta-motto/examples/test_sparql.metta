!(import! &self motto)

;People who were born in Berlin before 1900
;http://sites.linkeddata.center/help/devop/examples/sparql-examples

!(select ((fields ($name $birth $death $person )) (where (($person dbo:birthPlace dbr:Berlin) ($person dbo:birthDate $birth) ($person foaf:name $name)
($person dbo:deathDate $death) (filter (< $birth  "'1900-01-01'^^xsd:date")))) (order_by $name)
(limit 3)))


; Soccer players, who are born in a country with more than 10 million inhabitants,
; who played as goalkeeper for a club that has a stadium with more than 30.000 seats and the club country is different from the birth country
; http://sites.linkeddata.center/help/devop/examples/sparql-examples

!(select_distinct ((fields ($soccerplayer $countryOfBirth $team $countryOfTeam $stadiumcapacity )) (where (($soccerplayer a dbo:SoccerPlayer)
($soccerplayer  dbo:position "<http://dbpedia.org/resource/Goalkeeper_(association_football)>")
($soccerplayer  dbp:position "<http://dbpedia.org/resource/Goalkeeper_(association_football)>")
($soccerplayer dbo:birthPlace/dbo:country* $countryOfBirth)
($soccerplayer dbo:team $team)
($team dbo:capacity $stadiumcapacity)
($team dbo:ground $countryOfTeam)
($countryOfBirth a dbo:Country)
($countryOfBirth dbo:populationTotal $population)
($countryOfTeam a dbo:Country)
(filter (!= $countryOfTeam  $countryOfBirth))
(filter (> $stadiumcapacity 30000)) (filter (> $population 10000000))))  (order_by  $soccerplayer)(limit 3))
)

;Display names and dates of birth of all actors born in London after year 1930. who are still alive
;http://ai.fon.bg.ac.rs/wp-content/uploads/2016/10/SPARQL-examples-2016-eng.pdf

!(select ((fields ($name $birth) )(where (($person dbo:birthPlace dbr:London) ($person dbo:occupation dbr:Actor) ($person foaf:name $name)
($person dbo:birthDate $birth)
(filter (> $birth  "'1930-01-01'^^xsd:date"))
(filter_not_exists ($person dbo:deathDate $death)) )) (limit 3)))

;Display names and dates of birth of all actors born in London after year 1930. who are still alive
;takes into account cases where the occupation is given as a plain string

!(select ((fields ($name $birth)) (where (($person dbo:birthPlace dbr:London) ($person foaf:name $name)
(sparql_union (($person dbo:occupation dbr:Actor) ($person dbp:occupation $occupation)
(filter (or (contains (str $occupation) 'Actor')  (contains (str $occupation) 'Actress') ) )
))
($person dbo:birthDate $birth)
(filter (> $birth "'1930-01-01'^^xsd:date"))
(filter_not_exists ($person dbo:deathDate $death)) ) ) (limit 3)))

;Display names and dates of birth and death of all people born in
;London between 1900-1950; sort the results based on the birth date

!(select ( (fields ($name $birth)) (where (($person dbo:birthPlace dbr:London) ($person dbo:birthDate $birth)
($person rdfs:label $name)
(filter (= (lang $name) 'en'))
(optional ($person dbo:deathDate $death ))
(filter (and (> $birth "'1900-01-01'^^xsd:date")  (< $birth "'1950-01-01'^^xsd:date")))))
   (order_by ($birth))(limit 3)))

;Find manufacturers who produce more than 10 different products and display the number of different products they produce
;http://ai.fon.bg.ac.rs/wp-content/uploads/2015/04/SPARQL_Dec-2014_eng.pdf
!(select ((fields ($manufacturer (as (count $product) $count))) (where (($product rdf:type <http://dbpedia.org/ontology/Device>)
($product dbo:manufacturer $manufacturer))) (group_by $manufacturer) (having (> (count $product) 10)) limit 3))


;Tell me whatever you'd like to tell me about the Ford Motor Company.
!(describe ((fields $ford) (where ( ($ford foaf:name "'FORD MOTOR CO'") ))))


; test group_concat
!(select ((fields ($name (as (group_concat $birth (= separator '|')) $bth))) (where (($person dbo:birthPlace dbr:Dublin)
($person dbo:birthDate $birth) ($person dbp:name  $name) (filter ( = (lang $name) 'en'))
(filter (contains (str $name) 'Murphy')))) (group_by  $name) (order_by $name) (limit 3)))



