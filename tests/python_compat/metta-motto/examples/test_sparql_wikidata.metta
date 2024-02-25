!(import! &self motto)
!(set-sparql-service-type "wikidata")
;sparql query to wikidata which lists names and places of birth of all Nobel laureates in literature over the past 10 years

!(select ((fields ($laureateLabel $placeOfBirthLabel $when))
(where
    (($laureate p:P166 $event)
    ($laureate  wdt:P19 $placeOfBirth) ($event ps:P166 wd:Q37922 )
    ($event pq:P585 $when)
    (filter (>= (year $when) (- (year (now)) 10)))
    (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
    )
)
(order_by $when) (limit 1)))

;List all actors born in US after 2000
!(select ((fields ($actor $actorLabel))
(where
    (($actor wdt:P106 wd:Q33999) ($actor wdt:P569 $dob)
    ($actor wdt:P27 wd:Q30)
    (filter (> (year $dob) 2000 ))
    (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
    )
)
(limit 2)))


;Sculptures by Max Bill
!(select ((fields ($item $itemLabel $countryLabel $placeLabel (as (year $date) $year) $coords $image))
(where (($item wdt:P31/wdt:P279* wd:Q860861) ($item wdt:P170 wd:Q123454) (optional ($item wdt:P17 $country))
    (optional ($item wdt:P131 $place))(optional ($item wdt:P571 $date))  (optional ($item wdt:P625 $coords))
    (optional ($item wdt:P18 $image))
    (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
))
(order_by ($itemLabel $placeLabel)) (limit 3)))
