!(import! &self motto)
!(set-sparql-service-type "wikidata")
(llm (Agent 10_generate_query_wikidata.msa)(user "List all known works of Vivaldi"))
;$result
(select ((fields ($workLabel))
        (where
            (
            ($composer rdfs:label "'Antonio Vivaldi'@en") ($work wdt:P86 $composer)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
            )
        )
    ))

(llm (Agent 10_generate_query_wikidata.msa)(user "Who wrote the book 'To the Lighthouse'"))
;result
(select ((fields ($authorLabel))
        (where
            (($book rdfs:label "'To the Lighthouse'@en") ($book wdt:P50 $author)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
            )
        )
    ))

(llm (Agent 10_generate_query_wikidata.msa)(user "Which of the actors who played in the TV series 'Game of Thrones' was born in London?"))
;result
(select ((fields ($actorLabel))
        (where
            (
            ($actor wdt:P106 wd:Q33999) ($actor wdt:P19 wd:Q84) ($actor wdt:P569 $dob)
            ($actor wdt:P27 wd:Q145) ($actor wdt:P161 wd:Q25585)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
            )
        )
    ))
;correct result
(select ((fields ($actorLabel))
        (where
            (($actor wdt:P106 wd:Q33999) ($actor wdt:P19 wd:Q84)
            (filter_exists (wd:Q23572 wdt:P161 $actor ))
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
            )
        )))

(llm (Agent 10_generate_query_wikidata.msa)(user "List all writers born in London after 1780 year and before 1800"))
;result
(select ((fields ($actorLabel))
        (where
            (
            ($actor wdt:P106 wd:Q33999) ($actor wdt:P19 wd:Q84) ($actor wdt:P569 $dob)
            ($actor wdt:P27 wd:Q145) ($actor wdt:P161 wd:Q25585)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
            )
        )
 ))

(llm (Agent 10_generate_query_wikidata.msa)(user "List  names and length (in kilometres) of rivers located in Africa continent"))
;result
(select
  (
    (fields ($riverLabel $length))
    (where
      (
        ($river wdt:P31 wd:Q4022) ($river wdt:P17 wd:Q15) ($river wdt:P2043 $length)
        (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
      )
    )
  )
)
;correct result
(select
  (
    (fields ($riverLabel $length))
    (where
      (
        ($river wdt:P31 wd:Q4022) ($river wdt:P30 wd:Q15) ($river wdt:P2043 $length)
        (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
      )
    )
  )
)

(llm (Agent 10_generate_query_wikidata.msa)(user "When did the first season of 'True Detective' come out?"))
;result
(select
(
    (fields ($releaseDateLabel))
    (where
        (
            ($tvShow rdfs:label "'True Detective'@en") ($tvShow wdt:P31 wd:Q5398426) ($tvShow wdt:P571 $releaseDate)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))

;correct result
(select
(
    (fields ($releaseDateLabel))
    (where
        (
            ($tvShow rdfs:label "'True Detective'@en") ($tvShow wdt:P31 wd:Q5398426) ($tvShow wdt:P580 $releaseDate)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))

(llm (Agent 10_generate_query_wikidata.msa)(user "What is the birth name of John F. Kennedy"))
;result
(select
(
    (fields ($birthNameLabel))
    (where
        (
            ($person rdfs:label "'John F. Kennedy'@en")
            ($person wdt:P1477 $birthName)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))

(llm (Agent 10_generate_query_wikidata.msa)(user "Who is the director of the film 'The Grand Budapest Hotel'"))
;result
(select
(
    (fields ($directorLabel))
    (where
        (
            ($film wdt:P31 wd:Q11424) ($film rdfs:label "'The Grand Budapest Hotel'@en")
            ($film wdt:P57 $director)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))

(llm (Agent 10_generate_query_wikidata.msa)(user "Which team is football payer Neymar is playing now?"))
;result
(select
(
    (fields ($teamLabel))
    (where
        (
            ($player wdt:P106 wd:Q937857) ($player wdt:P54 $team) ($player rdfs:label "'Neymar'@en")
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))

!(llm (Agent 10_generate_query_wikidata.msa)(user "When did Jodie Foster won her first Academy Award?"))
;result
(select
(
    (fields ($awardYearLabel))
    (where
        (
            ($person rdfs:label "'Jodie Foster'@en")
            ($person wdt:P166 $award)
            ($award wdt:P585 $awardYear)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))
;correct
(select
(
    (fields ($awardYearLabel))
    (where
        (
            ($person rdfs:label "'Jodie Foster'@en")
            ($person p:P166 $award)
            ($award ps:P166 wd:Q103618)
            ($award pq:P585 $awardYear)
            (service (wikibase:label (bd:serviceParam wikibase:language "'en'")))
        )
    )
))