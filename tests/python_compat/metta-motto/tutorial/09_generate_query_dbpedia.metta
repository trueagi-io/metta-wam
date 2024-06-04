!(import! &self motto)
(llm (Agent 09_generate_query_dbpedia.msa)(user "List all known works of Vivaldi"))
;result
(select
    (
        (fields ($work))
        (where
            (
            ($composer foaf:name "'Antonio Vivaldi'@en")
            ($work dbo:composer $composer)
            )
        )
    ))

;correct
(select
    (
        (fields ($work))
        (where
            (
            ($composer rdfs:label  'Antonio Vivaldi'@en )
            ($work dbo:musicComposer $composer)
            )
        )
    ))



(llm (Agent 09_generate_query_dbpedia.msa)(user "Who wrote the book 'To the Lighthouse'"))
;result
(select
    (
        (fields ($author))
        (where
            (
            ($book dbo:author $author)
            ($book foaf:name "'To the Lighthouse'@en")
            )
        )
    ))

(llm (Agent 09_generate_query_dbpedia.msa)(user "Which of the actors who played in the TV series 'Game of Thrones' was born in London?"))
;result
(select
    (
        (fields ($actor))
        (where
            (
            ($actor dbo:birthPlace "<http://dbpedia.org/resource/London>")
            ($actor dbo:starring <http://dbpedia.org/resource/Game_of_Thrones>)
            )
        )
    ))
;looks correct but there is no dbo:starring for Game_of_Thrones

(llm (Agent 09_generate_query_dbpedia.msa)(user "List all writers born in London after 1780 year and before 1800"))
;result
(select
    (
        (fields ($writer $writerLabel))
        (where
            (
            ($writer a dbo:Writer)
            ($writer dbo:birthPlace "<http://dbpedia.org/resource/London>")
            ($writer dbo:birthDate $birthdate)
            ($writer rdfs:label $writerLabel)
            (filter (and (>  (year $birthdate) 1780) (<=  (year $birthdate) 1800)))
            )
        )
        (order_by $writer)
    )
)

(llm (Agent 09_generate_query_dbpedia.msa)(user "List  names and length (in kilometres) of rivers located in Africa continent"))
;incorrect result
(select
    (
        (fields ($river $length))
        (where
            (
            ($river rdf:type "<http://dbpedia.org/ontology/River>")
            ($river dbo:locatedIn "<http://dbpedia.org/resource/Africa>")
            ($river dbo:length $length)
            )
        )
))


(llm (Agent 09_generate_query_dbpedia.msa)(user "When did the first season of 'True Detective' come out?"))
;result
(select
    (
        (fields ($tvShow $releaseDate))
        (where
            (
            ($tvShow foaf:name "'True Detective'@en")
            ($tvShow dbo:releaseDate $releaseDate)
            )
        )
    ))

;correct result
(select
    (
        (fields ($tvShow $releaseDate))
        (where
            (
            ($tvShow rdfs:label "'True Detective'@en")
            ($tvShow dbo:releaseDate $releaseDate)
            )
        )
    ))

(llm (Agent 09_generate_query_dbpedia.msa)(user "What is the birth name of John F. Kennedy"))
;result
(select
    (
        (fields ($person $birthName))
        (where
            (
            ($person foaf:name "'John F. Kennedy'@en")
            ($person dbo:birthName $birthName)
            )
        )
    ))

(llm (Agent 09_generate_query_dbpedia.msa)(user "Who is the director of the film 'The Grand Budapest Hotel'"))
;result
(select
    (
        (fields ($director))
        (where
            (
            ($film foaf:name "'The Grand Budapest Hotel'@en")
            ($film dbo:director $director)
            )
        )
    ))
;correct result
(select
    (
        (fields ($director))
        (where
            (
            ($film rdfs:label "'The Grand Budapest Hotel'@en")
            ($film dbp:director $director)
            )
        )
    ))

(llm (Agent 09_generate_query_dbpedia.msa)(user "Which team is football payer Neymar is playing now?"))
;result
(select
    (
        (fields ($player $team))
        (where
            (
            ($player foaf:name "'Neymar'@en")
            ($player dbo:team $team)
            )
        )))

!(llm (Agent 09_generate_query_dbpedia.msa)(user "When did Jodie Foster won her first Academy Award?"))
;incorrect result
(select
    (
        (fields ($person $awardYear))
        (where
            (
            ($person foaf:name "'Jodie Foster'@en")
            ($person dbo:award "<http://dbpedia.org/resource/Academy_Award>")
            ($person dbo:awardYear $awardYear)
            )
        )
    )
)

