from hyperon import MeTTa

def test_select():

    m = MeTTa()
    #m.load_module_at_path('motto:sparql_gate')
    m.run("!(import! &self motto)")
    result =  m.run('''
        !(select (
            (fields ($name $birth))
            (where (($person dbo:birthPlace dbr:London)
                    ($person dbo:birthDate $birth)
                    ($person dbp:name $name)
                    (filter (= (lang $name) 'en'))
            (optional ($person dbo:deathDate $death ))
            (filter (and (> $birth "'1900-01-01'^^xsd:date")
                         (< $birth "'1950-01-01'^^xsd:date")))))
            (order_by ($birth)) (limit 3)))
    ''', True)
    correct = [["Phil Scott", "1900-01-03"],
               ["Harry Kernoff", "1900-01-09"], ["Violette Cordery", "1900-01-10"]]
    length = len(result)
    # FIXME: results are returned twice
    for i in range(length):
        for item in result[i].get_object().value:
            assert item.get_object().value in correct[i % len(correct)]

