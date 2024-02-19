
print(";;  Doing HD...",__name__)

from hyperon import *

from mettalog import *


from hyperon_das import DistributedAtomSpace


def _name(link, index, typed = False):
    named_type = f"{link['targets'][index]['type']}:" if typed else ''
    return f"{named_type}{link['targets'][index]['name']}"

def _print_query_answer(query_answer, typed = False):
    if query_answer:
        for link in query_answer:
            if len(link['targets']) == 2:
                print(f"{link['type']}: {_name(link, 0)} -> {_name(link, 1)}")
            elif len(link['targets']) == 3:
                print(f"{link['type']}: {_name(link, 0)}({_name(link, 1, typed)}) -> {_name(link, 2, typed)}")
            else:
                assert False

host = '104.238.183.115'
port = '8081'

das = DistributedAtomSpace(query_engine='remote', host=host, port=port)

print(f"Connected to DAS at {host}:{port}")

def _filter(query_answer, index, value):
    filtered = []
    for link in query_answer:
        if link['targets'][index]['type'] == value:
            filtered.append(link)
    return filtered

def _fbgns(das, symbol, handles=False):
    answer = das.query({
        "atom_type": "link",
        "type": "Execution",
        "targets": [
            {"atom_type": "node", "type": "Schema", "name": "Schema:fb_synonym_primary_FBid"},
            {"atom_type": "node", "type": "Verbatim", "name": symbol},
            {"atom_type": "variable", "name": "v1"},
        ]
    })
    if handles:
        return [link['targets'][2]['handle'] for link in answer]
    else:
        return [link['targets'][2]['name'] for link in answer]

def query(das, symbol, node_type = None):
    fbgns = _fbgns(das, symbol)
    print(f"FBgn: {fbgns}")
    answer = []
    for fbgn in fbgns:
        query_answer = das.query({
            "atom_type": "link",
            "type": "Execution",
            "targets": [
                {"atom_type": "variable", "name": "v0"},
                {"atom_type": "variable", "name": "v1"},
                {"atom_type": "node", "type": "Verbatim", "name": fbgn},
            ]
        })
        if node_type:
            query_answer = _filter(query_answer, 1, node_type)
        answer.extend(query_answer)
        query_answer = das.query({
            "atom_type": "link",
            "type": "Execution",
            "targets": [
                {"atom_type": "variable", "name": "v0"},
                {"atom_type": "node", "type": "Verbatim", "name": fbgn},
                {"atom_type": "variable", "name": "v1"},
            ]
        })
        if node_type:
            query_answer = _filter(query_answer, 0, node_type)
        answer.extend(query_answer)
    return answer

_print_query_answer(query(das, "Myc", "BiologicalProcess"))


