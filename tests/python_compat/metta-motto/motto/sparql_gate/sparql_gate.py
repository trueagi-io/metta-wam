from hyperon import *
from hyperon.ext import register_atoms
from motto.utils import get_string_value

# TODO: requirement for SPARQLWrapper

class ServiceFeatures:
    def __init__(self, service_type):
        self.prefixes = {}
        self.service = ""
        service_type = repr(service_type).lower() if not isinstance(service_type, str) else service_type.lower()
        if service_type == "dbpedia":
            self.prefixes["dbo"] = "<http://dbpedia.org/ontology/>"
            self.prefixes["dbp"] = "<http://dbpedia.org/property/>"
            self.prefixes["dbr"] = "<http://dbpedia.org/resource/>"
            self.prefixes["rdf"] = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
            self.prefixes["rdfs"] = "<http://www.w3.org/2000/01/rdf-schema#>"
            self.prefixes["foaf"] = "<http://xmlns.com/foaf/0.1/>"
            self.prefixes["dc"] = "<http://purl.org/dc/elements/1.1/>"
            self.prefixes["dct"] = "<http://purl.org/dc/terms/>"
            self.prefixes["skos"] = "<http://www.w3.org/2004/02/skos/core#>"
            self.service = "https://dbpedia.org/sparql"

        elif service_type == "wikidata":
            self.prefixes["wd"] = "<http://www.wikidata.org/entity/>"
            self.prefixes["wds"] = "<http://www.wikidata.org/entity/statement/>"
            self.prefixes["wdv"] = "<http://www.wikidata.org/value/>"
            self.prefixes["wdt"] = "<http://www.wikidata.org/prop/direct/>"
            self.prefixes["wikibase"] = "<http://wikiba.se/ontology#>"
            self.prefixes["p"] = "<http://www.wikidata.org/prop/>"
            self.prefixes["ps"] = "<http://www.wikidata.org/prop/statement/>"
            self.prefixes["pq"] = "<http://www.wikidata.org/prop/qualifier/>"
            self.prefixes["rdfs"] = "<http://www.w3.org/2000/01/rdf-schema#>"
            self.prefixes["bd"] = "<http://www.bigdata.com/rdf#>"

            self.prefixes["wdref"] = "<http://www.wikidata.org/reference/>"
            self.prefixes["psv"] = "<http://www.wikidata.org/prop/statement/value/>"
            self.prefixes["psn"] = "<http://www.wikidata.org/prop/statement/value-normalized/>"
            self.prefixes["pqv"] = "<http://www.wikidata.org/prop/qualifier/value/>"
            self.prefixes["pqn"] = "<http://www.wikidata.org/prop/qualifier/value-normalized/>"
            self.prefixes["pr"] = "<http://www.wikidata.org/prop/reference/>"
            self.prefixes["prv"] = "<http://www.wikidata.org/prop/reference/value/>"
            self.prefixes["prn"] = "<http://www.wikidata.org/prop/reference/value-normalized/>"
            self.prefixes["wdno"] = "<http://www.wikidata.org/prop/novalue/>"
            self.prefixes["wdata"] = "<http://www.wikidata.org/wiki/Special:EntityData/>"

            self.prefixes["schema"] = "<http://schema.org/>"
            self.prefixes["rdf"] = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
            self.prefixes["owl"] = "<http://www.w3.org/2002/07/owl#>"
            self.prefixes["skos"] = "<http://www.w3.org/2004/02/skos/core#>"
            self.prefixes["xsd"] = "<http://www.w3.org/2001/XMLSchema#>"
            self.prefixes["prov"] = "<http://www.w3.org/ns/prov#>"
            self.prefixes["bds"] = "<http://www.bigdata.com/rdf/search#>"
            self.prefixes["gas"] = "<http://www.bigdata.com/rdf/gas#>"
            self.prefixes["hint"] = "<http://www.bigdata.com/queryHints#>"

            self.service = "https://query.wikidata.org/sparql"




class RdfHelper:
    complex_filters = ["filter_exists", "filter_not_exists", "sparql_union", "minus"]
    binary_operations_dict = {"+": "+", "-": "-", "*": "*", "/": "*", "=": "=", "!=": "!=", "<": "<", ">": ">",
                              "<=": "<=", ">=": ">=",
                              "or": "||", "and": "&&", "as": "as"}
    unary_operations_dict = {"not": "!"}

    output_options_functions = ['limit', 'offset', 'group by', 'order by']
    # all aggregate_functions are handled by 'fields' this array is not used in code
    aggregate_functions = ['count', 'sum', 'avg', 'min', 'max', 'group_concat', 'sample']

    def __init__(self, service_type="dbpedia"):
        self.set_service_type(service_type)

    def set_service_type(self, service_type):
        self.service_type = get_string_value(service_type).lower() if not isinstance(service_type, str) else service_type.lower()
        self.service_features = ServiceFeatures(self.service_type)
        return []

    @staticmethod
    def parse_functions_and_args(atom):
        '''
        :param atom:
        :return: converts expression like  (>  (count $product) 10)) into next: count($product) > 10
        '''
        if not hasattr(atom, 'get_children'):
            return get_string_value(atom)
        children = atom.get_children()
        if len(children) > 2:
            # for the case of binary operations like >,<, or, and
            key = get_string_value(children[0]).lower()
            if key in RdfHelper.binary_operations_dict:
                left = RdfHelper.parse_functions_and_args(children[1])
                right = RdfHelper.parse_functions_and_args(children[2])
                return left + " " + RdfHelper.binary_operations_dict[key] + " " + right
        args = ""
        last = len(children) - 1
        for i in range(last + 1):
            child = children[i]
            if not hasattr(child, 'get_children'):
                key = get_string_value(child)
                # takes into account unary operations
                args += RdfHelper.unary_operations_dict[
                    key] if key in RdfHelper.unary_operations_dict else get_string_value(child)
                if i == 0:
                    args += "("
                elif i < last:
                    atom_repr = get_string_value(children[0]).lower()
                    args += ", " if  atom_repr not in RdfHelper.aggregate_functions else "; "

            else:
                args += f"{RdfHelper.parse_functions_and_args(child)}"
                if i < last:
                    args += ", "
            if i == last:
                args += ")"
        return args

    @staticmethod
    def __filter_inner(atom):
        '''
        :param atom:
        :return: parses metta style function to sparql function  (contains $item1 $item2)  -> contains($item1, item2)
        '''
        if hasattr(atom, 'get_children'):
            result = RdfHelper.parse_functions_and_args(atom)
        else:
            result = f"({atom})"
        return result


    def filter(self, atom):
        result = RdfHelper.__filter_inner(atom)
        return [ValueAtom(f"filter ({result})")]

    def fields(self, atom):
        result = ""
        if hasattr(atom, 'get_children'):
            for child in atom.get_children():
                if hasattr(child, 'get_children'):
                    result += f"({RdfHelper.parse_functions_and_args(child)}) "
                else:
                    result += f"{child}" + " "
        else:
            result = f"{atom}"
        return [ValueAtom(f"{result.strip()}")]

    def having(self, atom):
        result = RdfHelper.__filter_inner(atom)
        return [ValueAtom(f"having ({result})")]

    @staticmethod
    def repr_children(atom):
        if hasattr(atom, 'get_children'):
            children = atom.get_children()
            return [get_string_value(child) for child in children]
        return get_string_value(atom)

    def where(self, atom, function):
        conditions, _ = self.__get_conditions_from_children(atom)
        result = "WHERE{\n" if function == "where" else "{\n"
        result += " .\n".join(conditions) + "}"
        return [ValueAtom(result)]

    def service(self, atom):
        result = ""
        conditions, _ = self.__get_conditions_from_children(atom)
        if len(conditions) > 0:
            result += "service "
        for condition in conditions:
            if " " not in condition:
                result += f" {condition} "
            else:
                result += f"{{{condition}}}"
        return [ValueAtom(result)]

    def __get_conditions_from_children(self, atom):
        '''

        :param atom:
        :return:  joins triples and another arguments into one string :
        (?craft foaf:name "Apollo 7" ) (  ?craft foaf:homepage ?homepage) ->    {?craft foaf:name "Apollo 7" .   ?craft foaf:homepage ?homepage}
        '''
        conditions = []
        # simple means that function has one argument consisting of one ExpressionAtom
        is_simple = True
        if hasattr(atom, 'get_children'):
            children = atom.get_children()
            for child in children:
                condition = RdfHelper.repr_children(child)
                if isinstance(condition, list):
                    is_simple = False
                    conditions.append(" ".join(condition).strip())
                else:
                    conditions.append(condition.strip())
        return conditions, is_simple

    def collect_conditions(self, atom, function: str):
        function = function.lower()
        if hasattr(atom, 'get_children'):
            conditions, is_simple = self.__get_conditions_from_children(atom)
            if function == "sparql_union":
                return [ValueAtom("{{" + "} UNION {".join(conditions) + "}}")]
            # if function is limit, order by, offset, ...
            elif function in RdfHelper.output_options_functions:
                return [ValueAtom(f"{function} " + " ".join(conditions))]
            joiner = " .\n" if not is_simple else " "
            return [ValueAtom(f"{function} {{" + (joiner.join(conditions)) + "}")]
        # order by desc
        elif function == 'order by desc':
            return [ValueAtom(f"{function}({atom})")]
        # limit, group by
        elif function in RdfHelper.output_options_functions:
            return [ValueAtom(f"{function} {atom}")]
        return []

    def execute_query(self, query_atom, function, distinct=False):
        '''
        :param query_atom:
        :param function:
        :param distinct:
        :return: call select, ask or 'describe' query
        '''
        values = []
        try:
            conditions = []
            if hasattr(query_atom, 'get_children'):
                children = query_atom.get_children()
                for child in children:
                    condition = RdfHelper.repr_children(child)
                    conditions.append(" ".join(condition) if isinstance(condition, list) else condition)
            if function == "select" and distinct:
                str_select = "select distinct"
            else:
                str_select = function
            sparql_query = ""
            for key, prefix in self.service_features.prefixes.items():
                sparql_query += f"PREFIX {key}: {prefix}" + "\n"
            sparql_query += str_select + " " + " ".join(conditions)
            if sparql_query:
                from SPARQLWrapper import SPARQLWrapper, JSON
                sparql = SPARQLWrapper(self.service_features.service)
                sparql.setQuery(sparql_query)
                sparql.setReturnFormat(JSON)
                result = sparql.query().convert()
                if 'boolean' in result:
                    values.append(ValueAtom(result['boolean']))
                else:
                    vars = result['head']['vars']
                    for res in result["results"]["bindings"]:
                        row = []
                        for var in vars:
                            row.append(ValueAtom(str(res[var]['value'])))
                        values.append(ValueAtom(row))
        except Exception as error:
            print(error)
        finally:
            return values


@register_atoms
def sql_space_atoms():
    helper = RdfHelper()

    return {
        r"set-sparql-service-type": G(
            OperationObject('set-sparql-service-type', lambda a: helper.set_service_type(a), unwrap=False)),
        'filter':
            OperationAtom('filter', lambda a: helper.filter(a), type_names=['Atom', 'Atom'], unwrap=False),
        'sparql_union':
            G(OperationObject('sparql_union', lambda a: helper.collect_conditions(a, "sparql_union"), unwrap=False)),
        'filter_not_exists':
            G(OperationObject('filter_not_exists', lambda a: helper.collect_conditions(a, "filter not exists"),
                              unwrap=False)),
        'minus':
            G(OperationObject('minus', lambda a: helper.collect_conditions(a, "minus"),
                              unwrap=False)),
        'filter_exists':
            G(OperationObject('filter_exists', lambda a: helper.collect_conditions(a, "filter exists"),
                              unwrap=False)),
        'optional':
            G(OperationObject('optional', lambda a: helper.collect_conditions(a, "optional"), unwrap=False)),
        'where':
            G(OperationObject('where', lambda a: helper.where(a, "where"), unwrap=False)),
        'conditions':
            G(OperationObject('conditions', lambda a: helper.where(a, "conditions"), unwrap=False)),
        'fields':
            G(OperationObject('fields', lambda a: helper.fields(a), unwrap=False)),
        'limit':
            G(OperationObject('limit', lambda a: helper.collect_conditions(a, "limit"), unwrap=False)),
        'offset':
            G(OperationObject('offset', lambda a: helper.collect_conditions(a, "offset"), unwrap=False)),
        'order_by':
            G(OperationObject('order_by', lambda a: helper.collect_conditions(a, "order by"), unwrap=False)),
        'order_by_desc':
            G(OperationObject('order_by_desc', lambda a: helper.collect_conditions(a, "order by desc"), unwrap=False)),
        'select':
            G(OperationObject('select', lambda a: helper.execute_query(a, "select"), unwrap=False)),
        'ask':
            G(OperationObject('ask', lambda a: helper.execute_query(a, "ask"), unwrap=False)),
        'describe':
            G(OperationObject('describe', lambda a: helper.execute_query(a, "describe"), unwrap=False)),
        'select_distinct':
            G(OperationObject('select_distinct', lambda a: helper.execute_query(a, "select", True), unwrap=False)),
        'group_by':
            G(OperationObject('group_by', lambda a: helper.collect_conditions(a, "group by"), unwrap=False)),
        'having':
            OperationAtom('having', lambda a: helper.having(a), type_names=['Atom', 'Atom'], unwrap=False),
        'service':
            G(OperationObject('where', lambda a: helper.service(a), unwrap=False)),

    }
