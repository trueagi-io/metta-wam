import re

# Mapping of MeTTaLog constructs to Prolog
METTALOG_TO_PROLOG = {
    # Comments
    r"^;": r"%",  # Convert comments
    # Basic function declaration and equivalence
    r"\(: (\w+) \(-> (.+)\)\)": r"% Function Declaration: \1 :: \2",
    r"=\s*\(([\w-]+) (.+)\)": r"\1(\2) :-",
    # Basic operators
    r"\(\+= ([^()]+) ([^()]+)\)": r"\1 is \1 + \2",
    r"\(-= ([^()]+) ([^()]+)\)": r"\1 is \1 - \2",
    r"\(== ([^()]+) ([^()]+)\)": r"\1 =:= \2",
    r"\(!= ([^()]+) ([^()]+)\)": r"\1 =\= \2",
    r"\(< ([^()]+) ([^()]+)\)": r"\1 < \2",
    r"\(> ([^()]+) ([^()]+)\)": r"\1 > \2",
    r"\(<= ([^()]+) ([^()]+)\)": r"\1 =< \2",
    r"\(>= ([^()]+) ([^()]+)\)": r"\1 >= \2",
    r"\(not ([^()]+)\)": r"not(\1)",
    # If statements
    r"\(if ([^()]+) ([^()]+) ([^()]+)\)": r"(\1 -> \2 ; \3)",
    # Function calls and nested calls
    r"\((\w+) ([^()]+)\)": r"\1(\2)",
    r"\((\w+) ([^()]+) ([^()]+)\)": r"\1(\2, \3)",
    # Lists
    r"\((car-atom) ([^()]+)\)": r"[\1|_]",
    r"\((cdr-atom) ([^()]+)\)": r"[_|\1]",
    r"\(\(\(([^()]+)\)\)\)": r"\[\1\]",
}

# Translation function
def translate_mettalog_to_prolog(mettalog_code):
    prolog_code = mettalog_code
    for pattern, replacement in METTALOG_TO_PROLOG.items():
        prolog_code = re.sub(pattern, replacement, prolog_code, flags=re.MULTILINE)
    return prolog_code

# Example usage
if __name__ == "__main__":
    # MeTTaLog Code with Function Declarations and Comments
    mettalog_code = """
    ; This is a comment
    (: car-atom (-> Atom Atom))
    (: cdr-atom (-> Atom Atom))
    (= (car-atom-or-fail $atom)
       (if (== $atom ()) 
           (let 1 2 3) 
           (car-atom $atom)))
    (= (range $x $y)
       (if (== $x $y)
           $x
           (let $z (range (+ $x 1) $y)
               (cons-atom $x $z))))
    """

    print("Original MeTTaLog Code:\n")
    print(mettalog_code)
    
    print("\nTranslated Prolog Code:\n")
    print(translate_mettalog_to_prolog(mettalog_code))

